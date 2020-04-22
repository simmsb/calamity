-- | The client
module Calamity.Client.Client
    ( Client(..)
    , react
    , runBotIO
    , stopBot ) where

import           Calamity.Cache.Eff
import           Calamity.Client.ShardManager
import           Calamity.Client.Types
import           Calamity.Gateway.DispatchEvents
import           Calamity.Gateway.Types
import           Calamity.HTTP.Internal.Ratelimit
import qualified Calamity.Internal.SnowflakeMap              as SM
import           Calamity.Internal.Updateable
import           Calamity.Internal.Utils
import           Calamity.LogEff
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.Guild.UnavailableGuild
import           Calamity.Types.Model.Presence               ( Presence(..) )
import           Calamity.Types.Snowflake
import           Calamity.Types.Token

import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad

import           Data.Default.Class
import           Data.Dynamic
import           Data.Foldable
import           Data.Maybe
import           Data.Traversable
import qualified Data.TypeRepMap                             as TM

import qualified DiPolysemy                                  as Di

import           Fmt

import           GHC.TypeLits

import           Polysemy                                    ( Sem )
import qualified Polysemy                                    as P
import qualified Polysemy.Async                              as P
import qualified Polysemy.AtomicState                        as P
import qualified Polysemy.Error                              as P
import qualified Polysemy.Fail                               as P
import qualified Polysemy.Reader                             as P


newClient :: Token -> IO Client
newClient token = do
  shards'        <- newTVarIO []
  numShards'     <- newEmptyMVar
  rlState'       <- newRateLimitState
  eventQueue'    <- newTQueueIO

  pure $ Client shards'
                numShards'
                token
                rlState'
                eventQueue'

type SetupEff r = Sem (LogEff ': P.Reader Client ': P.AtomicState EventHandlers ': P.Async ': r) ()

runBotIO :: (P.Members '[P.Embed IO, P.Final IO, CacheEff] r, Typeable r) => Token -> SetupEff r -> Sem r ()
runBotIO token setup = do
  client <- P.embed $ newClient token
  handlers <- P.embed $ newTVarIO def
  P.asyncToIOFinal . P.runAtomicStateTVar handlers . P.runReader client . Di.runDiToStderrIO $ do
    setup
    shardBot
    clientLoop
    finishUp

react :: forall (s :: Symbol) r. (KnownSymbol s, BotC r, EHType' s ~ Dynamic, Typeable (EHType s (Sem r))) => EHType s (Sem r) -> Sem r ()
react f =
  let handlers = EventHandlers . TM.one $ EH @s [toDyn f]
  in P.atomicModify (handlers <>)

stopBot :: BotC r => Sem r ()
stopBot = do
  debug "stopping bot"
  shards <- P.asks (^. #shards) >>= P.embed . readTVarIO
  for_ shards $ \shard ->
    P.embed . atomically $ writeTQueue (shard ^. _1 . #cmdQueue) ShutDownShard
  eventQueue <- P.asks (^. #eventQueue)
  P.embed . atomically $ writeTQueue eventQueue ShutDown

finishUp :: BotC r => Sem r ()
finishUp = do
  debug "finishing up"
  shards <- P.asks (^. #shards) >>= P.embed . readTVarIO
  for_ shards $ \shard -> void . P.await $ (shard ^. _2)
  debug "bot has stopped"

-- | main loop of the client, handles fetching the next event, processing the event
-- and invoking it's handler functions
clientLoop :: BotC r => Sem r ()
clientLoop = do
  evtQueue <- P.asks (^. #eventQueue)
  void . P.runError . forever $ do
    evt' <- P.embed . atomically $ readTQueue evtQueue
    case evt' of
      DispatchData' evt -> P.raise $ handleEvent evt
      ShutDown          -> P.throw ()
  debug "leaving client loop"

handleEvent :: BotC r => DispatchData -> Sem r ()
handleEvent data' = do
  debug "handling an event"
  eventHandlers <- P.atomicGet
  actions <- P.runFail $ handleEvent' eventHandlers data'
  case actions of
    Right actions -> for_ actions P.async
    Left err      -> debug $ "Failed handling actions for event: " +| err |+ ""

-- NOTE: We have to be careful with how we run event handlers
--       They're registered through `react` which ensures the value of `r` in the event handler
--       is the same as the final value of `r`, but:
--       because they're held inside a 'Dynamic' to prevent the value of `r` being recursive,
--       we have to make sure that we don't accidentally try to execute the event handler inside a
--       nested effect, ie: `P.runError $ {- Handle events here -}` since that will result the value of
--       `r` where we handle events be: `(P.Error a ': r)`, which will make stuff explode when we unwrap the
--       event handlers

unwrapEvent :: forall s r.
            (KnownSymbol s, EHType' s ~ Dynamic, Typeable r, Typeable (EHType s (Sem r)))
            => EventHandlers
            -> [EHType s (Sem r)]
unwrapEvent (EventHandlers eh) = map (fromJust . fromDynamic) . unwrapEventHandler @s . fromJust
  $ (TM.lookup eh :: Maybe (EventHandler s))
-- where unwrapEach handler =
--         let msg = "wanted: " <> show (typeRep $ Proxy @r) <> ", got: " <> show (dynTypeRep handler)
--         in unwrapEvt msg . fromDynamic $ handler

handleEvent' :: BotC r
              => EventHandlers
              -> DispatchData
              -> Sem (P.Fail ': r) [Sem r ()]
handleEvent' eh evt@(Ready rd@ReadyData { user, guilds }) = do
  updateCache evt
  pure $ map ($ rd) (unwrapEvent @"ready" eh)

handleEvent' eh evt@(ChannelCreate (DMChannel' chan)) = do
  updateCache evt
  Just newChan <- DMChannel' <<$>> getDM (getID chan)
  pure $ map ($ newChan) (unwrapEvent @"channelcreate" eh)

handleEvent' eh evt@(ChannelCreate (GuildChannel' chan)) = do
  updateCache evt
  Just guild <- getGuild (getID chan)
  Just newChan <- pure $ GuildChannel' <$> guild ^. #channels . at (getID chan)
  pure $ map ($ newChan) (unwrapEvent @"channelcreate" eh)

handleEvent' eh evt@(ChannelUpdate (DMChannel' chan)) = do
  Just oldChan <- DMChannel' <<$>> getDM (getID chan)
  updateCache evt
  Just newChan <- DMChannel' <<$>> getDM (getID chan)
  pure $ map (\f -> f oldChan newChan) (unwrapEvent @"channelupdate" eh)

handleEvent' eh evt@(ChannelUpdate (GuildChannel' chan)) = do
  Just oldGuild <- getGuild (getID chan)
  Just oldChan <- pure $ GuildChannel' <$> oldGuild ^. #channels . at (getID chan)
  updateCache evt
  Just newGuild <- getGuild (getID chan)
  Just newChan <- pure $ GuildChannel' <$> newGuild ^. #channels . at (getID chan)
  pure $ map (\f -> f oldChan newChan) (unwrapEvent @"channelupdate" eh)

handleEvent' eh evt@(ChannelDelete (GuildChannel' chan)) = do
  Just oldGuild <- getGuild (getID chan)
  Just oldChan <- pure $ GuildChannel' <$> oldGuild ^. #channels . at (getID chan)
  updateCache evt
  pure $ map (\f -> f oldChan) (unwrapEvent @"channeldelete" eh)

handleEvent' eh evt@(ChannelDelete (DMChannel' chan)) = do
  Just oldChan <- DMChannel' <<$>> getDM (getID chan)
  updateCache evt
  pure $ map (\f -> f oldChan) (unwrapEvent @"channeldelete" eh)

-- handleEvent' eh evt@(ChannelPinsUpdate ChannelPinsUpdateData { channelID, lastPinTimestamp }) = do
--   chan <- (GuildChannel' <$> os ^? #channels . at (coerceSnowflake channelID) . _Just)
--     <|> (DMChannel' <$> os ^? #dms . at (coerceSnowflake channelID) . _Just)
--   pure $ map (\f -> f chan lastPinTimestamp) (unwrapEvent @"channelpinsupdate" eh)

handleEvent' eh evt@(GuildCreate guild) = do
  isNew <- isUnavailableGuild (getID guild)
  updateCache evt
  Just guild <- getGuild (getID guild)
  pure $ map (\f -> f guild isNew) (unwrapEvent @"guildcreate" eh)

handleEvent' eh evt@(GuildUpdate guild) = do
  Just oldGuild <- getGuild (getID guild)
  updateCache evt
  Just newGuild <- getGuild (getID guild)
  pure $ map (\f -> f oldGuild newGuild) (unwrapEvent @"guildupdate" eh)

-- NOTE: Guild will be deleted in the new cache if unavailable was false
handleEvent' eh evt@(GuildDelete UnavailableGuild { id, unavailable }) = do
  Just oldGuild <- getGuild id
  updateCache evt
  pure $ map (\f -> f oldGuild unavailable) (unwrapEvent @"guilddelete" eh)

handleEvent' eh evt@(GuildBanAdd GuildBanData { guildID, user }) = do
  Just guild <- getGuild guildID
  updateCache evt
  pure $ map (\f -> f guild user) (unwrapEvent @"guildbanadd" eh)

handleEvent' eh evt@(GuildBanRemove GuildBanData { guildID, user }) = do
  Just guild <- getGuild guildID
  updateCache evt
  pure $ map (\f -> f guild user) (unwrapEvent @"guildbanremove" eh)

-- NOTE: we fire this event using the guild data with old emojis
handleEvent' eh evt@(GuildEmojisUpdate GuildEmojisUpdateData { guildID, emojis }) = do
  Just guild <- getGuild guildID
  updateCache evt
  pure $ map (\f -> f guild emojis) (unwrapEvent @"guildemojisupdate" eh)

handleEvent' eh evt@(GuildIntegrationsUpdate GuildIntegrationsUpdateData { guildID }) = do
  updateCache evt
  Just guild <- getGuild guildID
  pure $ map ($ guild) (unwrapEvent @"guildintegrationsupdate" eh)

handleEvent' eh evt@(GuildMemberAdd member) = do
  updateCache evt
  Just guild <- getGuild (getID member)
  Just member <- pure $ guild ^. #members . at (getID member)
  pure $ map ($ member) (unwrapEvent @"guildmemberadd" eh)

handleEvent' eh evt@(GuildMemberRemove GuildMemberRemoveData { user, guildID }) = do
  Just guild <- getGuild guildID
  Just member <- pure $ guild ^. #members . at (getID user)
  updateCache evt
  pure $ map ($ member) (unwrapEvent @"guildmemberremove" eh)

handleEvent' eh evt@(GuildMemberUpdate GuildMemberUpdateData { user, guildID }) = do
  Just oldGuild <- getGuild guildID
  Just oldMember <- pure $ oldGuild ^. #members . at (getID user)
  updateCache evt
  Just newGuild <- getGuild guildID
  Just newMember <- pure $ newGuild ^. #members . at (getID user)
  pure $ map (\f -> f oldMember newMember) (unwrapEvent @"guildmemberupdate" eh)

handleEvent' eh evt@(GuildMembersChunk GuildMembersChunkData { members, guildID }) = do
  updateCache evt
  Just guild <- getGuild guildID
  let members' = guild ^.. #members . foldMap (at . getID) members . _Just
  pure $ map (\f -> f guild members') (unwrapEvent @"guildmemberschunk" eh)

handleEvent' eh evt@(GuildRoleCreate GuildRoleData { guildID, role }) = do
  updateCache evt
  Just guild <- getGuild guildID
  Just role' <- pure $ guild ^. #roles . at (getID role)
  pure $ map (\f -> f guild role') (unwrapEvent @"guildrolecreate" eh)

handleEvent' eh evt@(GuildRoleUpdate GuildRoleData { guildID, role }) = do
  Just oldGuild <- getGuild guildID
  Just oldRole <- pure $ oldGuild ^. #roles . at (getID role)
  updateCache evt
  Just newGuild <- getGuild guildID
  Just newRole <- pure $ newGuild ^. #roles . at (getID role)
  pure $ map (\f -> f newGuild oldRole newRole) (unwrapEvent @"guildroleupdate" eh)

handleEvent' eh evt@(GuildRoleDelete GuildRoleDeleteData { guildID, roleID }) = do
  Just guild <- getGuild guildID
  Just role <- pure $ guild ^. #roles . at roleID
  updateCache evt
  pure $ map (\f -> f guild role) (unwrapEvent @"guildroledelete" eh)

handleEvent' eh evt@(MessageCreate msg) = do
  updateCache evt
  pure $ map ($ msg) (unwrapEvent @"messagecreate" eh)

handleEvent' eh evt@(MessageUpdate msg) = do
  Just oldMsg <- getMessage (getID msg)
  updateCache evt
  Just newMsg <- getMessage (getID msg)
  pure $ map (\f -> f oldMsg newMsg) (unwrapEvent @"messageupdate" eh)

handleEvent' eh evt@(MessageDelete MessageDeleteData { id }) = do
  Just oldMsg <- getMessage id
  updateCache evt
  pure $ map ($ oldMsg) (unwrapEvent @"messagedelete" eh)

handleEvent' eh evt@(MessageDeleteBulk MessageDeleteBulkData { ids }) = do
  messages <- catMaybes <$> mapM getMessage ids
  updateCache evt
  join <$> for messages (\msg -> pure $ map ($ msg) (unwrapEvent @"messagedelete" eh))

handleEvent' eh evt@(MessageReactionAdd reaction) = do
  updateCache evt
  Just msg <- getMessage (getID reaction)
  pure $ map (\f -> f msg reaction) (unwrapEvent @"messagereactionadd" eh)

handleEvent' eh evt@(MessageReactionRemove reaction) = do
  Just msg <- getMessage (getID reaction)
  updateCache evt
  pure $ map (\f -> f msg reaction) (unwrapEvent @"messagereactionremove" eh)

handleEvent' eh evt@(MessageReactionRemoveAll MessageReactionRemoveAllData { messageID }) = do
  Just msg <- getMessage messageID
  updateCache evt
  pure $ map ($ msg) (unwrapEvent @"messagereactionremoveall" eh)

handleEvent' eh evt@(PresenceUpdate PresenceUpdateData { userID, presence = Presence { guildID } }) = do
  Just oldGuild <- getGuild guildID
  Just oldMember <- pure $ oldGuild ^. #members . at (coerceSnowflake userID)
  updateCache evt
  Just newGuild <- getGuild guildID
  Just newMember <- pure $ newGuild ^. #members . at (coerceSnowflake userID)
  let userUpdates = if oldMember ^. #user /= newMember ^. #user
                    then map (\f -> f (oldMember ^. #user) (newMember ^. #user)) (unwrapEvent @"userupdate" eh)
                    else mempty
  pure $ userUpdates <> map (\f -> f oldMember newMember) (unwrapEvent @"guildmemberupdate" eh)

handleEvent' eh (TypingStart TypingStartData { channelID, guildID, userID, timestamp }) =
  case guildID of
    Just gid -> do
      Just guild <- getGuild gid
      Just member <- pure $ guild ^. #members . at (coerceSnowflake userID)
      Just chan <- pure $ GuildChannel' <$> guild ^. #channels . at (coerceSnowflake channelID)
      pure $ map (\f -> f chan (Just member) timestamp) (unwrapEvent @"typingstart" eh)
    Nothing -> do
      Just chan <- DMChannel' <<$>> getDM (coerceSnowflake channelID)
      pure $ map (\f -> f chan Nothing timestamp) (unwrapEvent @"typingstart" eh)

handleEvent' eh evt@(UserUpdate _) = do
  Just oldUser <- getBotUser
  updateCache evt
  Just newUser <- getBotUser
  pure $ map (\f -> f oldUser newUser) (unwrapEvent @"userupdate" eh)

handleEvent' _ e = fail $ "Unhandled event: " <> show e

updateCache :: P.Members '[CacheEff, P.Fail] r => DispatchData -> Sem r ()
updateCache (Ready ReadyData { user, guilds }) = do
  setBotUser user
  for_ (map getID guilds) setUnavailableGuild

updateCache (ChannelCreate (DMChannel' chan)) =
  setDM chan

updateCache (ChannelCreate (GuildChannel' chan)) =
  updateGuild (getID chan) (#channels %~ SM.insert chan)

updateCache (ChannelUpdate (DMChannel' chan)) =
  updateDM (getID chan) (update chan)

updateCache (ChannelUpdate (GuildChannel' chan)) =
  updateGuild (getID chan) (#channels . at (getID chan) . _Just %~ update chan)

updateCache (ChannelDelete (DMChannel' chan)) =
  delDM (getID chan)

updateCache (ChannelDelete (GuildChannel' chan)) =
  updateGuild (getID chan) (#channels %~ sans (getID chan))

updateCache (GuildCreate guild) = do
  isNew <- isUnavailableGuild (getID guild)
  when isNew $ delUnavailableGuild (getID guild)
  setGuild guild
  for_ (SM.fromList (guild ^.. #members . traverse . #user)) setUser

updateCache (GuildUpdate guild) =
  updateGuild (getID guild) (update guild)

updateCache (GuildDelete guild) =
  delGuild (getID guild)

updateCache (GuildEmojisUpdate GuildEmojisUpdateData { guildID, emojis }) =
  updateGuild guildID (#emojis .~ SM.fromList emojis)

updateCache (GuildMemberAdd member) = do
  setUser (member ^. #user)
  updateGuild (getID member) (#members . at (getID member) ?~ member)

updateCache (GuildMemberRemove GuildMemberRemoveData { guildID, user }) =
  updateGuild guildID (#members %~ sans (getID user))

updateCache (GuildMemberUpdate GuildMemberUpdateData { guildID, roles, user, nick }) = do
  setUser user
  updateGuild guildID (#members . at (getID user) . _Just %~ (#roles .~ roles) . (#nick .~ nick))

updateCache (GuildMembersChunk GuildMembersChunkData { members }) =
  traverse_ (updateCache . GuildMemberAdd) members

updateCache (GuildRoleCreate GuildRoleData { guildID, role }) =
  updateGuild guildID (#roles %~ SM.insert role)

updateCache (GuildRoleUpdate GuildRoleData { guildID, role }) =
  updateGuild guildID (#roles %~ SM.insert role)

updateCache (GuildRoleDelete GuildRoleDeleteData { guildID, roleID }) =
  updateGuild guildID (#roles %~ sans roleID)

updateCache (MessageCreate msg) = setMessage msg

updateCache (MessageUpdate msg) =
  updateMessage (getID msg) (update msg)

updateCache (MessageDelete MessageDeleteData { id }) = delMessage id

updateCache (MessageDeleteBulk MessageDeleteBulkData { ids }) =
  for_ ids delMessage

updateCache (MessageReactionAdd reaction) =
  updateMessage (getID reaction) (#reactions %~ cons reaction)

updateCache (MessageReactionRemove reaction) =
  updateMessage (getID reaction) (#reactions %~ filter (\r -> r ^. #emoji /= reaction ^. #emoji))

updateCache (MessageReactionRemoveAll MessageReactionRemoveAllData { messageID }) =
  updateMessage messageID (#reactions .~ mempty)

updateCache (PresenceUpdate PresenceUpdateData { userID, roles, presence }) =
  updateGuild (getID presence) ((#members . at (coerceSnowflake userID) . _Just . #roles .~ roles)
                                . (#presences . at userID ?~ presence))

updateCache (UserUpdate user) = setBotUser user

updateCache _data' = pure () -- TODO
