{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | The client
module Calamity.Client.Client
    ( Client(..)
    , CalamityEvent(Dispatch, ShutDown)
    , react
    , runBotIO
    , stopBot
    , sendPresence
    , events
    , fire
    , customEvt ) where

import           Calamity.Cache.Eff
import           Calamity.Client.ShardManager
import           Calamity.Client.Types
import           Calamity.Gateway.DispatchEvents
import           Calamity.Gateway.Types
import           Calamity.HTTP.Internal.Ratelimit
import qualified Calamity.Internal.SnowflakeMap   as SM
import           Calamity.Internal.Updateable
import           Calamity.Internal.Utils
import           Calamity.Metrics.Eff
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.Guild
import           Calamity.Types.Model.Presence    ( Presence(..) )
import           Calamity.Types.Snowflake
import           Calamity.Types.Token

import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad

import           Data.Default.Class
import           Data.Dynamic
import           Data.Foldable
import           Data.Maybe
import           Data.Proxy
import           Data.Time.Clock.POSIX
import           Data.Traversable
import           Data.Typeable

import qualified DiPolysemy                       as Di

import           Fmt

import qualified Polysemy                         as P
import qualified Polysemy.Async                   as P
import qualified Polysemy.AtomicState             as P
import qualified Polysemy.Error                   as P
import qualified Polysemy.Fail                    as P
import qualified Polysemy.Reader                  as P

timeA :: P.Member (P.Embed IO) r => P.Sem r a -> P.Sem r (Double, a)
timeA m = do
  start <- P.embed getPOSIXTime
  res <- m
  end <- P.embed getPOSIXTime
  let duration = fromRational . toRational $ end - start
  pure (duration, res)


newClient :: Token -> IO Client
newClient token = do
  shards'        <- newTVarIO []
  numShards'     <- newEmptyMVar
  rlState'       <- newRateLimitState
  (inc, outc)    <- newChan

  pure $ Client shards'
                numShards'
                token
                rlState'
                inc
                outc

runBotIO :: (P.Members '[P.Embed IO, P.Final IO, P.Fail, CacheEff, MetricEff] r, Typeable r) => Token -> SetupEff r -> P.Sem r ()
runBotIO token setup = do
  client <- P.embed $ newClient token
  handlers <- P.embed $ newTVarIO def
  P.asyncToIOFinal . P.runAtomicStateTVar handlers . P.runReader client . Di.runDiToStderrIO $ do
    setup
    shardBot
    clientLoop
    finishUp

react :: forall s r. (BotC r, InsertEventHandler s (P.Sem r)) => EHType s (P.Sem r) -> P.Sem r ()
react handler = let handlers = makeEventHandlers (Proxy @s) (Proxy @(P.Sem r)) handler
                in P.atomicModify (handlers <>)

fire :: BotC r => CalamityEvent -> P.Sem r ()
fire e = do
  inc <- P.asks (^. #eventsIn)
  P.embed $ writeChan inc e

-- | Build a Custom CalamityEvent
customEvt :: forall s a. (Typeable s, Typeable a) => a -> CalamityEvent
customEvt x = Custom (typeRep $ Proxy @s) (toDyn x)

events :: BotC r => P.Sem r (OutChan CalamityEvent)
events = do
  inc <- P.asks (^. #eventsIn)
  P.embed $ dupChan inc

sendPresence :: BotC r => StatusUpdateData -> P.Sem r ()
sendPresence s = do
  shards <- P.asks (^. #shards) >>= P.embed . readTVarIO
  for_ shards $ \(inc, _) ->
    P.embed $ writeChan inc (SendPresence s)

stopBot :: BotC r => P.Sem r ()
stopBot = do
  debug "stopping bot"
  shards <- P.asks (^. #shards) >>= P.embed . readTVarIO
  for_ shards $ \(inc, _) ->
    P.embed $ writeChan inc ShutDownShard
  inc <- P.asks (^. #eventsIn)
  P.embed $ writeChan inc ShutDown

finishUp :: BotC r => P.Sem r ()
finishUp = do
  debug "finishing up"
  shards <- P.asks (^. #shards) >>= P.embed . readTVarIO
  for_ shards $ \(_, shardThread) -> P.await shardThread
  debug "bot has stopped"

-- | main loop of the client, handles fetching the next event, processing the event
-- and invoking it's handler functions
clientLoop :: BotC r => P.Sem r ()
clientLoop = do
  outc <- P.asks (^. #eventsOut)
  void . P.runError . forever $ do
    evt' <- P.embed $ readChan outc
    case evt' of
      Dispatch evt -> P.raise $ handleEvent evt
      Custom s d   -> handleCustomEvent s d
      ShutDown     -> P.throw ()
  debug "leaving client loop"

handleCustomEvent :: forall r. BotC r => TypeRep -> Dynamic -> P.Sem r ()
handleCustomEvent s d = do
  debug "handling a custom event"
  eventHandlers <- P.atomicGet

  for_ (getCustomEventHandlers s (dynTypeRep d) eventHandlers) (\h -> fromJust . fromDynamic @(P.Sem r ()) $ dynApp h d)

handleEvent :: BotC r => DispatchData -> P.Sem r ()
handleEvent data' = do
  debug "handling an event"
  eventHandlers <- P.atomicGet

  actions <- P.runFail $ do
    cacheUpdateHisto <- registerHistogram "cache_update" mempty [10, 20..100]
    (time, res) <- timeA $ handleEvent' eventHandlers data'
    void $ observeHistogram time cacheUpdateHisto
    pure res

  eventHandleHisto <- registerHistogram "event_handle" mempty [10, 20..100]

  case actions of
    Right actions -> for_ actions $ \action -> P.async $ do
      (time, _) <- timeA action
      void $ observeHistogram time eventHandleHisto
    Left err      -> debug $ "Failed handling actions for event: " +| err |+ ""

-- NOTE: We have to be careful with how we run event handlers
--       They're registered through `react` which ensures the value of `r` in the event handler
--       is the same as the final value of `r`, but:
--       because they're held inside a 'Dynamic' to prevent the value of `r` being recursive,
--       we have to make sure that we don't accidentally try to execute the event handler inside a
--       nested effect, ie: `P.runError $ {- Handle events here -}` since that will result the value of
--       `r` where we handle events be: `(P.Error a ': r)`, which will make stuff explode when we unwrap the
--       event handlers

handleEvent' :: BotC r
              => EventHandlers
              -> DispatchData
              -> P.Sem (P.Fail ': r) [P.Sem r ()]
handleEvent' eh evt@(Ready rd@ReadyData {}) = do
  updateCache evt
  pure $ map ($ rd) (getEventHandlers @'ReadyEvt eh)

handleEvent' _ Resumed = pure []

handleEvent' eh evt@(ChannelCreate (DMChannel' chan)) = do
  updateCache evt
  Just newChan <- DMChannel' <<$>> getDM (getID chan)
  pure $ map ($ newChan) (getEventHandlers @'ChannelCreateEvt eh)

handleEvent' eh evt@(ChannelCreate (GuildChannel' chan)) = do
  updateCache evt
  Just guild <- getGuild (getID chan)
  Just newChan <- pure $ GuildChannel' <$> guild ^. #channels . at (getID chan)
  pure $ map ($ newChan) (getEventHandlers @'ChannelCreateEvt eh)

handleEvent' eh evt@(ChannelUpdate (DMChannel' chan)) = do
  Just oldChan <- DMChannel' <<$>> getDM (getID chan)
  updateCache evt
  Just newChan <- DMChannel' <<$>> getDM (getID chan)
  pure $ map (\f -> f oldChan newChan) (getEventHandlers @'ChannelUpdateEvt eh)

handleEvent' eh evt@(ChannelUpdate (GuildChannel' chan)) = do
  Just oldGuild <- getGuild (getID chan)
  Just oldChan <- pure $ GuildChannel' <$> oldGuild ^. #channels . at (getID chan)
  updateCache evt
  Just newGuild <- getGuild (getID chan)
  Just newChan <- pure $ GuildChannel' <$> newGuild ^. #channels . at (getID chan)
  pure $ map (\f -> f oldChan newChan) (getEventHandlers @'ChannelUpdateEvt eh)

handleEvent' eh evt@(ChannelDelete (GuildChannel' chan)) = do
  Just oldGuild <- getGuild (getID chan)
  Just oldChan <- pure $ GuildChannel' <$> oldGuild ^. #channels . at (getID chan)
  updateCache evt
  pure $ map (\f -> f oldChan) (getEventHandlers @'ChannelDeleteEvt eh)

handleEvent' eh evt@(ChannelDelete (DMChannel' chan)) = do
  Just oldChan <- DMChannel' <<$>> getDM (getID chan)
  updateCache evt
  pure $ map (\f -> f oldChan) (getEventHandlers @'ChannelDeleteEvt eh)

-- handleEvent' eh evt@(ChannelPinsUpdate ChannelPinsUpdateData { channelID, lastPinTimestamp }) = do
--   chan <- (GuildChannel' <$> os ^? #channels . at (coerceSnowflake channelID) . _Just)
--     <|> (DMChannel' <$> os ^? #dms . at (coerceSnowflake channelID) . _Just)
--   pure $ map (\f -> f chan lastPinTimestamp) (getEventHandlers @"channelpinsupdate" eh)

handleEvent' eh evt@(GuildCreate guild) = do
  isNew <- isUnavailableGuild (getID guild)
  updateCache evt
  Just guild <- getGuild (getID guild)
  pure $ map (\f -> f guild isNew) (getEventHandlers @'GuildCreateEvt eh)

handleEvent' eh evt@(GuildUpdate guild) = do
  Just oldGuild <- getGuild (getID guild)
  updateCache evt
  Just newGuild <- getGuild (getID guild)
  pure $ map (\f -> f oldGuild newGuild) (getEventHandlers @'GuildUpdateEvt eh)

-- NOTE: Guild will be deleted in the new cache if unavailable was false
handleEvent' eh evt@(GuildDelete UnavailableGuild { id, unavailable }) = do
  Just oldGuild <- getGuild id
  updateCache evt
  pure $ map (\f -> f oldGuild unavailable) (getEventHandlers @'GuildDeleteEvt eh)

handleEvent' eh evt@(GuildBanAdd BanData { guildID, user }) = do
  Just guild <- getGuild guildID
  updateCache evt
  pure $ map (\f -> f guild user) (getEventHandlers @'GuildBanAddEvt eh)

handleEvent' eh evt@(GuildBanRemove BanData { guildID, user }) = do
  Just guild <- getGuild guildID
  updateCache evt
  pure $ map (\f -> f guild user) (getEventHandlers @'GuildBanRemoveEvt eh)

-- NOTE: we fire this event using the guild data with old emojis
handleEvent' eh evt@(GuildEmojisUpdate GuildEmojisUpdateData { guildID, emojis }) = do
  Just guild <- getGuild guildID
  updateCache evt
  pure $ map (\f -> f guild emojis) (getEventHandlers @'GuildEmojisUpdateEvt eh)

handleEvent' eh evt@(GuildIntegrationsUpdate GuildIntegrationsUpdateData { guildID }) = do
  updateCache evt
  Just guild <- getGuild guildID
  pure $ map ($ guild) (getEventHandlers @'GuildIntegrationsUpdateEvt eh)

handleEvent' eh evt@(GuildMemberAdd member) = do
  updateCache evt
  Just guild <- getGuild (getID member)
  Just member <- pure $ guild ^. #members . at (getID member)
  pure $ map ($ member) (getEventHandlers @'GuildMemberAddEvt eh)

handleEvent' eh evt@(GuildMemberRemove GuildMemberRemoveData { user, guildID }) = do
  Just guild <- getGuild guildID
  Just member <- pure $ guild ^. #members . at (getID user)
  updateCache evt
  pure $ map ($ member) (getEventHandlers @'GuildMemberRemoveEvt eh)

handleEvent' eh evt@(GuildMemberUpdate GuildMemberUpdateData { user, guildID }) = do
  Just oldGuild <- getGuild guildID
  Just oldMember <- pure $ oldGuild ^. #members . at (getID user)
  updateCache evt
  Just newGuild <- getGuild guildID
  Just newMember <- pure $ newGuild ^. #members . at (getID user)
  pure $ map (\f -> f oldMember newMember) (getEventHandlers @'GuildMemberUpdateEvt eh)

handleEvent' eh evt@(GuildMembersChunk GuildMembersChunkData { members, guildID }) = do
  updateCache evt
  Just guild <- getGuild guildID
  let members' = guild ^.. #members . foldMap (at . getID) members . _Just
  pure $ map (\f -> f guild members') (getEventHandlers @'GuildMembersChunkEvt eh)

handleEvent' eh evt@(GuildRoleCreate GuildRoleData { guildID, role }) = do
  updateCache evt
  Just guild <- getGuild guildID
  Just role' <- pure $ guild ^. #roles . at (getID role)
  pure $ map (\f -> f guild role') (getEventHandlers @'GuildRoleCreateEvt eh)

handleEvent' eh evt@(GuildRoleUpdate GuildRoleData { guildID, role }) = do
  Just oldGuild <- getGuild guildID
  Just oldRole <- pure $ oldGuild ^. #roles . at (getID role)
  updateCache evt
  Just newGuild <- getGuild guildID
  Just newRole <- pure $ newGuild ^. #roles . at (getID role)
  pure $ map (\f -> f newGuild oldRole newRole) (getEventHandlers @'GuildRoleUpdateEvt eh)

handleEvent' eh evt@(GuildRoleDelete GuildRoleDeleteData { guildID, roleID }) = do
  Just guild <- getGuild guildID
  Just role <- pure $ guild ^. #roles . at roleID
  updateCache evt
  pure $ map (\f -> f guild role) (getEventHandlers @'GuildRoleDeleteEvt eh)

handleEvent' eh evt@(MessageCreate msg) = do
  messagesReceived <- registerCounter "messages_received" mempty
  void $ addCounter 1 messagesReceived
  updateCache evt
  pure $ map ($ msg) (getEventHandlers @'MessageCreateEvt eh)

handleEvent' eh evt@(MessageUpdate msg) = do
  Just oldMsg <- getMessage (getID msg)
  updateCache evt
  Just newMsg <- getMessage (getID msg)
  pure $ map (\f -> f oldMsg newMsg) (getEventHandlers @'MessageUpdateEvt eh)

handleEvent' eh evt@(MessageDelete MessageDeleteData { id }) = do
  Just oldMsg <- getMessage id
  updateCache evt
  pure $ map ($ oldMsg) (getEventHandlers @'MessageDeleteEvt eh)

handleEvent' eh evt@(MessageDeleteBulk MessageDeleteBulkData { ids }) = do
  messages <- catMaybes <$> mapM getMessage ids
  updateCache evt
  join <$> for messages (\msg -> pure $ map ($ msg) (getEventHandlers @'MessageDeleteEvt eh))

handleEvent' eh evt@(MessageReactionAdd reaction) = do
  updateCache evt
  Just msg <- getMessage (getID reaction)
  pure $ map (\f -> f msg reaction) (getEventHandlers @'MessageReactionAddEvt eh)

handleEvent' eh evt@(MessageReactionRemove reaction) = do
  Just msg <- getMessage (getID reaction)
  updateCache evt
  pure $ map (\f -> f msg reaction) (getEventHandlers @'MessageReactionRemoveEvt eh)

handleEvent' eh evt@(MessageReactionRemoveAll MessageReactionRemoveAllData { messageID }) = do
  Just msg <- getMessage messageID
  updateCache evt
  pure $ map ($ msg) (getEventHandlers @'MessageReactionRemoveAllEvt eh)

handleEvent' eh evt@(PresenceUpdate PresenceUpdateData { userID, presence = Presence { guildID } }) = do
  Just oldGuild <- getGuild guildID
  Just oldMember <- pure $ oldGuild ^. #members . at (coerceSnowflake userID)
  updateCache evt
  Just newGuild <- getGuild guildID
  Just newMember <- pure $ newGuild ^. #members . at (coerceSnowflake userID)
  let userUpdates = if oldMember ^. #user /= newMember ^. #user
                    then map (\f -> f (oldMember ^. #user) (newMember ^. #user)) (getEventHandlers @'UserUpdateEvt eh)
                    else mempty
  pure $ userUpdates <> map (\f -> f oldMember newMember) (getEventHandlers @'GuildMemberUpdateEvt eh)

handleEvent' eh (TypingStart TypingStartData { channelID, guildID, userID, timestamp }) =
  case guildID of
    Just gid -> do
      Just guild <- getGuild gid
      Just member <- pure $ guild ^. #members . at (coerceSnowflake userID)
      Just chan <- pure $ GuildChannel' <$> guild ^. #channels . at (coerceSnowflake channelID)
      pure $ map (\f -> f chan (Just member) timestamp) (getEventHandlers @'TypingStartEvt eh)
    Nothing -> do
      Just chan <- DMChannel' <<$>> getDM (coerceSnowflake channelID)
      pure $ map (\f -> f chan Nothing timestamp) (getEventHandlers @'TypingStartEvt eh)

handleEvent' eh evt@(UserUpdate _) = do
  Just oldUser <- getBotUser
  updateCache evt
  Just newUser <- getBotUser
  pure $ map (\f -> f oldUser newUser) (getEventHandlers @'UserUpdateEvt eh)

handleEvent' _ e = fail $ "Unhandled event: " <> show e

updateCache :: P.Members '[CacheEff, P.Fail] r => DispatchData -> P.Sem r ()
updateCache (Ready ReadyData { user, guilds }) = do
  setBotUser user
  for_ (map getID guilds) setUnavailableGuild

updateCache Resumed = pure ()

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
  updateGuild guildID (#members . ix (getID user) %~ (#roles .~ roles) . (#nick .~ nick))

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

-- we don't handle group channels currently
updateCache (ChannelCreate (GroupChannel' _)) = pure ()
updateCache (ChannelUpdate (GroupChannel' _)) = pure ()
updateCache (ChannelDelete (GroupChannel' _)) = pure ()

-- these don't modify state
updateCache (GuildBanAdd _) = pure ()
updateCache (GuildBanRemove _) = pure ()
updateCache (GuildIntegrationsUpdate _) = pure ()
updateCache (TypingStart _) = pure ()
updateCache (ChannelPinsUpdate _) = pure ()
updateCache (WebhooksUpdate _) = pure ()

-- we don't handle voice state currently
updateCache (VoiceStateUpdate _) = pure ()
updateCache (VoiceServerUpdate _) = pure ()
