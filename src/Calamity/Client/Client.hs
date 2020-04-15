-- | The client
module Calamity.Client.Client
    ( Client(..)
    , react
    , runBotIO ) where

import           Calamity.Client.ShardManager
import           Calamity.Client.Types
import           Calamity.Gateway.DispatchEvents
import           Calamity.HTTP.Internal.Ratelimit
import           Calamity.Internal.MessageStore
import qualified Calamity.Internal.SnowflakeMap              as SM
import           Calamity.Internal.Updateable
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.Guild.Guild
import           Calamity.Types.Model.Guild.UnavailableGuild
import           Calamity.Types.Model.Presence               ( Presence(..) )
import           Calamity.Types.Snowflake
import           Calamity.Types.Token

import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Control.Lens                                ( (.=) )

import           Data.Default.Class
import           Data.Dynamic
import qualified Data.HashSet                                as LS
import           Data.HashSet.Lens
import           Data.Maybe
import qualified Data.TypeRepMap                             as TM
import qualified Data.Vector                                 as V

import qualified DiPolysemy                                  as Di

import           Polysemy                                    ( Sem )
import qualified Polysemy                                    as P
import qualified Polysemy.Async                              as P
import qualified Polysemy.AtomicState                        as P
import qualified Polysemy.Reader                             as P

import qualified StmContainers.Set                           as TS

newClient :: Token -> IO Client
newClient token = do
  shards'        <- newTVarIO []
  numShards'     <- newEmptyMVar
  rlState'       <- newRateLimitState
  eventQueue'    <- newTQueueIO
  cache'         <- newTVarIO emptyCache
  activeTasks'   <- TS.newIO

  pure $ Client shards'
                numShards'
                token
                rlState'
                eventQueue'
                cache'
                activeTasks'

type SetupEff r = Sem (LogEff ': P.Reader Client ': P.AtomicState EventHandlers ': P.Async ': r) ()

runBotIO :: (P.Members '[P.Embed IO, P.Final IO] r, Typeable r) => Token -> SetupEff r -> Sem r ()
runBotIO token setup = do
  client <- P.embed $ newClient token
  handlers <- P.embed $ newTVarIO def
  P.asyncToIO . P.runAtomicStateTVar handlers . P.runReader client . Di.runDiToStderrIO $ do
    setup
    shardBot
    clientLoop

react :: forall (s :: Symbol) r. (KnownSymbol s, BotC r, EHType' s ~ Dynamic, Typeable (EHType s (Sem r))) => EHType s (Sem r) -> Sem r ()
react f =
  let handlers = EventHandlers . TM.one $ EH @s [toDyn f]
  in P.atomicModify (handlers <>)

emptyCache :: Cache
emptyCache = Cache Nothing SM.empty SM.empty SM.empty SM.empty LS.empty def

-- | main loop of the client, handles fetching the next event, processing the event
-- and invoking it's handler functions
clientLoop :: BotC r => Sem r ()
clientLoop = do
  evtQueue <- P.asks eventQueue
  forever $ do
    evt <- P.embed . atomically $ readTQueue evtQueue
    handleEvent evt

handleEvent :: BotC r => DispatchData -> Sem r ()
handleEvent data' = do
  debug "handling an event"
  cache' <- P.asks cache
  (oldCache, newCache) <- P.embed . atomically $ do
    oldCache <- readTVar cache'
    let newCache = execState (updateCache data') oldCache
    writeTVar cache' newCache
    pure (oldCache, newCache)

  runEventHandlers oldCache newCache data'

runEventHandlers :: BotC r => Cache -> Cache -> DispatchData -> Sem r ()
runEventHandlers oldCache newCache data' = do
  eventHandlers <- P.atomicGet
  let actionHandlers = handleActions oldCache newCache eventHandlers data'
  case actionHandlers of
    Just actions -> for_ actions $ \action -> P.async $ action newCache
    Nothing
      -> debug $ "Failed handling actions for event: " +|| data' ||+ ""

unwrapEvent :: forall s r. (KnownSymbol s, EHType' s ~ Dynamic, Typeable (EHType s (Sem r))) => EventHandlers -> [EHType s (Sem r)]
unwrapEvent (EventHandlers eh) = map (fromJust . fromDynamic) . unwrapEventHandler @s . fromJust $ (TM.lookup eh :: Maybe (EventHandler s))

handleActions :: BotC r
              => Cache -- ^ The old cache
              -> Cache -- ^ The new cache
              -> EventHandlers
              -> DispatchData
              -> Maybe [Cache -> Sem r ()]
handleActions _ _ eh (Ready rd) = pure $ map ($ rd) (unwrapEvent @"ready" eh)

handleActions _ ns eh (ChannelCreate (DMChannel' chan)) = do
  newChan' <- DMChannel' <$> ns ^? #dms . at (getID chan) . _Just
  pure $ map ($ newChan') (unwrapEvent @"channelcreate" eh)

handleActions _ ns eh (ChannelCreate (GuildChannel' chan)) = do
  newChan' <- GuildChannel' <$> ns ^? #channels . at (getID chan) . _Just
  pure $ map ($ newChan') (unwrapEvent @"channelcreate" eh)

handleActions os ns eh (ChannelUpdate (DMChannel' chan)) = do
  oldChan  <- DMChannel' <$> os ^? #dms . at (getID chan) . _Just
  newChan' <- DMChannel' <$> ns ^? #dms . at (getID chan) . _Just
  pure $ map (\f -> f oldChan newChan') (unwrapEvent @"channelupdate" eh)

handleActions os ns eh (ChannelUpdate (GuildChannel' chan)) = do
  oldChan  <- GuildChannel' <$> os ^? #channels . at (getID chan) . _Just
  newChan' <- GuildChannel' <$> ns ^? #channels . at (getID chan) . _Just
  pure $ map (\f -> f oldChan newChan') (unwrapEvent @"channelupdate" eh)

-- NOTE: Channel will be deleted in the new cache
handleActions os _ eh (ChannelDelete (GuildChannel' chan)) = do
  oldChan <- GuildChannel' <$> os ^? #channels . at (getID chan) . _Just
  pure $ map (\f -> f oldChan) (unwrapEvent @"channeldelete" eh)

handleActions os _ eh (ChannelDelete (DMChannel' chan)) = do
  oldChan <- DMChannel' <$> os ^? #dms . at (getID chan) . _Just
  pure $ map (\f -> f oldChan) (unwrapEvent @"channeldelete" eh)

handleActions os _ eh (ChannelPinsUpdate ChannelPinsUpdateData { channelID, lastPinTimestamp }) = do
  chan <- (GuildChannel' <$> os ^? #channels . at (coerceSnowflake channelID) . _Just)
    <|> (DMChannel' <$> os ^? #dms . at (coerceSnowflake channelID) . _Just)
  pure $ map (\f -> f chan lastPinTimestamp) (unwrapEvent @"channelpinsupdate" eh)

handleActions _ ns eh (GuildCreate guild) = do
  let isNew = ns ^. #unavailableGuilds . contains (guild ^. #id)
  pure $ map (\f -> f guild isNew) (unwrapEvent @"guildcreate" eh)

handleActions os ns eh (GuildUpdate guild) = do
  oldGuild <- os ^? #guilds . at (coerceSnowflake $ guild ^. #id) . _Just
  newGuild <- ns ^? #guilds . at (coerceSnowflake $ guild ^. #id) . _Just
  pure $ map (\f -> f oldGuild newGuild) (unwrapEvent @"guildupdate" eh)

-- NOTE: Guild will be deleted in the new cache if unavailable was false
handleActions os _ eh (GuildDelete UnavailableGuild { id, unavailable }) = do
  oldGuild <- os ^? #guilds . at id . _Just
  pure $ map (\f -> f oldGuild unavailable) (unwrapEvent @"guilddelete" eh)

handleActions os _ eh (GuildBanAdd GuildBanData { guildID, user }) = do
  guild <- os ^? #guilds . at guildID . _Just
  pure $ map (\f -> f guild user) (unwrapEvent @"guildbanadd" eh)

handleActions os _ eh (GuildBanRemove GuildBanData { guildID, user }) = do
  guild <- os ^? #guilds . at guildID . _Just
  pure $ map (\f -> f guild user) (unwrapEvent @"guildbanremove" eh)

-- NOTE: we fire this event using the guild data with old emojis
handleActions os _ eh (GuildEmojisUpdate GuildEmojisUpdateData { guildID, emojis }) = do
  guild <- os ^? #guilds . at guildID . _Just
  pure $ map (\f -> f guild emojis) (unwrapEvent @"guildemojisupdate" eh)

handleActions _ ns eh (GuildIntegrationsUpdate GuildIntegrationsUpdateData { guildID }) = do
  guild <- ns ^? #guilds . at guildID . _Just
  pure $ map ($ guild) (unwrapEvent @"guildintegrationsupdate" eh)

handleActions _ ns eh (GuildMemberAdd member) = do
  newMember <- ns ^? #guilds . at (member ^. #guildID) . _Just . #members . at (getID member) . _Just
  pure $ map ($ newMember) (unwrapEvent @"guildmemberadd" eh)

handleActions os _ eh (GuildMemberRemove GuildMemberRemoveData { user, guildID }) = do
  oldMember <- os ^? #guilds . at guildID . _Just . #members . at (coerceSnowflake $ getID user) . _Just
  pure $ map ($ oldMember) (unwrapEvent @"guildmemberremove" eh)

handleActions os ns eh (GuildMemberUpdate GuildMemberUpdateData { user, guildID }) = do
  oldMember <- os ^? #guilds . at guildID . _Just . #members . at (coerceSnowflake $ getID user) . _Just
  newMember <- ns ^? #guilds . at guildID . _Just . #members . at (coerceSnowflake $ getID user) . _Just
  pure $ map (\f -> f oldMember newMember) (unwrapEvent @"guildmemberupdate" eh)

handleActions _ ns eh (GuildMembersChunk GuildMembersChunkData { members, guildID }) = do
  guild <- ns ^? #guilds . at guildID . _Just
  let members' = guild ^.. #members . foldMap at (map getID members) . _Just
  pure $ map (\f -> f guild members') (unwrapEvent @"guildmemberschunk" eh)

handleActions _ ns eh (GuildRoleCreate GuildRoleData { guildID, role }) = do
  guild <- ns ^? #guilds . at guildID . _Just
  role' <- guild ^? #roles . at (getID role) . _Just
  pure $ map (\f -> f guild role') (unwrapEvent @"guildrolecreate" eh)

handleActions os ns eh (GuildRoleUpdate GuildRoleData { guildID, role }) = do
  oldRole <- os ^? #guilds . at guildID . _Just . #roles . at (getID role) . _Just
  newGuild <- ns ^? #guilds . at guildID . _Just
  newRole <- newGuild ^? #roles . at (getID role) . _Just
  pure $ map (\f -> f newGuild oldRole newRole) (unwrapEvent @"guildroleupdate" eh)

handleActions os ns eh (GuildRoleDelete GuildRoleDeleteData { guildID, roleID }) = do
  newGuild <- ns ^? #guilds . at guildID . _Just
  role' <- os ^? #guilds . at guildID . _Just . #roles . at roleID . _Just
  pure $ map (\f -> f newGuild role') (unwrapEvent @"guildroledelete" eh)

handleActions _ _ eh (MessageCreate msg) =
  pure $ map ($ msg) (unwrapEvent @"messagecreate" eh)

handleActions os ns eh (MessageUpdate msg) = do
  let msgID = coerceSnowflake $ msg ^. #id
  oldMsg <- os ^. #messages . at msgID
  newMsg <- ns ^. #messages . at msgID
  pure $ map (\f -> f oldMsg newMsg) (unwrapEvent @"messageupdate" eh)

handleActions os _ eh (MessageDelete MessageDeleteData { id }) = do
  oldMsg <- os ^. #messages . at id
  pure $ map ($ oldMsg) (unwrapEvent @"messagedelete" eh)

handleActions os _ eh (MessageDeleteBulk MessageDeleteBulkData { ids }) = join
  <$> for ids (\id -> do
                 oldMsg <- os ^. #messages . at id
                 pure $ map ($ oldMsg) (unwrapEvent @"messagedelete" eh))

handleActions _ ns eh (MessageReactionAdd reaction) = do
  message <- ns ^. #messages . at (coerceSnowflake $ reaction ^. #messageID)
  pure $ map (\f -> f message reaction) (unwrapEvent @"messagereactionadd" eh)

handleActions _ ns eh (MessageReactionRemove reaction) = do
  message <- ns ^. #messages . at (coerceSnowflake $ reaction ^. #messageID)
  pure $ map (\f -> f message reaction) (unwrapEvent @"messagereactionremove" eh)

handleActions os _ eh (MessageReactionRemoveAll MessageReactionRemoveAllData { messageID }) = do
  oldMsg <- os ^. #messages . at (coerceSnowflake messageID)
  pure $ map ($ oldMsg) (unwrapEvent @"messagereactionremoveall" eh)

handleActions os ns eh (PresenceUpdate PresenceUpdateData { userID, presence = Presence { guildID } }) = do
  oldMember <- os ^? #guilds . at guildID . _Just . #members . at (coerceSnowflake userID) . _Just
  newMember <- ns ^? #guilds . at guildID . _Just . #members . at (coerceSnowflake userID) . _Just
  let userUpdates = if oldMember ^. #user /= newMember ^. #user
                    then map (\f -> f (oldMember ^. #user) (newMember ^. #user)) (unwrapEvent @"userupdate" eh)
                    else mempty
  pure $ userUpdates <> map (\f -> f oldMember newMember) (unwrapEvent @"guildmemberupdate" eh)

handleActions _ ns eh (TypingStart TypingStartData { channelID, guildID, userID, timestamp }) = do
  let member = ns ^? #guilds . at guildID . _Just . #members . at (coerceSnowflake userID) . _Just
  channel <- (GuildChannel' <$> ns ^? #channels . at (coerceSnowflake channelID) . _Just)
    <|> (DMChannel' <$> ns ^? #dms . at (coerceSnowflake channelID) . _Just)
  pure $ map (\f -> f channel member timestamp) (unwrapEvent @"typingstart" eh)

handleActions os ns eh (UserUpdate _) = do
  oldUser <- os ^? #user . _Just
  newUser <- ns ^? #user . _Just
  pure $ map (\f -> f oldUser newUser) (unwrapEvent @"userupdate" eh)

handleActions _ _ _ _ = Nothing -- pure []

getGuildID :: GuildChannel -> Snowflake Guild
getGuildID (GuildTextChannel c) = c ^. #guildID
getGuildID (GuildVoiceChannel c) = c ^. #guildID
getGuildID (GuildCategory c) = c ^. #guildID

updateCache :: DispatchData -> State Cache ()
updateCache (Ready ReadyData { user, guilds }) = do
  #user ?= user
  #unavailableGuilds .= setOf (folded . #id) guilds

updateCache (ChannelCreate (DMChannel' chan)) = do
  #dms %= SM.insert chan

updateCache (ChannelCreate (GuildChannel' chan)) = do
  #channels %= SM.insert chan
  #guilds . at (getGuildID chan) . _Just . #channels %= SM.insert chan

updateCache (ChannelUpdate (DMChannel' chan)) =
  #dms . at (getID chan) . _Just %= update chan

updateCache (ChannelUpdate (GuildChannel' chan)) = do
  #channels . at (getID chan) . _Just %= update chan
  #guilds . at (getGuildID chan) . _Just . #channels . at (getID chan) . _Just
    %= update chan

updateCache (ChannelDelete (DMChannel' chan)) =
  #dms %= sans (getID chan)

updateCache (ChannelDelete (GuildChannel' chan)) = do
  #channels %= sans (getID chan)
  #guilds . at (getGuildID chan) . _Just . #channels %= sans (getID chan)

updateCache (GuildCreate guild) = do
  #guilds %= SM.insert guild
  -- also insert all channels from this guild
  #channels %= SM.union (guild ^. #channels)
  #users %= SM.union (SM.fromList (guild ^.. #members . traverse . #user))

updateCache (GuildUpdate guild) =
  #guilds . at (guild ^. #id) . _Just %= update guild

updateCache (GuildDelete guild) = do
  guild' <- use $ #guilds . at (guild ^. #id)
  whenJust guild' $ \guild'' -> do
    #guilds %= sans (guild ^. #id)
    #channels %= (`SM.difference` (guild'' ^. #channels))

updateCache (GuildEmojisUpdate GuildEmojisUpdateData { guildID, emojis }) =
  #guilds . at guildID . _Just . #emojis .= SM.fromList emojis

updateCache (GuildMemberAdd member) = do
  #users %= SM.insert (member ^. #user)
  #guilds . at (member ^. #guildID) . _Just . #members . at (getID member) ?= member

updateCache (GuildMemberRemove GuildMemberRemoveData { guildID, user }) = do
  #guilds . at guildID . _Just . #members %= sans (coerceSnowflake $ user ^. #id)

updateCache (GuildMemberUpdate GuildMemberUpdateData { guildID, roles, user, nick }) = do
  #guilds . at guildID . _Just . #members . at (coerceSnowflake $ user ^. #id) . _Just . #roles .= roles
  #guilds . at guildID . _Just . #members . at (coerceSnowflake $ user ^. #id) . _Just . #nick
    %= (`lastMaybe` nick)
  #users %= SM.insert user

updateCache (GuildMembersChunk GuildMembersChunkData { members }) =
  traverse_ (updateCache . GuildMemberAdd) members

updateCache (GuildRoleCreate GuildRoleData { guildID, role }) =
  #guilds . at guildID . _Just . #roles %= SM.insert role

updateCache (GuildRoleUpdate GuildRoleData { guildID, role }) =
  #guilds . at guildID . _Just . #roles %= SM.insert role

updateCache (GuildRoleDelete GuildRoleDeleteData { guildID, roleID }) =
  #guilds . at guildID . _Just . #roles %= sans roleID

updateCache (MessageCreate msg) = #messages %= addMessage msg

updateCache (MessageUpdate newMsg) = do
  let id = coerceSnowflake $ newMsg ^. #id
  #messages . at id . _Just %= update newMsg

updateCache (MessageDelete MessageDeleteData { id }) = #messages %= sans id

updateCache (MessageDeleteBulk MessageDeleteBulkData { ids }) = #messages %= flip (foldl $ flip dropMessage) ids

updateCache (MessageReactionAdd reaction) =
  #messages . at (reaction ^. #messageID) . _Just . #reactions %= V.cons reaction

updateCache (MessageReactionRemove reaction) =
  #messages . at (reaction ^. #messageID) . _Just . #reactions %= V.filter (\r -> r ^. #userID /= reaction ^. #userID)

updateCache (MessageReactionRemoveAll MessageReactionRemoveAllData { messageID }) =
  #messages . at messageID . _Just . #reactions .= V.empty

updateCache (PresenceUpdate PresenceUpdateData { userID, roles, presence }) = do
  #guilds . at (presence ^. #guildID) . _Just . #members . at (coerceSnowflake userID) . _Just . #roles .= roles
  #guilds . at (presence ^. #guildID) . _Just . #presences . at userID ?= presence

updateCache (UserUpdate user) = #user ?= user

updateCache _data' = pure () -- TODO
