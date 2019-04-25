-- | The client
module Calamity.Client.Client
    ( module Calamity.Client.Types
    , newClient
    , clientLoop
    , startClient ) where

import           Calamity.Client.ShardManager
import           Calamity.Client.Types
import           Calamity.Gateway.DispatchEvents
import           Calamity.HTTP.Ratelimit
import           Calamity.Types.General
import           Calamity.Types.MessageStore
import           Calamity.Types.Snowflake

import           Control.Concurrent.Async        ( forConcurrently_ )
import           Control.Concurrent.STM.TVar
import           Control.Lens                    ( (.=) )

import           Data.Default
import qualified Data.HashMap.Lazy               as LH
import qualified Data.HashSet                    as LS
import           Data.HashSet.Lens
import           Data.Maybe
import qualified Data.TypeRepMap                 as TM

import qualified StmContainers.Set               as TS

import qualified Streamly.Prelude                as S

import qualified System.Log.Simple               as SLS


-- TODO: merge event handlers with default
-- and give writerT for adding events
newClient :: Token -> EventHandlers -> IO Client
newClient token eventHandlers = do
  shards'                     <- newTVarIO []
  numShards'                  <- newEmptyMVar
  rlState'                    <- newRateLimitState
  (eventStream', eventQueue') <- mkQueueRecvStream
  cache'                      <- newTVarIO emptyCache
  activeTasks'                <- TS.newIO

  pure $ Client shards'
                numShards'
                token
                rlState'
                eventStream'
                eventQueue'
                cache'
                activeTasks'
                eventHandlers

-- TODO: user & bot logins
-- TODO: more login types

startClient :: Client -> IO ()
startClient client = do
  logEnv <- newLog
    (logCfg [("", SLS.Info), ("bot", SLS.Trace)])
    [handler text coloredConsole]
  runBotM client logEnv . component "bot" $ do
    shardBot
    clientLoop

emptyCache :: Cache
emptyCache = Cache Nothing LH.empty LH.empty LH.empty LS.empty def

-- | main loop of the client, handles fetching the next event, processing the event
-- and invoking it's handler functions
clientLoop :: BotM ()
clientLoop = do
  evtStream <- asks eventStream
  client' <- ask
  logEnv' <- askLog
  trace "entering clientLoop"
  liftIO $ S.mapM_ (runBotM client' logEnv' . handleEvent) evtStream
  trace "exiting clientLoop"

handleEvent :: DispatchData -> BotM ()
handleEvent data' = do
  trace "handling an event"
  cache' <- asks cache
  (oldCache, newCache) <- liftIO . atomically $ do
    oldCache <- readTVar cache'
    let newCache = execState (updateCache data') oldCache
    writeTVar cache' newCache
    pure (oldCache, newCache)

  runEventHandlers oldCache newCache data'
  trace $ "finished handling an event, new cache is: " <> show newCache

runEventHandlers :: Cache -> Cache -> DispatchData -> BotM ()
runEventHandlers oldCache newCache data' = do
  eventHandlers <- asks eventHandlers
  client' <- ask
  logEnv' <- askLog
  let actionHandlers = handleActions oldCache newCache eventHandlers data'
  case actionHandlers of
    Just actions -> liftIO
      $ forConcurrently_ actions (runBotM client' logEnv' . runEventM newCache)
    Nothing
      -> error $ "Failed handling actions for event: " +|| data' ||+ ""

unwrapEvent :: forall a. KnownSymbol a => EventHandlers -> [EHType a]
unwrapEvent (EventHandlers eh) = unwrapEventHandler . fromJust $ (TM.lookup eh :: Maybe (EventHandler a))

handleActions :: Cache -- ^ The old cache
              -> Cache -- ^ The new cache
              -> EventHandlers
              -> DispatchData
              -> Maybe [EventM ()]
handleActions _ _ eh (Ready rd) = pure $ map ($ rd) (unwrapEvent @"ready" eh)

handleActions _ ns eh (ChannelCreate chan) = do
  newChan' <- ns ^? #channels . at (getID chan) . _Just
  pure $ map ($ newChan') (unwrapEvent @"channelcreate" eh)

handleActions os ns eh (ChannelUpdate chan) = do
  oldChan  <- os ^? #channels . at (getID chan) . _Just
  newChan' <- ns ^? #channels . at (getID chan) . _Just
  pure $ map (\f -> f oldChan newChan') (unwrapEvent @"channelupdate" eh)

-- NOTE: Channel will be deleted in the new cache
handleActions os _ eh (ChannelDelete chan) = do
  oldChan <- os ^? #channels . at (getID chan) . _Just
  pure $ map (\f -> f oldChan) (unwrapEvent @"channeldelete" eh)

handleActions os _ eh (ChannelPinsUpdate ChannelPinsUpdateData { channelID, lastPinTimestamp }) = do
  chan <- os ^? #channels . at channelID . _Just
  pure $ map (\f -> f chan lastPinTimestamp) (unwrapEvent @"channelpinsupdate" eh)

handleActions _ _ eh (GuildCreate guild) = pure $ map ($ guild) (unwrapEvent @"guildcreate" eh)

handleActions os ns eh (GuildUpdate guild) = do
  oldGuild <- os ^? #guilds . at (getID guild) . _Just
  newGuild <- ns ^? #guilds . at (getID guild) . _Just
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
  oldMsg <- getMessage (coerceSnowflake $ msg ^. #id) $ os ^. #messages
  newMsg <- getMessage (coerceSnowflake $ msg ^. #id) $ ns ^. #messages
  pure $ map (\f -> f oldMsg newMsg) (unwrapEvent @"messageupdate" eh)

handleActions os _ eh (MessageDelete MessageDeleteData { id }) = do
  oldMsg <- getMessage id $ os ^. #messages
  pure $ map ($ oldMsg) (unwrapEvent @"messagedelete" eh)

handleActions os _ eh (MessageDeleteBulk MessageDeleteBulkData { ids }) = join
  <$> forM ids (\id -> do
                  oldMsg <- getMessage id $ os ^. #messages
                  pure $ map ($ oldMsg) (unwrapEvent @"messagedelete" eh))

handleActions _ ns eh (MessageReactionAdd reaction) = do
  message <- getMessage (coerceSnowflake $ reaction ^. #messageID) $ ns ^. #messages
  pure $ map (\f -> f message reaction) (unwrapEvent @"messagereactionadd" eh)

handleActions _ ns eh (MessageReactionRemove reaction) = do
  message <- getMessage (coerceSnowflake $ reaction ^. #messageID) $ ns ^. #messages
  pure $ map (\f -> f message reaction) (unwrapEvent @"messagereactionremove" eh)

handleActions os _ eh (MessageReactionRemoveAll MessageReactionRemoveAllData { messageID }) = do
  oldMsg <- getMessage (coerceSnowflake messageID) $ os ^. #messages
  pure $ map ($ oldMsg) (unwrapEvent @"messagereactionremoveall" eh)

handleActions os ns eh (PresenceUpdate Presence { user, guildID }) = do
  oldMember <- os ^? #guilds . at guildID . _Just . #members . at (coerceSnowflake $ user ^. #id) . _Just
  newMember <- ns ^? #guilds . at guildID . _Just . #members . at (coerceSnowflake $ user ^. #id) . _Just
  let userUpdates = if oldMember ^. #user /= newMember ^. #user
                    then map (\f -> f (oldMember ^. #user) (newMember ^. #user)) (unwrapEvent @"userupdate" eh)
                    else mempty
  pure $ userUpdates <> map (\f -> f oldMember newMember) (unwrapEvent @"guildmemberupdate" eh)

handleActions _ ns eh (TypingStart TypingStartData { channelID, guildID, userID, timestamp }) = do
  guild <- ns ^? #guilds . at guildID . _Just
  channel <- ns ^? #channels . at channelID . _Just
  member <- guild ^? #members . at (coerceSnowflake userID) . _Just
  pure $ map (\f -> f channel member timestamp) (unwrapEvent @"typingstart" eh)

handleActions os ns eh (UserUpdate _) = do
  oldUser <- os ^? #user . _Just
  newUser <- ns ^? #user . _Just
  pure $ map (\f -> f oldUser newUser) (unwrapEvent @"userupdate" eh)

handleActions _ _ _ _ = Nothing -- pure []



-- TODO: actually update the cache
updateCache :: DispatchData -> State Cache ()
updateCache (Ready ReadyData { user, guilds }) = do
  #user ?= user
  #unavailableGuilds .= setOf (folded . #id) guilds

updateCache (ChannelCreate chan) = do
  #channels . at (chan ^. #id) ?= chan
  whenJust (chan ^. #guildID) $ \guildID -> #guilds . at guildID . _Just . #channels . at (chan ^. #id) ?= chan

updateCache (MessageCreate msg) = #messages %= addMessage msg

updateCache (MessageUpdate newMsg) = do
  let id = coerceSnowflake $ newMsg ^. #id
  oldMsg <- getMessage id <$> use #messages
  whenJust oldMsg $ \oldMsg' -> let newMsg' = mergeMessage oldMsg' newMsg
                                in #messages %= addMessage newMsg' . dropMessage id

updateCache (MessageDelete MessageDeleteData { id }) = #messages %= dropMessage id

updateCache (MessageDeleteBulk MessageDeleteBulkData { ids }) = #messages %= flip (foldl $ flip dropMessage) ids

updateCache data' = pure () -- TODO
