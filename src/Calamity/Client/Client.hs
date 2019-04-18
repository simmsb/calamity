-- | The client

module Calamity.Client.Client
  ( module Calamity.Client.Types
  , newClient
  , clientLoop
  , startClient
  )
where

import qualified System.Log.Simple             as SLS
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar
import qualified Data.HashMap.Lazy             as LH
import           Data.Maybe
import qualified Data.TypeRepMap               as TM
import qualified StmContainers.Set             as TS
import qualified Streamly.Prelude              as S

import           Calamity.Client.ShardManager
import           Calamity.Client.Types
import           Calamity.HTTP.Ratelimit
import           Calamity.Types.DispatchEvents
import           Calamity.Types.General

-- TODO: merge event handlers with default
-- and give writerT for adding events
newClient :: Token -> EventHandlers -> IO Client
newClient token eventHandlers = do
  shards'                    <- newTVarIO []
  numShards'                 <- newEmptyMVar
  rlState'                   <- newRateLimitState
  (eventStream', eventQueue') <- mkQueueRecvStream
  cache'                     <- newTVarIO emptyCache
  activeTasks'               <- TS.newIO

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
emptyCache = Cache Nothing LH.empty LH.empty LH.empty

-- | main loop of the client, handles fetching the next event, processing the event
-- and invoking it's handler functions
clientLoop :: BotM ()
clientLoop = do
  evtStream  <- asks eventStream
  client' <- ask
  logEnv' <- askLog
  trace "entering clientLoop"
  liftIO $ S.mapM_ (runBotM client' logEnv' . handleEvent) evtStream
  trace "exiting clientLoop"

handleEvent :: DispatchData -> BotM ()
handleEvent data' = do
  trace "handling an event"
  cache'   <- asks cache
  (oldCache, newCache) <- liftIO . atomically $ do
    oldCache <- readTVar cache'
    let newCache = updateCache oldCache data'
    writeTVar cache' newCache
    pure (oldCache, newCache)

  runEventHandlers oldCache newCache data'
  trace "finished handling an event"

runEventHandlers :: Cache -> Cache -> DispatchData -> BotM ()
runEventHandlers oldCache newCache data' = do
  eventHandlers <- asks eventHandlers
  client'       <- ask
  logEnv'       <- askLog
  let actionHandlers = handleActions oldCache newCache eventHandlers data'
  case actionHandlers of
    Just actions ->
      liftIO $ forConcurrently_ actions (runBotM client' logEnv' . runEventM newCache)
    Nothing ->
      error $ "Failed handling actions for event: " +|| data' ||+""

unwrapEvent :: forall a. KnownSymbol a => EventHandlers -> [EHType a]
unwrapEvent (EventHandlers eh) = unwrapEventHandler . fromJust $ (TM.lookup eh :: Maybe (EventHandler a))

handleActions :: Cache -- ^ The old cache
              -> Cache -- ^ The new cache
              -> EventHandlers
              -> DispatchData
              -> Maybe [EventM ()]
handleActions _ _ eh (Ready rd) =
  pure $ map ($ rd) (unwrapEvent @"ready" eh)

handleActions _ _ eh (ChannelCreate chan) =
  pure $ map ($ chan) (unwrapEvent @"channelcreate" eh)

handleActions os _ eh (ChannelUpdate chan) = do
  oldChan <- os ^? #channels . at (chan ^. #id) . _Just
  pure $ map (\f -> f oldChan chan) (unwrapEvent @"channelupdate" eh)

-- NOTE: Channel will be deleted in the new cache
handleActions os _ eh (ChannelDelete chan) = do
  oldChan <- os ^? #channels . at (chan ^. #id) . _Just
  pure $ map (\f -> f oldChan) (unwrapEvent @"channeldelete" eh)

handleActions os _ eh (ChannelPinsUpdate ChannelPinsUpdateData {channelID, lastPinTimestamp}) = do
  chan <- os ^? #channels . at channelID . _Just
  pure $ map (\f -> f chan lastPinTimestamp) (unwrapEvent @"channelpinsupdate" eh)

handleActions _ _ eh (GuildCreate chan) =
  pure $ map ($ chan) (unwrapEvent @"guildcreate" eh)

handleActions os _ eh (GuildUpdate guild) = do
  oldGuild <- os ^? #guilds . at (guild ^. #id) . _Just
  pure $ map (\f -> f oldGuild guild) (unwrapEvent @"guildupdate" eh)

-- NOTE: Guild will be deleted in the new cache if unavailable was false
handleActions os _ eh (GuildDelete UnavailableGuild {id, unavailable}) = do
  oldGuild <- os ^? #guilds . at id . _Just
  pure $ map (\f -> f oldGuild unavailable) (unwrapEvent @"guilddelete" eh)

handleActions os _ eh (GuildBanAdd GuildBanData {guildID, user}) = do
  guild <- os ^? #guilds . at guildID . _Just
  pure $ map (\f -> f guild user) (unwrapEvent @"guildbanadd" eh)

handleActions os _ eh (GuildBanRemove GuildBanData {guildID, user}) = do
  guild <- os ^? #guilds . at guildID . _Just
  pure $ map (\f -> f guild user) (unwrapEvent @"guildbanremove" eh)

-- NOTE: we fire this event using the guild data with old emojis
handleActions os _ eh (GuildEmojisUpdate GuildEmojisUpdateData {guildID, emojis}) = do
  guild <- os ^? #guilds . at guildID . _Just
  pure $ map (\f -> f guild emojis) (unwrapEvent @"guildemojisupdate" eh)

handleActions _ ns eh (GuildIntegrationsUpdate GuildIntegrationsUpdateData {guildID}) = do
  guild <- ns ^? #guilds . at guildID . _Just
  pure $ map ($ guild) (unwrapEvent @"guildintegrationsupdate" eh)

-- TODO: the rest of these
handleActions _ _ (EventHandlers _) _ = pure []

-- TODO: actually update the cache
updateCache :: Cache -> DispatchData -> Cache
updateCache cache data' = cache -- TODO
