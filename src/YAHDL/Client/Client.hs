-- | The client

module YAHDL.Client.Client
  ( module YAHDL.Client.Types
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

import           YAHDL.Client.ShardManager
import           YAHDL.Client.Types
import           YAHDL.HTTP.Ratelimit
import           YAHDL.Types.DispatchEvents
import           YAHDL.Types.General

-- TODO: merge event handlers with default
-- and give writerT for adding events
newClient :: Token -> EventHandlers -> IO Client
newClient token eventHandlers = do
  shards'                    <- newTVarIO []
  numShards'                 <- newEmptyMVar
  rlState'                   <- newRateLimitState
  (eventStream', eventChan') <- mkChanRecvStream
  cache'                     <- newTVarIO emptyCache
  activeTasks'               <- TS.newIO

  pure $ Client shards'
                numShards'
                token
                rlState'
                eventStream'
                eventChan'
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
  debug "entering clientLoop"
  liftIO $ S.mapM_ (runBotM client' logEnv' . handleEvent) evtStream
  debug "exiting clientLoop"

handleEvent :: DispatchData -> BotM ()
handleEvent data' = do
  trace "handling an event"
  cache'   <- asks cache
  newCache <- liftIO . atomically $ updateCache cache' data'
  runEventHandlers newCache data'
  trace "finished handling an event"

runEventHandlers :: Cache -> DispatchData -> BotM ()
runEventHandlers cache data' = do
  eventHandlers <- asks eventHandlers
  client'       <- ask
  logEnv'       <- askLog
  liftIO $ forConcurrently_ (handleActions eventHandlers data')
                            (runBotM client' logEnv' . runEventM cache)

handleActions :: EventHandlers -> DispatchData -> [EventM ()]
handleActions (EventHandlers eh) (Ready rd) =
  map ($ rd) (unwrapEventHandler . fromJust $ (TM.lookup eh :: Maybe (EventHandler "ready")))
-- TODO: the rest of these
handleActions (EventHandlers _) _ = []

-- TODO: actually update the cache
updateCache :: TVar Cache -> DispatchData -> STM Cache
updateCache cache data' = readTVar cache -- TODO
