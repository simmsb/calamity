-- | The client

module YAHDL.Client.Client
  ( module YAHDL.Client.Types
  , newClient
  , clientLoop
  )
where

import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar
import qualified Data.HashMap.Lazy             as LH
import           Data.Maybe
import qualified Data.TypeRepMap               as TM
import qualified StmContainers.Set             as TS
import qualified Streamly.Prelude              as S

import           YAHDL.Types.General
import           YAHDL.Types.DispatchEvents
import           YAHDL.Client.ShardManager
import           YAHDL.Client.Types
import           YAHDL.HTTP.Ratelimit


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

emptyCache :: Cache
emptyCache = Cache Nothing LH.empty LH.empty LH.empty

-- | main loop of the client, handles fetching the next event, processing the event
-- | and invoking it's handler functions
clientLoop :: BotM ()
clientLoop = do
  stream  <- asks eventStream
  client' <- ask
  logEnv' <- askLogger
  liftIO $ S.mapM_ (runBotM client' logEnv' . handleEvent) stream

handleEvent :: DispatchData -> BotM ()
handleEvent data' = do
  cache'   <- asks cache
  newCache <- liftIO . atomically $ updateCache cache' data'
  runEventHandlers newCache data'

runEventHandlers :: Cache -> DispatchData -> BotM ()
runEventHandlers cache data' = do
  eventHandlers <- asks eventHandlers
  client'       <- ask
  logEnv'       <- askLogger
  liftIO $ forConcurrently_ (handleActions eventHandlers data')
                            (runBotM client' logEnv' . runEventM cache)

handleActions :: EventHandlers -> DispatchData -> [EventM ()]
handleActions (EventHandlers eh) (Ready rd) =
  map ($ rd) (unwrapEventHandler . fromJust $ (TM.lookup eh :: Maybe (EventHandler "ready")))
handleActions (EventHandlers _) _ = []

updateCache :: TVar Cache -> DispatchData -> STM Cache
updateCache cache data' = readTVar cache -- TODO
