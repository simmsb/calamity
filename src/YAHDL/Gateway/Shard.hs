-- |

{-# LANGUAGE RecursiveDo #-}

module YAHDL.Gateway.Shard
  ( Shard(..)
  , newShard
  )
where

-- import Control
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Control.Monad.Log              ( Logger
                                                , askLogger
                                                , runLogTSafe
                                                )
import           Control.Lens                   ( (.=) )
import           Control.Monad.State.Concurrent.Strict
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import qualified Data.Aeson                    as A
import           Data.Text.Strict.Lens
import           Data.Maybe
import           Network.WebSockets             ( Connection
                                                , receiveData
                                                , sendClose
                                                , sendTextData
                                                )
import qualified Streamly                      as S
import qualified Streamly.Prelude              as S
import           Wuss

import           YAHDL.Gateway.Types

newShardState :: Shard -> ShardState
newShardState shard =
  ShardState shard Nothing Nothing False Nothing Nothing Nothing

-- | Creates and launches a shard
newShard :: Integer -> Text -> Logger env -> TChan ShardMsg -> IO (Shard, Async ())
newShard id token logEnv evtChan = mdo
  cmdChan' <- newTChanIO
  stateVar <- newTVarIO (newShardState shard)

  let shard = Shard id evtChan cmdChan' stateVar token

  thread' <- async . runShardM shard logEnv $ shardLoop

  pure (shard, thread')

runShardM :: Shard -> Logger env -> ShardM env a -> IO a
runShardM shard logEnv = evalStateC' . runLogTSafe logEnv . unShardM
  where evalStateC' s = evalStateC s (shard ^. #shardState)

extractMaybeStream :: (S.IsStream t, Monad m) => t m (Maybe a) -> t m a
extractMaybeStream = S.mapMaybe identity

-- TODO: correct this, add compression
getGatewayHost :: IO Text
getGatewayHost = pure "gateway.discord.gg"

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return

sendToWs :: SentDiscordMessage -> ShardM env ()
sendToWs data' = do
  wsConn <- fromJust <$> use #wsConn
  liftIO . sendTextData wsConn . A.encode $ data'

-- | The loop a shard will run on
shardLoop :: ShardM env ()
shardLoop = outerloop $> ()
 where
  controlStream shard = S.repeatM $ do
    msg <- liftIO . atomically . readTChan $ (shard ^. #cmdChan)
    pure . Control $ msg

  discordStream ws = extractMaybeStream . S.repeatM . runMaybeT $ do
    msg <- liftIO $ receiveData ws
    d   <- liftMaybe . A.decode $ msg
    pure . Discord $ d

  mergedStream shard ws = S.async (controlStream shard) (discordStream ws)

  -- | The outer loop, sets up the ws conn, etc handles reconnecting and such
  -- Currently if this goes to the error path we just exit the forever loop
  -- and the shard stops, maybe we might want to do some extra logic to reboot
  -- the shard, or maybe force a resharding
  outerloop :: ShardM env (Either ControlMessage a)
  outerloop = runExceptT . forever $ do
    state <- get
    shard <- use #shardS

    host  <- case state ^. #wsHost of
      Just host -> pure host
      Nothing   -> do
        host <- liftIO getGatewayHost
        #wsHost ?= host
        pure host

    -- TODO: handle result of innerloop
    logEnv <- askLogger

    innerLoopVal <- liftIO $ runSecureClient (host ^. unpacked)
                    443
                    "/?v=7&encoding=json"
                    (runShardM shard logEnv . innerloop)

    case innerLoopVal of
      Left ShutDown -> throwE ShutDown

      Left Restart -> pure () -- we restart normally
      _ -> pure ()

  -- | The inner loop, handles receiving a message from discord or a command message
  -- | and then decides what to do with it
  innerloop :: Connection -> ShardM env (Either ControlMessage a)
  innerloop ws = runExceptT . forever $ do
    shard <- use #shardS
    #wsConn ?= ws
    logEnv <- askLogger

    -- rather nasty, use IO exceptions to break the stream
    -- so use this to convert back to ExceptT stuff

    err :: Either ControlMessage () <- liftIO . try . S.mapM_ handleMsg $ mergedStream shard ws

    case err of
      Left x -> throwE x
      _      -> pure ()

    #wsConn .= Nothing

  -- | Handlers for each message, not sure what they'll need to do exactly yet
  -- handleMsg :: ShardMsg -> ShardM env ()
  handleMsg (Discord msg) = pure ()
  handleMsg (Control msg) = case msg of
    Restart  -> liftIO $ throwIO Restart
    ShutDown -> liftIO $ throwIO ShutDown

startHeartBeatLoop :: Int -> ShardM env ()
startHeartBeatLoop interval = do
  haltHeartBeat -- cancel any currently running hb thread
  shard  <- use #shardS
  logEnv <- askLogger
  liftIO -- TODO: we need to send a close to the ws depending on how we exited from the hb loop
    $  finally (async . runShardM shard logEnv $ heartBeatLoop interval)
               (runShardM shard logEnv haltHeartBeat)
    $> ()

haltHeartBeat :: ShardM env ()
haltHeartBeat = do
  thread <- use #hbThread
  case thread of
    Just t  -> liftIO $ cancel t $> ()
    Nothing -> pure ()
  #hbThread .= Nothing

sendHeartBeat :: ShardM env ()
sendHeartBeat = do
  seq <- use #seqNum
  sendToWs $ HeartBeat seq

heartBeatLoop :: Int -> ShardM env ()
heartBeatLoop interval = ($> ()) . runMaybeT . forever $ do
  lift sendHeartBeat
  #hbResponse .= False
  liftIO . threadDelay $ interval * 1000
  hasResp <- use #hbResponse
  unless hasResp mzero -- exit the loop, we had no response
