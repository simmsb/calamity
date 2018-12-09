-- |

{-# LANGUAGE RecursiveDo #-}

module YAHDL.Gateway.Shard
  ( Shard(..)
  , newShard
  )
where

import Control.Monad.Log (Logger, runLogTSafe, askLogger)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Except
import           Control.Monad.State.Concurrent.Strict
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM.TMVar
import           Data.Text.Strict.Lens
import qualified Streamly.Prelude              as S
import qualified Streamly                      as S
import           Wuss
import           Network.WebSockets             ( Connection
                                                , receiveData
                                                , sendClose
                                                , sendTextData
                                                )
import qualified Data.Aeson                    as A

import           YAHDL.Gateway.Types

newShardState :: Shard -> ShardState
newShardState shard = ShardState shard Nothing Nothing Nothing False Nothing Nothing

-- | Creates and launches a shard
newShard :: Integer -> Text -> Logger env -> TChan () -> IO (Shard, Async ())
newShard id token logEnv evtChan = mdo
  cmdChan' <- newTChanIO
  stateVar <- newTVarIO (newShardState shard)

  let shard = Shard id evtChan cmdChan' stateVar token

  thread' <- async . runShardM shard logEnv $ shardLoop

  pure (shard, thread')

runShardM :: Shard -> Logger env -> ShardM env a -> IO a
runShardM shard logEnv action = evalStateC' . runLogTSafe logEnv . unShardM $ action
  where evalStateC' s = evalStateC s (shard ^. shardState)

extractMaybeStream :: (S.IsStream t, Monad m) => t m (Maybe a) -> t m a
extractMaybeStream = S.mapMaybe identity

-- TODO: correct this, add compression
getGatewayHost :: IO Text
getGatewayHost = pure "gateway.discord.gg"

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return

-- | The loop a shard will run on
shardLoop :: ShardM env ()
shardLoop = outerloop $> ()
 where
  controlStream shard = S.repeatM $ do
    msg <- atomically . readTChan $ (shard ^. cmdChan)
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
  outerloop :: ShardM env (Either e a)
  outerloop = runExceptT . forever $ do
    state <- get
    shard <- use shardS

    host  <- case state ^. wsHost of
      Just host -> pure host
      Nothing   -> do
        host <- liftIO getGatewayHost
        wsHost ?= host
        pure host

    -- TODO: handle result of innerloop
    logEnv <- askLogger

    liftIO
      $ runSecureClient (host ^. unpacked) 443 "/?v=7&encoding=json"
                        (runShardM shard logEnv . innerloop)

  -- | The inner loop, handles receiving a message from discord or a command message
  -- | and then decides what to do with it
  innerloop :: Connection -> ShardM env ()
  innerloop ws = do
    shard <- use shardS
    wsConn ?= ws
    logEnv <- askLogger
    liftIO $ S.runStream $ mergedStream shard ws & S.mapM (runShardM shard logEnv . handleMsg)
    wsConn .= Nothing

  -- | Handlers for each message, not sure what they'll need to do exactly yet
  handleMsg (Discord msg) = pure ()
  handleMsg (Control msg) = pure ()

startHeartBeatLoop :: ShardM env ()
startHeartBeatLoop = do
  haltHeartBeat -- cancel any currently running hb thread
  shard <- use shardS
  logEnv <- askLogger
  liftIO $ finally (async . runShardM shard logEnv $ heartBeatLoop)
                   (runShardM shard logEnv haltHeartBeat) $> ()

haltHeartBeat :: ShardM env ()
haltHeartBeat = do
  thread <- use hbThread
  case thread of
    Just t -> liftIO $ cancel t $> ()
    Nothing -> pure ()
  hbThread .= Nothing

sendHeartBeat :: ShardM env ()
sendHeartBeat = pure ()

heartBeatLoop :: ShardM env ()
heartBeatLoop = ($> ()) . liftIO . runMaybeT . forever $ do
  pure ()
  pure ()
