-- |

module YAHDL.Gateway.Shard
  ( Shard(..)
  , newShard
  )
where

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

newShardState :: ShardState
newShardState = ShardState Nothing Nothing Nothing False

-- | Creates and launches a shard
newShard :: Integer -> Text -> TChan () -> IO (Shard, Async ())
newShard id token evtChan = do
  cmdChan' <- newTChanIO
  stateVar <- newTVarIO newShardState

  let shard = Shard id evtChan cmdChan' stateVar token

  thread' <- async . runShardM shard . shardLoop $ shard

  pure (shard, thread')

runShardM :: Shard -> ShardM a -> IO a
runShardM shard action = evalStateC action (shard ^. shardState)

extractMaybeStream :: (S.IsStream t, Monad m) => t m (Maybe a) -> t m a
extractMaybeStream = S.mapMaybe identity

-- TODO: correct this, add compression
getGatewayHost :: IO Text
getGatewayHost = pure "gateway.discord.gg"

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return

-- | The loop a shard will run on
shardLoop :: Shard -> ShardM ()
shardLoop shard = outerloop $> ()
 where
  controlStream = S.repeatM $ do
    msg <- atomically . readTChan $ (shard ^. cmdChan)
    pure . Control $ msg

  discordStream wsConn = extractMaybeStream . S.repeatM . runMaybeT $ do
    msg <- liftIO $ receiveData wsConn
    d   <- liftMaybe . A.decode $ msg
    pure . Discord $ d

  mergedStream wsConn = S.async controlStream (discordStream wsConn)

  -- | The outer loop, sets up the ws conn, etc handles reconnecting and such
  -- Currently if this goes to the error path we just exit the forever loop
  -- and the shard stops, maybe we might want to do some extra logic to reboot
  -- the shard, or maybe force a resharding
  outerloop = runExceptT . forever $ do
    state <- get

    host  <- case state ^. wsHost of
      Just host -> pure host
      Nothing   -> do
        host <- liftIO getGatewayHost
        wsHost ?= host
        pure host

    liftIO
      $ runSecureClient (host ^. unpacked) 443 "/?v=7&encoding=json" innerloop

  -- | The inner loop, handles receiving a message from discord or a command message
  -- | and then decides what to do with it
  innerloop wsConn =
    S.runStream $ mergedStream wsConn & S.mapM (atomically . handleMsg)

  -- | Handlers for each message, not sure what they'll need to do exactly yet
  handleMsg (Discord msg) = pure ()
  handleMsg (Control msg) = pure ()


sendHeartBeat :: Shard -> ShardM ()
sendHeartBeat shard = pure ()

heartBeatLoop :: Shard -> ShardM ()
heartBeatLoop shard = liftIO loop >> final
 where
  loop = forever . runMaybeT $ do
    pure ()
    pure ()

  final = hbThread .= Nothing
