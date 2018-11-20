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
import           Data.Aeson

import           YAHDL.Gateway.Types

newShardState :: ShardState
newShardState = ShardState None None None False

-- | Creates and launches a shard
newShard :: Integer -> Text -> TChan () -> IO (Shard, Async ())
newShard id token evtChan = do
  cmdChan' <- newTChanIO
  stateVar  <- newTVarIO newShardState

  let shard = Shard id evtChan cmdChan' stateVar token

  thread' <- async . shardLoop $ shard

  pure (shard, thread')

runShardM :: Shard -> ShardM a -> IO a
runShardM shard action = evalStateC (unShardM action) (shard ^. field @"shardState")

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
    msg <- atomically . readTChan $ (shard ^. field @"cmdChan")
    pure . Control $ msg

  discordStream wsConn = extractMaybeStream . S.repeatM . runMaybeT $ do
    msg <- liftIO $ receiveData wsConn
    d   <- liftMaybe . decode $ msg
    pure . Discord $ d

  mergedStream wsConn = S.async controlStream (discordStream wsConn)

  -- | The outer loop, sets up the ws conn, etc handles reconnecting and such
  outerloop = runExceptT . liftIO . forever $ do
    state <- get

    host <- case state ^. field @"wsHost" of
      Just host ->
        pure host
      Nothing -> do
        host <- liftIO getGatewayHost
        field @"wsHost" .= host
        pure host

    -- host <- get >>= (field @"wsHost" %= \case
    --                    Just field -> pure field
    --                    Nothing -> do
    --                      host <- getGatewayHost

    --                    )

    -- host <- isEmptyMVar (shard ^. field @"wsHost") >>= \case
    --   True -> do
    --     host <- getGatewayHost
    --     putMVar (shard ^. field @"wsHost") host
    --     pure host
    --   _ -> readMVar (shard ^. field @"wsHost")

    liftIO $ runSecureClient (host ^. unpacked) 443 "/?v=7&encoding=json" innerloop

  -- | The inner loop, handles receiving a message from discord or a command message
  -- | and then decides what to do with it
  innerloop wsConn =
    S.runStream $ mergedStream wsConn & S.mapM (atomically . handleMsg)

  -- | Handlers for each message, not sure what they'll need to do exactly yet
  handleMsg (Discord msg) = pure ()
  handleMsg (Control msg) = pure ()


sendHeartBeat :: Shard -> IO ()
sendHeartBeat shard = undefined


heartBeatLoop :: Shard -> IO ()
heartBeatLoop shard = loop >> final
 where
  loop = forever . runMaybeT $ do
    pure ()
    pure ()

  final = tryTakeMVar (shard ^. field @"hbThread") $> ()
