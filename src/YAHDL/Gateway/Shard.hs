-- |

module YAHDL.Gateway.Shard
  ( Shard(..)
  , newShard
  )
where

import           Control.Monad.Trans.Maybe
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Data.Text.Strict.Lens
import           Control.Concurrent.STM.TMVar
import qualified Streamly.Prelude              as S
import qualified Streamly                      as S
import           Wuss
import           Network.WebSockets             ( Connection
                                                , receiveData
                                                , sendClose
                                                , sendTextData
                                                )
import           Data.Aeson

data Shard = Shard
  { shardId :: Integer
  , evtChan :: TChan () -- TODO: replace this with the event type
  , cmdChan :: TChan ControlMessage -- TODO: replace this with the shard command type
  , seqNum :: TMVar Integer
  , wsHost :: MVar Text
  , token :: Text
  } deriving (Generic)

-- TODO: change this from RawDiscordMessage to DiscordMessage, add decoder & handler
data ShardMsg = Discord RawDiscordMessage | Control ControlMessage

-- TODO: this
data DiscordMessage = DEvent
  deriving (Show, Generic)

data RawDiscordMessage = RDEvent
  deriving (Show, Generic)

instance ToJSON RawDiscordMessage
instance FromJSON RawDiscordMessage

data ControlMessage = Restart
  deriving (Show)

-- | Creates and launches a shard
newShard :: Integer -> Text -> TChan () -> IO (Shard, Async ())
newShard id token evtChan = do
  cmdChan' <- newTChanIO
  seqNum'  <- newEmptyTMVarIO
  wsHost'  <- newEmptyMVar

  let shard = Shard id evtChan cmdChan' seqNum' wsHost' token

  thread' <- async . shardLoop $ shard

  pure (shard, thread')

extractMaybeStream :: (S.IsStream t, Monad m) => t m (Maybe a) -> t m a
extractMaybeStream = S.mapMaybe identity

-- TODO: correct this, add compression
getGatewayHost :: IO Text
getGatewayHost = pure "gateway.discord.gg"

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return

-- | The loop a shard will run on
shardLoop :: Shard -> IO ()
shardLoop shard = outerloop >> pure ()
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
  outerloop = runMaybeT . liftIO . forever $ do
    host <- isEmptyMVar (shard ^. field @"wsHost") >>= \case
      True -> do
        host <- getGatewayHost
        putMVar (shard ^. field @"wsHost") host
        pure host
      _ -> readMVar (shard ^. field @"wsHost")

    runSecureClient (host ^. unpacked) 443 "/?v=7&encoding=json" innerloop

  -- | The inner loop, handles receiving a message from discord or a command message
  -- | and then decides what to do with it
  innerloop wsConn =
    S.runStream $ mergedStream wsConn & S.mapM (atomically . handleMsg)

  handleMsg (Discord msg) = pure ()
  handleMsg (Control msg) = pure ()
