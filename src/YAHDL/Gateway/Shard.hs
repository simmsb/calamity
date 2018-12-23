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
                                                , formatter
                                                , defaultFormatter
                                                , toLogStr
                                                , TextShow
                                                , Level
                                                , FormattedTime
                                                , LogStr
                                                )
import           Control.Lens                   ( (.=) )
import           Control.Lens.Lens
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
import           YAHDL.Types.DispatchEvents

newShardState :: Shard -> ShardState
newShardState shard =
  ShardState shard Nothing Nothing False Nothing Nothing Nothing Nothing

-- | Creates and launches a shard
newShard :: Int -> Int -> Text -> Logger env -> TChan DispatchData -> IO (Shard, Async ())
newShard id count token logEnv evtChan = mdo
  cmdChan' <- newTChanIO
  stateVar <- newTVarIO (newShardState shard)

  let shard = Shard id count evtChan cmdChan' stateVar token

  thread' <- async . runShardM shard logEnv $ shardLoop

  pure (shard, thread')

runShardM :: Shard -> Logger env -> ShardM env a -> IO a
runShardM shard logEnv = evalStateC' . runLogTSafe logEnv . unShardM
  where evalStateC' s = evalStateC s (shard ^. #shardState)

        withShardIDLog logEnv = logEnv { formatter = shardIDFormatter }

        shardIDFormatter :: TextShow env => Level -> FormattedTime -> env -> Text -> LogStr
        shardIDFormatter level time env msg = (toLogStr ("[ShardID: "+|shard ^. #shardID|+"]" :: Text))
          <> defaultFormatter level time env msg

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
  controlStream shard = Control <$> (liftIO . atomically . readTChan $ (shard ^. #cmdChan))

  discordStream ws = runMaybeT $ do
    msg <- liftIO $ receiveData ws
    d   <- liftMaybe . A.decode $ msg
    pure . Discord $ d

  mergedStream :: Shard -> Connection -> ExceptT ShardException (ShardM env) ShardMsg
  mergedStream shard ws = do
    setExc <- use #setExc
    #setExc .= Nothing

    -- someone set an exception (probably the heartbeat loop, shove it downstream)
    case setExc of
      Just x -> throwE x
      _      -> pure ()

    res <- liftIO $ race (controlStream shard) (discordStream ws)

    case res of
      Left x         -> pure x
      Right (Just x) -> pure x
      Right Nothing  -> mergedStream shard ws

  -- | The outer loop, sets up the ws conn, etc handles reconnecting and such
  -- Currently if this goes to the error path we just exit the forever loop
  -- and the shard stops, maybe we might want to do some extra logic to reboot
  -- the shard, or maybe force a resharding
  outerloop :: ShardM env (Either ShardException ())
  outerloop = runExceptT . forever $ do
    state <- get
    shard <- use #shardS

    host  <- case state ^. #wsHost of
      Just host -> pure host
      Nothing   -> do
        host <- liftIO getGatewayHost
        #wsHost ?= host
        pure host

    logEnv <- askLogger

    innerLoopVal <- liftIO $ runSecureClient (host ^. unpacked)
                    443
                    "/?v=7&encoding=json"
                    (runShardM shard logEnv . innerloop)

    case innerLoopVal of
      Left ShardExcShutDown -> do
        debug "Shutting down shard"
        throwE ShardExcShutDown

      Left ShardExcRestart ->
        debug "Restaring shard"
        -- we restart normally when we loop
      _ -> pure ()

  -- | The inner loop, handles receiving a message from discord or a command message
  -- | and then decides what to do with it
  innerloop :: Connection -> ShardM env (Either ShardException a)
  innerloop ws = do
    shard <- use #shardS
    #wsConn ?= ws
    logEnv <- askLogger

    seqNum'    <- use #seqNum
    sessionID' <- use #sessionID

    case (seqNum', sessionID') of
      (Just n, Just s) -> do
        debug $ "Resuming shard (sessionID: "+|s|+", seq: "+|n|+")"
        sendToWs (Resume ResumeData
                  { token = shard ^. #token
                  , sessionID = s
                  , seq = n
                  })
      _ -> do
        debug "Identifying shard"
        sendToWs (Identify IdentifyData
                  { token = shard ^. #token
                  , properties = IdentifyProps
                                 { os = "Linux" -- TODO: correct
                                 , browser = "YetAnotherHaskellLib: "
                                 , device = "YetAnotherHaskellLib"
                                 }
                  , compress = False
                  , largeThreshold = 250
                  , shard = (shard ^. #shardID,
                             shard ^. #shardCount)
                  , presence = Nothing
                  })

    result <- runExceptT . forever $ mergedStream shard ws >>= handleMsg

    #wsConn .= Nothing
    pure result

  -- | Handlers for each message, not sure what they'll need to do exactly yet
  handleMsg :: ShardMsg -> ExceptT ShardException (ShardM env) ()
  handleMsg (Discord msg) = case msg of
    Dispatch seq data' -> do
      debug $ "Handling event: ("+||data'||+")"
      #seqNum ?= seq

      case data' of
        Ready rdata' ->
          #sessionID ?= (rdata' ^. #sessionID)

        _ -> pure ()

      shard <- lift $ use #shardS
      liftIO . atomically $ writeTChan (shard ^. #evtChan) data'

    HeartBeatReq -> do
      debug "Received heartbeat request"
      lift sendHeartBeat

    Reconnect -> do
      debug "Being asked to restart by Discord"
      throwE ShardExcRestart

    InvalidSession resumable -> do
      if resumable
      then do
        debug "Received non-resumable invalid session"
        #sessionID .= Nothing
        #seqNum .= Nothing
        liftIO $ threadDelay 1500000
      else
        debug "Received resumable invalid session"
      throwE ShardExcRestart

    Hello interval -> do
      debug $ "Received hello, beginning to heartbeat at an interval of "+|interval|+"ms"
      lift $ startHeartBeatLoop interval

    HeartBeatAck -> do
      debug "Received heartbeat ack"
      lift $ #hbResponse .= True

  handleMsg (Control msg) = case msg of
    SendPresence data' -> do
      debug $ "Sending presence: ("+||data'||+")"
      lift . sendToWs $ StatusUpdate data'

    Restart            -> throwE ShardExcRestart
    ShutDown           -> throwE ShardExcShutDown

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
  debug $ "Sending heartbeat (seq: "+|seq|+")"
  sendToWs $ HeartBeat seq
  #hbResponse .= False

heartBeatLoop :: Int -> ShardM env ()
heartBeatLoop interval = ($> ()) . runMaybeT . forever $ do
  lift sendHeartBeat
  liftIO . threadDelay $ interval * 1000
  hasResp <- use #hbResponse
  unless hasResp $ do
    debug "No heartbeat response, restarting shard"
    #setExc ?= ShardExcRestart
    mzero -- exit the loop, we had no response
