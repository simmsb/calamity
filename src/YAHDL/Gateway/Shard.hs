-- |

{-# LANGUAGE RecursiveDo #-}

module YAHDL.Gateway.Shard
  ( Shard(..)
  , newShard
  )
where

import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Lens                   ( (.=) )
import           Control.Monad.State.Concurrent.Strict
import           Control.Monad.Trans.Maybe
import qualified Data.Aeson                    as A
import           Data.Text                      ( stripPrefix )
import           Data.Text.Strict.Lens
import           Data.Maybe
import qualified System.Log.Simple             as SLS
import           Network.WebSockets             ( Connection
                                                , receiveData
                                                , sendCloseCode
                                                , sendTextData
                                                , ConnectionException(..)
                                                )
import           Wuss

import           YAHDL.Gateway.Types
import           YAHDL.Types.DispatchEvents
import           YAHDL.Types.General


newShardState :: Shard -> ShardState
newShardState shard =
  ShardState shard Nothing Nothing False Nothing Nothing Nothing

-- | Creates and launches a shard
newShard :: Text -> Int -> Int -> Token -> Log -> TChan DispatchData -> IO (Shard, Async ())
newShard gateway id count token logEnv evtChan = mdo
  cmdChan' <- newTChanIO
  stateVar <- newTVarIO (newShardState shard)
  let shard = Shard id count gateway evtChan cmdChan' stateVar (rawToken token) thread'

  let action = scope ("[ShardID: "+|id|+"]") shardLoop

  thread' <- async . runShardM shard logEnv $ action

  pure (shard, thread')

runShardM :: Shard -> Log -> ShardM a -> IO a
runShardM shard logEnv = evalStateC' . withLog logEnv . unShardM
  where evalStateC' s = evalStateC s (shard ^. #shardState)

sendToWs :: SentDiscordMessage -> ShardM ()
sendToWs data' = do
  wsConn <- fromJust <$> use #wsConn
  let encodedData = A.encode data'
  debug $ "sending "+||data'||+" encoded to "+|| encodedData ||+" to gateway"
  liftIO . sendTextData wsConn $ encodedData
  trace "done sending data"

untilResult :: Monad m => m (Maybe a) -> m a
untilResult m = m >>= \case
  Just a  -> pure a
  Nothing -> untilResult m

fromEither :: Either a a -> a
fromEither (Left a)  = a
fromEither (Right a) = a

fromEitherVoid :: Either a Void -> a
fromEitherVoid (Left a) = a
fromEitherVoid (Right a) = absurd a -- yeet

-- | Catches ws close events and decides if we can restart or not
checkWSClose :: IO a -> IO (Either ControlMessage a)
checkWSClose m = (Right <$> m) `catch` \case
  e@(CloseRequest code _) -> do
    print e
    if code `elem` [1000, 4004, 4010, 4011]
    then pure . Left $ Restart
    else throwIO e

  e -> throwIO e

-- | The loop a shard will run on
shardLoop :: ShardM ()
shardLoop = do
  trace "entering shardLoop"
  void outerloop
  trace "leaving shardLoop"
 where
  controlStream shard = Control <$> (liftIO . atomically . readTChan $ (shard ^. #cmdChan))

  discordStream logEnv ws = untilResult . runMaybeT $ do
    msg <- liftIO . checkWSClose $ receiveData ws

    case msg of
      Left c ->
        pure . Control $ c

      Right msg' -> do
        let decoded = A.eitherDecode msg'
        d <- case decoded of
          Right a -> pure a
          Left e -> do
            SLS.writeLog logEnv SLS.Error $ "Failed to decode: "+|e|+""
            mzero
        pure . Discord $ d

  mergedStream :: Log -> Shard -> Connection -> ExceptT ShardException ShardM ShardMsg
  mergedStream logEnv shard ws =
    liftIO (fromEither <$> race (controlStream shard) (discordStream logEnv ws))

  -- | The outer loop, sets up the ws conn, etc handles reconnecting and such
  -- Currently if this goes to the error path we just exit the forever loop
  -- and the shard stops, maybe we might want to do some extra logic to reboot
  -- the shard, or maybe force a resharding
  outerloop :: ShardM (Either ShardException ())
  outerloop = runExceptT . forever $ do
    shard <- use #shardS
    let host = shard ^. #gateway
    let host' =  fromMaybe host $ stripPrefix "wss://" host
    info $ "starting up shard "+| (shard ^. #shardID) |+" of "+| (shard ^. #shardCount) |+""

    logEnv <- askLog

    innerLoopVal <- liftIO $ runSecureClient (host' ^. unpacked)
                    443
                    "/?v=7&encoding=json"
                    (runShardM shard logEnv . innerloop)

    case innerLoopVal of
      ShardExcShutDown -> do
        trace "Shutting down shard"
        throwE ShardExcShutDown

      ShardExcRestart ->
        trace "Restaring shard"
        -- we restart normally when we loop

  -- | The inner loop, handles receiving a message from discord or a command message
  -- and then decides what to do with it
  innerloop :: Connection -> ShardM ShardException
  innerloop ws = do
    trace "Entering inner loop of shard"

    shard <- use #shardS
    #wsConn ?= ws

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

    logEnv <- askLog
    result <- runExceptT . forever $ (mergedStream logEnv shard ws >>= handleMsg)

    debug "Exiting inner loop of shard"

    #wsConn .= Nothing
    pure . fromEitherVoid $ result

  -- | Handlers for each message, not sure what they'll need to do exactly yet
  handleMsg :: ShardMsg -> ExceptT ShardException ShardM ()
  handleMsg (Discord msg) = case msg of
    Dispatch seqNum data' -> do
      debug $ "Handling event: ("+||data'||+")"
      #seqNum ?= seqNum

      case data' of
        Ready rdata' ->
          #sessionID ?= (rdata' ^. #sessionID)

        _ -> pure ()

      shard <- lift $ use #shardS
      liftIO . atomically $ writeTChan (shard ^. #evtChan) data'
      seqNum' <- use #seqNum
      trace $ "Done handling event, seq is now: "+||seqNum'||+""

    HeartBeatReq -> do
      debug "Received heartbeat request"
      lift sendHeartBeat

    Reconnect -> do
      debug "Being asked to restart by Discord"
      throwE ShardExcRestart

    InvalidSession resumable -> do
      if resumable
      then do
        debug "Received non-resumable invalid session, sleeping for 15 seconds then retrying"
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

startHeartBeatLoop :: Int -> ShardM ()
startHeartBeatLoop interval = do
  haltHeartBeat -- cancel any currently running hb thread
  shard  <- use #shardS
  logEnv <- askLog
  -- TODO: we need to send a close to the ws depending on how we exited from the hb loop
  void . liftIO . async $ finally (runShardM shard logEnv $ heartBeatLoop interval) (runShardM shard logEnv haltHeartBeat)

haltHeartBeat :: ShardM ()
haltHeartBeat = do
  thread <- use #hbThread
  case thread of
    Just t  -> liftIO $ cancel t $> ()
    Nothing -> pure ()
  #hbThread .= Nothing

sendHeartBeat :: ShardM ()
sendHeartBeat = do
  seqNum <- use #seqNum
  debug $ "Sending heartbeat (seq: "+||seqNum||+")"
  sendToWs $ HeartBeat seqNum
  #hbResponse .= False

heartBeatLoop :: Int -> ShardM ()
heartBeatLoop interval = ($> ()) . runMaybeT . forever $ do
  lift sendHeartBeat
  liftIO . threadDelay $ interval * 1000
  hasResp <- use #hbResponse
  unless hasResp $ do
    debug "No heartbeat response, restarting shard"
    wsConn <- fromJust <$> use #wsConn
    liftIO $ sendCloseCode wsConn 4000 ("No heartbeat in time" :: Text)
    mzero -- exit the loop, we had no response
