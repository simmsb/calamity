-- |
{-# LANGUAGE RecursiveDo #-}

module Calamity.Gateway.Shard
    ( Shard(..)
    , newShard ) where

import           Calamity.Gateway.DispatchEvents
import           Calamity.Gateway.Types
import           Calamity.Types.General

import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Control.Lens                          ( (.=) )
import           Control.Monad.State.Concurrent.Strict
import           Control.Monad.Trans.Maybe

import qualified Data.Aeson                            as A
import           Data.Maybe
import           Data.Text                             ( stripPrefix )
import           Data.Text.Strict.Lens

import           Network.WebSockets                    ( Connection, ConnectionException(..), receiveData, sendCloseCode
                                                       , sendTextData )
import Control.Concurrent.STM.TBMQueue

import           Polysemy                              ( Sem )
import qualified Polysemy.Embed                        as P
import qualified Polysemy.Error                        as P
import qualified Polysemy.Final                        as P
import qualified Polysemy.Async                        as P
import qualified Polysemy.Resource                     as P
import qualified Polysemy.AtomicState                  as P

import qualified System.Log.Simple                     as SLS

import qualified Polysemy as P
import qualified Polysemy.Embed as P

import           Wuss

newShardState :: Shard -> ShardState
newShardState shard = ShardState shard Nothing Nothing False Nothing Nothing Nothing

-- | Creates and launches a shard
newShard :: ShardC r => Text -> Int -> Int -> Token -> TQueue DispatchData -> Sem r (Shard, Async ())
newShard gateway id count token logEnv evtQueue = mdo
  cmdQueue' <- newTQueueIO
  stateVar <- newTVarIO (newShardState shard)
  let shard = Shard id count gateway evtQueue cmdQueue' stateVar (rawToken token) thread'

  let action = attr "shard-id" id . push "calamity-shard" $ shardLoop

  thread' <- P.async . shard logEnv $ action

  pure (shard, thread')

sendToWs :: ShardC r => SentDiscordMessage -> Sem r ()
sendToWs data' = do
  wsConn <- fromJust <$> P.atomicGets wsConn
  let encodedData = A.encode data'
  debug $ "sending " +|| data' ||+ " encoded to " +|| encodedData ||+ " to gateway"
  P.embed . sendTextData wsConn $ encodedData
  trace "done sending data"

untilResult :: Monad m => m (Maybe a) -> m a
untilResult m = m >>= \case
  Just a  -> pure a
  Nothing -> untilResult m

fromEither :: Either a a -> a
fromEither (Left a) = a
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
      then pure . Left $ ShutDown
      else pure . Left $ Restart

  e                       -> throwIO e

-- | The loop a shard will run on
shardLoop :: ShardC r => Sem r ()
shardLoop = do
  trace "entering shardLoop"
  void outerloop
  trace "leaving shardLoop"
 where
  controlStream :: Shard -> TBMQueue ShardMsg -> IO ()
  controlStream shard outqueue = inner
    where
      q = shard ^. #cmdQueue
      inner = do
        v <- atomically $ readTQueue q
        r <- atomically $ tryWriteTBMQueue (Control v)
        case r of
          Nothing -> pure ()
          _       -> inner

  discordStream :: P.Members '[LogEff, Embed IO] r => _ -> TBMQueue ShardMsg -> Sem r ()
  discordStream ws outqueue = inner
    where inner = do
            msg <- P.embed . checkWSClose $ receiveData ws

            trace $ "Received from stream: "+||msg||+""

            case msg of
              Left c ->
                P.embed . atomically $ writeTBMQueue c

              Right msg' -> do
                let decoded = A.eitherDecode msg'
                d <- case decoded of
#ifndef PARSE_PRESENCES
                  -- if we're discarding presences, bin them earlier, here
                  Right (Dispatch _ (PresenceUpdate _)) ->
                    pure ()
#endif
                  Right a -> pure a
                  Left e -> do
                    error $ "Failed to decode: "+|e|+""
                r <- P.embed . atomically $ tryWriteTBMQueue a
                case r of
                  Nothing -> pure ()
                  _       -> inner

  mergedStream ::  Log -> Shard -> Connection -> ExceptT ShardException ShardM ShardMsg
  mergedStream logEnv shard ws =
    liftIO (fromEither <$> race (controlStream shard) (discordStream logEnv ws))

  -- | The outer loop, sets up the ws conn, etc handles reconnecting and such
  -- Currently if this goes to the error path we just exit the forever loop
  -- and the shard stops, maybe we might want to do some extra logic to reboot
  -- the shard, or maybe force a resharding
  outerloop :: ShardC r => Sem r (Either ShardException ())
  outerloop = P.runError . forever $ do
    shard <- fromJust <$> P.atomicGets shardS
    let host = shard ^. #gateway
    let host' =  fromMaybe host $ stripPrefix "wss://" host
    info $ "starting up shard "+| (shard ^. #shardID) |+" of "+| (shard ^. #shardCount) |+""

    ins <- P.getInspectorS
    -- innerloop' <- P.runS innerloop
    innerLoopVal <- P.embed $ runSecureClient (host' ^. unpacked)
                    443
                    "/?v=7&encoding=json"
                    ((P.inspect ins <$>) . innerloop)

    case innerLoopVal of
      ShardExcShutDown -> do
        info "Shutting down shard"
        P.throw ShardExcShutDown

      ShardExcRestart ->
        info "Restaring shard"
        -- we restart normally when we loop

  -- | The inner loop, handles receiving a message from discord or a command message
  -- and then decides what to do with it
  innerloop :: ShardC r => Connection -> Sem r ShardException
  innerloop ws = do
    trace "Entering inner loop of shard"

    shard <- fromJust <$> P.atomicGets shardS
    P.atomicPut $ Just ws

    seqNum'    <- P.atomicGets seqNum
    sessionID' <- P.atomicGets sessionID

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
                                 { browser = "Calamity: https://github.com/nitros12/calamity"
                                 , device = "Calamity: https://github.com/nitros12/calamity"
                                 }
                  , compress = False
                  , largeThreshold = 250
                  , shard = (shard ^. #shardID,
                             shard ^. #shardCount)
                  , presence = Nothing
                  })

    result <- runResourceToIO $ P.bracket (P.embed newTBMQueueIO) (\q -> do
        controlThread <- P.async $ controlStream shard q
        discordThread <- P.async $ discordStream ws q
        P.runError $ forever $ do
          -- only we close the queue
          Just msg <- P.embed . atomically $ readTBMQueue q
          handleMsg msg)
      (\q -> P.embed . atomically $ closeTBMQueue q)

    debug "Exiting inner loop of shard"

    P.atomicPut wsConn Nothing
    pure . fromEitherVoid $ result

  -- | Handlers for each message, not sure what they'll need to do exactly yet
  handleMsg :: (ShardC r, P.Member (P.Error ShardException) r) => ShardMsg -> Sem r ()
  handleMsg (Discord msg) = case msg of
    Dispatch seqNum data' -> do
      trace $ "Handling event: ("+||data'||+")"
      #seqNum ?= seqNum

      case data' of
        Ready rdata' ->
          #sessionID ?= (rdata' ^. #sessionID)

        _ -> pure ()

      shard <- lift $ use #shardS
      liftIO . atomically $ writeTQueue (shard ^. #evtQueue) data'
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
        info "Received non-resumable invalid session, sleeping for 15 seconds then retrying"
        #sessionID .= Nothing
        #seqNum .= Nothing
        liftIO $ threadDelay 1500000
      else
        info "Received resumable invalid session"
      throwE ShardExcRestart

    Hello interval -> do
      info $ "Received hello, beginning to heartbeat at an interval of "+|interval|+"ms"
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
  shard <- use #shardS
  logEnv <- askLog
  -- TODO: we need to send a close to the ws depending on how we exited from the hb loop
  void . liftIO . async $ finally (runShardM shard logEnv $ heartBeatLoop interval)
    (runShardM shard logEnv haltHeartBeat)

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
  debug $ "Sending heartbeat (seq: " +|| seqNum ||+ ")"
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
