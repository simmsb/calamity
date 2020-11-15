-- | The shard logic
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Calamity.Gateway.Shard
    ( Shard(..)
    , newShard ) where

import           Calamity.Gateway.DispatchEvents
import           Calamity.Gateway.Intents
import           Calamity.Gateway.Types
import           Calamity.Internal.Utils
import           Calamity.Internal.RunIntoIO
import           Calamity.Metrics.Eff
import           Calamity.Types.LogEff
import           Calamity.Types.Token

import           Control.Concurrent
import           Control.Concurrent.Async
import qualified Control.Concurrent.Chan.Unagi   as UC
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBMQueue
import           Control.Exception
import qualified Control.Exception.Safe          as Ex
import           Control.Lens
import           Control.Monad
import           Control.Monad.State.Lazy

import qualified Data.Aeson                      as A
import           Data.Functor
import           Data.IORef
import           Data.Maybe
import qualified Data.Text.Lazy                  as L

import           DiPolysemy                      hiding ( debug, error, info )

import           Fmt

import           Network.WebSockets              ( Connection, ConnectionException(..), receiveData, sendCloseCode
                                                 , sendTextData )

import           Polysemy                        ( Sem )
import qualified Polysemy                        as P
import qualified Polysemy.Async                  as P
import qualified Polysemy.AtomicState            as P
import qualified Polysemy.Error                  as P
import qualified Polysemy.Resource               as P

import           Prelude                         hiding ( error )

import           Wuss

runWebsocket :: P.Members '[P.Final IO, P.Embed IO] r
  => L.Text
  -> L.Text
  -> (Connection -> P.Sem r a)
  -> P.Sem r (Maybe a)
runWebsocket host path ma = do
  inner <- bindSemToIO ma
  P.embed $ runSecureClient (L.unpack host) 443 (L.unpack path) inner

newShardState :: Shard -> ShardState
newShardState shard = ShardState shard Nothing Nothing False Nothing Nothing Nothing

-- | Creates and launches a shard
newShard :: P.Members '[LogEff, MetricEff, P.Embed IO, P.Final IO, P.Async] r
         => L.Text
         -> Int
         -> Int
         -> Token
         -> Maybe StatusUpdateData
         -> Intents
         -> UC.InChan CalamityEvent
         -> Sem r (UC.InChan ControlMessage, Async (Maybe ()))
newShard gateway id count token presence intents evtIn = do
  (cmdIn, stateVar) <- P.embed $ mdo
    (cmdIn, cmdOut) <- UC.newChan
    stateVar <- newIORef $ newShardState shard
    let shard = Shard id count gateway evtIn cmdOut stateVar (rawToken token) presence intents
    pure (cmdIn, stateVar)

  let runShard = P.runAtomicStateIORef stateVar shardLoop
  let action = push "calamity-shard" . attr "shard-id" id $ runShard

  thread' <- P.async action

  pure (cmdIn, thread')

sendToWs :: ShardC r => SentDiscordMessage -> Sem r ()
sendToWs data' = do
  wsConn' <- P.atomicGets wsConn
  case wsConn' of
    Just wsConn -> do
      let encodedData = A.encode data'
      debug $ "sending " +|| data' ||+ " encoded to " +|| encodedData ||+ " to gateway"
      P.embed . sendTextData wsConn $ encodedData
    Nothing -> debug "tried to send to closed WS"

tryWriteTBMQueue' :: TBMQueue a -> a -> STM Bool
tryWriteTBMQueue' q v = do
  v' <- tryWriteTBMQueue q v
  case v' of
    Just False -> retry
    Just True  -> pure True
    Nothing    -> pure False

restartUnless :: P.Members '[LogEff, P.Error ShardFlowControl] r => L.Text -> Maybe a -> P.Sem r a
restartUnless _   (Just a) = pure a
restartUnless msg Nothing  = do
  error msg
  P.throw ShardFlowRestart

-- | The loop a shard will run on
shardLoop :: ShardC r => Sem r ()
shardLoop = do
  activeShards <- registerGauge "active_shards" mempty
  void $ modifyGauge (+ 1) activeShards
  void outerloop
  void $ modifyGauge (subtract 1) activeShards
  debug "Shard shut down"
 where
  controlStream :: Shard -> TBMQueue ShardMsg -> IO ()
  controlStream shard outqueue = inner
    where
      q = shard ^. #cmdOut
      inner = do
        v <- UC.readChan q
        r <- atomically $ tryWriteTBMQueue' outqueue (Control v)
        when r inner

  handleWSException :: SomeException -> IO (Either (ControlMessage, Maybe L.Text) a)
  handleWSException e = pure $ case fromException e of
    Just (CloseRequest code _)
      | code `elem` [1000, 4004, 4010, 4011] ->
        Left (ShutDownShard, Nothing)
    e -> Left (RestartShard, Just . L.pack . show $ e)

  discordStream :: P.Members '[LogEff, MetricEff, P.Embed IO, P.Final IO] r => Connection -> TBMQueue ShardMsg -> Sem r ()
  discordStream ws outqueue = inner
    where inner = do
            msg <- P.embed $ Ex.catchAny (Right <$> receiveData ws) handleWSException

            case msg of
              Left (c, reason) -> do
                whenJust reason (\r -> error $ "Shard closed with reason: " <> r)
                P.embed . atomically $ writeTBMQueue outqueue (Control c)

              Right msg' -> do
                let decoded = A.eitherDecode msg'
                r <- case decoded of
                  Right a ->
                    P.embed . atomically $ tryWriteTBMQueue' outqueue (Discord a)
                  Left e -> do
                    error $ "Failed to decode: "+|e|+""
                    pure True
                when r inner

  -- | The outer loop, sets up the ws conn, etc handles reconnecting and such
  outerloop :: ShardC r => Sem r ()
  outerloop = whileMFinalIO $ do
    shard :: Shard <- P.atomicGets (^. #shardS)
    let host = shard ^. #gateway
    let host' =  fromMaybe host $ L.stripPrefix "wss://" host
    info $ "starting up shard "+| (shard ^. #shardID) |+" of "+| (shard ^. #shardCount) |+""

    innerLoopVal <- runWebsocket host' "/?v=8&encoding=json" innerloop

    case innerLoopVal of
      Just ShardFlowShutDown -> do
        info "Shutting down shard"
        pure False

      Just ShardFlowRestart -> do
        info "Restaring shard"
        pure True
        -- we restart normally when we loop

      Nothing -> do -- won't happen unless innerloop starts using a non-deterministic effect
        info "Restarting shard (abnormal reasons?)"
        pure True

  -- | The inner loop, handles receiving a message from discord or a command message
  -- and then decides what to do with it
  innerloop :: ShardC r => Connection -> Sem r ShardFlowControl
  innerloop ws = do
    debug "Entering inner loop of shard"

    shard <- P.atomicGets (^. #shardS)
    P.atomicModify (#wsConn ?~ ws)

    seqNum'    <- P.atomicGets (^. #seqNum)
    sessionID' <- P.atomicGets (^. #sessionID)

    case (seqNum', sessionID') of
      (Just n, Just s) -> do
        debug $ "Resuming shard (sessionID: "+|s|+", seq: "+|n|+")"
        sendToWs (Resume ResumeData
                  { token = shard ^. #token
                  , sessionID = s
                  , seq = n
                  })
      _noActiveSession -> do
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
                  , presence = shard ^. #initialStatus
                  , intents = shard ^. #intents
                  })

    result <- P.resourceToIOFinal $ P.bracket (P.embed $ newTBMQueueIO 1)
      (P.embed . atomically . closeTBMQueue)
      (\q -> do
        debug "handling events now"
        _controlThread <- P.async . P.embed $ controlStream shard q
        _discordThread <- P.async $ discordStream ws q
        P.raise . untilJustFinalIO . (leftToMaybe <$>) . P.runError $ do
          -- only we close the queue
          msg <- P.embed . atomically $ readTBMQueue q
          handleMsg =<< restartUnless "shard message stream closed by someone other than the sink" msg)

    debug "Exiting inner loop of shard"

    P.atomicModify (#wsConn .~ Nothing)
    haltHeartBeat
    pure result

  -- | Handlers for each message, not sure what they'll need to do exactly yet
  handleMsg :: (ShardC r, P.Member (P.Error ShardFlowControl) r) => ShardMsg -> Sem r ()
  handleMsg (Discord msg) = case msg of
    EvtDispatch sn data' -> do
      -- trace $ "Handling event: ("+||data'||+")"
      P.atomicModify (#seqNum ?~ sn)

      case data' of
        Ready rdata' ->
          P.atomicModify (#sessionID ?~ (rdata' ^. #sessionID))

        _NotReady -> pure ()

      shard <- P.atomicGets (^. #shardS)
      P.embed $ UC.writeChan (shard ^. #evtIn) (Dispatch (shard ^. #shardID) data')

    HeartBeatReq -> do
      debug "Received heartbeat request"
      sendHeartBeat

    Reconnect -> do
      debug "Being asked to restart by Discord"
      P.throw ShardFlowRestart

    InvalidSession resumable -> do
      if resumable
      then
        info "Received resumable invalid session"
      else do
        info "Received non-resumable invalid session, sleeping for 15 seconds then retrying"
        P.atomicModify (#sessionID .~ Nothing)
        P.atomicModify (#seqNum .~ Nothing)
        P.embed $ threadDelay (15 * 1000 * 1000)
      P.throw ShardFlowRestart

    Hello interval -> do
      info $ "Received hello, beginning to heartbeat at an interval of "+|interval|+"ms"
      startHeartBeatLoop interval

    HeartBeatAck -> do
      debug "Received heartbeat ack"
      P.atomicModify (#hbResponse .~ True)

  handleMsg (Control msg) = case msg of
    SendPresence data' -> do
      debug $ "Sending presence: ("+||data'||+")"
      sendToWs $ StatusUpdate data'

    RestartShard       -> P.throw ShardFlowRestart
    ShutDownShard      -> P.throw ShardFlowShutDown

startHeartBeatLoop :: ShardC r => Int -> Sem r ()
startHeartBeatLoop interval = do
  haltHeartBeat -- cancel any currently running hb thread
  thread <- P.async $ heartBeatLoop interval
  P.atomicModify (#hbThread ?~ thread)

haltHeartBeat :: ShardC r => Sem r ()
haltHeartBeat = do
  thread <- P.atomicState @ShardState . (swap .) . runState $ do
    thread <- use #hbThread
    #hbThread .= Nothing
    pure thread
  case thread of
    Just t  -> do
      debug "Stopping heartbeat thread"
      P.embed (void $ cancel t)
    Nothing -> pure ()

sendHeartBeat :: ShardC r => Sem r ()
sendHeartBeat = do
  sn <- P.atomicGets (^. #seqNum)
  debug $ "Sending heartbeat (seq: " +|| sn ||+ ")"
  sendToWs $ HeartBeat sn
  P.atomicModify (#hbResponse .~ False)

heartBeatLoop :: ShardC r => Int -> Sem r ()
heartBeatLoop interval = untilJustFinalIO . (leftToMaybe <$>) . P.runError $ do
  sendHeartBeat
  P.embed . threadDelay $ interval * 1000
  unlessM (P.atomicGets (^. #hbResponse)) $ do
    debug "No heartbeat response, restarting shard"
    wsConn <- P.note () =<< P.atomicGets (^. #wsConn)
    P.embed $ sendCloseCode wsConn 4000 ("No heartbeat in time" :: L.Text)
    P.throw ()
