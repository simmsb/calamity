{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
-- | The shard logic
module Calamity.Gateway.Shard (
  Shard (..),
  newShard,
) where

import Calamity.Gateway.DispatchEvents (
  CalamityEvent (Dispatch),
  DispatchData (Ready),
 )
import Calamity.Gateway.Intents (Intents)
import Calamity.Gateway.Types (
  ControlMessage (..),
  IdentifyData (
    IdentifyData,
    compress,
    intents,
    largeThreshold,
    presence,
    properties,
    shard,
    token
  ),
  IdentifyProps (IdentifyProps, browser, device),
  ReceivedDiscordMessage (
    EvtDispatch,
    HeartBeatAck,
    HeartBeatReq,
    Hello,
    InvalidSession,
    Reconnect
  ),
  ResumeData (ResumeData, seq, sessionID, token),
  SentDiscordMessage (HeartBeat, Identify, Resume, StatusUpdate),
  Shard (..),
  ShardC,
  ShardFlowControl (..),
  ShardMsg (..),
  ShardState (ShardState, wsConn),
  StatusUpdateData,
 )
import Calamity.Internal.RunIntoIO (bindSemToIO)
import Calamity.Internal.Utils (
  debug,
  error,
  info,
  leftToMaybe,
  swap,
  unlessM,
  untilJustFinalIO,
  whenJust,
  whileMFinalIO,
 )
import Calamity.Metrics.Eff (
  MetricEff,
  modifyGauge,
  registerGauge,
 )
import Calamity.Types.LogEff (LogEff)
import Calamity.Types.Token (Token, rawToken)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, cancel)
import Control.Concurrent.Chan.Unagi qualified as UC
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TBMQueue (
  TBMQueue,
  closeTBMQueue,
  newTBMQueueIO,
  readTBMQueue,
  tryWriteTBMQueue,
  writeTBMQueue,
 )
import Control.Exception (
  Exception (fromException),
  SomeException,
 )
import Control.Exception.Safe qualified as Ex
import Control.Monad (void, when)
import Control.Monad.State.Lazy (runState)
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as LBS
import Data.Default.Class (def)
import Data.IORef (newIORef)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import DiPolysemy (attr, push)
import Network.Connection qualified as NC
import Network.TLS qualified as NT
import Network.TLS.Extra qualified as NT
import Network.WebSockets (
  Connection,
  ConnectionException (..),
  receiveData,
  sendCloseCode,
  sendTextData,
 )
import Network.WebSockets qualified as NW
import Network.WebSockets.Stream qualified as NW
import Optics
import Optics.State.Operators
import Polysemy (Sem)
import Polysemy qualified as P
import Polysemy.Async qualified as P
import Polysemy.AtomicState qualified as P
import Polysemy.Error qualified as P
import Polysemy.Resource qualified as P
import System.X509 qualified as X509
import TextShow (showt)
import Prelude hiding (error)

runWebsocket ::
  P.Members '[LogEff, P.Final IO, P.Embed IO] r =>
  T.Text ->
  T.Text ->
  (Connection -> P.Sem r a) ->
  P.Sem r (Maybe a)
runWebsocket host path ma = do
  inner <- bindSemToIO ma

  -- We have to do this all ourself I think?
  -- TODO: see if this isn't needed
  let logExc e = debug $ "runWebsocket raised with " <> (T.pack . show $ e)
  logExc' <- bindSemToIO logExc
  let handler e = do
        void $ logExc' e
        pure Nothing

  P.embed . Ex.handleAny handler $ do
    ctx <- NC.initConnectionContext
    certStore <- X509.getSystemCertificateStore
    let clientParams =
          (NT.defaultParamsClient (T.unpack host) "443")
            { NT.clientSupported = def {NT.supportedCiphers = NT.ciphersuite_default}
            , NT.clientShared =
                def
                  { NT.sharedCAStore = certStore
                  }
            }
    let tlsSettings = NC.TLSSettings clientParams
        connParams = NC.ConnectionParams (T.unpack host) 443 (Just tlsSettings) Nothing

    Ex.bracket
      (NC.connectTo ctx connParams)
      NC.connectionClose
      ( \conn -> do
          stream <-
            NW.makeStream
              (Just <$> NC.connectionGetChunk conn)
              (maybe (pure ()) (NC.connectionPut conn . LBS.toStrict))
          NW.runClientWithStream stream (T.unpack host) (T.unpack path) NW.defaultConnectionOptions [] inner
      )

newShardState :: Shard -> ShardState
newShardState shard = ShardState shard Nothing Nothing False Nothing Nothing Nothing

-- | Creates and launches a shard
newShard ::
  P.Members '[LogEff, MetricEff, P.Embed IO, P.Final IO, P.Async] r =>
  T.Text ->
  Int ->
  Int ->
  Token ->
  Maybe StatusUpdateData ->
  Intents ->
  UC.InChan CalamityEvent ->
  Sem r (UC.InChan ControlMessage, Async (Maybe ()))
newShard gateway id count token presence intents evtIn = do
  (cmdIn, cmdOut) <- P.embed UC.newChan
  let shard = Shard id count gateway evtIn cmdOut (rawToken token) presence intents
  stateVar <- P.embed . newIORef $ newShardState shard

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
      debug . T.pack $ "sending " <> show data' <> " encoded to " <> show encodedData <> " to gateway"
      P.embed . sendTextData wsConn $ encodedData
    Nothing -> debug "tried to send to closed WS"

tryWriteTBMQueue' :: TBMQueue a -> a -> STM Bool
tryWriteTBMQueue' q v = do
  v' <- tryWriteTBMQueue q v
  case v' of
    Just False -> retry
    Just True -> pure True
    Nothing -> pure False

restartUnless :: P.Members '[LogEff, P.Error ShardFlowControl] r => T.Text -> Maybe a -> P.Sem r a
restartUnless _ (Just a) = pure a
restartUnless msg Nothing = do
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

    handleWSException :: SomeException -> IO (Either (ControlMessage, Maybe T.Text) a)
    handleWSException e = pure $ case fromException e of
      Just (CloseRequest code _)
        | code `elem` [4004, 4010, 4011, 4012, 4013, 4014] ->
            Left (ShutDownShard, Just . showt $ code)
      e -> Left (RestartShard, Just . T.pack . show $ e)

    discordStream :: P.Members '[LogEff, MetricEff, P.Embed IO, P.Final IO] r => Connection -> TBMQueue ShardMsg -> Sem r ()
    discordStream ws outqueue = inner
      where
        inner = do
          msg <- P.embed $ Ex.catchAny (Right <$> receiveData ws) handleWSException

          case msg of
            Left (c, reason) -> do
              whenJust reason (\r -> error . T.pack $ "Shard closed with reason: " <> show r)
              P.embed . atomically $ writeTBMQueue outqueue (Control c)
            Right msg' -> do
              -- debug [fmt|Got msg: {msg'}|]
              let decoded = A.eitherDecode msg'
              r <- case decoded of
                Right a ->
                  P.embed . atomically $ tryWriteTBMQueue' outqueue (Discord a)
                Left e -> do
                  error . T.pack $ "Failed to decode " <> e <> ": " <> show msg'
                  pure True
              when r inner
    outerloop :: ShardC r => Sem r ()
    outerloop = whileMFinalIO $ do
      shard :: Shard <- P.atomicGets (^. #shardS)
      let host = shard ^. #gateway
      let host' = fromMaybe host $ T.stripPrefix "wss://" host
      info . T.pack $ "starting up shard " <> show (shardID shard) <> " of " <> show (shardCount shard)

      innerLoopVal <- runWebsocket host' "/?v=9&encoding=json" innerloop

      case innerLoopVal of
        Just ShardFlowShutDown -> do
          info "Shutting down shard"
          pure False
        Just ShardFlowRestart -> do
          info "Restaring shard"
          pure True
        -- we restart normally when we loop

        Nothing -> do
          -- won't happen unless innerloop starts using a non-deterministic effect or connecting to the ws dies
          info "Restarting shard (abnormal reasons?)"
          pure True

    innerloop :: ShardC r => Connection -> Sem r ShardFlowControl
    innerloop ws = do
      debug "Entering inner loop of shard"

      shard <- P.atomicGets (^. #shardS)
      P.atomicModify' (#wsConn ?~ ws)

      seqNum' <- P.atomicGets (^. #seqNum)
      sessionID' <- P.atomicGets (^. #sessionID)

      case (seqNum', sessionID') of
        (Just n, Just s) -> do
          debug $ "Resuming shard (sessionID: " <> s <> ", seq: " <> T.pack (show n)
          sendToWs
            ( Resume
                ResumeData
                  { token = shard ^. #token
                  , sessionID = s
                  , seq = n
                  }
            )
        _noActiveSession -> do
          debug "Identifying shard"
          sendToWs
            ( Identify
                IdentifyData
                  { token = shard ^. #token
                  , properties =
                      IdentifyProps
                        { browser = "Calamity: https://github.com/simmsb/calamity"
                        , device = "Calamity: https://github.com/simmsb/calamity"
                        }
                  , compress = False
                  , largeThreshold = Nothing
                  , shard =
                      Just (shard ^. #shardID, shard ^. #shardCount)
                  , presence = shard ^. #initialStatus
                  , intents = shard ^. #intents
                  }
            )

      result <-
        P.resourceToIOFinal $
          P.bracket
            (P.embed $ newTBMQueueIO 1)
            (P.embed . atomically . closeTBMQueue)
            ( \q -> do
                debug "handling events now"
                _controlThread <- P.async . P.embed $ controlStream shard q
                _discordThread <- P.async $ discordStream ws q
                P.raise . untilJustFinalIO . (leftToMaybe <$>) . P.runError $ do
                  -- only we close the queue
                  msg <- P.embed . atomically $ readTBMQueue q
                  handleMsg =<< restartUnless "shard message stream closed by someone other than the sink" msg
            )

      debug "Exiting inner loop of shard"

      P.atomicModify' (#wsConn .~ Nothing)
      haltHeartBeat
      pure result
    handleMsg :: (ShardC r, P.Member (P.Error ShardFlowControl) r) => ShardMsg -> Sem r ()
    handleMsg (Discord msg) = case msg of
      EvtDispatch sn data' -> do
        -- trace $ "Handling event: ("+||data'||+")"
        P.atomicModify' (#seqNum ?~ sn)

        case data' of
          Ready rdata' ->
            P.atomicModify' (#sessionID ?~ (rdata' ^. #sessionID))
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
          then info "Received resumable invalid session"
          else do
            info "Received non-resumable invalid session, sleeping for 15 seconds then retrying"
            P.atomicModify' (#sessionID .~ Nothing)
            P.atomicModify' (#seqNum .~ Nothing)
            P.embed $ threadDelay (15 * 1000 * 1000)
        P.throw ShardFlowRestart
      Hello interval -> do
        info . T.pack $ "Received hello, beginning to heartbeat at an interval of " <> show interval <> "ms"
        startHeartBeatLoop interval
      HeartBeatAck -> do
        debug "Received heartbeat ack"
        P.atomicModify' (#hbResponse .~ True)
    handleMsg (Control msg) = case msg of
      SendPresence data' -> do
        debug . T.pack $ "Sending presence: (" <> show data' <> ")"
        sendToWs $ StatusUpdate data'
      RestartShard -> P.throw ShardFlowRestart
      ShutDownShard -> P.throw ShardFlowShutDown

startHeartBeatLoop :: ShardC r => Int -> Sem r ()
startHeartBeatLoop interval = do
  haltHeartBeat -- cancel any currently running hb thread
  thread <- P.async $ heartBeatLoop interval
  P.atomicModify' (#hbThread ?~ thread)

haltHeartBeat :: ShardC r => Sem r ()
haltHeartBeat = do
  thread <- P.atomicState @ShardState . (swap .) . runState $ do
    thread <- use #hbThread
    #hbThread .= Nothing
    pure thread
  case thread of
    Just t -> do
      debug "Stopping heartbeat thread"
      P.embed (void $ cancel t)
    Nothing -> pure ()

sendHeartBeat :: ShardC r => Sem r ()
sendHeartBeat = do
  sn <- P.atomicGets (^. #seqNum)
  debug . T.pack $ "Sending heartbeat (seq: " <> show sn <> ")"
  sendToWs $ HeartBeat sn
  P.atomicModify' (#hbResponse .~ False)

heartBeatLoop :: ShardC r => Int -> Sem r ()
heartBeatLoop interval = untilJustFinalIO . (leftToMaybe <$>) . P.runError $ do
  sendHeartBeat
  P.embed . threadDelay $ interval * 1000
  unlessM (P.atomicGets (^. #hbResponse)) $ do
    debug "No heartbeat response, restarting shard"
    wsConn <- P.note () =<< P.atomicGets (^. #wsConn)
    P.embed $ sendCloseCode wsConn 4000 ("No heartbeat in time" :: T.Text)
    P.throw ()
