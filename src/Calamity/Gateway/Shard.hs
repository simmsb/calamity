-- | The shard logic
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Calamity.Gateway.Shard
    ( Shard(..)
    , newShard ) where

import           Calamity.Gateway.DispatchEvents
import           Calamity.Gateway.Types
import           Calamity.Internal.Utils
import           Calamity.LogEff
import           Calamity.Metrics.Eff
import           Calamity.Types.Token

import           Control.Concurrent
import           Control.Concurrent.Async
import qualified Control.Concurrent.Chan.Unagi as UC
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBMQueue
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.State.Lazy

import qualified Data.Aeson                      as A
import           Data.Functor
import           Data.Maybe
import           Data.IORef
import           Data.Text.Lazy                  ( Text, stripPrefix )
import           Data.Text.Lazy.Lens
import           Data.Void

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
import qualified Control.Exception.Safe as Ex
import           Prelude                         hiding ( error )

import           TextShow

import           Wuss

data Websocket m a where
  RunWebsocket :: Text -> Text -> (Connection -> m a) -> Websocket m a

P.makeSem ''Websocket

websocketToIO :: forall r a. P.Member (P.Embed IO) r => Sem (Websocket ': r) a -> Sem r a
websocketToIO = P.interpretH
  (\case
     RunWebsocket host path a -> do
       istate <- P.getInitialStateT
       ma <- P.bindT a

       P.withLowerToIO $ \lower finish -> do
         let done :: Sem (Websocket ': r) x -> IO x
             done = lower . P.raise . websocketToIO

         runSecureClient (host ^. unpacked) 443 (path ^. unpacked)
           (\x -> do
              res <- done (ma $ istate $> x)
              finish
              pure res))

newShardState :: Shard -> ShardState
newShardState shard = ShardState shard Nothing Nothing False Nothing Nothing Nothing

-- | Creates and launches a shard
newShard :: P.Members '[LogEff, MetricEff, P.Embed IO, P.Final IO, P.Async] r
         => Text
         -> Int
         -> Int
         -> Token
         -> UC.InChan CalamityEvent
         -> Sem r (UC.InChan ControlMessage, Async (Maybe ()))
newShard gateway id count token evtIn = do
  (cmdIn, stateVar) <- P.embed $ mdo
    (cmdIn, cmdOut) <- UC.newChan
    stateVar <- newIORef $ newShardState shard
    let shard = Shard id count gateway evtIn cmdOut stateVar (rawToken token)
    pure (cmdIn, stateVar)

  let runShard = P.runAtomicStateIORef stateVar shardLoop
  let action = attr "shard-id" id . push "calamity-shard" $ runShard

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

fromEitherVoid :: Either a Void -> a
fromEitherVoid (Left a) = a
fromEitherVoid (Right a) = absurd a -- yeet

tryWriteTBMQueue' :: TBMQueue a -> a -> STM Bool
tryWriteTBMQueue' q v = do
  v' <- tryWriteTBMQueue q v
  case v' of
    Just False -> retry
    Just True  -> pure True
    Nothing    -> pure False

-- | The loop a shard will run on
shardLoop :: ShardC r => Sem r ()
shardLoop = do
  activeShards <- registerGauge "active_shards" mempty
  void $ modifyGauge succ activeShards
  void outerloop
  void $ modifyGauge pred activeShards
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

  handleWSException :: SomeException -> IO (Either ControlMessage a)
  handleWSException e = pure $ case fromException e of
    Just (CloseRequest code _)
      | code `elem` [1000, 4004, 4010, 4011] ->
        Left ShutDownShard
    _ -> Left RestartShard

  discordStream :: P.Members '[LogEff, MetricEff, P.Embed IO, P.Final IO] r => Connection -> TBMQueue ShardMsg -> Sem r ()
  discordStream ws outqueue = inner
    where inner = do
            msg <- P.embed $ Ex.catchAny (Right <$> receiveData ws) handleWSException

            case msg of
              Left c ->
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
  -- Currently if this goes to the error path we just exit the forever loop
  -- and the shard stops, maybe we might want to do some extra logic to reboot
  -- the shard, or maybe force a resharding
  outerloop :: ShardC r => Sem r (Either ShardFlowControl ())
  outerloop = P.runError . forever $ do
    shard :: Shard <- P.atomicGets (^. #shardS)
    let host = shard ^. #gateway
    let host' =  fromMaybe host $ stripPrefix "wss://" host
    info $ "starting up shard "+| (shard ^. #shardID) |+" of "+| (shard ^. #shardCount) |+""


    innerLoopVal <- websocketToIO $ runWebsocket host' "/?v=7&encoding=json" innerloop

    case innerLoopVal of
      ShardFlowShutDown -> do
        info "Shutting down shard"
        P.throw ShardFlowShutDown

      ShardFlowRestart ->
        info "Restaring shard"
        -- we restart normally when we loop

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

    receivedMessages <- registerCounter "received_messages" [("shard", showt $ shard ^. #shardID)]

    result <- P.resourceToIOFinal $ P.bracket (P.embed $ newTBMQueueIO 1)
      (P.embed . atomically . closeTBMQueue)
      (\q -> do
        debug "handling events now"
        _controlThread <- P.async . P.embed $ controlStream shard q
        _discordThread <- P.async $ discordStream ws q
        (fromEitherVoid <$>) . P.raise . P.runError . forever $ do
          -- only we close the queue
          void $ addCounter 1 receivedMessages
          msg <- P.embed . atomically $ readTBMQueue q
          handleMsg $ fromJust msg)

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

        _ -> pure ()

      shard <- P.atomicGets (^. #shardS)
      P.embed $ UC.writeChan (shard ^. #evtIn) (Dispatch data')

    HeartBeatReq -> do
      debug "Received heartbeat request"
      sendHeartBeat

    Reconnect -> do
      debug "Being asked to restart by Discord"
      P.throw ShardFlowRestart

    InvalidSession resumable -> do
      if resumable
      then do
        info "Received non-resumable invalid session, sleeping for 15 seconds then retrying"
        P.atomicModify (#sessionID .~ Nothing)
        P.atomicModify (#seqNum .~ Nothing)
        P.embed $ threadDelay (15 * 1000 * 1000)
      else
        info "Received resumable invalid session"
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
heartBeatLoop interval = void . P.runError . forever $ do
  sendHeartBeat
  P.embed . threadDelay $ interval * 1000
  unlessM (P.atomicGets (^. #hbResponse)) $ do
    debug "No heartbeat response, restarting shard"
    wsConn <- fromJust <$> P.atomicGets (^. #wsConn)
    P.embed $ sendCloseCode wsConn 4000 ("No heartbeat in time" :: Text)
    P.throw ()
