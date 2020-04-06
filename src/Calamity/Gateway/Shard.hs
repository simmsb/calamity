-- |
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Calamity.Gateway.Shard
    ( Shard(..)
    , newShard ) where

import           Calamity.Gateway.DispatchEvents
import           Calamity.Gateway.Types
import           Calamity.Types.General

import           Control.Concurrent.STM.TBMQueue
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar

import qualified Data.Aeson                      as A
import           Data.Maybe
import           Data.Text                       ( stripPrefix )
import           Data.Text.Strict.Lens

import           Network.WebSockets              ( Connection, ConnectionException(..), receiveData, sendCloseCode
                                                 , sendTextData )

import           Polysemy                        ( Sem )
import qualified Polysemy                        as P
import qualified Polysemy.Async                  as P
import qualified Polysemy.AtomicState            as P
import qualified Polysemy.Error                  as P
import qualified Polysemy.Resource               as P

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
newShard :: P.Members '[LogEff, P.Embed IO, P.Final IO, P.Async] r
         => Text
         -> Int
         -> Int
         -> Token
         -> TQueue DispatchData
         -> Sem r (Shard, Async (Maybe ()))
newShard gateway id count token evtQueue = do
  (shard, stateVar) <- P.embed $ mdo
    cmdQueue' <- newTQueueIO
    stateVar <- newTVarIO (newShardState shard)
    let shard = Shard id count gateway evtQueue cmdQueue' stateVar (rawToken token)
    pure (shard, stateVar)

  let runShard = P.runAtomicStateTVar stateVar shardLoop
  let action = attr "shard-id" id . push "calamity-shard" $ runShard

  thread' <- P.async action

  pure (shard, thread')

sendToWs :: ShardC r => SentDiscordMessage -> Sem r ()
sendToWs data' = do
  wsConn <- fromJust <$> P.atomicGets wsConn
  let encodedData = A.encode data'
  debug $ "sending " +|| data' ||+ " encoded to " +|| encodedData ||+ " to gateway"
  P.embed . sendTextData wsConn $ encodedData
  -- trace "done sending data"

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

tryWriteTBMQueue' :: TBMQueue a -> a -> STM Bool
tryWriteTBMQueue' q v = do
  v <- tryWriteTBMQueue q v
  case v of
    Just False -> retry
    Just True  -> pure True
    Nothing    -> pure False

-- | The loop a shard will run on
shardLoop :: ShardC r => Sem r ()
shardLoop = do
  -- trace "entering shardLoop"
  void outerloop
  -- trace "leaving shardLoop"
 where
  controlStream :: Shard -> TBMQueue ShardMsg -> IO ()
  controlStream shard outqueue = inner
    where
      q = shard ^. #cmdQueue
      inner = do
        v <- atomically $ readTQueue q
        r <- atomically $ tryWriteTBMQueue' outqueue (Control v)
        when r inner

  discordStream :: P.Members '[LogEff, P.Embed IO] r => Connection -> TBMQueue ShardMsg -> Sem r ()
  discordStream ws outqueue = inner
    where inner = do
            msg <- P.embed . checkWSClose $ receiveData ws

            -- trace $ "Received from stream: "+||msg||+""

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

  -- mergedStream ::  Log -> Shard -> Connection -> ExceptT ShardException ShardM ShardMsg
  -- mergedStream logEnv shard ws =
  --   liftIO (fromEither <$> race (controlStream shard) (discordStream logEnv ws))

  -- | The outer loop, sets up the ws conn, etc handles reconnecting and such
  -- Currently if this goes to the error path we just exit the forever loop
  -- and the shard stops, maybe we might want to do some extra logic to reboot
  -- the shard, or maybe force a resharding
  outerloop :: ShardC r => Sem r (Either ShardException ())
  outerloop = P.runError . forever $ do
    shard :: Shard <- P.atomicGets (^. #shardS)
    let host = shard ^. #gateway
    let host' =  fromMaybe host $ stripPrefix "wss://" host
    info $ "starting up shard "+| (shard ^. #shardID) |+" of "+| (shard ^. #shardCount) |+""


    innerLoopVal <- websocketToIO $ runWebsocket host' "/?v=7&encoding=json" innerloop

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

    result <- P.runResource $ P.bracket (P.embed $ newTBMQueueIO 1)
      (\q -> P.embed . atomically $ closeTBMQueue q)
      (\q -> do
        debug "handling events now"
        _controlThread <- P.async . P.embed $ controlStream shard q
        _discordThread <- P.async $ discordStream ws q
        (fromEitherVoid <$>) . P.raise . P.runError . forever $ do
          -- only we close the queue
          msg <- P.embed . atomically $ readTBMQueue q
          handleMsg $ fromJust msg)

    debug "Exiting inner loop of shard"

    P.atomicModify (#wsConn .~ Nothing)
    pure result

  -- | Handlers for each message, not sure what they'll need to do exactly yet
  handleMsg :: (ShardC r, P.Member (P.Error ShardException) r) => ShardMsg -> Sem r ()
  handleMsg (Discord msg) = case msg of
    Dispatch sn data' -> do
      -- trace $ "Handling event: ("+||data'||+")"
      P.atomicModify (#seqNum ?~ sn)

      case data' of
        Ready rdata' ->
          P.atomicModify (#sessionID ?~ (rdata' ^. #sessionID))

        _ -> pure ()

      shard <- P.atomicGets (^. #shardS)
      P.embed . atomically $ writeTQueue (shard ^. #evtQueue) data'
      -- sn' <- P.atomicGets (^. #seqNum)
      -- trace $ "Done handling event, seq is now: "+||sn'||+""

    HeartBeatReq -> do
      debug "Received heartbeat request"
      sendHeartBeat

    Reconnect -> do
      debug "Being asked to restart by Discord"
      P.throw ShardExcRestart

    InvalidSession resumable -> do
      if resumable
      then do
        info "Received non-resumable invalid session, sleeping for 15 seconds then retrying"
        P.atomicModify (#sessionID .~ Nothing)
        P.atomicModify (#seqNum .~ Nothing)
        P.embed $ threadDelay 1500000
      else
        info "Received resumable invalid session"
      P.throw ShardExcRestart

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

    Restart            -> P.throw ShardExcRestart
    ShutDown           -> P.throw ShardExcShutDown

startHeartBeatLoop :: ShardC r => Int -> Sem r ()
startHeartBeatLoop interval = do
  haltHeartBeat -- cancel any currently running hb thread
  void . P.async $ heartBeatLoop interval

haltHeartBeat :: ShardC r => Sem r ()
haltHeartBeat = do
  thread <- P.atomicGets (^. #hbThread)
  case thread of
    Just t  -> P.embed (void $ cancel t)
    Nothing -> pure ()
  P.atomicModify (#hbThread .~ Nothing)

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
  hasResp <- P.atomicGets (^. #hbResponse)
  unless hasResp $ do
    debug "No heartbeat response, restarting shard"
    wsConn <- fromJust <$> P.atomicGets (^. #wsConn)
    P.embed $ sendCloseCode wsConn 4000 ("No heartbeat in time" :: Text)
    P.throw ()
