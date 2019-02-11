-- | Contains stuff for managing shards

module YAHDL.Client.ShardManager
  ( mkChanRecvStream
  , shardBot
  )
where

import qualified Protolude.Error
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad
import qualified Streamly                      as S
import qualified Streamly.Prelude              as S

import           YAHDL.Gateway
import           YAHDL.Client.Types
import           YAHDL.HTTP.MiscRoutes
import           YAHDL.HTTP.Request

mkChanRecvStream :: IO (S.Serial a, TChan a)
mkChanRecvStream = do
  chan <- newTChanIO

  let evtStream = S.repeatM . atomically . readTChan $ chan

  pure (evtStream, chan)


-- TODO: delete this
aa :: Show a => Either a b -> b
aa (Right x) = x
aa (Left  x) = Protolude.Error.error $ show x

shardBot :: BotM ()
shardBot = do
  numShardsVar <- asks numShards
  shardsVar <- asks shards

  hasShards <- liftIO $ (not . null) <$> readTVarIO shardsVar
  when hasShards $
    fail "don't use shardBot on an already running bot."

  token <- (^. #token) <$> ask
  eventChan <- asks eventChan
  logEnv <- askLog

  gateway <- aa <$> invokeRequest GetGatewayBot

  let numShards' = gateway ^. #shards
  liftIO $ putMVar numShardsVar numShards'

  info $ "Number of shards: "+| numShards' |+""

  shards <- liftIO $ forM [0 .. numShards' - 1] \id ->
    newShard id numShards' token logEnv eventChan

  liftIO . atomically $ writeTVar shardsVar shards
