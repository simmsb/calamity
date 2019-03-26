-- | Contains stuff for managing shards

module YAHDL.Client.ShardManager
  ( mkQueueRecvStream
  , shardBot
  , shardUserBot
  )
where

import qualified Protolude.Error
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Control.Monad
import qualified Streamly                      as S
import qualified Streamly.Prelude              as S

import           YAHDL.Gateway
import           YAHDL.Client.Types
import           YAHDL.HTTP.MiscRoutes
import           YAHDL.HTTP.Request

mkQueueRecvStream :: IO (S.Serial a, TQueue a)
mkQueueRecvStream = do
  queue <- newTQueueIO

  let evtStream = S.repeatM . atomically . readTQueue $ queue

  pure (evtStream, queue)


-- TODO: delete this
aa :: Show a => Either a b -> b
aa (Right x) = x
aa (Left  x) = Protolude.Error.error $ show x

-- | Connects the bot to the gateway over n shards
shardBot :: BotM ()
shardBot = do
  numShardsVar <- asks numShards
  shardsVar <- asks shards

  hasShards <- liftIO $ (not . null) <$> readTVarIO shardsVar
  when hasShards $
    fail "don't use shardBot on an already running bot."

  token <- asks YAHDL.Client.Types.token
  eventQueue <- asks eventQueue
  logEnv <- askLog

  gateway <- aa <$> invokeRequest GetGatewayBot

  let numShards' = gateway ^. #shards
  let host = gateway ^. #url
  liftIO $ putMVar numShardsVar numShards'

  info $ "Number of shards: "+| numShards' |+""

  liftIO do
    shards <- forM [0 .. numShards' - 1] \id ->
      newShard host id numShards' token logEnv eventQueue

    atomically $ writeTVar shardsVar shards

-- | Connects the bot to the gateway over 1 shard (userbot)
shardUserBot :: BotM ()
shardUserBot = do
  numShardsVar <- asks numShards
  shardsVar <- asks shards

  hasShards <- liftIO $ (not . null) <$> readTVarIO shardsVar
  when hasShards $
    fail "don't use shardUserBot on an already running bot."

  token <- asks YAHDL.Client.Types.token
  eventQueue <- asks eventQueue
  logEnv <- askLog

  gateway <- aa <$> invokeRequest GetGateway

  let host = gateway ^. #url
  liftIO $ putMVar numShardsVar 1

  liftIO do
    shard <- newShard host 0 1 token logEnv eventQueue
    atomically $ writeTVar shardsVar [shard]
