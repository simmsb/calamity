-- | Contains stuff for managing shards
module Calamity.Client.ShardManager
    ( shardBot ) where

-- , shardUserBot
import           Calamity.Client.Types
import           Calamity.Gateway
import           Calamity.HTTP

import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Control.Monad

import           Polysemy                      ( Sem )
import qualified Polysemy                      as P
import qualified Polysemy.Async                as P
import qualified Polysemy.Reader               as P

import qualified Protolude.Error

-- TODO: delete this
aa :: Show a => Either a b -> b
aa (Right x) = x
aa (Left x) = Protolude.Error.error $ show x

-- | Connects the bot to the gateway over n shards
shardBot :: BotC r => Sem r ()
shardBot = do
  numShardsVar <- P.asks numShards
  shardsVar <- P.asks shards

  hasShards <- P.embed $ not . null <$> readTVarIO shardsVar
  when hasShards $ fail "don't use shardBot on an already running bot."

  token <- P.asks Calamity.Client.Types.token
  eventQueue <- P.asks eventQueue

  gateway <- aa <$> invokeRequest GetGatewayBot

  let numShards' = gateway ^. #shards
  let host = gateway ^. #url
  P.embed $ putMVar numShardsVar numShards'

  info $ "Number of shards: " +| numShards' |+ ""

  shards <- P.asyncToIO $ for [0 .. numShards' - 1] $ \id ->
    newShard host id numShards' token eventQueue

  P.embed . atomically $ writeTVar shardsVar shards

-- | Connects the bot to the gateway over 1 shard (userbot)
-- shardUserBot :: BotM ()
-- shardUserBot = do
--   numShardsVar <- asks numShards
--   shardsVar <- asks shards

--   hasShards <- liftIO $ (not . null) <$> readTVarIO shardsVar
--   when hasShards $ fail "don't use shardUserBot on an already running bot."

--   token <- asks Calamity.Client.Types.token
--   eventQueue <- asks eventQueue
--   logEnv <- askLog

--   gateway <- aa <$> invokeRequest GetGateway

--   let host = gateway ^. #url
--   liftIO $ putMVar numShardsVar 1

--   liftIO $ do
--     shard <- newShard host 0 1 token logEnv eventQueue
--     atomically $ writeTVar shardsVar [shard]
