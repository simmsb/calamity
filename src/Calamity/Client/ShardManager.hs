-- | Contains stuff for managing shards
module Calamity.Client.ShardManager
    ( shardBot ) where

import           Calamity.Client.Types
import           Calamity.Gateway
import           Calamity.HTTP
import           Calamity.Internal.Utils

import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad

import           Data.Traversable

import           Fmt

import           Polysemy                    ( Sem )
import qualified Polysemy.Fail               as P
import qualified Polysemy                    as P
import qualified Polysemy.Reader             as P

-- | Connects the bot to the gateway over n shards
shardBot :: (BotC r, P.Member P.Fail r) => Sem r ()
shardBot = do
  numShardsVar <- P.asks numShards
  shardsVar <- P.asks shards

  hasShards <- P.embed $ not . null <$> readTVarIO shardsVar
  when hasShards $ fail "don't use shardBot on an already running bot."

  token <- P.asks Calamity.Client.Types.token
  inc <- P.asks (^. #eventsIn)

  Right gateway <- invoke GetGatewayBot

  let numShards' = gateway ^. #shards
  let host = gateway ^. #url
  P.embed $ putMVar numShardsVar numShards'

  info $ "Number of shards: " +| numShards' |+ ""

  shards <- for [0 .. numShards' - 1] $ \id ->
    newShard host id numShards' token inc

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

--   gateway <- aa <$> invoke GetGateway

--   let host = gateway ^. #url
--   liftIO $ putMVar numShardsVar 1

--   liftIO $ do
--     shard <- newShard host 0 1 token logEnv eventQueue
--     atomically $ writeTVar shardsVar [shard]
