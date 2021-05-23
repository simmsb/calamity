-- | Contains stuff for managing shards
module Calamity.Client.ShardManager (shardBot) where

import Calamity.Client.Types
import Calamity.Gateway
import Calamity.HTTP
import Calamity.Internal.Utils
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Data.Traversable
import Polysemy (Sem)
import qualified Polysemy as P
import qualified Polysemy.Fail as P
import qualified Polysemy.Reader as P
import PyF

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left $ f a
mapLeft _ (Right b) = Right b

-- | Connects the bot to the gateway over n shards
shardBot :: BotC r => Maybe StatusUpdateData -> Intents -> Sem r (Either StartupError ())
shardBot initialStatus intents = (mapLeft StartupError <$>) . P.runFail $ do
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

  info [fmt|Number of shards: {numShards'}|]

  shards <- for [0 .. numShards' - 1] $ \id ->
    newShard host id numShards' token initialStatus intents inc

  P.embed . atomically $ writeTVar shardsVar shards
