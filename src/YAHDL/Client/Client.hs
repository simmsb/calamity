-- |

module YAHDL.Client.Client
  ( Client(..)
  )
where

import           YAHDL.Gateway.Shard
import           YAHDL.Client.ShardManager
import           Control.Concurrent.STM.TVar

data Client = Client
  { shards :: TVar [(Shard, Async ())]
  }
