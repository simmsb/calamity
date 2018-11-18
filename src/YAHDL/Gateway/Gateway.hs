-- |

module YAHDL.Gateway.Gateway
  ( Shard (..)
  ) where


data Shard = Shard
  { shardId :: Integer
  , thread :: Async ()
  }
