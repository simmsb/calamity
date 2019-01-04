-- | Contains stuff for managing shards

module YAHDL.Client.ShardManager
  ( mkChanRecvStream )
where

import           Control.Concurrent.STM.TChan
import qualified Streamly                      as S
import qualified Streamly.Prelude              as S

mkChanRecvStream :: STM (S.Serial a, TChan a)
mkChanRecvStream = do
  chan <- newTChan

  let stream = S.repeatM . atomically . readTChan $ chan

  pure (stream, chan)
