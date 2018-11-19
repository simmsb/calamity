-- | Contains stuff for managing shards

module YAHDL.Client.ShardManager
  ()
where

import           YAHDL.Gateway
import           Control.Concurrent.STM.TChan
import qualified Streamly                      as S
import qualified Streamly.Prelude              as S

mkChanRecvStream :: S.IsStream t => STM (t IO a, TChan a)
mkChanRecvStream = do
  chan <- newTChan

  let stream = S.repeatM . atomically . readTChan $ chan

  pure (stream, chan)
