-- | Types for shards

module YAHDL.Gateway.Types
        ( Shard(..)
        , ShardState(..)
        , ShardM(..)
        , ShardMsg(..)
        , DiscordMessage(..)
        , RawDiscordMessage(..)
        , ControlMessage(..)
        )
where

import           Data.Aeson
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM.TMVar
import           Control.Monad.State.Concurrent.Strict


data Shard = Shard
  { shardId :: Integer
  , evtChan :: TChan () -- TODO: replace this with the event type
  , cmdChan :: TChan ControlMessage -- TODO: replace this with the shard command type
  , shardState :: TVar ShardState
  , token :: Text
  } deriving (Generic)

data ShardState = ShardState
  { seqNum :: Maybe Integer
  , hbThread :: Maybe (Async ())
  , wsHost :: Maybe Text
  , wsResponse :: Bool
  } deriving (Generic)

newtype ShardM a = ShardM { unShardM :: StateC ShardState IO a }

-- TODO: change this from RawDiscordMessage to DiscordMessage, add decoder & handler
data ShardMsg = Discord RawDiscordMessage | Control ControlMessage

-- TODO: this
data DiscordMessage = DEvent
  deriving (Show, Generic)

data RawDiscordMessage = RDEvent
  deriving (Show, Generic)

instance ToJSON RawDiscordMessage
instance FromJSON RawDiscordMessage

data ControlMessage = Restart
  deriving (Show)
