-- | Types for shards

{-# LANGUAGE TemplateHaskell #-}

module YAHDL.Gateway.Types where
  -- ( Shard(..)
  -- , ShardState(..)
  -- , ShardM(..)
  -- , ShardMsg(..)
  -- , DiscordMessage(..)
  -- , RawDiscordMessage(..)
  -- , ControlMessage(..)
  -- )

import           Control.Lens                   ( makeLenses )
import           Data.Aeson
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM.TMVar
import           Control.Monad.State.Concurrent.Strict
import           Network.WebSockets.Connection  ( Connection )

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

data Shard = Shard
  { _shardId :: Integer
  , _evtChan :: TChan () -- TODO: replace this with the event type
  , _cmdChan :: TChan ControlMessage -- TODO: replace this with the shard command type
  , _shardState :: TVar ShardState
  , _token :: Text
  } deriving (Generic)

data ShardState = ShardState
  { _shardS :: Shard
  , _seqNum :: Maybe Integer
  , _hbThread :: Maybe (Async ())
  , _wsHost :: Maybe Text
  , _wsResponse :: Bool
  , _sessionID :: Maybe Integer
  , _wsConn :: Maybe Connection
  } deriving (Generic)

makeLenses ''Shard
makeLenses ''ShardState

type ShardM a = StateC ShardState IO a

