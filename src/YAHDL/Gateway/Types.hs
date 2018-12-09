-- | Types for shards

{-# LANGUAGE TemplateHaskell #-}

module YAHDL.Gateway.Types where
  -- ( Shard(..)
  -- , ShardState(..)
  -- , ShardM(..)
  -- , ShardMsg(..)
  -- , DiscordMessage(..)
  -- , RawWsPayload(..)
  -- , ControlMessage(..)
  -- )

import Control.Monad.Log (LogT, MonadLog)
import  GHC.Generics
import           Control.Lens                   ( makeFields )
import           Data.Aeson
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM.TMVar
import           Control.Monad.State.Concurrent.Strict
import           Network.WebSockets.Connection  ( Connection )

-- TODO: change this from RawWsPayload to DiscordMessage, add decoder & handler
data ShardMsg = Discord RawWsPayload | Control ControlMessage

data RawWsPayload = RawWsPayload
  { _op :: Integer
  , _d  :: Maybe Value
  , _s  :: Maybe Int
  , _t  :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON RawWsPayload
instance FromJSON RawWsPayload

data DiscordMessage
  = Dispatch DispatchData
  | HeartBeat Integer
  | Identify IdentifyData
  | StatusUpdate StatusUpdateData
  | VoiceStatusUpdate
  | VoiceServerPing
  | Resume
  | Reconnect
  | RequestGuildMembers
  | InvalidSession
  | Hello
  | HeartBeatAck
  deriving (Show, Generic)

instance ToJSON DiscordMessage
instance FromJSON DiscordMessage

-- Thanks sbrg:
-- https://github.com/saevarb/haskord/blob/d1bb07bcc4f3dbc29f2dfd3351ff9f16fc100c07/haskord-lib/src/Haskord/Types/Common.hs
data EventType
    = READY
    | CHANNEL_CREATE
    | CHANNEL_UPDATE
    | CHANNEL_DELETE
    | CHANNEL_PINS_UPDATE
    | GUILD_CREATE
    | GUILD_UPDATE
    | GUILD_DELETE
    | GUILD_BAN_ADD
    | GUILD_BAN_REMOVE
    | GUILD_EMOJIS_UPDATE
    | GUILD_INTEGRATIONS_UPDATE
    | GUILD_MEMBER_ADD
    | GUILD_MEMBER_REMOVE
    | GUILD_MEMBER_UPDATE
    | GUILD_MEMBERS_CHUNK
    | GUILD_ROLE_CREATE
    | GUILD_ROLE_UPDATE
    | GUILD_ROLE_DELETE
    | MESSAGE_CREATE
    | MESSAGE_UPDATE
    | MESSAGE_DELETE
    | MESSAGE_DELETE_BULK
    | MESSAGE_REACTION_ADD
    | MESSAGE_REACTION_REMOVE
    | MESSAGE_REACTION_REMOVE_ALL
    | PRESENCE_UPDATE
    | TYPING_START
    | USER_UPDATE
    | VOICE_STATE_UPDATE
    | VOICE_SERVER_UPDATE
    | WEBHOOKS_UPDATE
    deriving (Show, Eq, Generic)

instance ToJSON EventType
instance FromJSON EventType

data DispatchData = DispatchData
  { _dispatchDataData :: Value
  , _dispatchDataName :: EventType
  } deriving (Show, Generic)

instance ToJSON DispatchData
instance FromJSON DispatchData

data IdentifyData = IdentifyData
  { _identifyDataToken :: Text
  , _identifyDataProperties :: IdentifyProps
  , _identifyDataCompress :: Maybe Bool
  , _identifyDataLarge_threshold :: Maybe Int
  , _identifyDataShard :: Maybe Int
  , _identifyDataPresence :: Maybe PresenceData
  } deriving (Show, Generic)

instance ToJSON IdentifyData
instance FromJSON IdentifyData

data StatusUpdateData = StatusUpdateData
  { _statusUpdateDataSice :: Maybe Int
  , _statusUpdateDataGame :: Maybe Value -- TODO: activity object
  , _statusUpdateDataStatus :: Text
  , _statusUpdateDataAfk :: Bool
  } deriving (Show, Generic)

instance ToJSON StatusUpdateData
instance FromJSON StatusUpdateData

-- TODO(ben): Work on filling out data types and aeson stuff

data IdentifyProps = IdentifyProps
  { _identifyPropsOs :: Text
  , _identifyPropsBrowser :: Text
  , _identifyPropsDevice :: Text
  } deriving (Show, Generic)

instance ToJSON IdentifyProps
instance FromJSON IdentifyProps

data PresenceData = PresenceData
  { _presenceDataSince :: Maybe Int
  , _presenceDataGame :: Maybe Value -- TODO: activity object
  , _presenceDataStatus :: Text
  , _presenceDataAfk :: Bool
  } deriving (Show, Generic)

instance ToJSON PresenceData
instance FromJSON PresenceData

data ControlMessage = Restart | ShutDown
  deriving (Show)

data Shard = Shard
  { _shardShardId :: Integer
  , _shardEvtChan :: TChan () -- TODO: replace this with the event type
  , _shardCmdChan :: TChan ControlMessage -- TODO: replace this with the shard command type
  , _shardShardState :: TVar ShardState
  , _shardToken :: Text
  } deriving (Generic)

data ShardState = ShardState
  { _shardStateShardS :: Shard
  , _shardStateSeqNum :: Maybe Integer
  , _shardStateHbThread :: Maybe (Async ())
  , _shardStateWsHost :: Maybe Text
  , _shardStateWsResponse :: Bool
  , _shardStateSessionID :: Maybe Integer
  , _shardStateWsConn :: Maybe Connection
  } deriving (Generic)

makeFields ''Shard
makeFields ''ShardState

newtype ShardM env a = ShardM
  { unShardM :: LogT env (StateC ShardState IO) a
  } deriving (Applicative, Monad, MonadIO, MonadLog env,
              Functor, MonadState ShardState)

instance MonadState s m => MonadState s (LogT env m) where
  get = lift get
  put = lift . put
  state = lift . state
