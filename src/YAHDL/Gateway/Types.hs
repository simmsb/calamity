-- | Types for shards

module YAHDL.Gateway.Types where
  -- ( Shard(..)
  -- , ShardState(..)
  -- , ShardM(..)
  -- , ShardMsg(..)
  -- , DiscordMessage(..)
  -- , RawWsPayload(..)
  -- , ControlMessage(..)
  -- )

import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Control.Lens
import           Control.Monad.Log              ( LogT
                                                , MonadLog
                                                )
import           Control.Monad.State.Concurrent.Strict
import           Data.Aeson
import           Data.Generics.Labels           ( )
import           GHC.Generics
import           Network.WebSockets.Connection  ( Connection )

-- TODO: change this from RawWsPayload to DiscordMessage, add decoder & handler
data ShardMsg = Discord RawWsPayload | Control ControlMessage

data RawWsPayload = RawWsPayload
  { op :: Integer
  , d  :: Maybe Value
  , s  :: Maybe Int
  , t  :: Maybe Text
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
  { dataData :: Value
  , dataName :: EventType
  } deriving (Show, Generic)

instance ToJSON DispatchData
instance FromJSON DispatchData

data IdentifyData = IdentifyData
  { dataToken :: Text
  , dataProperties :: IdentifyProps
  , dataCompress :: Maybe Bool
  , dataLarge_threshold :: Maybe Int
  , dataShard :: Maybe Int
  , dataPresence :: Maybe PresenceData
  } deriving (Show, Generic)

instance ToJSON IdentifyData
instance FromJSON IdentifyData

data StatusUpdateData = StatusUpdateData
  { updateDataSice :: Maybe Int
  , updateDataGame :: Maybe Value -- TODO: activity object
  , updateDataStatus :: Text
  , updateDataAfk :: Bool
  } deriving (Show, Generic)

instance ToJSON StatusUpdateData
instance FromJSON StatusUpdateData

-- TODO(ben): Work on filling out data types and aeson stuff

data IdentifyProps = IdentifyProps
  { propsOs :: Text
  , propsBrowser :: Text
  , propsDevice :: Text
  } deriving (Show, Generic)

instance ToJSON IdentifyProps
instance FromJSON IdentifyProps

data PresenceData = PresenceData
  { dataSince :: Maybe Int
  , dataGame :: Maybe Value -- TODO: activity object
  , dataStatus :: Text
  , dataAfk :: Bool
  } deriving (Show, Generic)

instance ToJSON PresenceData
instance FromJSON PresenceData

data ControlMessage = Restart | ShutDown
  deriving (Show)

data Shard = Shard
  { shardId :: Integer
  , evtChan :: TChan () -- TODO: replace this with the event type
  , cmdChan :: TChan ControlMessage -- TODO: replace this with the shard command type
  , shardState :: TVar ShardState
  , token :: Text
  } deriving (Generic)

data ShardState = ShardState
  { shardS :: Shard
  , seqNum :: Maybe Integer
  , hbThread :: Maybe (Async ())
  , wsHost :: Maybe Text
  , wsResponse :: Bool
  , sessionID :: Maybe Integer
  , wsConn :: Maybe Connection
  } deriving (Generic)

newtype ShardM env a = ShardM
  { unShardM :: LogT env (StateC ShardState IO) a
  } deriving (Applicative, Monad, MonadIO, MonadLog env,
              Functor, MonadState ShardState)

instance MonadState s m => MonadState s (LogT env m) where
  get   = lift get
  put   = lift . put
  state = lift . state
