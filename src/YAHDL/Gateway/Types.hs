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

import           Control.Exception
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Control.Monad.Log              ( LogT
                                                , MonadLog
                                                )
import           Control.Monad.State.Concurrent.Strict
import           Data.Aeson
import           Data.Generics.Labels           ( )
import           GHC.Generics
import           Network.WebSockets.Connection  ( Connection )

import           YAHDL.Types.Snowflake
import           YAHDL.Types.General

data ShardMsg = Discord ReceivedDiscordMessage | Control ControlMessage

data ReceivedDiscordMessage
  = Dispatch InnerDispatch
  | HeartBeatReq
  | Reconnect
  | InvalidSession Bool
  | Hello Int
  | HeartBeatAck
  deriving (Show, Generic)

instance FromJSON ReceivedDiscordMessage where
  parseJSON = withObject "ReceivedDiscordMessage" $ \v -> do
    op :: Int <- v .: "op"
    case op of
      0 -> Dispatch <$>
        (InnerDispatch
         <$> v .: "d"
         <*> v .: "t"
         <*> v .: "s")

      1 -> pure HeartBeatReq

      7 -> pure Reconnect

      9 -> InvalidSession
        <$> v .: "d"

      10 -> Hello <$> do
        d <- v .: "d"
        d .: "heartbeat_interval"

      11 -> pure HeartBeatAck

      op -> fail $ "invalid opcode: " <> show op

data SentDiscordMessage
  = StatusUpdate StatusUpdateData
  | Identify IdentifyData
  | HeartBeat (Maybe Int)
  | VoiceStatusUpdate VoiceState
  | Resume ResumeData
  | RequestGuildMembers RequestGuildMembersData
  deriving (Show, Generic)

instance ToJSON SentDiscordMessage where
  toEncoding (HeartBeat data') =
    pairs ("op" .= (1 :: Int) <> "d" .= data')

  toEncoding (Identify data') =
    pairs ("op" .= (2 :: Int) <> "d" .= data')

  toEncoding (StatusUpdate data') =
    pairs ("op" .= (3 :: Int) <> "d" .= data')

  toEncoding (Voice data') =
    pairs ("op" .= (4 :: Int) <> "d" .= data')

  toEncoding (Resume data') =
    pairs ("op" .= (6 :: Int) <> "d" .= data')

  toEncoding (RequestGuildMembers data') =
    pairs ("op" .= (8 :: Int) <> "d" .= data')

-- Thanks sbrg:
-- https://github.com/saevarb/haskord/blob/d1bb07bcc4f3dbc29f2dfd3351ff9f16fc100c07/haskord-lib/src/Haskord/Types/Common.hs
data DispatchType
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
    deriving (Show, Eq, Enum, Generic)

instance ToJSON DispatchType

instance FromJSON DispatchType

-- TODO: write fromjson instance for dispatchdata, etc
data InnerDispatch = InnerDispatch
  { data' :: Value
  , type' :: DispatchType
  , seq   :: Int
  } deriving (Show, Generic)

data IdentifyData = IdentifyData
  { token          :: Text
  , properties     :: IdentifyProps
  , compress       :: Bool
  , largeThreshold :: Int
  , shard          :: (Int, Int)
  , presence       :: Maybe StatusUpdateData
  } deriving (Show, Generic)

instance ToJSON IdentifyData where
  toEncoding = genericToEncoding jsonOptions

data StatusUpdateData = StatusUpdateData
  { since  :: Maybe Integer
  , game   :: Maybe Value -- TODO: activity object
  , status :: Text
  , afk    :: Bool
  } deriving (Show, Generic)

instance ToJSON StatusUpdateData where
  toEncoding = genericToEncoding jsonOptionsKeepNothing


data ResumeData = ResumeData
  { token     :: Text
  , sessionID :: Text
  , seq       :: Int
  } deriving (Show, Generic)

instance ToJSON ResumeData where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON ResumeData where
  parseJSON = genericParseJSON jsonOptions


data RequestGuildMembersData = RequestGuildMembersData
  { guildID :: Snowflake Guild
  , query   :: Maybe Text
  , limit   :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON RequestGuildMembersData where
  toEncoding RequestGuildMembersData{..} =
    pairs ("guild_id" .= guildID <>
           "query" .= fromMaybe "" query <>
           "limit" .= fromMaybe 0 limit)

-- TODO: Substitute void in snowflakes to correct types
-- TODO: Work on filling out data types and aeson stuff

data IdentifyProps = IdentifyProps
  { os      :: Text
  , browser :: Text
  , device  :: Text
  } deriving (Show, Generic)

instance ToJSON IdentifyProps where
  toEncoding IdentifyProps{..} =
    pairs ("$os" .= os <>
           "$browser" .= browser <>
           "$device" .= device)

data ControlMessage
  = Restart
  | ShutDown
  | SendPresence StatusUpdateData
  deriving (Show)

data ShardException = ShardExcRestart | ShardExcShutDown
  deriving (Show)

instance Exception ShardException

data Shard = Shard
  { shardID    :: Int
  , shardCount :: Int
  , evtChan    :: TChan ShardMsg
  , cmdChan    :: TChan ControlMessage
  , shardState :: TVar ShardState
  , token      :: Text
  } deriving (Generic)

data ShardState = ShardState
  { shardS     :: Shard
  , seqNum     :: Maybe Int
  , hbThread   :: Maybe (Async ())
  , hbResponse :: Bool
  , wsHost     :: Maybe Text
  , sessionID  :: Maybe Text
  , wsConn     :: Maybe Connection
  , setExc     :: Maybe ShardException
  } deriving (Generic)

newtype ShardM env a = ShardM
  { unShardM :: LogT env (StateC ShardState IO) a
  } deriving (Applicative, Monad, MonadIO, MonadLog env,
              Functor, MonadState ShardState)

instance MonadState s m => MonadState s (LogT env m) where
  get   = lift get
  put   = lift . put
  state = lift . state
