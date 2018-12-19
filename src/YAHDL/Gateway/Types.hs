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

data ShardMsg = Discord ReceivedDiscordMessage | Control ControlMessage

data ReceivedDiscordMessage
  = Dispatch DispatchData
  | HeartBeatReq
  | Reconnect
  | InvalidSession
  | Hello Int
  | HeartBeatAck
  deriving (Show, Generic)

instance FromJSON ReceivedDiscordMessage where
  parseJSON = withObject "ReceivedDiscordMessage" $ \v -> do
    op :: Int <- v .: "op"
    case op of
      0 -> Dispatch <$>
        (DispatchData
         <$> v .: "d"
         <*> v .: "t"
         <*> v .: "s")

      1 -> pure HeartBeatReq

      7 -> pure Reconnect

      9 -> pure InvalidSession

      10 -> Hello <$> do
        d <- v .: "d"
        d .: "heartbeat_interval"

      11 -> pure HeartBeatAck

      op -> fail $ "invalid opcode: " <> show op

data SentDiscordMessage
  = StatusUpdate StatusUpdateData
  | Identify IdentifyData
  | HeartBeat (Maybe Integer)
  | VoiceStatusUpdate VoiceStateUpdateData
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

  toEncoding (VoiceStatusUpdate data') =
    pairs ("op" .= (4 :: Int) <> "d" .= data')

  toEncoding (Resume data') =
    pairs ("op" .= (6 :: Int) <> "d" .= data')

  toEncoding (RequestGuildMembers data') =
    pairs ("op" .= (8 :: Int) <> "d" .= data')

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

instance ToJSON EventType where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON EventType where
  parseJSON = genericParseJSON jsonOptions


data DispatchData = DispatchData
  { dispatchData :: Value
  , dispatchType :: EventType
  , sequenceNum  :: Int
  } deriving (Show, Generic)

data IdentifyData = IdentifyData
  { dataToken :: Text
  , dataProperties :: IdentifyProps
  , dataCompress :: Maybe Bool
  , dataLargeThreshold :: Maybe Int
  , dataShard :: Maybe Int
  , dataPresence :: Maybe PresenceData
  } deriving (Show, Generic)

instance ToJSON IdentifyData where
  toEncoding = genericToEncoding jsonOptions


data StatusUpdateData = StatusUpdateData
  { updateDataSince :: Maybe Integer
  , updateDataGame :: Maybe Value -- TODO: activity object
  , updateDataStatus :: Text
  , updateDataAfk :: Bool
  } deriving (Show, Generic)

instance ToJSON StatusUpdateData where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON StatusUpdateData where
  parseJSON = genericParseJSON jsonOptions



-- TODO: move this to the general types module
data VoiceStateUpdateData = VoiceStateUpdateData
  { guildID :: Maybe (Snowflake ())
  , channelID :: Maybe (Snowflake ())
  , userID :: Snowflake ()
  , member :: Maybe Value -- TODO: member object
  , sessionID :: Text
  , deaf :: Bool
  , mute :: Bool
  , selfDeaf :: Bool
  , selfMute :: Bool
  , suppress :: Bool
  } deriving (Show, Generic)

instance ToJSON VoiceStateUpdateData where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON VoiceStateUpdateData where
  parseJSON = genericParseJSON jsonOptions


data ResumeData = ResumeData
  { token :: Text
  , sessionID :: Text
  , seq :: Int
  } deriving (Show, Generic)

instance ToJSON ResumeData where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON ResumeData where
  parseJSON = genericParseJSON jsonOptions


data RequestGuildMembersData = RequestGuildMembersData
  { guildID :: Snowflake ()
  , query :: Maybe Text
  , limit :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON RequestGuildMembersData where
  toEncoding RequestGuildMembersData{..} =
    pairs ("guild_id" .= guildID <>
           "query" .= fromMaybe "" query <>
           "limit" .= fromMaybe 0 limit)

-- TODO: Substitute void in snowflakes to correct types
-- TODO: Work on filling out data types and aeson stuff

data IdentifyProps = IdentifyProps
  { propsOs :: Text
  , propsBrowser :: Text
  , propsDevice :: Text
  } deriving (Show, Generic)

instance ToJSON IdentifyProps where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON IdentifyProps where
  parseJSON = genericParseJSON jsonOptions


data PresenceData = PresenceData
  { dataSince :: Maybe Int
  , dataGame :: Maybe Value -- TODO: activity object
  , dataStatus :: Text
  , dataAfk :: Bool
  } deriving (Show, Generic)

instance ToJSON PresenceData where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON PresenceData where
  parseJSON = genericParseJSON jsonOptions

data ControlMessage = Restart | ShutDown
  deriving (Show)

instance Exception ControlMessage

data Shard = Shard
  { shardID :: Integer
  , evtChan :: TChan ShardMsg
  , cmdChan :: TChan ControlMessage
  , shardState :: TVar ShardState
  , token :: Text
  } deriving (Generic)

data ShardState = ShardState
  { shardS :: Shard
  , seqNum :: Maybe Integer
  , hbThread :: Maybe (Async ())
  , hbResponse :: Bool
  , wsHost :: Maybe Text
  , sessionID :: Maybe Text
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

jsonOptions :: Options
jsonOptions = defaultOptions { sumEncoding        = UntaggedValue
                             , fieldLabelModifier = camelTo2 '_'
                             , omitNothingFields  = True
                             }
