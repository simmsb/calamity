-- | Types for shards
module Calamity.Gateway.Types
    ( ShardC
    , ShardMsg(..)
    , ReceivedDiscordMessage(..)
    , SentDiscordMessage(..)
    , DispatchType(..)
    , IdentifyData(..)
    , StatusUpdateData(..)
    , ResumeData(..)
    , RequestGuildMembersData(..)
    , IdentifyProps(..)
    , ControlMessage(..)
    , ShardFlowControl(..)
    , Shard(..)
    , ShardState(..) ) where

import           Calamity.Gateway.DispatchEvents
import           Calamity.Gateway.Intents
import           Calamity.Internal.AesonThings
import           Calamity.Metrics.Eff
import           Calamity.Types.LogEff
import           Calamity.Types.Model.Guild.Guild
import           Calamity.Types.Model.Voice
import           Calamity.Types.Model.Presence.Activity
import           Calamity.Types.Model.User (StatusType)
import           Calamity.Types.Snowflake

import           Control.Concurrent.Async
import           Control.Concurrent.Chan.Unagi

import           Data.Aeson
import qualified Data.Aeson.Types                 as AT
import           Data.Generics.Labels             ()
import           Data.Maybe
import           Data.Text                   ( Text )

import           GHC.Generics

import           Network.WebSockets.Connection    ( Connection )

import qualified Polysemy                         as P
import qualified Polysemy.Async                   as P
import qualified Polysemy.AtomicState             as P
import qualified TextShow.Generic as TSG
import TextShow (TextShow)
import Control.Lens.Operators ((^?))
import Data.Aeson.Types (parseMaybe)
import Control.Lens (Ixed(ix))
import Data.Aeson.Lens

type ShardC r = (P.Members '[LogEff, P.AtomicState ShardState, P.Embed IO, P.Final IO,
  P.Async, MetricEff] r)

data ShardMsg
  = Discord ReceivedDiscordMessage
  | Control ControlMessage
  deriving ( Show, Generic )

data ReceivedDiscordMessage
  = EvtDispatch Int !DispatchData
  | HeartBeatReq
  | Reconnect
  | InvalidSession Bool
  | Hello Int
  | HeartBeatAck
  deriving ( Show, Generic )

instance FromJSON ReceivedDiscordMessage where
  parseJSON = withObject "ReceivedDiscordMessage" $ \v -> do
    op :: Int <- v .: "op"
    case op of
      0  -> do
        d <- v .: "d"
        t <- v .: "t"
        s <- v .: "s"
        EvtDispatch s <$> parseDispatchData t d

      1  -> pure HeartBeatReq

      7  -> pure Reconnect

      9  -> InvalidSession <$> v .: "d"

      10 -> Hello <$> do
        d <- v .: "d"
        d .: "heartbeat_interval"

      11 -> pure HeartBeatAck

      _  -> fail $ "invalid opcode: " <> show op

parseDispatchData :: DispatchType -> Value -> AT.Parser DispatchData
parseDispatchData READY data'                       = Ready <$> parseJSON data'
parseDispatchData RESUMED _                         = pure Resumed
parseDispatchData CHANNEL_CREATE data'              = ChannelCreate <$> parseJSON data'
parseDispatchData CHANNEL_UPDATE data'              = ChannelUpdate <$> parseJSON data'
parseDispatchData CHANNEL_DELETE data'              = ChannelDelete <$> parseJSON data'
parseDispatchData CHANNEL_PINS_UPDATE data'         = ChannelPinsUpdate <$> parseJSON data'
parseDispatchData GUILD_CREATE data'                = GuildCreate <$> parseJSON data'
parseDispatchData GUILD_UPDATE data'                = GuildUpdate <$> parseJSON data'
parseDispatchData GUILD_DELETE data'                = GuildDelete <$> parseJSON data'
parseDispatchData GUILD_BAN_ADD data'               = GuildBanAdd <$> parseJSON data'
parseDispatchData GUILD_BAN_REMOVE data'            = GuildBanRemove <$> parseJSON data'
parseDispatchData GUILD_EMOJIS_UPDATE data'         = GuildEmojisUpdate <$> parseJSON data'
parseDispatchData GUILD_INTEGRATIONS_UPDATE data'   = GuildIntegrationsUpdate <$> parseJSON data'
parseDispatchData GUILD_MEMBER_ADD data'            = GuildMemberAdd <$> parseJSON data'
parseDispatchData GUILD_MEMBER_REMOVE data'         = GuildMemberRemove <$> parseJSON data'
parseDispatchData GUILD_MEMBER_UPDATE data'         = GuildMemberUpdate <$> parseJSON data'
parseDispatchData GUILD_MEMBERS_CHUNK data'         = GuildMembersChunk <$> parseJSON data'
parseDispatchData GUILD_ROLE_CREATE data'           = GuildRoleCreate <$> parseJSON data'
parseDispatchData GUILD_ROLE_UPDATE data'           = GuildRoleUpdate <$> parseJSON data'
parseDispatchData GUILD_ROLE_DELETE data'           = GuildRoleDelete <$> parseJSON data'
parseDispatchData INVITE_CREATE data'               = InviteCreate <$> parseJSON data'
parseDispatchData INVITE_DELETE data'               = InviteDelete <$> parseJSON data'
parseDispatchData MESSAGE_CREATE data'              = do
  message <- parseJSON data'
  let member = parseMaybe (withObject "MessageCreate.member" $ \o -> do
                                         userObject :: Object <- o .: "author"
                                         memberObject :: Object <- o .: "member"
                                         guildID :: String <- o .: "guild_id"
                                         parseJSON $ Object (memberObject <> "user" .= userObject <> "guild_id" .= guildID)) data'

  let user = parseMaybe parseJSON =<< data' ^? _Object . ix "author"
  pure $ MessageCreate message user member
parseDispatchData MESSAGE_UPDATE data'              = do
  message <- parseJSON data'
  let member = parseMaybe (withObject "MessageCreate.member" $ \o -> do
                                         userObject :: Object <- o .: "author"
                                         memberObject :: Object <- o .: "member"
                                         guildID :: String <- o .: "guild_id"
                                         parseJSON $ Object (memberObject <> "user" .= userObject <> "guild_id" .= guildID)) data'
  let user = parseMaybe parseJSON =<< data' ^? _Object . ix "author"
  pure $ MessageUpdate message user member
parseDispatchData MESSAGE_DELETE data'              = MessageDelete <$> parseJSON data'
parseDispatchData MESSAGE_DELETE_BULK data'         = MessageDeleteBulk <$> parseJSON data'
parseDispatchData MESSAGE_REACTION_ADD data'        = MessageReactionAdd <$> parseJSON data'
parseDispatchData MESSAGE_REACTION_REMOVE data'     = MessageReactionRemove <$> parseJSON data'
parseDispatchData MESSAGE_REACTION_REMOVE_ALL data' = MessageReactionRemoveAll <$> parseJSON data'
parseDispatchData PRESENCE_UPDATE data'             = PresenceUpdate <$> parseJSON data'
parseDispatchData TYPING_START data'                = TypingStart <$> parseJSON data'
parseDispatchData USER_UPDATE data'                 = UserUpdate <$> parseJSON data'
parseDispatchData VOICE_STATE_UPDATE data'          = VoiceStateUpdate <$> parseJSON data'
parseDispatchData VOICE_SERVER_UPDATE data'         = VoiceServerUpdate <$> parseJSON data'
parseDispatchData WEBHOOKS_UPDATE data'             = WebhooksUpdate <$> parseJSON data'

data SentDiscordMessage
  = StatusUpdate StatusUpdateData
  | Identify IdentifyData
  | HeartBeat (Maybe Int)
  | VoiceStatusUpdate VoiceState
  | Resume ResumeData
  | RequestGuildMembers RequestGuildMembersData
  deriving ( Show, Generic )

instance ToJSON SentDiscordMessage where
  toJSON (HeartBeat data') = object ["op" .= (1 :: Int), "d" .= data']

  toJSON (Identify data') = object ["op" .= (2 :: Int), "d" .= data']

  toJSON (StatusUpdate data') = object ["op" .= (3 :: Int), "d" .= data']

  toJSON (VoiceStatusUpdate data') = object ["op" .= (4 :: Int), "d" .= data']

  toJSON (Resume data') = object ["op" .= (6 :: Int), "d" .= data']

  toJSON (RequestGuildMembers data') = object ["op" .= (8 :: Int), "d" .= data']

  toEncoding (HeartBeat data') = pairs ("op" .= (1 :: Int) <> "d" .= data')

  toEncoding (Identify data') = pairs ("op" .= (2 :: Int) <> "d" .= data')

  toEncoding (StatusUpdate data') = pairs ("op" .= (3 :: Int) <> "d" .= data')

  toEncoding (VoiceStatusUpdate data') = pairs ("op" .= (4 :: Int) <> "d" .= data')

  toEncoding (Resume data') = pairs ("op" .= (6 :: Int) <> "d" .= data')

  toEncoding (RequestGuildMembers data') = pairs ("op" .= (8 :: Int) <> "d" .= data')

-- Thanks sbrg:
-- https://github.com/saevarb/haskord/blob/d1bb07bcc4f3dbc29f2dfd3351ff9f16fc100c07/haskord-lib/src/Haskord/Types/Common.hs
data DispatchType
  = READY
  | RESUMED
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
  | INVITE_CREATE
  | INVITE_DELETE
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
  deriving ( Show, Eq, Enum, Generic )
  deriving anyclass ( ToJSON, FromJSON )

data IdentifyData = IdentifyData
  { token          :: Text
  , properties     :: IdentifyProps
  , compress       :: Bool
  , largeThreshold :: Int
  , shard          :: (Int, Int)
  , presence       :: Maybe StatusUpdateData
  , intents        :: Intents
  }
  deriving ( Show, Generic )
  deriving ToJSON via CalamityJSON IdentifyData

data StatusUpdateData = StatusUpdateData
  { since  :: Maybe Integer
  , game   :: Maybe Activity
  , status :: StatusType
  , afk    :: Bool
  }
  deriving ( Show, Generic )
  deriving ToJSON via CalamityJSONKeepNothing StatusUpdateData

data ResumeData = ResumeData
  { token     :: Text
  , sessionID :: Text
  , seq       :: Int
  }
  deriving ( Show, Generic )
  deriving ( ToJSON, FromJSON ) via CalamityJSON ResumeData

data RequestGuildMembersData = RequestGuildMembersData
  { guildID :: Snowflake Guild
  , query   :: Maybe Text
  , limit   :: Maybe Int
  }
  deriving ( Show, Generic )

instance ToJSON RequestGuildMembersData where
  toJSON RequestGuildMembersData { guildID, query, limit } =
    object ["guild_id" .= guildID, "query" .= fromMaybe "" query, "limit" .= fromMaybe 0 limit]

data IdentifyProps = IdentifyProps
  { browser :: Text
  , device  :: Text
  }
  deriving ( Show, Generic )

instance ToJSON IdentifyProps where
  toJSON IdentifyProps { browser, device } = object ["$browser" .= browser, "$device" .= device]

data ControlMessage
  = RestartShard
  | ShutDownShard
  | SendPresence StatusUpdateData
  deriving ( Show, Generic )

data ShardFlowControl
  = ShardFlowRestart
  | ShardFlowShutDown
  deriving ( Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric ShardFlowControl

data Shard = Shard
  { shardID       :: Int
  , shardCount    :: Int
  , gateway       :: Text
  , evtIn         :: InChan CalamityEvent
  , cmdOut        :: OutChan ControlMessage
  , token         :: Text
  , initialStatus :: Maybe StatusUpdateData
  , intents       :: Intents
  }
  deriving ( Generic )

data ShardState = ShardState
  { shardS     :: Shard
  , seqNum     :: Maybe Int
  , hbThread   :: Maybe (Async (Maybe ()))
  , hbResponse :: Bool
  , wsHost     :: Maybe Text
  , sessionID  :: Maybe Text
  , wsConn     :: Maybe Connection
  }
  deriving ( Generic )
