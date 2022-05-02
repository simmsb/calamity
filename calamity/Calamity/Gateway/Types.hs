{-# LANGUAGE TemplateHaskell #-}

-- | Types for shards
module Calamity.Gateway.Types (
  ShardC,
  ShardMsg (..),
  ReceivedDiscordMessage (..),
  SentDiscordMessage (..),
  DispatchType (..),
  IdentifyData (..),
  StatusUpdateData (..),
  ResumeData (..),
  RequestGuildMembersData (..),
  IdentifyProps (..),
  ControlMessage (..),
  ShardFlowControl (..),
  Shard (..),
  ShardState (..),
) where

import Calamity.Gateway.DispatchEvents
import Calamity.Gateway.Intents
import Calamity.Internal.Utils (CalamityToJSON (..), CalamityToJSON' (..), (.=), (.?=))
import Calamity.Metrics.Eff
import Calamity.Types.LogEff
import Calamity.Types.Model.Guild.Guild
import Calamity.Types.Model.Presence.Activity
import Calamity.Types.Model.User
import Calamity.Types.Model.Voice
import Calamity.Types.Snowflake
import Control.Concurrent.Async
import Control.Concurrent.Chan.Unagi
import qualified Data.Aeson as Aeson
import Data.Aeson.Optics
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Types as AT
import Data.Text (Text)
import GHC.Generics
import Network.WebSockets.Connection (Connection)
import Optics
import qualified Polysemy as P
import qualified Polysemy.Async as P
import qualified Polysemy.AtomicState as P

type ShardC r =
  ( P.Members
      '[ LogEff
       , P.AtomicState ShardState
       , P.Embed IO
       , P.Final IO
       , P.Async
       , MetricEff
       ]
      r
  )

data ShardMsg
  = Discord ReceivedDiscordMessage
  | Control ControlMessage
  deriving (Show)

data ReceivedDiscordMessage
  = EvtDispatch Int !DispatchData
  | HeartBeatReq
  | Reconnect
  | InvalidSession Bool
  | Hello Int
  | HeartBeatAck
  deriving (Show)

instance Aeson.FromJSON ReceivedDiscordMessage where
  parseJSON = Aeson.withObject "ReceivedDiscordMessage" $ \v -> do
    op :: Int <- v Aeson..: "op"
    case op of
      0 -> do
        d <- v Aeson..: "d"
        t <- v Aeson..: "t"
        s <- v Aeson..: "s"
        EvtDispatch s <$> parseDispatchData t d
      1 -> pure HeartBeatReq
      7 -> pure Reconnect
      9 -> InvalidSession <$> v Aeson..: "d"
      10 ->
        Hello <$> do
          d <- v Aeson..: "d"
          d Aeson..: "heartbeat_interval"
      11 -> pure HeartBeatAck
      _ -> fail $ "invalid opcode: " <> show op

parseDispatchData :: DispatchType -> Aeson.Value -> AT.Parser DispatchData
parseDispatchData READY data' = Ready <$> Aeson.parseJSON data'
parseDispatchData RESUMED _ = pure Resumed
parseDispatchData CHANNEL_CREATE data' = ChannelCreate <$> Aeson.parseJSON data'
parseDispatchData CHANNEL_UPDATE data' = ChannelUpdate <$> Aeson.parseJSON data'
parseDispatchData CHANNEL_DELETE data' = ChannelDelete <$> Aeson.parseJSON data'
parseDispatchData CHANNEL_PINS_UPDATE data' = ChannelPinsUpdate <$> Aeson.parseJSON data'
parseDispatchData GUILD_CREATE data' = GuildCreate <$> Aeson.parseJSON data'
parseDispatchData GUILD_UPDATE data' = GuildUpdate <$> Aeson.parseJSON data'
parseDispatchData GUILD_DELETE data' = GuildDelete <$> Aeson.parseJSON data'
parseDispatchData GUILD_BAN_ADD data' = GuildBanAdd <$> Aeson.parseJSON data'
parseDispatchData GUILD_BAN_REMOVE data' = GuildBanRemove <$> Aeson.parseJSON data'
parseDispatchData GUILD_EMOJIS_UPDATE data' = GuildEmojisUpdate <$> Aeson.parseJSON data'
parseDispatchData GUILD_INTEGRATIONS_UPDATE data' = GuildIntegrationsUpdate <$> Aeson.parseJSON data'
parseDispatchData GUILD_MEMBER_ADD data' = do
  guildID <- Aeson.withObject "GuildMemberAdd.guild_id" (Aeson..: "guild_id") data'
  GuildMemberAdd guildID <$> Aeson.parseJSON data'
parseDispatchData GUILD_MEMBER_REMOVE data' = GuildMemberRemove <$> Aeson.parseJSON data'
parseDispatchData GUILD_MEMBER_UPDATE data' = GuildMemberUpdate <$> Aeson.parseJSON data'
parseDispatchData GUILD_MEMBERS_CHUNK data' = GuildMembersChunk <$> Aeson.parseJSON data'
parseDispatchData GUILD_ROLE_CREATE data' = GuildRoleCreate <$> Aeson.parseJSON data'
parseDispatchData GUILD_ROLE_UPDATE data' = GuildRoleUpdate <$> Aeson.parseJSON data'
parseDispatchData GUILD_ROLE_DELETE data' = GuildRoleDelete <$> Aeson.parseJSON data'
parseDispatchData INVITE_CREATE data' = InviteCreate <$> Aeson.parseJSON data'
parseDispatchData INVITE_DELETE data' = InviteDelete <$> Aeson.parseJSON data'
parseDispatchData MESSAGE_CREATE data' = do
  message <- Aeson.parseJSON data'
  let member =
        parseMaybe
          ( Aeson.withObject "MessageCreate.member" $ \o -> do
              userObject :: Aeson.Object <- o Aeson..: "author"
              memberObject :: Aeson.Object <- o Aeson..: "member"
              Aeson.parseJSON $ Aeson.Object (memberObject <> "user" Aeson..= userObject)
          )
          data'

  let user = parseMaybe Aeson.parseJSON =<< data' ^? _Object % ix "author"
  pure $ MessageCreate message user member
parseDispatchData MESSAGE_UPDATE data' = do
  message <- Aeson.parseJSON data'
  let member =
        parseMaybe
          ( Aeson.withObject "MessageCreate.member" $ \o -> do
              userObject :: Aeson.Object <- o Aeson..: "author"
              memberObject :: Aeson.Object <- o Aeson..: "member"
              Aeson.parseJSON $ Aeson.Object (memberObject <> "user" Aeson..= userObject)
          )
          data'
  let user = parseMaybe Aeson.parseJSON =<< data' ^? _Object % ix "author"
  pure $ MessageUpdate message user member
parseDispatchData MESSAGE_DELETE data' = MessageDelete <$> Aeson.parseJSON data'
parseDispatchData MESSAGE_DELETE_BULK data' = MessageDeleteBulk <$> Aeson.parseJSON data'
parseDispatchData MESSAGE_REACTION_ADD data' = MessageReactionAdd <$> Aeson.parseJSON data'
parseDispatchData MESSAGE_REACTION_REMOVE data' = MessageReactionRemove <$> Aeson.parseJSON data'
parseDispatchData MESSAGE_REACTION_REMOVE_ALL data' = MessageReactionRemoveAll <$> Aeson.parseJSON data'
parseDispatchData PRESENCE_UPDATE data' = PresenceUpdate <$> Aeson.parseJSON data'
parseDispatchData TYPING_START data' = TypingStart <$> Aeson.parseJSON data'
parseDispatchData USER_UPDATE data' = UserUpdate <$> Aeson.parseJSON data'
parseDispatchData VOICE_STATE_UPDATE data' = VoiceStateUpdate <$> Aeson.parseJSON data'
parseDispatchData VOICE_SERVER_UPDATE data' = VoiceServerUpdate <$> Aeson.parseJSON data'
parseDispatchData WEBHOOKS_UPDATE data' = WebhooksUpdate <$> Aeson.parseJSON data'
parseDispatchData INTERACTION_CREATE data' = InteractionCreate <$> Aeson.parseJSON data'

data SentDiscordMessage
  = StatusUpdate StatusUpdateData
  | Identify IdentifyData
  | HeartBeat (Maybe Int)
  | VoiceStatusUpdate VoiceState
  | Resume ResumeData
  | RequestGuildMembers RequestGuildMembersData
  deriving (Show)

instance Aeson.ToJSON SentDiscordMessage where
  toJSON (HeartBeat data') = Aeson.object ["op" Aeson..= (1 :: Int), "d" Aeson..= data']
  toJSON (Identify data') = Aeson.object ["op" Aeson..= (2 :: Int), "d" Aeson..= data']
  toJSON (StatusUpdate data') = Aeson.object ["op" Aeson..= (3 :: Int), "d" Aeson..= data']
  toJSON (VoiceStatusUpdate data') = Aeson.object ["op" Aeson..= (4 :: Int), "d" Aeson..= data']
  toJSON (Resume data') = Aeson.object ["op" Aeson..= (6 :: Int), "d" Aeson..= data']
  toJSON (RequestGuildMembers data') = Aeson.object ["op" Aeson..= (8 :: Int), "d" Aeson..= data']

  toEncoding (HeartBeat data') = Aeson.pairs ("op" Aeson..= (1 :: Int) <> "d" Aeson..= data')
  toEncoding (Identify data') = Aeson.pairs ("op" Aeson..= (2 :: Int) <> "d" Aeson..= data')
  toEncoding (StatusUpdate data') = Aeson.pairs ("op" Aeson..= (3 :: Int) <> "d" Aeson..= data')
  toEncoding (VoiceStatusUpdate data') = Aeson.pairs ("op" Aeson..= (4 :: Int) <> "d" Aeson..= data')
  toEncoding (Resume data') = Aeson.pairs ("op" Aeson..= (6 :: Int) <> "d" Aeson..= data')
  toEncoding (RequestGuildMembers data') = Aeson.pairs ("op" Aeson..= (8 :: Int) <> "d" Aeson..= data')

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
  | INTERACTION_CREATE
  deriving (Show, Eq, Enum, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON)

data IdentifyData = IdentifyData
  { token :: Text
  , properties :: IdentifyProps
  , compress :: Bool
  , largeThreshold :: Maybe Int
  , shard :: Maybe (Int, Int)
  , presence :: Maybe StatusUpdateData
  , intents :: Intents
  }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON IdentifyData

instance CalamityToJSON' IdentifyData where
  toPairs IdentifyData {..} =
    [ "token" .= token
    , "properties" .= properties
    , "compress" .= compress
    , "large_threshold" .?= largeThreshold
    , "shard" .?= shard
    , "presence" .?= presence
    , "intents" .= intents
    ]

data StatusUpdateData = StatusUpdateData
  { since :: Maybe Integer
  , activities :: [Activity]
  , status :: StatusType
  , afk :: Bool
  }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON StatusUpdateData

instance CalamityToJSON' StatusUpdateData where
  toPairs StatusUpdateData {..} =
    [ "since" .= since
    , "activities" .= activities
    , "status" .= status
    , "afk" .= afk
    ]

data ResumeData = ResumeData
  { token :: Text
  , sessionID :: Text
  , seq :: Int
  }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON ResumeData

instance CalamityToJSON' ResumeData where
  toPairs ResumeData {..} =
    [ "token" .= token
    , "session_id" .= sessionID
    , "seq" .= seq
    ]

data RequestGuildMembersData = RequestGuildMembersData
  { guildID :: Snowflake Guild
  , query :: Maybe Text
  , limit :: Int
  , presences :: Maybe Bool
  , userIDs :: Maybe [Snowflake User]
  , nonce :: Maybe Text
  }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON RequestGuildMembersData

instance CalamityToJSON' RequestGuildMembersData where
  toPairs RequestGuildMembersData {..} =
    [ "guild_id" .= guildID
    , "query" .?= query
    , "limit" .= limit
    , "presences" .?= presences
    , "user_ids" .?= userIDs
    , "nonce" .?= nonce
    ]

data IdentifyProps = IdentifyProps
  { browser :: Text
  , device :: Text
  }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON IdentifyProps

instance CalamityToJSON' IdentifyProps where
  toPairs IdentifyProps {..} = ["$browser" .= browser, "$device" .= device]

data ControlMessage
  = RestartShard
  | ShutDownShard
  | SendPresence StatusUpdateData
  deriving (Show)

data ShardFlowControl
  = ShardFlowRestart
  | ShardFlowShutDown
  deriving (Show)

data Shard = Shard
  { shardID :: Int
  , shardCount :: Int
  , gateway :: Text
  , evtIn :: InChan CalamityEvent
  , cmdOut :: OutChan ControlMessage
  , token :: Text
  , initialStatus :: Maybe StatusUpdateData
  , intents :: Intents
  }

data ShardState = ShardState
  { shardS :: Shard
  , seqNum :: Maybe Int
  , hbThread :: Maybe (Async (Maybe ()))
  , hbResponse :: Bool
  , wsHost :: Maybe Text
  , sessionID :: Maybe Text
  , wsConn :: Maybe Connection
  }

$(makeFieldLabelsNoPrefix ''Shard)
$(makeFieldLabelsNoPrefix ''ShardState)
