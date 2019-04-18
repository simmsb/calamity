-- | module containing all dispatch events

module Calamity.Types.DispatchEvents where

import           Data.Aeson
import           Data.Time

import           Calamity.Types.Snowflake
import           Calamity.Types.General

data DispatchData
  = Ready ReadyData
  | ChannelCreate Channel
  | ChannelUpdate Channel
  | ChannelDelete Channel
  | ChannelPinsUpdate ChannelPinsUpdateData
  | GuildCreate Guild
  | GuildUpdate Guild
  | GuildDelete UnavailableGuild
  | GuildBanAdd GuildBanData
  | GuildBanRemove GuildBanData
  | GuildEmojisUpdate GuildEmojisUpdateData
  | GuildIntegrationsUpdate GuildIntegrationsUpdateData
  | GuildMemberAdd GuildMemberAddData
  | GuildMemberRemove GuildMemberRemoveData
  | GuildMemberUpdate GuildMemberUpdateData
  | GuildMembersChunk GuildMembersChunkData
  | GuildRoleCreate GuildRoleCreateData
  | GuildRoleUpdate GuildRoleUpdateData
  | GuildRoleDelete GuildRoleDeleteData
  | MessageCreate Message
  | MessageUpdate MessageUpdateData
  | MessageDelete MessageDeleteData
  | MessageDeleteBulk MessageDeleteBulkData
  | MessageReactionAdd MessageReactionAddData
  | MessageReactionRemove MessageReactionRemoveData
  | MessageReactionRemoveAll MessageReactionRemoveAllData
  | PresenceUpdate PresenceUpdateData
  | TypingStart TypingStartData
  | UserUpdate UserUpdateData
  | VoiceStateUpdate VoiceStateUpdateData
  | VoiceServerUpdate VoiceServerUpdateData
  | WebhooksUpdate WebhooksUpdateData
  deriving (Show, Generic)

data ReadyData = ReadyData
  { v         :: Integer
  , user      :: User
  , guilds    :: [UnavailableGuild]
  , sessionID :: Text
  } deriving (Show, Generic)

instance FromJSON ReadyData where
  parseJSON = genericParseJSON jsonOptions

-- TODO: literally all of these

data ChannelPinsUpdateData = ChannelPinsUpdateData
  { channelID :: Snowflake Channel
  , lastPinTimestamp :: Maybe UTCTime
  } deriving (Show, Generic)

instance FromJSON ChannelPinsUpdateData where
  parseJSON = genericParseJSON jsonOptions


data GuildBanData = GuildBanData
  { guildID :: Snowflake Guild
  , user :: User
  } deriving (Show, Generic)

instance FromJSON GuildBanData where
  parseJSON = genericParseJSON jsonOptions


data GuildEmojisUpdateData = GuildEmojisUpdateData
  { guildID :: Snowflake Guild
  , emojis :: [Emoji]
  } deriving (Show, Generic)

instance FromJSON GuildEmojisUpdateData where
  parseJSON = genericParseJSON jsonOptions


data GuildIntegrationsUpdateData = GuildIntegrationsUpdateData
  { guildID :: Snowflake Guild
  } deriving (Show, Generic)

instance FromJSON GuildIntegrationsUpdateData where
  parseJSON = genericParseJSON jsonOptions


data GuildMemberAddData = GuildMemberAddData
  { member :: Member
  , guildID :: Snowflake Guild
  } deriving (Show, Generic)

instance FromJSON GuildMemberAddData where
  parseJSON = withObject "GuildMemberAddData" $ \v -> GuildMemberAddData
    <$> v .: "member"
    <*> v .: "guild_id"


newtype GuildMemberRemoveData = GuildMemberRemoveData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype GuildMemberUpdateData = GuildMemberUpdateData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype GuildMembersChunkData = GuildMembersChunkData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype GuildRoleCreateData = GuildRoleCreateData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype GuildRoleUpdateData = GuildRoleUpdateData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype GuildRoleDeleteData = GuildRoleDeleteData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype MessageUpdateData = MessageUpdateData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype MessageDeleteData = MessageDeleteData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype MessageDeleteBulkData = MessageDeleteBulkData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype MessageReactionAddData = MessageReactionAddData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype MessageReactionRemoveData = MessageReactionRemoveData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype MessageReactionRemoveAllData = MessageReactionRemoveAllData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype PresenceUpdateData = PresenceUpdateData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype TypingStartData = TypingStartData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype UserUpdateData = UserUpdateData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype VoiceStateUpdateData = VoiceStateUpdateData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype VoiceServerUpdateData = VoiceServerUpdateData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype WebhooksUpdateData = WebhooksUpdateData Value
  deriving (Show, Generic, ToJSON, FromJSON)
