-- | module containing all dispatch events

module Calamity.Types.DispatchEvents where

import           Data.Aeson
import           Data.Time

import           Calamity.Types.General
import           Calamity.Types.Snowflake

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
  | GuildMemberAdd Member
  | GuildMemberRemove GuildMemberRemoveData
  | GuildMemberUpdate GuildMemberUpdateData
  | GuildMembersChunk GuildMembersChunkData
  | GuildRoleCreate GuildRoleData
  | GuildRoleUpdate GuildRoleData
  | GuildRoleDelete GuildRoleDeleteData
  | MessageCreate Message
  | MessageUpdate UpdatedMessage
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
  { channelID        :: Snowflake Channel
  , lastPinTimestamp :: Maybe UTCTime
  } deriving (Show, Generic)

instance FromJSON ChannelPinsUpdateData where
  parseJSON = genericParseJSON jsonOptions


data GuildBanData = GuildBanData
  { guildID :: Snowflake Guild
  , user    :: User
  } deriving (Show, Generic)

instance FromJSON GuildBanData where
  parseJSON = genericParseJSON jsonOptions


data GuildEmojisUpdateData = GuildEmojisUpdateData
  { guildID :: Snowflake Guild
  , emojis  :: [Emoji]
  } deriving (Show, Generic)

instance FromJSON GuildEmojisUpdateData where
  parseJSON = genericParseJSON jsonOptions


data GuildIntegrationsUpdateData = GuildIntegrationsUpdateData
  { guildID :: Snowflake Guild
  } deriving (Show, Generic)

instance FromJSON GuildIntegrationsUpdateData where
  parseJSON = genericParseJSON jsonOptions


data GuildMemberRemoveData = GuildMemberRemoveData
  { guildID :: Snowflake Guild
  , user    :: User
  } deriving (Show, Generic)

instance FromJSON GuildMemberRemoveData where
  parseJSON = genericParseJSON jsonOptions


data GuildMemberUpdateData = GuildMemberUpdateData
  { guildID :: Snowflake Guild
  , roles   :: [Snowflake Role]
  , user    :: User
  , nick    :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON GuildMemberUpdateData where
  parseJSON = genericParseJSON jsonOptions


data GuildMembersChunkData = GuildMembersChunkData
  { guildID :: Snowflake Guild
  , members :: [Member]
  } deriving (Show, Generic)

instance FromJSON GuildMembersChunkData where
  parseJSON = withObject "GuildMembersChunkData" $ \v -> do
    guildID <- v .: "guild_id"

    members' <- do
      members' <- v .: "members"
      mapM (\m -> parseJSON $ Object (m <> "guild_id" .= guildID)) members'

    pure $ GuildMembersChunkData guildID members'


data GuildRoleData = GuildRoleData
  { guildID :: Snowflake Guild
  , role    :: Role
  } deriving (Show, Generic)

instance FromJSON GuildRoleData where
  parseJSON = genericParseJSON jsonOptions


data GuildRoleDeleteData = GuildRoleDeleteData
  { guildID :: Snowflake Guild
  , roleID  :: Snowflake Role
  } deriving (Show, Generic)

instance FromJSON GuildRoleDeleteData where
  parseJSON = genericParseJSON jsonOptions


data MessageDeleteData = MessageDeleteData
  { id        :: Snowflake Message
  , channelID :: Snowflake Channel
  , guildID   :: Snowflake Guild
  } deriving (Show, Generic)

instance FromJSON MessageDeleteData where
  parseJSON = genericParseJSON jsonOptions


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
