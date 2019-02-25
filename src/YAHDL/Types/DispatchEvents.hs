-- | module containing all dispatch events

module YAHDL.Types.DispatchEvents where

import           Data.Aeson
import           YAHDL.Types.General

data DispatchData
  = Ready ReadyData
  | ChannelCreate ChannelCreateData
  | ChannelUpdate ChannelUpdateData
  | ChannelDelete ChannelDeleteData
  | ChannelPinsUpdate ChannelPinsUpdateData
  | GuildCreate GuildCreateData
  | GuildUpdate GuildUpdateData
  | GuildDelete GuildDeleteData
  | GuildBanAdd GuildBanAddData
  | GuildBanRemove GuildBanRemoveData
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

newtype ChannelCreateData = ChannelCreateData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype ChannelUpdateData = ChannelUpdateData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype ChannelDeleteData = ChannelDeleteData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype ChannelPinsUpdateData = ChannelPinsUpdateData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype GuildCreateData = GuildCreateData Guild
  deriving (Show, Generic, ToJSON, FromJSON)

newtype GuildUpdateData = GuildUpdateData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype GuildDeleteData = GuildDeleteData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype GuildBanAddData = GuildBanAddData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype GuildBanRemoveData = GuildBanRemoveData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype GuildEmojisUpdateData = GuildEmojisUpdateData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype GuildIntegrationsUpdateData = GuildIntegrationsUpdateData Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype GuildMemberAddData = GuildMemberAddData Value
  deriving (Show, Generic, ToJSON, FromJSON)

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
