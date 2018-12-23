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
  | MessageCreate MessageCreateData
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
  , guilds    :: [Guild]
  , sessionID :: Text
  } deriving (Show, Generic)

instance FromJSON ReadyData where
  parseJSON = genericParseJSON jsonOptions

type ChannelCreateData            = Value
type ChannelUpdateData            = Value
type ChannelDeleteData            = Value
type ChannelPinsUpdateData        = Value
type GuildCreateData              = Value
type GuildUpdateData              = Value
type GuildDeleteData              = Value
type GuildBanAddData              = Value
type GuildBanRemoveData           = Value
type GuildEmojisUpdateData        = Value
type GuildIntegrationsUpdateData  = Value
type GuildMemberAddData           = Value
type GuildMemberRemoveData        = Value
type GuildMemberUpdateData        = Value
type GuildMembersChunkData        = Value
type GuildRoleCreateData          = Value
type GuildRoleUpdateData          = Value
type GuildRoleDeleteData          = Value
type MessageCreateData            = Value
type MessageUpdateData            = Value
type MessageDeleteData            = Value
type MessageDeleteBulkData        = Value
type MessageReactionAddData       = Value
type MessageReactionRemoveData    = Value
type MessageReactionRemoveAllData = Value
type PresenceUpdateData           = Value
type TypingStartData              = Value
type UserUpdateData               = Value
type VoiceStateUpdateData         = Value
type VoiceServerUpdateData        = Value
type WebhooksUpdateData           = Value
