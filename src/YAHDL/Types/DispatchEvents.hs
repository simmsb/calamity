-- | module containing all dispatch events

module YAHDL.Types.DispatchEvents where

import           YAHDL.Types.General

data DispatchData
  = Ready ReadyData
  | ChannelCreate ChannelCreateData
  | ChannelUpdate ChannelUpdateData
  | ChannelDelete ChannelDeleteData
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


data ReadyData = ReadyData
  { v    :: Integer
  , user :: User
  , guilds :: [Guild]
  , sessionID :: Text
  }


type ChannelCreate = Value
type ChannelUpdate = Value
type ChannelDelete = Value
type GuildCreate = Value
type GuildUpdate = Value
type GuildDelete = Value
type GuildBanAdd = Value
type GuildBanRemove = Value
type GuildEmojisUpdate = Value
type GuildIntegrationsUpdate = Value
type GuildMemberAdd = Value
type GuildMemberRemove = Value
type GuildMemberUpdate = Value
type GuildMembersChunk = Value
type GuildRoleCreate = Value
type GuildRoleUpdate = Value
type GuildRoleDelete = Value
type MessageCreate = Value
type MessageUpdate = Value
type MessageDelete = Value
type MessageDeleteBulk = Value
type MessageReactionAdd = Value
type MessageReactionRemove = Value
type MessageReactionRemoveAll = Value
type PresenceUpdate = Value
type TypingStart = Value
type UserUpdate = Value
type VoiceStateUpdate = Value
type VoiceServerUpdate = Value
type WebhooksUpdate = Value
