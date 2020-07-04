-- | module containing all dispatch events
module Calamity.Gateway.DispatchEvents where

import           Calamity.Internal.AesonThings
import           Calamity.Internal.ConstructorName
import           Calamity.Internal.Utils                     ()
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.Channel.UpdatedMessage
import           Calamity.Types.Model.Guild.Ban
import           Calamity.Types.Model.Guild.Emoji
import           Calamity.Types.Model.Guild.Guild
import           Calamity.Types.Model.Guild.Member
import           Calamity.Types.Model.Guild.Role
import           Calamity.Types.Model.Guild.UnavailableGuild
import           Calamity.Types.Model.Presence.Presence
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake
import           Calamity.Types.UnixTimestamp

import           Data.Aeson
import           Data.Dynamic
import           Data.Text.Lazy                              ( Text )
import           Data.Time
import           Data.Typeable
import           Data.Vector.Unboxing                        ( Vector )

import           GHC.Generics

data CalamityEvent
  = Dispatch
    Int -- ^ The shard that pushed this event
    DispatchData -- ^ The attached data
  | Custom
    TypeRep -- ^ The name of the custom event
    Dynamic -- ^ The data sent to the custom event
  | ShutDown
  deriving ( Show, Generic )

data DispatchData
  = Ready ReadyData
  | Resumed
  | ChannelCreate Channel
  | ChannelUpdate Channel
  | ChannelDelete Channel
  | ChannelPinsUpdate ChannelPinsUpdateData
  | GuildCreate Guild
  | GuildUpdate UpdatedGuild
  | GuildDelete UnavailableGuild
  | GuildBanAdd BanData
  | GuildBanRemove BanData
  | GuildEmojisUpdate GuildEmojisUpdateData
  | GuildIntegrationsUpdate GuildIntegrationsUpdateData
  | GuildMemberAdd Member
  | GuildMemberRemove GuildMemberRemoveData
  | GuildMemberUpdate GuildMemberUpdateData
  | GuildMembersChunk GuildMembersChunkData
  | GuildRoleCreate GuildRoleData
  | GuildRoleUpdate GuildRoleData
  | GuildRoleDelete GuildRoleDeleteData
  | InviteCreate InviteCreateData
  | InviteDelete InviteDeleteData
  | MessageCreate Message
  | MessageUpdate UpdatedMessage
  | MessageDelete MessageDeleteData
  | MessageDeleteBulk MessageDeleteBulkData
  | MessageReactionAdd Reaction
  | MessageReactionRemove Reaction
  | MessageReactionRemoveAll MessageReactionRemoveAllData
  | PresenceUpdate PresenceUpdateData
  | TypingStart TypingStartData
  | UserUpdate User
  | VoiceStateUpdate VoiceStateUpdateData
  | VoiceServerUpdate VoiceServerUpdateData
  | WebhooksUpdate WebhooksUpdateData
  deriving ( Show, Generic, CtorName )

data ReadyData = ReadyData
  { v         :: Integer
  , user      :: User
  , guilds    :: [UnavailableGuild]
  , sessionID :: Text
  }
  deriving ( Show, Generic )
  deriving FromJSON via CalamityJSON ReadyData

data ChannelPinsUpdateData = ChannelPinsUpdateData
  { channelID        :: Snowflake Channel
  , lastPinTimestamp :: Maybe UTCTime
  }
  deriving ( Show, Generic )
  deriving FromJSON via CalamityJSON ChannelPinsUpdateData

data GuildEmojisUpdateData = GuildEmojisUpdateData
  { guildID :: Snowflake Guild
  , emojis  :: [Emoji]
  }
  deriving ( Show, Generic )
  deriving FromJSON via CalamityJSON GuildEmojisUpdateData

newtype GuildIntegrationsUpdateData = GuildIntegrationsUpdateData
  { guildID :: Snowflake Guild
  }
  deriving newtype ( Show, Generic )
  deriving FromJSON via CalamityJSON GuildIntegrationsUpdateData

data GuildMemberRemoveData = GuildMemberRemoveData
  { guildID :: Snowflake Guild
  , user    :: User
  }
  deriving ( Show, Generic )
  deriving FromJSON via CalamityJSON GuildMemberRemoveData

data GuildMemberUpdateData = GuildMemberUpdateData
  { guildID :: Snowflake Guild
  , roles   :: Vector (Snowflake Role)
  , user    :: User
  , nick    :: Maybe Text
  }
  deriving ( Show, Generic )
  deriving FromJSON via CalamityJSON GuildMemberUpdateData

data GuildMembersChunkData = GuildMembersChunkData
  { guildID :: Snowflake Guild
  , members :: [Member]
  }
  deriving ( Show, Generic )

instance FromJSON GuildMembersChunkData where
  parseJSON = withObject "GuildMembersChunkData" $ \v -> do
    guildID <- v .: "guild_id"

    members' <- do
      members' <- v .: "members"
      traverse (\m -> parseJSON $ Object (m <> "guild_id" .= guildID)) members'

    pure $ GuildMembersChunkData guildID members'

data GuildRoleData = GuildRoleData
  { guildID :: Snowflake Guild
  , role    :: Role
  }
  deriving ( Show, Generic )
  deriving FromJSON via CalamityJSON GuildRoleData

data GuildRoleDeleteData = GuildRoleDeleteData
  { guildID :: Snowflake Guild
  , roleID  :: Snowflake Role
  }
  deriving ( Show, Generic )
  deriving FromJSON via CalamityJSON GuildRoleDeleteData

data InviteCreateData = InviteCreateData
  { channelID      :: Snowflake Channel
  , code           :: Text
  , createdAt      :: UnixTimestamp
  , guildID        :: Maybe (Snowflake Guild)
  , inviter        :: Maybe (Snowflake User)
  , maxAge         :: Int
  , maxUses        :: Int
  , targetUser     :: Maybe (Snowflake User)
  , targetUserType :: Maybe Integer
  , temporary      :: Bool
  , uses           :: Integer
  }
  deriving ( Show, Generic )
  deriving ( FromJSON ) via WithSpecialCases
      '["inviter" `ExtractFieldFrom` "id", "targetUser" `ExtractFieldFrom` "id"]
      InviteCreateData

data InviteDeleteData = InviteDeleteData
  { channelID :: Snowflake Channel
  , guildID   :: Maybe (Snowflake Guild)
  , code      :: Text
  }
  deriving ( Show, Generic )
  deriving ( FromJSON ) via CalamityJSON InviteDeleteData

data MessageDeleteData = MessageDeleteData
  { id        :: Snowflake Message
  , channelID :: Snowflake Channel
  , guildID   :: Snowflake Guild
  }
  deriving ( Show, Generic )
  deriving FromJSON via CalamityJSON MessageDeleteData

data MessageDeleteBulkData = MessageDeleteBulkData
  { guildID   :: Snowflake Guild
  , channelID :: Snowflake Channel
  , ids       :: [Snowflake Message]
  }
  deriving ( Show, Generic )
  deriving FromJSON via CalamityJSON MessageDeleteBulkData

data MessageReactionRemoveAllData = MessageReactionRemoveAllData
  { channelID :: Snowflake Channel
  , messageID :: Snowflake Message
  , guildID   :: Maybe (Snowflake Guild)
  }
  deriving ( Show, Generic )
  deriving FromJSON via CalamityJSON MessageReactionRemoveAllData

data PresenceUpdateData = PresenceUpdateData
  { userID   :: Snowflake User
  , roles    :: Vector (Snowflake Role)
  , presence :: Presence
  }
  deriving ( Show, Generic )

instance FromJSON PresenceUpdateData where
  parseJSON = withObject "PresenceUpdate" $ \v -> do
    user <- (v .: "user") >>= (.: "id")
    roles <- v .: "roles"
    presence <- parseJSON $ Object v
    pure $ PresenceUpdateData user roles presence

data TypingStartData = TypingStartData
  { channelID :: Snowflake Channel
  , guildID   :: Maybe (Snowflake Guild)
  , userID    :: Snowflake User
  , timestamp :: UnixTimestamp
  }
  deriving ( Show, Generic )
  deriving FromJSON via CalamityJSON TypingStartData

newtype VoiceStateUpdateData = VoiceStateUpdateData Value
  deriving newtype ( Show, Generic )
  deriving newtype ( ToJSON, FromJSON )

newtype VoiceServerUpdateData = VoiceServerUpdateData Value
  deriving newtype ( Show, Generic )
  deriving newtype ( ToJSON, FromJSON )

newtype WebhooksUpdateData = WebhooksUpdateData Value
  deriving newtype ( Show, Generic )
  deriving newtype ( ToJSON, FromJSON )
