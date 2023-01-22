{-# LANGUAGE TemplateHaskell #-}

-- | module containing all dispatch events
module Calamity.Gateway.DispatchEvents where

import Calamity.Internal.ConstructorName
import Calamity.Internal.UnixTimestamp
import Calamity.Internal.Utils
import Calamity.Types.Model.Channel
import Calamity.Types.Model.Channel.UpdatedMessage
import Calamity.Types.Model.Guild.Ban
import Calamity.Types.Model.Guild.Emoji
import Calamity.Types.Model.Guild.Guild
import Calamity.Types.Model.Guild.Member
import Calamity.Types.Model.Guild.Role
import Calamity.Types.Model.Guild.UnavailableGuild
import Calamity.Types.Model.Interaction
import Calamity.Types.Model.Presence.Presence
import Calamity.Types.Model.User
import Calamity.Types.Model.Voice
import Calamity.Types.Snowflake
import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Kind (Type)
import Data.Text (Text)
import Data.Time
import Data.Typeable (Typeable)
import GHC.Generics
import Optics.TH

data CalamityEvent
  = Dispatch
      Int
      -- ^ The shard that pushed this event
      DispatchData
      -- ^ The attached data
  | -- | The data sent to the custom event
    forall (a :: Type). Typeable a => Custom a
  | ShutDown

data DispatchData
  = Ready !ReadyData
  | Resumed
  | ChannelCreate !Channel
  | ChannelUpdate !Channel
  | ChannelDelete !Channel
  | ChannelPinsUpdate !ChannelPinsUpdateData
  | GuildCreate !Guild
  | GuildUpdate !UpdatedGuild
  | GuildDelete !UnavailableGuild
  | GuildBanAdd !BanData
  | GuildBanRemove !BanData
  | GuildEmojisUpdate !GuildEmojisUpdateData
  | GuildIntegrationsUpdate !GuildIntegrationsUpdateData
  | GuildMemberAdd !(Snowflake Guild) !Member
  | GuildMemberRemove !GuildMemberRemoveData
  | GuildMemberUpdate !GuildMemberUpdateData
  | GuildMembersChunk !GuildMembersChunkData
  | GuildRoleCreate !GuildRoleData
  | GuildRoleUpdate !GuildRoleData
  | GuildRoleDelete !GuildRoleDeleteData
  | InviteCreate !InviteCreateData
  | InviteDelete !InviteDeleteData
  | MessageCreate !Message !(Maybe User) !(Maybe Member)
  | MessageUpdate !UpdatedMessage !(Maybe User) !(Maybe Member)
  | MessageDelete !MessageDeleteData
  | MessageDeleteBulk !MessageDeleteBulkData
  | MessageReactionAdd !ReactionEvtData
  | MessageReactionRemove !ReactionEvtData
  | MessageReactionRemoveAll !MessageReactionRemoveAllData
  | PresenceUpdate !PresenceUpdateData
  | TypingStart !TypingStartData
  | UserUpdate !User
  | VoiceStateUpdate !VoiceState
  | VoiceServerUpdate !VoiceServerUpdateData
  | WebhooksUpdate !WebhooksUpdateData
  | InteractionCreate !Interaction
  deriving (Show, Generic, CtorName)

data ReadyData = ReadyData
  { v :: Integer
  , user :: User
  , guilds :: [UnavailableGuild]
  , sessionID :: Text
  }
  deriving (Show)

instance Aeson.FromJSON ReadyData where
  parseJSON = Aeson.withObject "ReadyData" $ \v ->
    ReadyData
      <$> v .: "v"
      <*> v .: "user"
      <*> v .: "guilds"
      <*> v .: "session_id"

data ChannelPinsUpdateData = ChannelPinsUpdateData
  { channelID :: Snowflake Channel
  , lastPinTimestamp :: Maybe UTCTime
  }
  deriving (Show)

instance Aeson.FromJSON ChannelPinsUpdateData where
  parseJSON = Aeson.withObject "ChannelPinsUpdateData" $ \v ->
    ChannelPinsUpdateData
      <$> v .: "channel_id"
      <*> v .: "last_pin_timestamp"

data GuildEmojisUpdateData = GuildEmojisUpdateData
  { guildID :: Snowflake Guild
  , emojis :: [Emoji]
  }
  deriving (Show)

instance Aeson.FromJSON GuildEmojisUpdateData where
  parseJSON = Aeson.withObject "GuildEmojisUpdateData" $ \v ->
    GuildEmojisUpdateData
      <$> v .: "guild_id"
      <*> v .: "emojis"

newtype GuildIntegrationsUpdateData = GuildIntegrationsUpdateData
  { guildID :: Snowflake Guild
  }
  deriving stock (Show)

instance Aeson.FromJSON GuildIntegrationsUpdateData where
  parseJSON = Aeson.withObject "GuildIntegrationsUpdateData" $ \v ->
    GuildIntegrationsUpdateData
      <$> v .: "guild_id"

data GuildMemberRemoveData = GuildMemberRemoveData
  { guildID :: Snowflake Guild
  , user :: User
  }
  deriving (Show)

instance Aeson.FromJSON GuildMemberRemoveData where
  parseJSON = Aeson.withObject "GuildMemberRemoveData" $ \v ->
    GuildMemberRemoveData
      <$> v .: "guild_id"
      <*> v .: "user"

data GuildMemberUpdateData = GuildMemberUpdateData
  { guildID :: Snowflake Guild
  , roles :: AesonVector (Snowflake Role)
  , user :: User
  , nick :: Maybe Text
  }
  deriving (Show)

instance Aeson.FromJSON GuildMemberUpdateData where
  parseJSON = Aeson.withObject "GuildMemberUpdateData" $ \v ->
    GuildMemberUpdateData
      <$> v .: "guild_id"
      <*> v .: "roles"
      <*> v .: "user"
      <*> v .:? "nick"

data GuildMembersChunkData = GuildMembersChunkData
  { guildID :: Snowflake Guild
  , members :: [Member]
  }
  deriving (Show)

instance Aeson.FromJSON GuildMembersChunkData where
  parseJSON = Aeson.withObject "GuildMembersChunkData" $ \v -> do
    guildID <- v Aeson..: "guild_id"

    members' <- do
      members' <- v Aeson..: "members"
      traverse (Aeson.parseJSON . Aeson.Object) members'

    pure $ GuildMembersChunkData guildID members'

data GuildRoleData = GuildRoleData
  { guildID :: Snowflake Guild
  , role :: Role
  }
  deriving (Show)

instance Aeson.FromJSON GuildRoleData where
  parseJSON = Aeson.withObject "GuildRoleData" $ \v ->
    GuildRoleData
      <$> v .: "guild_id"
      <*> v .: "role"

data GuildRoleDeleteData = GuildRoleDeleteData
  { guildID :: Snowflake Guild
  , roleID :: Snowflake Role
  }
  deriving (Show)
instance Aeson.FromJSON GuildRoleDeleteData where
  parseJSON = Aeson.withObject "GuildRoleDeleteData" $ \v ->
    GuildRoleDeleteData
      <$> v .: "guild_id"
      <*> v .: "role_id"

data InviteCreateData = InviteCreateData
  { channelID :: Snowflake Channel
  , code :: Text
  , createdAt :: UTCTime
  , guildID :: Maybe (Snowflake Guild)
  , inviter :: Maybe (Snowflake User)
  , maxAge :: Int
  , maxUses :: Int
  , targetUser :: Maybe (Snowflake User)
  , targetUserType :: Maybe Integer
  , temporary :: Bool
  , uses :: Integer
  }
  deriving (Show)

instance Aeson.FromJSON InviteCreateData where
  parseJSON = Aeson.withObject "InviteCreateData" $ \v -> do
    let inviter = (v .:? "inviter") >>= traverse (.: "id")
        targetUser = (v .:? "target_user") >>= traverse (.: "id")

    InviteCreateData
      <$> v .: "channel_id"
      <*> v .: "code"
      <*> v .: "created_at"
      <*> v .:? "guild_id"
      <*> inviter
      <*> v .: "max_age"
      <*> v .: "max_uses"
      <*> targetUser
      <*> v .:? "target_user_type"
      <*> v .: "temporary"
      <*> v .: "uses"

data InviteDeleteData = InviteDeleteData
  { channelID :: Snowflake Channel
  , guildID :: Maybe (Snowflake Guild)
  , code :: Text
  }
  deriving (Show)

instance Aeson.FromJSON InviteDeleteData where
  parseJSON = Aeson.withObject "InviteDeleteData" $ \v ->
    InviteDeleteData
      <$> v .: "channel_id"
      <*> v .:? "guild_id"
      <*> v .: "code"

data MessageDeleteData = MessageDeleteData
  { id :: Snowflake Message
  , channelID :: Snowflake Channel
  , guildID :: Snowflake Guild
  }
  deriving (Show)

instance Aeson.FromJSON MessageDeleteData where
  parseJSON = Aeson.withObject "MessageDeleteData" $ \v ->
    MessageDeleteData
      <$> v .: "id"
      <*> v .: "channel_id"
      <*> v .: "guild_id"

data MessageDeleteBulkData = MessageDeleteBulkData
  { guildID :: Snowflake Guild
  , channelID :: Snowflake Channel
  , ids :: [Snowflake Message]
  }
  deriving (Show)

instance Aeson.FromJSON MessageDeleteBulkData where
  parseJSON = Aeson.withObject "MessageDeleteBulkData" $ \v ->
    MessageDeleteBulkData
      <$> v .: "guild_id"
      <*> v .: "channel_id"
      <*> v .: "ids"

data MessageReactionRemoveAllData = MessageReactionRemoveAllData
  { channelID :: Snowflake Channel
  , messageID :: Snowflake Message
  , guildID :: Maybe (Snowflake Guild)
  }
  deriving (Show)

instance Aeson.FromJSON MessageReactionRemoveAllData where
  parseJSON = Aeson.withObject "MessageReactionRemoveAllData" $ \v ->
    MessageReactionRemoveAllData
      <$> v .: "channel_id"
      <*> v .: "message_id"
      <*> v .: "guild_id"

data PresenceUpdateData = PresenceUpdateData
  { userID :: Snowflake User
  , presence :: Presence
  }
  deriving (Show)

instance Aeson.FromJSON PresenceUpdateData where
  parseJSON = Aeson.withObject "PresenceUpdate" $ \v -> do
    user <- (v Aeson..: "user") >>= (Aeson..: "id")
    presence <- Aeson.parseJSON $ Aeson.Object v
    pure $ PresenceUpdateData user presence

data TypingStartData = TypingStartData
  { channelID :: Snowflake Channel
  , guildID :: Maybe (Snowflake Guild)
  , userID :: Snowflake User
  , timestamp :: UnixTimestamp
  }
  deriving (Show)

instance Aeson.FromJSON TypingStartData where
  parseJSON = Aeson.withObject "TypingStartData" $ \v ->
    TypingStartData
      <$> v .: "channel_id"
      <*> v .: "guild_id"
      <*> v .: "user_id"
      <*> v .: "timestamp"

newtype VoiceServerUpdateData = VoiceServerUpdateData Aeson.Value
  deriving stock (Show)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)

newtype WebhooksUpdateData = WebhooksUpdateData Aeson.Value
  deriving stock (Show)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)

data ReactionEvtData = ReactionEvtData
  { userID :: Snowflake User
  , channelID :: Snowflake Channel
  , messageID :: Snowflake Message
  , guildID :: Maybe (Snowflake Guild)
  , emoji :: RawEmoji
  }
  deriving (Eq, Show)
  deriving (HasID User) via HasIDField "userID" ReactionEvtData
  deriving (HasID Channel) via HasIDField "channelID" ReactionEvtData
  deriving (HasID Message) via HasIDField "messageID" ReactionEvtData

instance Aeson.FromJSON ReactionEvtData where
  parseJSON = Aeson.withObject "ReactionEvtData" $ \v ->
    ReactionEvtData
      <$> v .: "user_id"
      <*> v .: "channel_id"
      <*> v .: "message_id"
      <*> v .: "guild_id"
      <*> v .: "emoji"

$(makeFieldLabelsNoPrefix ''ReadyData)
$(makeFieldLabelsNoPrefix ''ReactionEvtData)
