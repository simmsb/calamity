{-# LANGUAGE TemplateHaskell #-}

-- | A message from a channel
module Calamity.Types.Model.Channel.Message (
  Message (..),
  MessageAuthor (..),
  MessageAuthorWebhook (..),
  ChannelMention (..),
  MessageType (..),
  MessageReference (..),
  Partial (PartialMessage),
) where

import Calamity.Internal.Utils (AesonVector (..), CalamityToJSON (..), CalamityToJSON' (..), (.=), (.?=))
import {-# SOURCE #-} Calamity.Types.Model.Channel
import Calamity.Types.Model.Channel.Attachment
import Calamity.Types.Model.Channel.ChannelType (ChannelType)
import Calamity.Types.Model.Channel.Component
import Calamity.Types.Model.Channel.Embed
import Calamity.Types.Model.Channel.Reaction
import Calamity.Types.Model.Channel.Webhook
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import Calamity.Types.Model.Guild.Role
import Calamity.Types.Model.User
import Calamity.Types.Snowflake
import Data.Aeson ((.!=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Maybe (isJust)
import Data.Scientific
import Data.Text (Text)
import Data.Time
import qualified Data.Vector.Unboxing as UV
import Data.Word (Word64)
import Optics.TH
import qualified TextShow
import TextShow.TH

data MessageAuthorWebhook = MessageAuthorWebhook
  { id :: Snowflake Webhook
  , username :: Text
  , avatar :: Maybe Text
  }
  deriving (Show)
  deriving (HasID Webhook) via HasIDField "id" MessageAuthorWebhook

data MessageAuthor
  = User' User
  | Webhook' MessageAuthorWebhook
  deriving (Show)

instance HasID User MessageAuthor where
  getID (User' u) = getID u
  getID (Webhook' MessageAuthorWebhook {id}) = coerceSnowflake id

data ChannelMention = ChannelMention
  { id :: Snowflake Channel
  , guildID :: Snowflake Guild
  , type_ :: ChannelType
  , name :: Text
  }
  deriving (Show)
  deriving (HasID Channel) via HasIDField "id" ChannelMention

instance Aeson.FromJSON ChannelMention where
  parseJSON = Aeson.withObject "Message.ChannelMention" $ \v ->
    ChannelMention
      <$> v .: "id"
      <*> v .: "guild_id"
      <*> v .: "type"
      <*> v .: "name"

data Message = Message
  { id :: Snowflake Message
  , channelID :: Snowflake Channel
  , guildID :: Maybe (Snowflake Guild)
  , author :: MessageAuthor
  , content :: Text
  , timestamp :: UTCTime
  , editedTimestamp :: Maybe UTCTime
  , tts :: Bool
  , mentionEveryone :: Bool
  , mentions :: [User]
  , mentionRoles :: UV.Vector (Snowflake Role)
  , mentionChannels :: [ChannelMention]
  , attachments :: [Attachment]
  , embeds :: [Embed]
  , reactions :: [Reaction]
  , nonce :: Maybe Aeson.Value
  , pinned :: Bool
  , webhookID :: Maybe (Snowflake Webhook)
  , type_ :: MessageType
  , activity :: Maybe Aeson.Object
  , application :: Maybe Aeson.Object
  , messageReference :: Maybe MessageReference
  , flags :: Word64
  , referencedMessage :: Maybe Message
  , interaction :: Maybe Aeson.Object
  , components :: [Component]
  }
  deriving (Show)
  deriving (TextShow.TextShow) via TextShow.FromStringShow Message
  deriving (HasID Message) via HasIDField "id" Message
  deriving (HasID Channel) via HasIDField "channelID" Message
  deriving (HasID User) via HasIDField "author" Message

instance Aeson.FromJSON Message where
  parseJSON = Aeson.withObject "Message" $ \v -> do
    webhookID <- v .:? "webhook_id"
    let author =
          if isJust webhookID
            then
              Aeson.withObject
                "Message.author"
                ( \v ->
                    Webhook'
                      <$> ( MessageAuthorWebhook
                              <$> v .: "id" <*> v .: "username" <*> v .:? "avatar"
                          )
                )
                =<< v .: "author"
            else User' <$> (Aeson.parseJSON =<< (v .: "author"))

    Message
      <$> v .: "id"
      <*> v .: "channel_id"
      <*> v .:? "guild_id"
      <*> author
      <*> v .: "content"
      <*> v .: "timestamp"
      <*> v .:? "edited_timestamp"
      <*> v .: "tts"
      <*> v .: "mention_everyone"
      <*> v .: "mentions"
      <*> (unAesonVector <$> v .: "mention_roles")
      <*> v .:? "mention_channels" .!= []
      <*> v .: "attachments"
      <*> v .: "embeds"
      <*> v .:? "reactions" .!= []
      <*> v .:? "nonce"
      <*> v .: "pinned"
      <*> pure webhookID
      <*> v .: "type"
      <*> v .:? "activity"
      <*> v .:? "application"
      <*> v .:? "message_reference"
      <*> v .:? "flags" .!= 0
      <*> v .:? "referenced_message"
      <*> v .:? "interaction"
      <*> v .:? "components" .!= []

data instance Partial Message = PartialMessage
  { channelID :: Snowflake Channel
  , guildID :: Maybe (Snowflake Guild)
  }
  deriving (Show)
  deriving (HasID Channel) via HasIDField "channelID" (Partial Message)

instance Aeson.FromJSON (Partial Message) where
  parseJSON = Aeson.withObject "Partial Message" $ \v ->
    PartialMessage
      <$> v .: "channel_id"
      <*> v .:? "guild_id"

data MessageReference = MessageReference
  { messageID :: Maybe (Snowflake Message)
  , channelID :: Maybe (Snowflake Channel)
  , guildID :: Maybe (Snowflake Guild)
  , failIfNotExists :: Bool
  }
  deriving (Eq, Show)
  deriving (Aeson.ToJSON) via CalamityToJSON MessageReference

instance CalamityToJSON' MessageReference where
  toPairs MessageReference {..} =
    [ "message_id" .?= messageID
    , "channel_id" .?= channelID
    , "guild_id" .?= guildID
    , "fail_if_not_exists" .= failIfNotExists
    ]

instance Aeson.FromJSON MessageReference where
  parseJSON = Aeson.withObject "MessageReference" $ \v ->
    MessageReference
      <$> v .:? "message_id"
      <*> v .:? "channel_id"
      <*> v .:? "guild_id"
      <*> v .:? "fail_if_not_exists" .!= False

-- Thanks sbrg (https://github.com/saevarb/haskord/blob/d1bb07bcc4f3dbc29f2dfd3351ff9f16fc100c07/haskord-lib/src/Haskord/Types/Common.hs#L264)
data MessageType
  = Default
  | RecipientAdd
  | RecipientRemove
  | Call
  | ChannelNameChange
  | ChannelIconChange
  | ChannelPinnedMessage
  | GuildMemberJoin
  | UserPremiumGuildSubscription
  | UserPremiumGuildSubscriptionTier1
  | UserPremiumGuildSubscriptionTier2
  | UserPremiumGuildSubscriptionTier3
  | ChannelFollowAdd
  | GuildDiscoveryDisqualified
  | GuildDiscoveryRequalified
  | Reply
  | ApplicationCommmand
  deriving (Eq, Show, Enum)

instance Aeson.FromJSON MessageType where
  parseJSON = Aeson.withScientific "MessageType" $ \n -> case toBoundedInteger @Int n of
    Just v -> case v of
      0 -> pure Default
      1 -> pure RecipientAdd
      2 -> pure RecipientRemove
      3 -> pure Call
      4 -> pure ChannelNameChange
      5 -> pure ChannelIconChange
      6 -> pure ChannelPinnedMessage
      7 -> pure GuildMemberJoin
      8 -> pure UserPremiumGuildSubscription
      9 -> pure UserPremiumGuildSubscriptionTier1
      10 -> pure UserPremiumGuildSubscriptionTier2
      11 -> pure UserPremiumGuildSubscriptionTier3
      12 -> pure ChannelFollowAdd
      14 -> pure GuildDiscoveryDisqualified
      15 -> pure GuildDiscoveryRequalified
      19 -> pure Reply
      20 -> pure ApplicationCommmand
      _ -> fail $ "Invalid MessageType: " <> show n
    Nothing -> fail $ "Invalid MessageType: " <> show n

$(deriveTextShow ''MessageAuthorWebhook)
$(deriveTextShow 'PartialMessage)
$(deriveTextShow ''ChannelMention)
$(deriveTextShow ''MessageType)

$(makeFieldLabelsNoPrefix ''MessageAuthorWebhook)
$(makeFieldLabelsNoPrefix ''ChannelMention)
$(makeFieldLabelsNoPrefix 'PartialMessage)
$(makeFieldLabelsNoPrefix ''Message)
$(makeFieldLabelsNoPrefix ''MessageReference)
