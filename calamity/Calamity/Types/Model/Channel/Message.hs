-- | A message from a channel
module Calamity.Types.Model.Channel.Message (
  Message (..),
  MessageType (..),
  MessageReference (..),
  Partial (PartialMessage),
) where

import Calamity.Internal.AesonThings
import Calamity.Internal.OverriddenVia
import Calamity.Internal.Utils
import {-# SOURCE #-} Calamity.Types.Model.Channel
import Calamity.Types.Model.Channel.Attachment
import Calamity.Types.Model.Channel.Component
import Calamity.Types.Model.Channel.Embed
import Calamity.Types.Model.Channel.Reaction
import Calamity.Types.Model.Channel.Webhook
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import Calamity.Types.Model.Guild.Role
import Calamity.Types.Model.User
import Calamity.Types.Snowflake
import Data.Aeson
import Data.Scientific
import Data.Text (Text)
import Data.Time
import qualified Data.Vector.Unboxing as UV
import Data.Word (Word64)
import GHC.Generics
import TextShow
import qualified TextShow.Generic as TSG

data Message' = Message'
  { id :: Snowflake Message
  , channelID :: Snowflake Channel
  , guildID :: Maybe (Snowflake Guild)
  , author :: Snowflake User
  , content :: Text
  , timestamp :: CalamityFromStringShow UTCTime
  , editedTimestamp :: Maybe (CalamityFromStringShow UTCTime)
  , tts :: Bool
  , mentionEveryone :: Bool
  , mentions :: AesonVector (Snowflake User)
  , mentionRoles :: AesonVector (Snowflake Role)
  , mentionChannels :: Maybe (AesonVector (Snowflake Channel))
  , attachments :: ![Attachment]
  , embeds :: ![Embed]
  , reactions :: ![Reaction]
  , nonce :: Maybe (CalamityFromStringShow Value)
  , pinned :: Bool
  , webhookID :: Maybe (Snowflake Webhook)
  , type_ :: !MessageType
  , activity :: Maybe (CalamityFromStringShow Object)
  , application :: Maybe (CalamityFromStringShow Object)
  , messageReference :: Maybe MessageReference
  , flags :: Word64
  , referencedMessage :: Maybe Message
  , interaction :: Maybe (CalamityFromStringShow Object)
  , components :: [Component]
  }
  deriving (Generic)
  deriving (TextShow) via TSG.FromGeneric Message'
  deriving
    (FromJSON)
    via WithSpecialCases
          '[ "author" `ExtractFieldFrom` "id"
           , "mentions" `ExtractArrayField` "id"
           , "mention_channels" `ExtractArrayField` "id"
           , "reactions" `IfNoneThen` DefaultToEmptyArray
           , "components" `IfNoneThen` DefaultToEmptyArray
           ]
          Message'

data Message = Message
  { id :: Snowflake Message
  , channelID :: Snowflake Channel
  , guildID :: Maybe (Snowflake Guild)
  , author :: Snowflake User
  , content :: Text
  , timestamp :: UTCTime
  , editedTimestamp :: Maybe UTCTime
  , tts :: Bool
  , mentionEveryone :: Bool
  , mentions :: UV.Vector (Snowflake User)
  , mentionRoles :: UV.Vector (Snowflake Role)
  , mentionChannels :: Maybe (UV.Vector (Snowflake Channel))
  , attachments :: ![Attachment]
  , embeds :: ![Embed]
  , reactions :: ![Reaction]
  , nonce :: Maybe Value
  , pinned :: Bool
  , webhookID :: Maybe (Snowflake Webhook)
  , type_ :: !MessageType
  , activity :: Maybe Object
  , application :: Maybe Object
  , messageReference :: Maybe MessageReference
  , flags :: Word64
  , referencedMessage :: Maybe Message
  , interaction :: Maybe Object
  , components :: [Component]
  }
  deriving (Show, Generic)
  deriving (TextShow, FromJSON) via OverriddenVia Message Message'
  deriving (HasID Message) via HasIDField "id" Message
  deriving (HasID Channel) via HasIDField "channelID" Message
  deriving (HasID User) via HasIDField "author" Message

data instance Partial Message = PartialMessage
  { channelID :: Snowflake Channel
  , guildID :: Maybe (Snowflake Guild)
  }
  deriving (Show, Generic)
  deriving (TextShow) via TSG.FromGeneric (Partial Message)
  deriving (FromJSON) via CalamityJSON (Partial Message)
  deriving (HasID Channel) via HasIDField "channelID" (Partial Message)

data MessageReference = MessageReference
  { messageID :: Maybe (Snowflake Message)
  , channelID :: Maybe (Snowflake Channel)
  , guildID :: Maybe (Snowflake Guild)
  , failIfNotExists :: Bool
  }
  deriving (Eq, Show, Generic)
  deriving (TextShow) via TSG.FromGeneric MessageReference
  deriving
    (FromJSON)
    via WithSpecialCases
          '[ "fail_if_not_exists" `IfNoneThen` DefaultToTrue
           ]
          MessageReference
  deriving (ToJSON) via CalamityJSON MessageReference

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
  deriving (Eq, Show, Enum, Generic)
  deriving (TextShow) via TSG.FromGeneric MessageType

instance FromJSON MessageType where
  parseJSON = withScientific "MessageType" $ \n -> case toBoundedInteger @Int n of
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
