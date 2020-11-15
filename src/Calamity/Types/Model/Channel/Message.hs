-- | A message from a channel
module Calamity.Types.Model.Channel.Message
    ( Message(..)
    , MessageType(..) ) where

import           Calamity.Internal.AesonThings
import           Calamity.Internal.Utils          ()
import           Calamity.Types.Model.Channel
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import           Calamity.Types.Model.Guild.Role
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Data.Aeson
import           Data.Scientific
import           Data.Text.Lazy                   ( Text )
import           Data.Time
import qualified Data.Vector.Unboxing             as UV

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic                 as TSG

-- NOTE: make sure we fill in the guildID field when retrieving from REST
data Message = Message
  { id              :: Snowflake Message
  , channelID       :: Snowflake Channel
  , guildID         :: Maybe (Snowflake Guild)
  , author          :: Snowflake User
  , content         :: Text
  , timestamp       :: UTCTime
  , editedTimestamp :: Maybe UTCTime
  , tts             :: Bool
  , mentionEveryone :: Bool
  , mentions        :: UV.Vector (Snowflake User)
  , mentionRoles    :: UV.Vector (Snowflake Role)
  , mentionChannels :: Maybe (UV.Vector (Snowflake Channel))
  , attachments     :: [Attachment]
  , embeds          :: [Embed]
  , reactions       :: [Reaction]
  , nonce           :: Maybe (Snowflake Message)
  , pinned          :: Bool
  , webhookID       :: Maybe (Snowflake ())
  , type_           :: MessageType
  }
  deriving ( Eq, Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric Message
  deriving ( FromJSON ) via WithSpecialCases
      '["author" `ExtractFieldFrom` "id", "mentions" `ExtractArrayField` "id",
        "mention_channels" `ExtractArrayField` "id",
        "reactions" `IfNoneThen` DefaultToEmptyArray]
      Message
  deriving ( HasID Message ) via HasIDField "id" Message
  deriving ( HasID Channel ) via HasIDField "channelID" Message
  deriving ( HasID User ) via HasIDField "author" Message

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
  deriving ( Eq, Show, Enum, Generic )
  deriving ( TextShow ) via TSG.FromGeneric MessageType

instance FromJSON MessageType where
  parseJSON = withScientific "MessageType" $ \n -> case toBoundedInteger @Int n of
    Just v  -> case v of
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
      13 -> pure GuildDiscoveryDisqualified
      14 -> pure GuildDiscoveryRequalified
      _ -> fail $ "Invalid MessageType: " <> show n
    Nothing -> fail $ "Invalid MessageType: " <> show n
