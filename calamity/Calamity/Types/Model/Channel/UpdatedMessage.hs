{-# LANGUAGE TemplateHaskell #-}

-- | Updated messages
module Calamity.Types.Model.Channel.UpdatedMessage (
  UpdatedMessage (..),
) where

import Calamity.Internal.Utils
import Calamity.Types.Model.Channel
import Calamity.Types.Model.Guild.Role
import Calamity.Types.Model.User
import Calamity.Types.Snowflake
import Data.Aeson ((.:), (.:!), (.:?))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Time
import Data.Vector.Unboxing qualified as UV
import Data.Word
import Optics.TH
import TextShow qualified

data UpdatedMessage = UpdatedMessage
  { id :: Snowflake Message
  , channelID :: Snowflake Channel
  , content :: Maybe Text
  , editedTimestamp :: Maybe (MaybeNull UTCTime)
  , tts :: Maybe Bool
  , mentionEveryone :: Maybe Bool
  , mentions :: Maybe [User]
  , mentionRoles :: Maybe (UV.Vector (Snowflake Role))
  , mentionChannels :: Maybe [ChannelMention]
  , attachments :: Maybe [Attachment]
  , embeds :: Maybe [Embed]
  , reactions :: Maybe [Reaction]
  , pinned :: Maybe Bool
  , type_ :: Maybe MessageType
  , activity :: Maybe (MaybeNull Aeson.Object)
  , application :: Maybe (MaybeNull Aeson.Object)
  , messageReference :: Maybe (MaybeNull MessageReference)
  , flags :: Maybe Word64
  , referencedMessage :: Maybe (MaybeNull Message)
  , interaction :: Maybe (MaybeNull Aeson.Object)
  , components :: Maybe [Component]
  }
  deriving (Show)
  deriving (TextShow.TextShow) via TextShow.FromStringShow UpdatedMessage
  deriving (HasID Message) via HasIDField "id" UpdatedMessage
  deriving (HasID Channel) via HasIDField "channelID" UpdatedMessage

instance Aeson.FromJSON UpdatedMessage where
  parseJSON = Aeson.withObject "UpdatedMessage" $ \v ->
    UpdatedMessage
      <$> v .: "id"
      <*> v .: "channel_id"
      <*> v .:? "content"
      <*> v .:! "edited_timestamp"
      <*> v .:? "tts"
      <*> v .:? "mention_everyone"
      <*> v .:? "mentions"
      <*> (fmap unAesonVector <$> v .:? "mention_roles")
      <*> v .:? "mention_channels"
      <*> v .:? "attachments"
      <*> v .:? "embeds"
      <*> v .:? "reactions"
      <*> v .:? "pinned"
      <*> v .:? "type"
      <*> v .:! "activity"
      <*> v .:! "application"
      <*> v .:! "message_reference"
      <*> v .:? "flags"
      <*> v .:! "referenced_message"
      <*> v .:! "interaction"
      <*> v .:? "components"

$(makeFieldLabelsNoPrefix ''UpdatedMessage)
