{-# LANGUAGE TemplateHaskell #-}

-- | The generic channel type
module Calamity.Types.Model.Channel (
  Channel (..),
  Partial (PartialChannel),
  module Calamity.Types.Model.Channel.DM,
  module Calamity.Types.Model.Channel.Group,
  module Calamity.Types.Model.Channel.Guild,
  module Calamity.Types.Model.Channel.Attachment,
  module Calamity.Types.Model.Channel.Reaction,
  module Calamity.Types.Model.Channel.Webhook,
  module Calamity.Types.Model.Channel.Embed,
  module Calamity.Types.Model.Channel.ChannelType,
  module Calamity.Types.Model.Channel.Message,
  module Calamity.Types.Model.Channel.Component,
) where

import Calamity.Internal.Utils
import Calamity.Types.Model.Channel.Attachment
import Calamity.Types.Model.Channel.ChannelType
import Calamity.Types.Model.Channel.Component
import Calamity.Types.Model.Channel.DM
import Calamity.Types.Model.Channel.Embed
import Calamity.Types.Model.Channel.Group
import Calamity.Types.Model.Channel.Guild
import Calamity.Types.Model.Channel.Message
import Calamity.Types.Model.Channel.Reaction
import Calamity.Types.Model.Channel.Webhook
import Calamity.Types.Model.Guild.Permissions (Permissions)
import Calamity.Types.Snowflake
import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Optics.TH
import TextShow.TH

data Channel
  = DMChannel' DMChannel
  | GroupChannel' GroupChannel
  | GuildChannel' GuildChannel
  deriving (Show, Eq)

instance HasID Channel Channel where
  getID (DMChannel' a) = getID a
  getID (GroupChannel' a) = getID a
  getID (GuildChannel' a) = getID a

instance Aeson.FromJSON Channel where
  parseJSON = Aeson.withObject "Channel" $ \v -> do
    type_ <- v Aeson..: "type"

    case type_ of
      GuildTextType -> GuildChannel' <$> Aeson.parseJSON (Aeson.Object v)
      GuildVoiceType -> GuildChannel' <$> Aeson.parseJSON (Aeson.Object v)
      GuildCategoryType -> GuildChannel' <$> Aeson.parseJSON (Aeson.Object v)
      DMType -> DMChannel' <$> Aeson.parseJSON (Aeson.Object v)
      GroupDMType -> GroupChannel' <$> Aeson.parseJSON (Aeson.Object v)
      GuildNewsType -> GuildChannel' <$> Aeson.parseJSON (Aeson.Object v)
      GuildNewsThreadType -> GuildChannel' <$> Aeson.parseJSON (Aeson.Object v)
      GuildPublicThreadType -> GuildChannel' <$> Aeson.parseJSON (Aeson.Object v)
      GuildPrivateThreadType -> GuildChannel' <$> Aeson.parseJSON (Aeson.Object v)
      GuildStageVoiceType -> GuildChannel' <$> Aeson.parseJSON (Aeson.Object v)
      GuildDirectoryType -> GuildChannel' <$> Aeson.parseJSON (Aeson.Object v)
      GuildForumType -> GuildChannel' <$> Aeson.parseJSON (Aeson.Object v)

data instance Partial Channel = PartialChannel
  { id :: Snowflake Channel
  , name :: Text
  , type_ :: ChannelType
  , permissions :: Maybe Permissions
  , parentID :: Maybe (Snowflake Category)
  }
  deriving (Show, Eq)
  deriving (HasID Channel) via HasIDField "id" (Partial Channel)
  deriving (Aeson.ToJSON) via CalamityToJSON (Partial Channel)

instance CalamityToJSON' (Partial Channel) where
  toPairs PartialChannel {..} =
    [ "id" .= id
    , "name" .= name
    , "type" .= type_
    , "permissions" .?= permissions
    , "parent_id" .?= parentID
    ]

instance Aeson.FromJSON (Partial Channel) where
  parseJSON = Aeson.withObject "Partial Channel" $ \v ->
    PartialChannel
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "type"
      <*> v .:? "permissions"
      <*> v .:? "parent_id"

$(deriveTextShow ''Channel)
$(deriveTextShow 'PartialChannel)
$(makeFieldLabelsNoPrefix ''Channel)
$(makeFieldLabelsNoPrefix 'PartialChannel)
