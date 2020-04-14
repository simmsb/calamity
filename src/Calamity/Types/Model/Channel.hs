-- | The generic channel type
module Calamity.Types.Model.Channel
    ( Channel(..)
    , Partial(PartialChannel)
    , ChannelType
    , module Calamity.Types.Model.Channel.DM
    , module Calamity.Types.Model.Channel.Group
    , module Calamity.Types.Model.Channel.Guild
    , module Calamity.Types.Model.Channel.Attachment
    , module Calamity.Types.Model.Channel.Reaction
    , module Calamity.Types.Model.Channel.Embed
    , module Calamity.Types.Model.Channel.ChannelType
    , module Calamity.Types.Model.Channel.Message ) where

import           Calamity.Internal.AesonThings
import           Calamity.Types.Model.Channel.Attachment
import           Calamity.Types.Model.Channel.ChannelType
import           Calamity.Types.Model.Channel.DM
import           Calamity.Types.Model.Channel.Embed
import           Calamity.Types.Model.Channel.Group
import           Calamity.Types.Model.Channel.Guild
import {-# SOURCE #-} Calamity.Types.Model.Channel.Message hiding ( UpdatedMessage )
import           Calamity.Types.Model.Channel.Reaction
import           Calamity.Types.Partial
import           Calamity.Types.Snowflake

import           Control.Monad

import           Data.Aeson
import           Data.Generics.Product.Fields

data Channel
  = DMChannel' DMChannel
  | GroupChannel' GroupChannel
  | GuildChannel' GuildChannel
  deriving ( Show, Eq, Generic )

instance HasID Channel where
  getID (DMChannel' a) = coerceSnowflake $ a ^. field' @"id"
  getID (GroupChannel' a) = coerceSnowflake $ a ^. field' @"id"
  getID (GuildChannel' a) = coerceSnowflake $ getID a

instance FromJSON Channel where
  parseJSON = withObject "Channel" $ \v -> do
    type_ <- v .: "type"

    case type_ of
      GuildTextType     -> GuildChannel' <$> parseJSON (Object v)
      GuildVoiceType    -> GuildChannel' <$> parseJSON (Object v)
      GuildCategoryType -> GuildChannel' <$> parseJSON (Object v)
      DMType            -> DMChannel' <$> parseJSON (Object v)
      GroupDMType       -> GroupChannel' <$> parseJSON (Object v)

data instance Partial Channel = PartialChannel
  { id    :: Snowflake Channel
  , name  :: ShortText
  , type_ :: ChannelType
  }
  deriving ( Show, Eq, Generic )
  deriving ( ToJSON, FromJSON ) via CalamityJSON (Partial Channel)
  deriving ( HasID ) via HasIDFieldAlt (Partial Channel) Channel

