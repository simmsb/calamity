-- | The generic channel type
module Calamity.Types.Model.Channel
    ( Channel(..)
    , Partial(PartialChannel)
    , module Calamity.Types.Model.Channel.DM
    , module Calamity.Types.Model.Channel.Group
    , module Calamity.Types.Model.Channel.Guild
    , module Calamity.Types.Model.Channel.Attachment
    , module Calamity.Types.Model.Channel.Reaction
    , module Calamity.Types.Model.Channel.Webhook
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
import {-# SOURCE #-} Calamity.Types.Model.Channel.Message
import           Calamity.Types.Model.Channel.Reaction
import           Calamity.Types.Model.Channel.Webhook
import           Calamity.Types.Partial
import           Calamity.Types.Snowflake

import           Data.Aeson
import           Data.Text.Lazy                           ( Text )

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic                         as TSG

data Channel
  = DMChannel' DMChannel
  | GroupChannel' GroupChannel
  | GuildChannel' GuildChannel
  deriving ( Show, Eq, Generic )
  deriving ( TextShow ) via TSG.FromGeneric Channel

instance HasID Channel Channel where
  getID (DMChannel' a) = getID a
  getID (GroupChannel' a) = getID a
  getID (GuildChannel' a) = getID a

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
  { id       :: Snowflake Channel
  , name     :: Text
  , type_    :: !ChannelType
  , parentID :: Maybe (Snowflake Category)
  }
  deriving ( Show, Eq, Generic )
  deriving ( TextShow ) via TSG.FromGeneric (Partial Channel)
  deriving ( ToJSON, FromJSON ) via CalamityJSON (Partial Channel)
  deriving ( HasID Channel ) via HasIDField "id" (Partial Channel)
