-- | Message embeds
module Calamity.Types.Model.Channel.Embed
    ( Embed(..)
    , EmbedFooter(..)
    , EmbedImage(..)
    , EmbedThumbnail(..)
    , EmbedVideo(..)
    , EmbedProvider(..)
    , EmbedAuthor(..)
    , EmbedField(..) ) where

import           Calamity.Internal.AesonThings

import           Data.Aeson
import           Data.Time

data Embed = Embed
  { title       :: Maybe ShortText
  , type_       :: Maybe ShortText
  , description :: Maybe ShortText
  , url         :: Maybe ShortText
  , timestamp   :: Maybe UTCTime
  , color       :: Maybe Word64
  , footer      :: Maybe EmbedFooter
  , image       :: Maybe EmbedImage
  , thumbnail   :: Maybe EmbedThumbnail
  , video       :: Maybe EmbedVideo
  , provider    :: Maybe EmbedProvider
  , author      :: Maybe EmbedAuthor
  , fields      :: [EmbedField]
  }
  deriving ( Eq, Show, Generic )
  deriving ( FromJSON ) via WithSpecialCases '[IfNoneThen "fields" DefaultToEmptyArray]
      Embed
  deriving ( ToJSON ) via CalamityJSON Embed

data EmbedFooter = EmbedFooter
  { text         :: ShortText
  , iconUrl      :: Maybe ShortText
  , proxyIconUrl :: Maybe ShortText
  }
  deriving ( Eq, Show, Generic )
  deriving ( ToJSON, FromJSON ) via CalamityJSON EmbedFooter

data EmbedImage = EmbedImage
  { url      :: ShortText
  , proxyUrl :: ShortText
  , width    :: Word64
  , height   :: Word64
  }
  deriving ( Eq, Show, Generic )
  deriving ( FromJSON, ToJSON ) via CalamityJSON EmbedImage

data EmbedThumbnail = EmbedThumbnail
  { url      :: ShortText
  , proxyUrl :: ShortText
  , width    :: Word64
  , height   :: Word64
  }
  deriving ( Eq, Show, Generic )
  deriving ( FromJSON, ToJSON ) via CalamityJSON EmbedThumbnail

data EmbedVideo = EmbedVideo
  { url    :: ShortText
  , width  :: Word64
  , height :: Word64
  }
  deriving ( Eq, Show, Generic )
  deriving ( FromJSON, ToJSON ) via CalamityJSON EmbedVideo

data EmbedProvider = EmbedProvider
  { name :: ShortText
  , url  :: Maybe ShortText
  }
  deriving ( Eq, Show, Generic )
  deriving ( ToJSON, FromJSON ) via CalamityJSON EmbedProvider

data EmbedAuthor = EmbedAuthor
  { name         :: Maybe ShortText
  , url          :: Maybe ShortText
  , iconUrl      :: Maybe ShortText
  , proxyIconURL :: Maybe ShortText
  }
  deriving ( Eq, Show, Generic )
  deriving ( ToJSON, FromJSON ) via CalamityJSON EmbedAuthor

data EmbedField = EmbedField
  { name   :: !ShortText
  , value  :: !ShortText
  , inline :: !Bool
  }
  deriving ( Eq, Show, Generic )
  deriving ( FromJSON ) via WithSpecialCases '[IfNoneThen "inline" DefaultToFalse]
      EmbedField
  deriving ( ToJSON ) via CalamityJSON EmbedField
