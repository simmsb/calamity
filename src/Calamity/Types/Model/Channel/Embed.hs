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
import           Calamity.Internal.Utils       ()

import           Data.Aeson
import           Data.Text.Lazy                ( Text )
import           Data.Time
import           Data.Word

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic              as TSG

data Embed = Embed
  { title       :: Maybe Text
  , type_       :: Maybe Text
  , description :: Maybe Text
  , url         :: Maybe Text
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
  deriving ( TextShow ) via TSG.FromGeneric Embed
  deriving ( FromJSON ) via WithSpecialCases '[IfNoneThen "fields" DefaultToEmptyArray]
      Embed
  deriving ( ToJSON ) via CalamityJSON Embed

data EmbedFooter = EmbedFooter
  { text         :: Text
  , iconUrl      :: Maybe Text
  , proxyIconUrl :: Maybe Text
  }
  deriving ( Eq, Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric EmbedFooter
  deriving ( ToJSON, FromJSON ) via CalamityJSON EmbedFooter

data EmbedImage = EmbedImage
  { url      :: Text
  , proxyUrl :: Text
  , width    :: Word64
  , height   :: Word64
  }
  deriving ( Eq, Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric EmbedImage
  deriving ( FromJSON, ToJSON ) via CalamityJSON EmbedImage

data EmbedThumbnail = EmbedThumbnail
  { url      :: Text
  , proxyUrl :: Text
  , width    :: Word64
  , height   :: Word64
  }
  deriving ( Eq, Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric EmbedThumbnail
  deriving ( FromJSON, ToJSON ) via CalamityJSON EmbedThumbnail

data EmbedVideo = EmbedVideo
  { url    :: Text
  , width  :: Word64
  , height :: Word64
  }
  deriving ( Eq, Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric EmbedVideo
  deriving ( FromJSON, ToJSON ) via CalamityJSON EmbedVideo

data EmbedProvider = EmbedProvider
  { name :: Text
  , url  :: Maybe Text
  }
  deriving ( Eq, Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric EmbedProvider
  deriving ( ToJSON, FromJSON ) via CalamityJSON EmbedProvider

data EmbedAuthor = EmbedAuthor
  { name         :: Maybe Text
  , url          :: Maybe Text
  , iconUrl      :: Maybe Text
  , proxyIconURL :: Maybe Text
  }
  deriving ( Eq, Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric EmbedAuthor
  deriving ( ToJSON, FromJSON ) via CalamityJSON EmbedAuthor

data EmbedField = EmbedField
  { name   :: Text
  , value  :: Text
  , inline :: Bool
  }
  deriving ( Eq, Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric EmbedField
  deriving ( FromJSON ) via WithSpecialCases '[IfNoneThen "inline" DefaultToFalse]
      EmbedField
  deriving ( ToJSON ) via CalamityJSON EmbedField
