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

import           Control.Lens

import           Data.Aeson
import           Data.Default.Class
import           Data.Generics.Labels          ()
import           Data.Semigroup
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
  deriving ( Eq, Show, Generic, Default )
  deriving ( TextShow ) via TSG.FromGeneric Embed
  deriving ( FromJSON ) via WithSpecialCases '[IfNoneThen "fields" DefaultToEmptyArray]
      Embed
  deriving ( ToJSON ) via CalamityJSON Embed

instance Semigroup Embed where
  l <> r = l
    & #title       %~ (<> (r ^. #title))
    & #type_       %~ getLast . (<> Last (r ^. #type_)) . Last
    & #description %~ (<> (r ^. #description))
    & #url         %~ getLast . (<> Last (r ^. #url)) . Last
    & #timestamp   %~ getLast . (<> Last (r ^. #timestamp)) . Last
    & #color       %~ getLast . (<> Last (r ^. #color)) . Last
    & #footer      %~ (<> (r ^. #footer))
    & #image       %~ getLast . (<> Last (r ^. #image)) . Last
    & #thumbnail   %~ getLast . (<> Last (r ^. #thumbnail)) . Last
    & #video       %~ getLast . (<> Last (r ^. #video)) . Last
    & #provider    %~ getLast . (<> Last (r ^. #provider)) . Last
    & #author      %~ getLast . (<> Last (r ^. #author)) . Last
    & #fields      %~ (<> (r ^. #fields))

instance Monoid Embed where
  mempty = def

data EmbedFooter = EmbedFooter
  { text         :: Text
  , iconUrl      :: Maybe Text
  , proxyIconUrl :: Maybe Text
  }
  deriving ( Eq, Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric EmbedFooter
  deriving ( ToJSON, FromJSON ) via CalamityJSON EmbedFooter

instance Semigroup EmbedFooter where
  l <> r = l
    & #text         %~ (<> (r ^. #text))
    & #iconUrl      %~ getLast . (<> Last (r ^. #iconUrl)) . Last
    & #proxyIconUrl %~ getLast . (<> Last (r ^. #proxyIconUrl)) . Last

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
