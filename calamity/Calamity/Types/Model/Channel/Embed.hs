-- | Message embeds
module Calamity.Types.Model.Channel.Embed (
    Embed (..),
    embedFooter,
    embedImage,
    embedThumbnail,
    embedAuthor,
    embedAuthor',
    embedField,
    EmbedFooter (..),
    EmbedImage (..),
    EmbedThumbnail (..),
    EmbedVideo (..),
    EmbedProvider (..),
    EmbedAuthor (..),
    EmbedField (..),
) where

import Calamity.Internal.AesonThings
import Calamity.Internal.IntColour ()
import Calamity.Internal.Utils ()

import Control.Lens

import Data.Aeson
import Data.Colour (Colour)
import Data.Default.Class
import Data.Generics.Labels ()
import Data.Semigroup
import Data.Text.Lazy (Text)
import Data.Time
import Data.Word

import GHC.Generics

import TextShow
import qualified TextShow.Generic as TSG

data Embed = Embed
    { title :: Maybe Text
    , type_ :: Maybe Text
    , description :: Maybe Text
    , url :: Maybe Text
    , timestamp :: Maybe UTCTime
    , color :: Maybe (Colour Double)
    , footer :: Maybe EmbedFooter
    , image :: Maybe EmbedImage
    , thumbnail :: Maybe EmbedThumbnail
    , video :: Maybe EmbedVideo
    , provider :: Maybe EmbedProvider
    , author :: Maybe EmbedAuthor
    , fields :: [EmbedField]
    }
    deriving (Eq, Show, Generic, Default)
    deriving (TextShow) via TSG.FromGeneric Embed
    deriving (FromJSON) via WithSpecialCases '[IfNoneThen "fields" DefaultToEmptyArray] Embed
    deriving (ToJSON) via CalamityJSON Embed

instance Semigroup Embed where
    l <> r =
        l
            & #title %~ (<> (r ^. #title))
            & #type_ %~ getLast . (<> Last (r ^. #type_)) . Last
            & #description %~ (<> (r ^. #description))
            & #url %~ getLast . (<> Last (r ^. #url)) . Last
            & #timestamp %~ getLast . (<> Last (r ^. #timestamp)) . Last
            & #color %~ getLast . (<> Last (r ^. #color)) . Last
            & #footer %~ (<> (r ^. #footer))
            & #image %~ getLast . (<> Last (r ^. #image)) . Last
            & #thumbnail %~ getLast . (<> Last (r ^. #thumbnail)) . Last
            & #video %~ getLast . (<> Last (r ^. #video)) . Last
            & #provider %~ getLast . (<> Last (r ^. #provider)) . Last
            & #author %~ getLast . (<> Last (r ^. #author)) . Last
            & #fields %~ (<> (r ^. #fields))

instance Monoid Embed where
    mempty = def

data EmbedFooter = EmbedFooter
    { text :: Text
    , iconUrl :: Maybe Text
    , proxyIconUrl :: Maybe Text
    }
    deriving (Eq, Show, Generic)
    deriving (TextShow) via TSG.FromGeneric EmbedFooter
    deriving (ToJSON, FromJSON) via CalamityJSON EmbedFooter

{- | Create an embed footer with a provided content

 The remaining fields are set to Nothing
-}
embedFooter :: Text -> EmbedFooter
embedFooter text = EmbedFooter text Nothing Nothing

instance Semigroup EmbedFooter where
    l <> r =
        l
            & #text %~ (<> (r ^. #text))
            & #iconUrl %~ getLast . (<> Last (r ^. #iconUrl)) . Last
            & #proxyIconUrl %~ getLast . (<> Last (r ^. #proxyIconUrl)) . Last

data EmbedImage = EmbedImage
    { url :: Text
    , proxyUrl :: Maybe Text
    , width :: Maybe Word64
    , height :: Maybe Word64
    }
    deriving (Eq, Show, Generic)
    deriving (TextShow) via TSG.FromGeneric EmbedImage
    deriving (FromJSON, ToJSON) via CalamityJSON EmbedImage

{- | Create an embed image with a provided url

 The remaining fields are set to Nothing
-}
embedImage :: Text -> EmbedImage
embedImage url = EmbedImage url Nothing Nothing Nothing

data EmbedThumbnail = EmbedThumbnail
    { url :: Text
    , proxyUrl :: Maybe Text
    , width :: Maybe Word64
    , height :: Maybe Word64
    }
    deriving (Eq, Show, Generic)
    deriving (TextShow) via TSG.FromGeneric EmbedThumbnail
    deriving (FromJSON, ToJSON) via CalamityJSON EmbedThumbnail

{- | Create an embed thumbnail with a provided url

 The remaining fields are set to Nothing
-}
embedThumbnail :: Text -> EmbedThumbnail
embedThumbnail url = EmbedThumbnail url Nothing Nothing Nothing

data EmbedVideo = EmbedVideo
    { url :: Maybe Text
    , proxyUrl :: Maybe Text
    , width :: Maybe Word64
    , height :: Maybe Word64
    }
    deriving (Eq, Show, Generic)
    deriving (TextShow) via TSG.FromGeneric EmbedVideo
    deriving (FromJSON, ToJSON) via CalamityJSON EmbedVideo

data EmbedProvider = EmbedProvider
    { name :: Maybe Text
    , url :: Maybe Text
    }
    deriving (Eq, Show, Generic)
    deriving (TextShow) via TSG.FromGeneric EmbedProvider
    deriving (ToJSON, FromJSON) via CalamityJSON EmbedProvider

data EmbedAuthor = EmbedAuthor
    { name :: Maybe Text
    , url :: Maybe Text
    , iconUrl :: Maybe Text
    , proxyIconURL :: Maybe Text
    }
    deriving (Eq, Show, Generic, Default)
    deriving (TextShow) via TSG.FromGeneric EmbedAuthor
    deriving (ToJSON, FromJSON) via CalamityJSON EmbedAuthor

{- | Create an embed author with the given name

 The remaining fields are set to Nothing
-}
embedAuthor :: Text -> EmbedAuthor
embedAuthor name = EmbedAuthor (Just name) Nothing Nothing Nothing

{- | Create an embed author with the given name, url, and icon url

 The remaining fields are set to Nothing
-}
embedAuthor' ::
    -- | Name
    Text ->
    -- | Url
    Text ->
    -- | Icon url
    Text ->
    EmbedAuthor
embedAuthor' name url iconUrl = EmbedAuthor (Just name) (Just url) (Just iconUrl) Nothing

data EmbedField = EmbedField
    { name :: Text
    , value :: Text
    , inline :: Bool
    }
    deriving (Eq, Show, Generic)
    deriving (TextShow) via TSG.FromGeneric EmbedField
    deriving
        (FromJSON)
        via WithSpecialCases
                '[IfNoneThen "inline" DefaultToFalse]
                EmbedField
    deriving (ToJSON) via CalamityJSON EmbedField

-- | Create a non-inline embed field
embedField ::
    -- | Name
    Text ->
    -- | Value
    Text ->
    EmbedField
embedField name value = EmbedField name value False
