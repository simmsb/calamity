{-# LANGUAGE TemplateHaskell #-}

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

import Calamity.Internal.IntColour (IntColour (..))
import Calamity.Internal.Utils (CalamityToJSON (..), CalamityToJSON' (..), (.=), (.?=))
import Data.Aeson ((.!=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Colour (Colour)
import Data.Default.Class
import Data.Semigroup
import Data.Text (Text)
import Data.Time
import Data.Word
import Optics ((%~), (&), (^.))
import Optics.TH
import qualified TextShow
import TextShow.TH

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
  deriving (Eq, Show)
  deriving (TextShow.TextShow) via TextShow.FromStringShow Embed
  deriving (Aeson.ToJSON) via CalamityToJSON Embed

instance Default Embed where
  def =
    Embed
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      []

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

instance CalamityToJSON' Embed where
  toPairs Embed {..} =
    [ "title" .?= title
    , "type" .?= type_
    , "description" .?= description
    , "url" .?= url
    , "timestamp" .?= timestamp
    , "color" .?= (IntColour <$> color)
    , "footer" .?= footer
    , "image" .?= image
    , "thumbnail" .?= thumbnail
    , "video" .?= video
    , "provider" .?= provider
    , "author" .?= author
    , "fields" .= fields
    ]

instance Aeson.FromJSON Embed where
  parseJSON = Aeson.withObject "Embed" $ \v ->
    Embed
      <$> v .:? "title"
      <*> v .:? "type"
      <*> v .:? "description"
      <*> v .:? "url"
      <*> v .:? "timestamp"
      <*> (fmap fromIntColour <$> v .:? "color")
      <*> v .:? "footer"
      <*> v .:? "image"
      <*> v .:? "thumbnail"
      <*> v .:? "video"
      <*> v .:? "provider"
      <*> v .:? "author"
      <*> v .:? "fields" .!= []

data EmbedFooter = EmbedFooter
  { text :: Text
  , iconUrl :: Maybe Text
  , proxyIconUrl :: Maybe Text
  }
  deriving (Eq, Show)
  deriving (Aeson.ToJSON) via CalamityToJSON EmbedFooter

instance Semigroup EmbedFooter where
  l <> r =
    l
      & #text %~ (<> (r ^. #text))
      & #iconUrl %~ getLast . (<> Last (r ^. #iconUrl)) . Last
      & #proxyIconUrl %~ getLast . (<> Last (r ^. #proxyIconUrl)) . Last

instance CalamityToJSON' EmbedFooter where
  toPairs EmbedFooter {..} =
    [ "text" .= text
    , "icon_url" .?= iconUrl
    , "proxy_icon_url" .?= proxyIconUrl
    ]

instance Aeson.FromJSON EmbedFooter where
  parseJSON = Aeson.withObject "EmbedFooter" $ \v ->
    EmbedFooter
      <$> v .: "text"
      <*> v .:? "icon_url"
      <*> v .:? "proxy_icon_url"

{- | Create an embed footer with a provided content

 The remaining fields are set to Nothing
-}
embedFooter :: Text -> EmbedFooter
embedFooter text = EmbedFooter text Nothing Nothing

data EmbedImage = EmbedImage
  { url :: Text
  , proxyUrl :: Maybe Text
  , width :: Maybe Word64
  , height :: Maybe Word64
  }
  deriving (Eq, Show)
  deriving (Aeson.ToJSON) via CalamityToJSON EmbedImage

instance CalamityToJSON' EmbedImage where
  toPairs EmbedImage {..} =
    [ "url" .= url
    , "proxy_url" .= proxyUrl
    , "width" .= width
    , "height" .= height
    ]

instance Aeson.FromJSON EmbedImage where
  parseJSON = Aeson.withObject "EmbedImage" $ \v ->
    EmbedImage
      <$> v .: "url"
      <*> v .:? "proxy_url"
      <*> v .:? "width"
      <*> v .:? "height"

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
  deriving (Eq, Show)
  deriving (Aeson.ToJSON) via CalamityToJSON EmbedThumbnail

instance CalamityToJSON' EmbedThumbnail where
  toPairs EmbedThumbnail {..} =
    [ "url" .= url
    , "proxy_url" .?= proxyUrl
    , "width" .?= width
    , "height" .?= height
    ]

instance Aeson.FromJSON EmbedThumbnail where
  parseJSON = Aeson.withObject "EmbedThumbnail" $ \v ->
    EmbedThumbnail
      <$> v .: "url"
      <*> v .:? "proxy_url"
      <*> v .:? "width"
      <*> v .:? "height"

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
  deriving (Eq, Show)
  deriving (Aeson.ToJSON) via CalamityToJSON EmbedVideo

instance CalamityToJSON' EmbedVideo where
  toPairs EmbedVideo {..} =
    [ "url" .= url
    , "proxy_url" .= proxyUrl
    , "width" .= width
    , "height" .= height
    ]

instance Aeson.FromJSON EmbedVideo where
  parseJSON = Aeson.withObject "EmbedVideo" $ \v ->
    EmbedVideo
      <$> v .: "url"
      <*> v .:? "proxy_url"
      <*> v .:? "width"
      <*> v .:? "height"

data EmbedProvider = EmbedProvider
  { name :: Maybe Text
  , url :: Maybe Text
  }
  deriving (Eq, Show)
  deriving (Aeson.ToJSON) via CalamityToJSON EmbedProvider

instance CalamityToJSON' EmbedProvider where
  toPairs EmbedProvider {..} =
    [ "name" .= name
    , "url" .= url
    ]

instance Aeson.FromJSON EmbedProvider where
  parseJSON = Aeson.withObject "EmbedProvider" $ \v ->
    EmbedProvider
      <$> v .:? "name"
      <*> v .:? "url"

data EmbedAuthor = EmbedAuthor
  { name :: Maybe Text
  , url :: Maybe Text
  , iconUrl :: Maybe Text
  , proxyIconUrl :: Maybe Text
  }
  deriving (Eq, Show)
  deriving (Aeson.ToJSON) via CalamityToJSON EmbedAuthor

instance Default EmbedAuthor where
  def = EmbedAuthor Nothing Nothing Nothing Nothing

instance CalamityToJSON' EmbedAuthor where
  toPairs EmbedAuthor {..} =
    [ "name" .?= name
    , "url" .?= url
    , "icon_url" .?= iconUrl
    , "proxy_icon_url" .?= proxyIconUrl
    ]

instance Aeson.FromJSON EmbedAuthor where
  parseJSON = Aeson.withObject "EmbedAuthor" $ \v ->
    EmbedAuthor
      <$> v .:? "name"
      <*> v .:? "url"
      <*> v .:? "icon_url"
      <*> v .:? "proxy_icon_url"

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
  deriving (Eq, Show)
  deriving (Aeson.ToJSON) via CalamityToJSON EmbedField

instance CalamityToJSON' EmbedField where
  toPairs EmbedField {..} =
    [ "name" .= name
    , "value" .= value
    , "inline" .= inline
    ]

instance Aeson.FromJSON EmbedField where
  parseJSON = Aeson.withObject "EmbedField" $ \v ->
    EmbedField
      <$> v .: "name"
      <*> v .: "value"
      <*> v .:? "inline" .!= False

-- | Create a non-inline embed field
embedField ::
  -- | Name
  Text ->
  -- | Value
  Text ->
  EmbedField
embedField name value = EmbedField name value False

$(makeFieldLabelsNoPrefix ''Embed)

$(deriveTextShow ''EmbedFooter)
$(makeFieldLabelsNoPrefix ''EmbedFooter)

$(deriveTextShow ''EmbedImage)
$(makeFieldLabelsNoPrefix ''EmbedImage)

$(deriveTextShow ''EmbedThumbnail)
$(makeFieldLabelsNoPrefix ''EmbedThumbnail)

$(deriveTextShow ''EmbedVideo)
$(makeFieldLabelsNoPrefix ''EmbedVideo)

$(deriveTextShow ''EmbedProvider)
$(makeFieldLabelsNoPrefix ''EmbedProvider)

$(deriveTextShow ''EmbedAuthor)
$(makeFieldLabelsNoPrefix ''EmbedAuthor)

$(deriveTextShow ''EmbedField)
$(makeFieldLabelsNoPrefix ''EmbedField)
