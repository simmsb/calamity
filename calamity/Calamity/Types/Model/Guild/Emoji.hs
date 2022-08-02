{-# LANGUAGE TemplateHaskell #-}

-- | Discord emojis
module Calamity.Types.Model.Guild.Emoji (
  Emoji (..),
  Partial (PartialEmoji),
  RawEmoji (..),
  emojiAsRawEmoji,
) where

import Calamity.Internal.Utils (
  AesonVector (unAesonVector),
  CalamityToJSON (..),
  CalamityToJSON' (..),
  (.=),
 )
import Calamity.Types.CDNAsset (CDNAsset (..))
import Calamity.Types.Model.Guild.Role
import Calamity.Types.Model.User
import Calamity.Types.Snowflake
import Calamity.Utils.CDNUrl (cdnURL)
import Data.Aeson ((.!=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Data.Vector.Unboxing (Vector)
import Network.HTTP.Req ((/:))
import Optics.TH
import TextShow (showt)
import qualified TextShow

data Emoji = Emoji
  { id :: Snowflake Emoji
  , name :: T.Text
  , roles :: Vector (Snowflake Role)
  , user :: Maybe User
  , requireColons :: Bool
  , managed :: Bool
  , animated :: Bool
  }
  deriving (Eq, Show)
  deriving (TextShow.TextShow) via TextShow.FromStringShow Emoji
  deriving (HasID Emoji) via HasIDField "id" Emoji

instance Aeson.FromJSON Emoji where
  parseJSON = Aeson.withObject "Emoji" $ \v ->
    Emoji
      <$> v .: "id"
      <*> v .: "name"
      <*> (fmap unAesonVector <$> v .:? "roles") .!= mempty
      <*> v .:? "user"
      <*> v .:? "require_colons" .!= False
      <*> v .:? "managed" .!= False
      <*> v .:? "animated" .!= False

instance CDNAsset Emoji where
  assetURL Emoji {id, animated} =
    cdnURL /: "emojis" /: (showt id <> if animated then ".gif" else ".png")

emojiAsRawEmoji :: Emoji -> RawEmoji
emojiAsRawEmoji Emoji {id, name, animated} = CustomEmoji $ PartialEmoji id name animated

data instance Partial Emoji = PartialEmoji
  { id :: Snowflake Emoji
  , name :: T.Text
  , animated :: Bool
  }
  deriving (Eq)
  deriving (HasID Emoji) via HasIDField "id" (Partial Emoji)
  deriving (Aeson.ToJSON) via CalamityToJSON (Partial Emoji)

instance Aeson.FromJSON (Partial Emoji) where
  parseJSON = Aeson.withObject "Partial Emoji" $ \v ->
    PartialEmoji
      <$> v .: "id"
      <*> v .: "name"
      <*> v .:? "animated" .!= False

instance CalamityToJSON' (Partial Emoji) where
  toPairs PartialEmoji {..} =
    [ "id" .= id
    , "name" .= name
    , "animated" .= animated
    ]

instance Show (Partial Emoji) where
  show PartialEmoji {id, name, animated} =
    "<" <> a <> ":" <> T.unpack name <> ":" <> show id <> ">"
    where
      a = if animated then "a" else ""

instance TextShow.TextShow (Partial Emoji) where
  showb PartialEmoji {id, name, animated} =
    "<" <> a <> ":" <> TextShow.fromText name <> ":" <> TextShow.showb id <> ">"
    where
      a = if animated then "a" else ""

data RawEmoji
  = UnicodeEmoji T.Text
  | CustomEmoji (Partial Emoji)
  deriving (Eq)

instance Show RawEmoji where
  show (UnicodeEmoji v) = T.unpack v
  show (CustomEmoji p) = show p

instance TextShow.TextShow RawEmoji where
  showb (UnicodeEmoji v) = TextShow.fromText v
  showb (CustomEmoji p) = TextShow.showb p

instance Aeson.ToJSON RawEmoji where
  toJSON (CustomEmoji e) = Aeson.toJSON e
  toJSON (UnicodeEmoji s) = Aeson.object ["name" Aeson..= s, "id" Aeson..= Nothing @()]
  toEncoding (CustomEmoji e) = Aeson.toEncoding e
  toEncoding (UnicodeEmoji s) = Aeson.pairs . mconcat $ ["name" Aeson..= s, "id" Aeson..= Nothing @()]

instance Aeson.FromJSON RawEmoji where
  parseJSON = Aeson.withObject "RawEmoji" $ \v -> do
    m_id :: Maybe (Snowflake Emoji) <- v .:? "id"
    anim <- v .:? "animated" .!= False
    name :: T.Text <- v .: "name"

    pure $ case m_id of
      Just id -> CustomEmoji $ PartialEmoji id name anim
      Nothing -> UnicodeEmoji name

$(makeFieldLabelsNoPrefix ''Emoji)
$(makeFieldLabelsNoPrefix 'PartialEmoji)
$(makeFieldLabelsNoPrefix ''RawEmoji)
