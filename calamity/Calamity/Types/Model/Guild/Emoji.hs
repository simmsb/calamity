-- | Discord emojis
module Calamity.Types.Model.Guild.Emoji (
  Emoji (..),
  Partial (PartialEmoji),
  RawEmoji (..),
  emojiAsRawEmoji,
) where

import Calamity.Internal.AesonThings
import Calamity.Internal.OverriddenVia
import Calamity.Internal.Utils
import Calamity.Types.Model.Guild.Role
import Calamity.Types.Model.User
import Calamity.Types.Snowflake
import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Generics.Product
import qualified Data.Text as T
import Data.Vector.Unboxing (Vector)
import GHC.Generics
import TextShow
import qualified TextShow.Generic as TSG

data Emoji' = Emoji'
  { id :: Snowflake Emoji
  , name :: T.Text
  , roles :: AesonVector (Snowflake Role)
  , user :: Maybe (Snowflake User)
  , requireColons :: Bool
  , managed :: Bool
  , animated :: Bool
  }
  deriving (Generic)
  deriving (TextShow) via TSG.FromGeneric Emoji'
  deriving (ToJSON) via CalamityJSON Emoji'
  deriving
    (FromJSON)
    via WithSpecialCases
          '[ "user" `ExtractFieldFrom` "id"
           , "roles" `IfNoneThen` DefaultToEmptyArray
           ]
          Emoji'

data Emoji = Emoji
  { id :: Snowflake Emoji
  , name :: T.Text
  , roles :: Vector (Snowflake Role)
  , user :: Maybe (Snowflake User)
  , requireColons :: Bool
  , managed :: Bool
  , animated :: Bool
  }
  deriving (Eq, Show, Generic, NFData)
  deriving (TextShow, ToJSON, FromJSON) via OverriddenVia Emoji Emoji'
  deriving (HasID Emoji) via HasIDField "id" Emoji

emojiAsRawEmoji :: Emoji -> RawEmoji
emojiAsRawEmoji = CustomEmoji . upcast

data instance Partial Emoji = PartialEmoji
  { id :: Snowflake Emoji
  , name :: T.Text
  , animated :: Bool
  }
  deriving (Eq, Generic)
  deriving (ToJSON) via CalamityJSON (Partial Emoji)
  deriving
    (FromJSON)
    via WithSpecialCases
          '["animated" `IfNoneThen` DefaultToFalse]
          (Partial Emoji)
  deriving (HasID Emoji) via HasIDField "id" (Partial Emoji)

instance Show (Partial Emoji) where
  show PartialEmoji{id, name, animated} =
    "<" <> a <> ":" <> T.unpack name <> ":" <> show id <> ">"
   where
    a = if animated then "a" else ""

instance TextShow (Partial Emoji) where
  showb PartialEmoji{id, name, animated} =
    "<" <> a <> ":" <> fromText name <> ":" <> showb id <> ">"
   where
    a = if animated then "a" else ""

data RawEmoji
  = UnicodeEmoji T.Text
  | CustomEmoji (Partial Emoji)
  deriving (Eq, Generic)

instance Show RawEmoji where
  show (UnicodeEmoji v) = T.unpack v
  show (CustomEmoji p) = show p

instance TextShow RawEmoji where
  showb (UnicodeEmoji v) = fromText v
  showb (CustomEmoji p) = showb p

instance ToJSON RawEmoji where
  toJSON (CustomEmoji e) = toJSON e
  toJSON (UnicodeEmoji s) = object ["name" .= s, "id" .= Nothing @()]

instance FromJSON RawEmoji where
  parseJSON = withObject "RawEmoji" $ \v -> do
    m_id :: Maybe (Snowflake Emoji) <- v .:? "id"
    anim <- v .:? "animated" .!= False
    name :: T.Text <- v .: "name"

    pure $ case m_id of
      Just id -> CustomEmoji $ PartialEmoji id name anim
      Nothing -> UnicodeEmoji name
