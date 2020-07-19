-- | Discord emojis
module Calamity.Types.Model.Guild.Emoji
    ( Emoji(..)
    , Partial(PartialEmoji)
    , RawEmoji(..)
    , emojiAsRawEmoji ) where

import           Calamity.Internal.AesonThings
import           Calamity.Internal.Utils         ()
import           Calamity.Types.Model.Guild.Role
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Data.Aeson
import qualified Data.Text.Lazy                  as L
import           Data.Text.Lazy                  ( Text )
import           Data.Vector.Unboxing            ( Vector )

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic                as TSG
import Data.Generics.Product

data Emoji = Emoji
  { id            :: Snowflake Emoji
  , name          :: Text
  , roles         :: Vector (Snowflake Role)
  , user          :: Maybe (Snowflake User)
  , requireColons :: Bool
  , managed       :: Bool
  , animated      :: Bool
  }
  deriving ( Eq, Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric Emoji
  deriving ( ToJSON ) via CalamityJSON Emoji
  deriving ( FromJSON ) via WithSpecialCases '["user" `ExtractFieldFrom` "id"] Emoji
  deriving ( HasID Emoji ) via HasIDField "id" Emoji

emojiAsRawEmoji :: Emoji -> RawEmoji
emojiAsRawEmoji = CustomEmoji . upcast

data instance Partial Emoji = PartialEmoji
  { id   :: Snowflake Emoji
  , name :: Text
  , animated :: Bool
  }
  deriving ( Eq, Generic )
  deriving ( ToJSON ) via CalamityJSON (Partial Emoji)
  deriving ( FromJSON ) via WithSpecialCases
      '["animated" `IfNoneThen` DefaultToFalse] (Partial Emoji)
  deriving ( HasID Emoji ) via HasIDField "id" (Partial Emoji)

instance Show (Partial Emoji) where
  show PartialEmoji { id, name, animated } =
    "<" <> a <> ":" <> L.unpack name <> ":" <> show id <> ">"
    where a = if animated then "a" else ""

instance TextShow (Partial Emoji) where
  showb PartialEmoji { id, name, animated } =
    "<" <> a <> ":" <> fromLazyText name <> ":" <> showb id <> ">"
    where a = if animated then "a" else ""

data RawEmoji
  = UnicodeEmoji Text
  | CustomEmoji (Partial Emoji)
  deriving ( Eq, Generic )

instance Show RawEmoji where
  show (UnicodeEmoji v) = L.unpack v
  show (CustomEmoji p) = show p

instance TextShow RawEmoji where
  showb (UnicodeEmoji v) = fromLazyText v
  showb (CustomEmoji p) = showb p

instance ToJSON RawEmoji where
  toJSON (CustomEmoji e) = object ["emoji" .= e]
  toJSON (UnicodeEmoji s) = object ["emoji" .= object ["name" .= s]]

instance FromJSON RawEmoji where
  parseJSON = withObject "RawEmoji" $ \v -> do
    m_id :: Maybe (Snowflake Emoji) <- v .:? "id"
    anim <- v .:? "animated" .!= False
    name :: Text <- v .: "name"

    pure $ case m_id of
      Just id -> CustomEmoji $ PartialEmoji id name anim
      Nothing -> UnicodeEmoji name
