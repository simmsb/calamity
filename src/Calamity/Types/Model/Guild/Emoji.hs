-- | Discord emojis
module Calamity.Types.Model.Guild.Emoji
    ( Emoji(..)
    , Partial(PartialEmoji)
    , RawEmoji(..) ) where

import           Calamity.Internal.AesonThings
import           Calamity.Internal.Utils         ()
import           Calamity.Types.Model.Guild.Role
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Data.Aeson
import           Data.Text.Lazy                  ( Text )
import           Data.Vector.Unboxed             ( Vector )

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic                as TSG

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

data instance Partial Emoji = PartialEmoji
  { id   :: Snowflake Emoji
  , name :: Text
  }
  deriving ( Eq, Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric (Partial Emoji)
  deriving ( ToJSON, FromJSON ) via CalamityJSON (Partial Emoji)
  deriving ( HasID Emoji ) via HasIDField "id" (Partial Emoji)

data RawEmoji
  = UnicodeEmoji Text
  | CustomEmoji (Partial Emoji)
  deriving ( Eq, Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric RawEmoji

instance ToJSON RawEmoji where
  toJSON (CustomEmoji e) = object ["emoji" .= e]
  toJSON (UnicodeEmoji s) = object ["emoji" .= object ["name" .= s]]

instance FromJSON RawEmoji where
  parseJSON = withObject "RawEmoji" $ \v -> do
    m_id :: Maybe (Snowflake Emoji) <- v .:? "id"
    name :: Text <- v .: "name"

    pure $ case m_id of
      Just id -> CustomEmoji $ PartialEmoji id name
      Nothing -> UnicodeEmoji name
