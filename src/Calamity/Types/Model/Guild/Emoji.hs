-- | Discord emojis
module Calamity.Types.Model.Guild.Emoji
    ( Emoji(..)
    , Partial(PartialEmoji)
    , RawEmoji(..) ) where

import           Calamity.Internal.AesonThings
import           Calamity.Types.Model.Guild.Role
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Data.Aeson
import           Data.Vector.Unboxed             ( Vector )

data Emoji = Emoji
  { id            :: Snowflake Emoji
  , name          :: ShortText
  , roles         :: Vector (Snowflake Role)
  , user          :: Maybe (Snowflake User)
  , requireColons :: Bool
  , managed       :: Bool
  , animated      :: Bool
  }
  deriving ( Eq, Show, Generic )
  deriving ( ToJSON ) via CalamityJSON Emoji
  deriving ( FromJSON ) via WithSpecialCases '["user" `ExtractField` "id"] Emoji
  deriving ( HasID Emoji ) via HasIDField "id" Emoji

data instance Partial Emoji = PartialEmoji
  { id   :: Snowflake Emoji
  , name :: ShortText
  }
  deriving ( Eq, Show, Generic )
  deriving ( ToJSON, FromJSON ) via CalamityJSON (Partial Emoji)
  deriving ( HasID Emoji ) via HasIDField "id" (Partial Emoji)

data RawEmoji
  = UnicodeEmoji ShortText
  | CustomEmoji (Partial Emoji)
  deriving ( Eq, Show, Generic )

instance ToJSON RawEmoji where
  toEncoding (CustomEmoji e) = pairs $ "emoji" .= e
  toEncoding (UnicodeEmoji s) = pairs $ "emoji" .= (("name" .= s) :: Object)

instance FromJSON RawEmoji where
  parseJSON = withObject "RawEmoji" $ \v -> do
    m_id :: Maybe (Snowflake Emoji) <- v .:? "id"
    name :: ShortText <- v .: "name"

    pure $ case m_id of
      Just id -> CustomEmoji $ PartialEmoji id name
      Nothing -> UnicodeEmoji name
