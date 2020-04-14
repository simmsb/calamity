-- | User activities
module Calamity.Types.Model.Presence.Activity
    ( Activity(..)
    , ActivityType(..)
    , ActivityTimestamps(..)
    , ActivityParty(..)
    , ActivityAssets(..)
    , ActivitySecrets(..) ) where

import           Calamity.Internal.AesonThings
import           Calamity.Types.Model.Channel.Guild
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import           Calamity.Types.Model.User
import           Calamity.Types.Partial
import           Calamity.Types.Snowflake
import           Calamity.Types.UnixTimestamp

import           Control.Monad

import           Data.Aeson
import           Data.Scientific

data ActivityType
  = Game
  | Streaming
  | Listening
  deriving ( Eq, Generic, Show, Enum )

instance ToJSON ActivityType where
  toJSON t = Number $ fromIntegral (fromEnum t)

instance FromJSON ActivityType where
  parseJSON = withScientific "ActivityType" $ \n -> case toBoundedInteger n of
    Just v  -> pure $ toEnum v
    Nothing -> fail $ "Invalid ActivityType: " <> show n

data Activity = Activity
  { name          :: ShortText
  , type_         :: ActivityType
  , url           :: Maybe ShortText
  , timestamps    :: Maybe ActivityTimestamps
  , applicationID :: Maybe (Snowflake ())
  , details       :: Maybe ShortText
  , state         :: Maybe ShortText
  , party         :: Maybe ActivityParty
  , assets        :: Maybe ActivityAssets
  , secrets       :: Maybe ActivitySecrets
  , instance_     :: Maybe Bool
  , flags         :: Maybe Word64
  }
  deriving ( Eq, Show, Generic )
  deriving ( ToJSON, FromJSON ) via CalamityJSON Activity

data ActivityTimestamps = ActivityTimestamps
  { start :: Maybe UnixTimestamp
  , end   :: Maybe UnixTimestamp
  }
  deriving ( Eq, Show, Generic )

instance ToJSON ActivityTimestamps where
  toEncoding ActivityTimestamps { start, end } = pairs
    ("start" .= (unixToMilliseconds <$> start) <> "end" .= (unixToMilliseconds <$> end))

instance FromJSON ActivityTimestamps where
  parseJSON = withObject "ActivityTimestamps" $ \v -> do
    start <- millisecondsToUnix <<$>> v .:? "start"
    end <- millisecondsToUnix <<$>> v .:? "end"

    pure $ ActivityTimestamps start end

data ActivityParty = ActivityParty
  { id   :: Maybe ShortText
  , size :: Maybe (Int, Int)
  }
  deriving ( Eq, Show, Generic )
  deriving ( ToJSON, FromJSON ) via CalamityJSON ActivityParty

data ActivityAssets = ActivityAssets
  { largeImage :: Maybe ShortText
  , largeText  :: Maybe ShortText
  , smallImage :: Maybe ShortText
  , smallText  :: Maybe ShortText
  }
  deriving ( Eq, Show, Generic )
  deriving ( ToJSON, FromJSON ) via CalamityJSON ActivityAssets

data ActivitySecrets = ActivitySecrets
  { join     :: Maybe ShortText
  , spectate :: Maybe ShortText
  , match    :: Maybe ShortText
  }
  deriving ( Eq, Show, Generic )
  deriving ( ToJSON, FromJSON ) via CalamityJSON ActivitySecrets
