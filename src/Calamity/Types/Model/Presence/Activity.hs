-- | User activities
module Calamity.Types.Model.Presence.Activity
    ( Activity(..)
    , ActivityType(..)
    , ActivityTimestamps(..)
    , ActivityParty(..)
    , ActivityAssets(..)
    , ActivitySecrets(..) ) where

import           Calamity.Internal.AesonThings
import           Calamity.Internal.Utils
import           Calamity.Types.Snowflake
import           Calamity.Types.UnixTimestamp

import           Data.Aeson
import           Data.Scientific
import           Data.Text.Lazy                ( Text )
import           Data.Word

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic              as TSG

data ActivityType
  = Game
  | Streaming
  | Listening
  deriving ( Eq, Generic, Show, Enum )
  deriving ( TextShow ) via TSG.FromGeneric ActivityType

instance ToJSON ActivityType where
  toJSON t = Number $ fromIntegral (fromEnum t)

instance FromJSON ActivityType where
  parseJSON = withScientific "ActivityType" $ \n -> case toBoundedInteger n of
    Just v  -> pure $ toEnum v
    Nothing -> fail $ "Invalid ActivityType: " <> show n

data Activity = Activity
  { name          :: Text
  , type_         :: ActivityType
  , url           :: Maybe Text
  , timestamps    :: Maybe ActivityTimestamps
  , applicationID :: Maybe (Snowflake ())
  , details       :: Maybe Text
  , state         :: Maybe Text
  , party         :: Maybe ActivityParty
  , assets        :: Maybe ActivityAssets
  , secrets       :: Maybe ActivitySecrets
  , instance_     :: Maybe Bool
  , flags         :: Maybe Word64
  }
  deriving ( Eq, Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric Activity
  deriving ( ToJSON, FromJSON ) via CalamityJSON Activity

data ActivityTimestamps = ActivityTimestamps
  { start :: Maybe UnixTimestamp
  , end   :: Maybe UnixTimestamp
  }
  deriving ( Eq, Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric ActivityTimestamps

instance ToJSON ActivityTimestamps where
  toEncoding ActivityTimestamps { start, end } = pairs
    ("start" .= (unixToMilliseconds <$> start) <> "end" .= (unixToMilliseconds <$> end))

instance FromJSON ActivityTimestamps where
  parseJSON = withObject "ActivityTimestamps" $ \v -> do
    start <- millisecondsToUnix <<$>> v .:? "start"
    end <- millisecondsToUnix <<$>> v .:? "end"

    pure $ ActivityTimestamps start end

data ActivityParty = ActivityParty
  { id   :: Maybe Text
  , size :: Maybe (Int, Int)
  }
  deriving ( Eq, Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric ActivityParty
  deriving ( ToJSON, FromJSON ) via CalamityJSON ActivityParty

data ActivityAssets = ActivityAssets
  { largeImage :: Maybe Text
  , largeText  :: Maybe Text
  , smallImage :: Maybe Text
  , smallText  :: Maybe Text
  }
  deriving ( Eq, Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric ActivityAssets
  deriving ( ToJSON, FromJSON ) via CalamityJSON ActivityAssets

data ActivitySecrets = ActivitySecrets
  { join     :: Maybe Text
  , spectate :: Maybe Text
  , match    :: Maybe Text
  }
  deriving ( Eq, Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric ActivitySecrets
  deriving ( ToJSON, FromJSON ) via CalamityJSON ActivitySecrets
