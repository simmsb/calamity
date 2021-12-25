-- | User activities
module Calamity.Types.Model.Presence.Activity (
  Activity (..),
  activity,
  ActivityType (..),
  ActivityTimestamps (..),
  ActivityParty (..),
  ActivityAssets (..),
  ActivitySecrets (..),
) where

import Calamity.Internal.AesonThings
import Calamity.Internal.OverriddenVia
import Calamity.Internal.UnixTimestamp
import Calamity.Types.Snowflake
import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Scientific
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word
import GHC.Generics
import TextShow
import qualified TextShow.Generic as TSG

data ActivityType
  = Game
  | Streaming
  | Listening
  | Custom
  | Other Int
  deriving (Eq, Generic, Show, NFData)
  deriving (TextShow) via TSG.FromGeneric ActivityType

instance ToJSON ActivityType where
  toJSON Game = Number 0
  toJSON Streaming = Number 1
  toJSON Listening = Number 2
  toJSON (Other n) = Number $ fromIntegral n
  toJSON Custom = Number 4

instance FromJSON ActivityType where
  parseJSON = withScientific "ActivityType" $ \n -> case toBoundedInteger @Int n of
    Just v -> case v of
      0 -> pure Game
      1 -> pure Streaming
      2 -> pure Listening
      4 -> pure Custom
      n -> pure $ Other n
    Nothing -> fail $ "Invalid ActivityType: " <> show n

data Activity = Activity
  { name :: Text
  , type_ :: !ActivityType
  , url :: Maybe Text
  , timestamps :: Maybe ActivityTimestamps
  , applicationID :: Maybe (Snowflake ())
  , details :: Maybe Text
  , state :: Maybe Text
  , party :: Maybe ActivityParty
  , assets :: Maybe ActivityAssets
  , secrets :: Maybe ActivitySecrets
  , instance_ :: Maybe Bool
  , flags :: Maybe Word64
  }
  deriving (Eq, Show, Generic, NFData)
  deriving (TextShow) via TSG.FromGeneric Activity
  deriving (ToJSON, FromJSON) via CalamityJSON Activity

-- | Make an 'Activity' with all optional fields set to Nothing
activity :: Text -> ActivityType -> Activity
activity !name !type_ =
  Activity
    { name = name
    , type_ = type_
    , url = Nothing
    , timestamps = Nothing
    , applicationID = Nothing
    , details = Nothing
    , state = Nothing
    , party = Nothing
    , assets = Nothing
    , secrets = Nothing
    , instance_ = Nothing
    , flags = Nothing
    }

data ActivityTimestamps' = ActivityTimestamps'
  { start :: !(Maybe UnixTimestamp)
  , end :: !(Maybe UnixTimestamp)
  }
  deriving (Generic, NFData)
  deriving (ToJSON, FromJSON) via CalamityJSON ActivityTimestamps'
  deriving (TextShow) via TSG.FromGeneric ActivityTimestamps'

data ActivityTimestamps = ActivityTimestamps
  { start :: !(Maybe UTCTime)
  , end :: !(Maybe UTCTime)
  }
  deriving (Eq, Show, Generic, NFData)
  deriving (TextShow, ToJSON, FromJSON) via OverriddenVia ActivityTimestamps ActivityTimestamps'

data ActivityParty = ActivityParty
  { id :: Maybe Text
  , size :: Maybe (Int, Int)
  }
  deriving (Eq, Show, Generic, NFData)
  deriving (TextShow) via TSG.FromGeneric ActivityParty
  deriving (ToJSON, FromJSON) via CalamityJSON ActivityParty

data ActivityAssets = ActivityAssets
  { largeImage :: Maybe Text
  , largeText :: Maybe Text
  , smallImage :: Maybe Text
  , smallText :: Maybe Text
  }
  deriving (Eq, Show, Generic, NFData)
  deriving (TextShow) via TSG.FromGeneric ActivityAssets
  deriving (ToJSON, FromJSON) via CalamityJSON ActivityAssets

data ActivitySecrets = ActivitySecrets
  { join :: Maybe Text
  , spectate :: Maybe Text
  , match :: Maybe Text
  }
  deriving (Eq, Show, Generic, NFData)
  deriving (TextShow) via TSG.FromGeneric ActivitySecrets
  deriving (ToJSON, FromJSON) via CalamityJSON ActivitySecrets
