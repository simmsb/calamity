{-# LANGUAGE TemplateHaskell #-}

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

import Calamity.Internal.UnixTimestamp
import Calamity.Internal.Utils
import Calamity.Types.Snowflake
import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Scientific
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word
import Optics.TH
import TextShow qualified
import TextShow.TH

data ActivityType
  = Game
  | Streaming
  | Listening
  | Custom
  | Other Int
  deriving (Eq, Show)

instance Aeson.ToJSON ActivityType where
  toJSON Game = Aeson.Number 0
  toJSON Streaming = Aeson.Number 1
  toJSON Listening = Aeson.Number 2
  toJSON Custom = Aeson.Number 4
  toJSON (Other n) = Aeson.Number $ fromIntegral n

instance Aeson.FromJSON ActivityType where
  parseJSON = Aeson.withScientific "ActivityType" $ \n -> case toBoundedInteger @Int n of
    Just v -> case v of
      0 -> pure Game
      1 -> pure Streaming
      2 -> pure Listening
      4 -> pure Custom
      n -> pure $ Other n
    Nothing -> fail $ "Invalid ActivityType: " <> show n

data Activity = Activity
  { name :: Text
  , type_ :: ActivityType
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
  deriving (Eq, Show)
  deriving (Aeson.ToJSON) via CalamityToJSON Activity

instance CalamityToJSON' Activity where
  toPairs Activity {..} =
    [ "name" .= name
    , "type" .= type_
    , "url" .?= url
    , "timestamps" .?= timestamps
    , "application_id" .?= applicationID
    , "details" .?= details
    , "state" .?= state
    , "party" .?= party
    , "assets" .?= assets
    , "secrets" .?= secrets
    , "instance" .?= instance_
    , "flags" .?= flags
    ]

instance Aeson.FromJSON Activity where
  parseJSON = Aeson.withObject "Activity" $ \v ->
    Activity
      <$> v .: "name"
      <*> v .: "type"
      <*> v .:? "url"
      <*> v .:? "timestamps"
      <*> v .:? "application_id"
      <*> v .:? "details"
      <*> v .:? "state"
      <*> v .:? "party"
      <*> v .:? "assets"
      <*> v .:? "secrets"
      <*> v .:? "instance"
      <*> v .:? "flags"

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

data ActivityTimestamps = ActivityTimestamps
  { start :: Maybe UTCTime
  , end :: Maybe UTCTime
  }
  deriving (Eq, Show)
  deriving (TextShow.TextShow) via TextShow.FromStringShow ActivityTimestamps
  deriving (Aeson.ToJSON) via CalamityToJSON ActivityTimestamps

instance CalamityToJSON' ActivityTimestamps where
  toPairs ActivityTimestamps {..} =
    [ "start" .?= fmap UnixTimestamp start
    , "end" .?= fmap UnixTimestamp end
    ]

instance Aeson.FromJSON ActivityTimestamps where
  parseJSON = Aeson.withObject "ActivityTimestamps" $ \v ->
    ActivityTimestamps
      <$> (fmap unUnixTimestamp <$> v .:? "start")
      <*> (fmap unUnixTimestamp <$> v .:? "end")

data ActivityParty = ActivityParty
  { id :: Maybe Text
  , size :: Maybe (Int, Int)
  }
  deriving (Eq, Show)
  deriving (Aeson.ToJSON) via CalamityToJSON ActivityParty

instance CalamityToJSON' ActivityParty where
  toPairs ActivityParty {..} =
    [ "id" .?= id
    , "size" .?= size
    ]

instance Aeson.FromJSON ActivityParty where
  parseJSON = Aeson.withObject "ActivityParty" $ \v ->
    ActivityParty
      <$> v .:? "id"
      <*> v .:? "size"

data ActivityAssets = ActivityAssets
  { largeImage :: Maybe Text
  , largeText :: Maybe Text
  , smallImage :: Maybe Text
  , smallText :: Maybe Text
  }
  deriving (Eq, Show)
  deriving (Aeson.ToJSON) via CalamityToJSON ActivityAssets

instance CalamityToJSON' ActivityAssets where
  toPairs ActivityAssets {..} =
    [ "large_image" .?= largeImage
    , "large_text" .?= largeText
    , "small_image" .?= smallImage
    , "small_text" .?= smallText
    ]

instance Aeson.FromJSON ActivityAssets where
  parseJSON = Aeson.withObject "ActivityAssets" $ \v ->
    ActivityAssets
      <$> v .:? "large_image"
      <*> v .:? "large_text"
      <*> v .:? "small_image"
      <*> v .:? "small_text"

data ActivitySecrets = ActivitySecrets
  { join :: Maybe Text
  , spectate :: Maybe Text
  , match :: Maybe Text
  }
  deriving (Eq, Show)
  deriving (Aeson.ToJSON) via CalamityToJSON ActivitySecrets

instance CalamityToJSON' ActivitySecrets where
  toPairs ActivitySecrets {..} =
    [ "join" .?= join
    , "spectate" .?= spectate
    , "match" .?= match
    ]

instance Aeson.FromJSON ActivitySecrets where
  parseJSON = Aeson.withObject "ActivitySecrets" $ \v ->
    ActivitySecrets
      <$> v .:? "join"
      <*> v .:? "spectate"
      <*> v .:? "match"

$(deriveTextShow ''ActivityType)
$(deriveTextShow ''ActivityParty)
$(deriveTextShow ''ActivityAssets)
$(deriveTextShow ''ActivitySecrets)
$(deriveTextShow ''Activity)
$(makeFieldLabelsNoPrefix ''Activity)
$(makeFieldLabelsNoPrefix ''ActivityTimestamps)
$(makeFieldLabelsNoPrefix ''ActivityParty)
$(makeFieldLabelsNoPrefix ''ActivityAssets)
$(makeFieldLabelsNoPrefix ''ActivitySecrets)
