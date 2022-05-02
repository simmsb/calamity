{-# LANGUAGE TemplateHaskell #-}

-- | User presences
module Calamity.Types.Model.Presence.Presence (
  Presence (..),
  ClientStatus (..),
) where

import Calamity.Internal.Utils
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import Calamity.Types.Model.Presence.Activity
import Calamity.Types.Model.User
import Calamity.Types.Snowflake
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Optics.TH
import TextShow.TH

data Presence = Presence
  { user :: Snowflake User
  , guildID :: Snowflake Guild
  , status :: StatusType
  , activities :: [Activity]
  , clientStatus :: ClientStatus
  }
  deriving (Eq, Show)
  deriving (HasID User) via HasIDField "user" Presence
  deriving (HasID Guild) via HasIDField "guildID" Presence

instance Aeson.FromJSON Presence where
  parseJSON = Aeson.withObject "Presence" $ \v -> do
    u :: Aeson.Object <- v .: "user"

    Presence
      <$> u .: "id"
      <*> v .: "guild_id"
      <*> v .: "status"
      <*> v .: "activities"
      <*> v .: "client_status"

data ClientStatus = ClientStatus
  { desktop :: Maybe Text
  , mobile :: Maybe Text
  , web :: Maybe Text
  }
  deriving (Eq, Show)
  deriving (Aeson.ToJSON) via CalamityToJSON ClientStatus

instance CalamityToJSON' ClientStatus where
  toPairs ClientStatus {..} =
    [ "desktop" .?= desktop
    , "mobile" .?= mobile
    , "web" .?= web
    ]

instance Aeson.FromJSON ClientStatus where
  parseJSON = Aeson.withObject "ClientStatus" $ \v ->
    ClientStatus
      <$> v .:? "desktop"
      <*> v .:? "mobile"
      <*> v .:? "web"

$(deriveTextShow ''Presence)
$(deriveTextShow ''ClientStatus)
$(makeFieldLabelsNoPrefix ''Presence)
$(makeFieldLabelsNoPrefix ''ClientStatus)
