{-# LANGUAGE TemplateHaskell #-}

-- | Guilds that are unavailable
module Calamity.Types.Model.Guild.UnavailableGuild (UnavailableGuild (..)) where

import Calamity.Internal.Utils
import Calamity.Types.Model.Guild.Guild
import Calamity.Types.Snowflake
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Optics.TH
import TextShow.TH

data UnavailableGuild = UnavailableGuild
  { id :: Snowflake Guild
  , unavailable :: Bool
  }
  deriving (Eq, Show)
  deriving (HasID Guild) via HasIDField "id" UnavailableGuild
  deriving (Aeson.ToJSON) via CalamityToJSON UnavailableGuild

instance CalamityToJSON' UnavailableGuild where
  toPairs UnavailableGuild {..} =
    [ "id" .= id
    , "unavailable" .= unavailable
    ]

instance Aeson.FromJSON UnavailableGuild where
  parseJSON = Aeson.withObject "UnavailableGuild" $ \v ->
    UnavailableGuild
      <$> v .: "id"
      <*> v .:? "disabled" .!= False

$(deriveTextShow ''UnavailableGuild)
$(makeFieldLabelsNoPrefix ''UnavailableGuild)
