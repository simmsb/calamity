{-# LANGUAGE TemplateHaskell #-}

-- | Guild ban objects
module Calamity.Types.Model.Guild.Ban (BanData (..)) where

import Calamity.Types.Model.Guild.Guild
import Calamity.Types.Model.User
import Calamity.Types.Snowflake
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import Optics.TH
import TextShow.TH

data BanData = BanData
  { guildID :: Snowflake Guild
  , user :: User
  }
  deriving (Show)
  deriving (HasID Guild) via HasIDField "guildID" BanData
  deriving (HasID User) via HasIDField "user" BanData

instance Aeson.FromJSON BanData where
  parseJSON = Aeson.withObject "BanData" $ \v ->
    BanData
      <$> v .: "guild_id"
      <*> v .: "user"

$(deriveTextShow ''BanData)
$(makeFieldLabelsNoPrefix ''BanData)
