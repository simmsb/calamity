{-# LANGUAGE TemplateHaskell #-}

-- | Permission overwrites
module Calamity.Types.Model.Guild.Overwrite (Overwrite (..)) where

import Calamity.Internal.Utils
import Calamity.Types.Model.Guild.Permissions
import Calamity.Types.Snowflake
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import Optics.TH
import TextShow.TH

data Overwrite = Overwrite
  { id :: Snowflake Overwrite
  , type_ :: Int
  , allow :: Permissions
  , deny :: Permissions
  }
  deriving (Eq, Show)
  deriving (HasID Overwrite) via HasIDField "id" Overwrite
  deriving (Aeson.ToJSON) via CalamityToJSON Overwrite

instance CalamityToJSON' Overwrite where
  toPairs Overwrite {..} =
    [ "id" .= id
    , "type" .= type_
    , "allow" .= allow
    , "deny" .= deny
    ]

instance Aeson.FromJSON Overwrite where
  parseJSON = Aeson.withObject "Overwrite" $ \v ->
    Overwrite
      <$> v .: "id"
      <*> v .: "type"
      <*> v .: "allow"
      <*> v .: "deny"

$(deriveTextShow ''Overwrite)
$(makeFieldLabelsNoPrefix ''Overwrite)
