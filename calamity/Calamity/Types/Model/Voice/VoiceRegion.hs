{-# LANGUAGE TemplateHaskell #-}

-- | Voice regions
module Calamity.Types.Model.Voice.VoiceRegion (VoiceRegion (..)) where

import Calamity.Internal.Utils
import Calamity.Types.Snowflake
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Optics.TH
import TextShow.TH

data VoiceRegion = VoiceRegion
  { id :: Snowflake VoiceRegion
  , name :: Text
  , vip :: Bool
  , optimal :: Bool
  , deprecated :: Bool
  , custom :: Bool
  }
  deriving (Show, Eq)
  deriving (Aeson.ToJSON) via CalamityToJSON VoiceRegion

instance CalamityToJSON' VoiceRegion where
  toPairs VoiceRegion {..} =
    [ "id" .= id
    , "name" .= name
    , "vip" .= vip
    , "optimal" .= optimal
    , "deprecated" .= deprecated
    , "custom" .= custom
    ]

instance Aeson.FromJSON VoiceRegion where
  parseJSON = Aeson.withObject "VoiceRegion" $ \v ->
    VoiceRegion
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "vip"
      <*> v .: "optimal"
      <*> v .: "deprecated"
      <*> v .: "custom"

$(deriveTextShow ''VoiceRegion)
$(makeFieldLabelsNoPrefix ''VoiceRegion)
