{-# LANGUAGE TemplateHaskell #-}

-- | Types of channels
module Calamity.Types.Model.Channel.ChannelType (ChannelType (..)) where

import qualified Data.Aeson as Aeson
import Data.Scientific
import Optics.TH
import TextShow.TH

-- Thanks sbrg (https://github.com/saevarb/haskord/blob/d1bb07bcc4f3dbc29f2dfd3351ff9f16fc100c07/haskord-lib/src/Haskord/Types/Common.hsfield#L182)
data ChannelType
  = GuildTextType
  | DMType
  | GuildVoiceType
  | GroupDMType
  | GuildCategoryType
  deriving (Eq, Show, Enum)

$(deriveTextShow ''ChannelType)
$(makeFieldLabelsNoPrefix ''ChannelType)

instance Aeson.ToJSON ChannelType where
  toJSON t = Aeson.toJSON (fromEnum t)
  toEncoding t = Aeson.toEncoding (fromEnum t)

instance Aeson.FromJSON ChannelType where
  parseJSON = Aeson.withScientific "ChannelType" $ \n -> case toBoundedInteger @Int n of
    Just v -> case v of
      0 -> pure GuildTextType
      1 -> pure DMType
      2 -> pure GuildVoiceType
      4 -> pure GuildCategoryType
      _ -> fail $ "Invalid ChannelType: " <> show n
    Nothing -> fail $ "Invalid ChannelType: " <> show n
