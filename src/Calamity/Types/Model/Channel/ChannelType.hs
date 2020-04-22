-- | Types of channels
module Calamity.Types.Model.Channel.ChannelType
    ( ChannelType(..) ) where

import           Data.Aeson
import           Data.Scientific

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic as TSG

-- Thanks sbrg (https://github.com/saevarb/haskord/blob/d1bb07bcc4f3dbc29f2dfd3351ff9f16fc100c07/haskord-lib/src/Haskord/Types/Common.hsfield#L182)
data ChannelType
  = GuildTextType
  | DMType
  | GuildVoiceType
  | GroupDMType
  | GuildCategoryType
  deriving ( Eq, Generic, Show, Enum )
  deriving ( TextShow ) via TSG.FromGeneric ChannelType

instance ToJSON ChannelType where
  toJSON t = Number $ fromIntegral (fromEnum t)

instance FromJSON ChannelType where
  parseJSON = withScientific "ChannelType" $ \n -> case toBoundedInteger n of
    Just v  -> pure $ toEnum v
    Nothing -> fail $ "Invalid ChannelType: " <> show n
