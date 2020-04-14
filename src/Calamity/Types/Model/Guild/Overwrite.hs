-- | Permission overwrites
module Calamity.Types.Model.Guild.Overwrite
    ( Overwrite(..) ) where

import           Calamity.Internal.AesonThings
import           Calamity.Types.Snowflake

import           Data.Aeson

data Overwrite = Overwrite
  { id    :: Snowflake Overwrite
  , type_ :: ShortText
  , allow :: Word64
  , deny  :: Word64
  }
  deriving ( Eq, Show, Generic )
  deriving ( ToJSON, FromJSON ) via CalamityJSON Overwrite
  deriving ( HasID ) via HasIDField Overwrite
