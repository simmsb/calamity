-- | Permission overwrites
module Calamity.Types.Model.Guild.Overwrite
    ( Overwrite(..) ) where

import           Calamity.Internal.AesonThings
import           Calamity.Types.Snowflake

import           Data.Aeson
import           Data.Text.Lazy                ( Text )
import           Data.Word

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic              as TSG

data Overwrite = Overwrite
  { id    :: Snowflake Overwrite
  , type_ :: Text
  , allow :: Word64
  , deny  :: Word64
  }
  deriving ( Eq, Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric Overwrite
  deriving ( ToJSON, FromJSON ) via CalamityJSON Overwrite
  deriving ( HasID Overwrite ) via HasIDField "id" Overwrite
