-- | Guild roles
module Calamity.Types.Model.Guild.Role
    ( Role(..) ) where

import           Calamity.Internal.AesonThings
import           Calamity.Types.Snowflake

import           Data.Aeson
import           Data.Text.Lazy                ( Text )
import           Data.Word

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic              as TSG

data Role = Role
  { id          :: Snowflake Role
  , name        :: Text
  , color       :: Word64
  , hoist       :: Bool
  , position    :: Int
  , permissions :: Word64
  , managed     :: Bool
  , mentionable :: Bool
  }
  deriving ( Eq, Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric Role
  deriving ( ToJSON, FromJSON ) via CalamityJSON Role
  deriving ( HasID Role ) via HasIDField "id" Role
