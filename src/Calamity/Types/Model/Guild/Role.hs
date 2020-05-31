-- | Guild roles
module Calamity.Types.Model.Guild.Role
    ( Role(..) ) where

import           Calamity.Internal.AesonThings
import           Calamity.Internal.IntColour            ()
import           Calamity.Internal.Utils                ()
import           Calamity.Types.Model.Guild.Permissions
import           Calamity.Types.Snowflake

import           Data.Aeson
import           Data.Colour
import           Data.Text.Lazy                         ( Text )

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic                       as TSG

data Role = Role
  { id          :: Snowflake Role
  , name        :: Text
  , color       :: Colour Double
  , hoist       :: Bool
  , position    :: Int
  , permissions :: Permissions
  , managed     :: Bool
  , mentionable :: Bool
  }
  deriving ( Eq, Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric Role
  deriving ( ToJSON, FromJSON ) via CalamityJSON Role
  deriving ( HasID Role ) via HasIDField "id" Role
