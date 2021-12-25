-- | Guild roles
module Calamity.Types.Model.Guild.Role (Role (..)) where

import Calamity.Internal.AesonThings
import Calamity.Internal.IntColour
import Calamity.Types.Model.Guild.Permissions
import Calamity.Types.Snowflake

import Data.Aeson
import Data.Colour
import Data.Text (Text)

import GHC.Generics

import Calamity.Internal.OverriddenVia
import Control.DeepSeq (NFData (rnf), rwhnf)
import TextShow
import qualified TextShow.Generic as TSG

data Role' = Role'
  { id :: Snowflake Role
  , name :: Text
  , color :: IntColour
  , hoist :: Bool
  , position :: Int
  , permissions :: Permissions
  , managed :: Bool
  , mentionable :: Bool
  }
  deriving (Generic)
  deriving (TextShow) via TSG.FromGeneric Role'
  deriving (ToJSON, FromJSON) via CalamityJSON Role'

data Role = Role
  { id :: Snowflake Role
  , name :: Text
  , color :: Colour Double
  , hoist :: Bool
  , position :: Int
  , permissions :: Permissions
  , managed :: Bool
  , mentionable :: Bool
  }
  deriving (Eq, Show, Generic)
  deriving (TextShow, FromJSON, ToJSON) via OverriddenVia Role Role'
  deriving (HasID Role) via HasIDField "id" Role

instance NFData Role where
  rnf (Role id name color hoist position permissions managed mentionable) =
    rnf id `seq` rnf name `seq` rwhnf color `seq` rnf hoist `seq` rnf position
      `seq` rnf permissions
      `seq` rnf managed
      `seq` rnf mentionable
