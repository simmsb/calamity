-- | Guild roles
module Calamity.Types.Model.Guild.Role (
  Role,
  RoleIcon,
) where

import Data.Aeson

data RoleIcon

data Role

instance Show Role
instance Eq Role

instance FromJSON Role
