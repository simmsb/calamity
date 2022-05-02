-- | Named boolean for determining if something is an alias or not
module CalamityCommands.AliasType (
  AliasType (..),
) where

import qualified TextShow

data AliasType
  = Alias
  | Original
  deriving (Eq, Enum, Show)
  deriving (TextShow.TextShow) via TextShow.FromStringShow AliasType
