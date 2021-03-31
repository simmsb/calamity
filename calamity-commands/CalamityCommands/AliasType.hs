-- | Named boolean for determining if something is an alias or not
module CalamityCommands.AliasType
  ( AliasType (..),
  )
where

import GHC.Generics (Generic)
import TextShow (TextShow)
import qualified TextShow.Generic as TSG

data AliasType
  = Alias
  | Original
  deriving (Eq, Enum, Show, Generic)
  deriving (TextShow) via TSG.FromGeneric AliasType
