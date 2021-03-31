-- | Commands and stuff
module CalamityCommands.Command
    ( Command
    ) where

import TextShow
import Data.Kind (Type)

type role Command representational representational nominal
data Command (m :: Type -> Type) (c :: Type) (a :: Type)

instance Show (Command m c a)
instance TextShow (Command m c a)
