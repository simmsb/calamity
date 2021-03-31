-- | A command handler
module CalamityCommands.Handler (CommandHandler (..)) where

import CalamityCommands.AliasType
import CalamityCommands.Command
import CalamityCommands.Group

import qualified Data.HashMap.Lazy as LH
import qualified Data.Text as S

import GHC.Generics

import TextShow
import qualified TextShow.Generic as TSG

data CommandHandler m c a = CommandHandler
    { -- | Top level groups
      groups :: LH.HashMap S.Text (Group m c a, AliasType)
    , -- | Top level commands
      commands :: LH.HashMap S.Text (Command m c a, AliasType)
    }
    deriving (Generic)

data CommandHandlerS m c a = CommandHandlerS
    { groups :: [(S.Text, (Group m c a, AliasType))]
    , commands :: [(S.Text, (Command m c a, AliasType))]
    }
    deriving (Show, Generic)
    deriving (TextShow) via TSG.FromGeneric (CommandHandlerS m c a)

instance Show (CommandHandler m c a) where
    showsPrec d CommandHandler{groups, commands} = showsPrec d $ CommandHandlerS (LH.toList groups) (LH.toList commands)

instance TextShow (CommandHandler m c a) where
    showbPrec d CommandHandler{groups, commands} = showbPrec d $ CommandHandlerS (LH.toList groups) (LH.toList commands)
