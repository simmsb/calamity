{-# LANGUAGE TemplateHaskell #-}

-- | A command handler
module CalamityCommands.Handler (CommandHandler (..)) where

import CalamityCommands.AliasType
import CalamityCommands.Command
import CalamityCommands.Group
import Data.HashMap.Lazy qualified as LH
import Data.Text qualified as S
import Optics.TH
import TextShow qualified
import TextShow.TH (deriveTextShow)

data CommandHandler m c a = CommandHandler
  { groups :: LH.HashMap S.Text (Group m c a, AliasType)
  -- ^ Top level groups
  , commands :: LH.HashMap S.Text (Command m c a, AliasType)
  -- ^ Top level commands
  }

data CommandHandlerS m c a = CommandHandlerS
  { groups :: [(S.Text, (Group m c a, AliasType))]
  , commands :: [(S.Text, (Command m c a, AliasType))]
  }
  deriving (Show)

instance (Show c, Show a) => Show (CommandHandler m c a) where
  showsPrec d CommandHandler {groups, commands} = showsPrec d $ CommandHandlerS (LH.toList groups) (LH.toList commands)

$(deriveTextShow ''CommandHandlerS)

instance (TextShow.TextShow c, TextShow.TextShow a) => TextShow.TextShow (CommandHandler m c a) where
  showbPrec d CommandHandler {groups, commands} = TextShow.showbPrec d $ CommandHandlerS (LH.toList groups) (LH.toList commands)

$(makeFieldLabelsNoPrefix ''CommandHandler)
