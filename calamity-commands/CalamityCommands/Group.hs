{-# LANGUAGE TemplateHaskell #-}

-- | Command groups
module CalamityCommands.Group (Group (..)) where

import CalamityCommands.AliasType
import CalamityCommands.Check
import {-# SOURCE #-} CalamityCommands.Command
import qualified Data.HashMap.Lazy as LH
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Optics
import qualified TextShow
import TextShow.TH (deriveTextShow)

-- | A group of commands
data Group m c a = Group
  { names :: NonEmpty T.Text
  , parent :: Maybe (Group m c a)
  , hidden :: Bool
  , -- | Any child commands of this group
    commands :: LH.HashMap T.Text (Command m c a, AliasType)
  , -- | Any child groups of this group
    children :: LH.HashMap T.Text (Group m c a, AliasType)
  , -- | A function producing the \'help\' for the group
    help :: c -> T.Text
  , -- | A list of checks that must pass
    checks :: [Check m c]
  }

data GroupS m c a = GroupS
  { names :: NonEmpty T.Text
  , parent :: Maybe T.Text
  , commands :: [(T.Text, (Command m c a, AliasType))]
  , children :: [(T.Text, (Group m c a, AliasType))]
  , hidden :: Bool
  }
  deriving (Show)

instance Show (Group m c a) where
  showsPrec d Group {names, parent, commands, children, hidden} =
    showsPrec d $ GroupS names (NE.head <$> parent ^? _Just % #names) (LH.toList commands) (LH.toList children) hidden

instance (TextShow.TextShow a, TextShow.TextShow c) => TextShow.TextShow (Group m c a) where
  showbPrec d Group {names, parent, commands, children, hidden} =
    TextShow.showbPrec d $ GroupS names (NE.head <$> parent ^? _Just % #names) (LH.toList commands) (LH.toList children) hidden

$(makeFieldLabelsNoPrefix ''Group)
$(deriveTextShow ''GroupS)
