{-# LANGUAGE TemplateHaskell #-}

-- | Command groups
module CalamityCommands.Group (Group (..)) where

import CalamityCommands.AliasType
import CalamityCommands.Check
import {-# SOURCE #-} CalamityCommands.Command
import Data.HashMap.Lazy qualified as LH
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Optics
import TextShow qualified
import TextShow.TH (deriveTextShow)

-- | A group of commands
data Group m c a = Group
  { names :: NonEmpty T.Text
  , parent :: Maybe (Group m c a)
  , hidden :: Bool
  , commands :: LH.HashMap T.Text (Command m c a, AliasType)
  -- ^ Any child commands of this group
  , children :: LH.HashMap T.Text (Group m c a, AliasType)
  -- ^ Any child groups of this group
  , help :: c -> T.Text
  -- ^ A function producing the \'help\' for the group
  , checks :: [Check m c]
  -- ^ A list of checks that must pass
  }

$(makeFieldLabelsNoPrefix ''Group)

data GroupS m c a = GroupS
  { names :: NonEmpty T.Text
  , parent :: Maybe T.Text
  , commands :: [(T.Text, (Command m c a, AliasType))]
  , children :: [(T.Text, (Group m c a, AliasType))]
  , hidden :: Bool
  }
  deriving (Show)

instance (Show a, Show c) => Show (Group m c a) where
  showsPrec d Group {names, parent, commands, children, hidden} =
    showsPrec d $ GroupS names (NE.head <$> parent ^? _Just % #names) (LH.toList commands) (LH.toList children) hidden

$(deriveTextShow ''GroupS)

instance (TextShow.TextShow a, TextShow.TextShow c) => TextShow.TextShow (Group m c a) where
  showbPrec d Group {names, parent, commands, children, hidden} =
    TextShow.showbPrec d $ GroupS names (NE.head <$> parent ^? _Just % #names) (LH.toList commands) (LH.toList children) hidden
