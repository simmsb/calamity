{-# LANGUAGE NoPolyKinds #-}

-- | Command groups
module CalamityCommands.Group (Group (..)) where

import CalamityCommands.AliasType
import CalamityCommands.Check
import {-# SOURCE #-} CalamityCommands.Command

import Control.Lens hiding (Context, (<.>))

import qualified Data.HashMap.Lazy as LH
import qualified Data.Text as S
import qualified Data.Text.Lazy as L

import GHC.Generics

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import TextShow
import qualified TextShow.Generic as TSG

-- | A group of commands
data Group m c a = Group
    { names :: NonEmpty S.Text
    , parent :: Maybe (Group m c a)
    , hidden :: Bool
    , -- | Any child commands of this group
      commands :: LH.HashMap S.Text (Command m c a, AliasType)
    , -- | Any child groups of this group
      children :: LH.HashMap S.Text (Group m c a, AliasType)
    , -- | A function producing the \'help\' for the group
      help :: c -> L.Text
    , -- | A list of checks that must pass
      checks :: [Check m c]
    }
    deriving (Generic)

data GroupS m c a = GroupS
    { names :: NonEmpty S.Text
    , parent :: Maybe S.Text
    , commands :: [(S.Text, (Command m c a, AliasType))]
    , children :: [(S.Text, (Group m c a, AliasType))]
    , hidden :: Bool
    }
    deriving (Generic, Show)
    deriving (TextShow) via TSG.FromGeneric (GroupS m c a)

instance Show (Group m c a) where
    showsPrec d Group{names, parent, commands, children, hidden} =
        showsPrec d $ GroupS names (NE.head <$> parent ^? _Just . #names) (LH.toList commands) (LH.toList children) hidden

instance TextShow (Group m c a) where
    showbPrec d Group{names, parent, commands, children, hidden} =
      showbPrec d $ GroupS names (NE.head <$> parent ^? _Just . #names) (LH.toList commands) (LH.toList children) hidden
