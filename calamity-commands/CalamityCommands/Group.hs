{-# LANGUAGE NoPolyKinds #-}

-- | Command groups
module CalamityCommands.Group (Group (..)) where

import CalamityCommands.AliasType
import CalamityCommands.Check
import {-# SOURCE #-} CalamityCommands.Command

import Control.Lens hiding (Context, (<.>))

import qualified Data.HashMap.Lazy as LH
import qualified Data.Text as T

import GHC.Generics

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import TextShow
import qualified TextShow.Generic as TSG

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
    deriving (Generic)

data GroupS m c a = GroupS
    { names :: NonEmpty T.Text
    , parent :: Maybe T.Text
    , commands :: [(T.Text, (Command m c a, AliasType))]
    , children :: [(T.Text, (Group m c a, AliasType))]
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
