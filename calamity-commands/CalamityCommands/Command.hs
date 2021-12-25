{-# LANGUAGE NoPolyKinds #-}

-- | Commands and stuff
module CalamityCommands.Command (Command (..)) where

import CalamityCommands.Check
import CalamityCommands.Error
import CalamityCommands.Group
import CalamityCommands.ParameterInfo

import Control.Lens hiding (Context, (<.>))

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Text as T

import GHC.Generics

import qualified Data.List.NonEmpty as NE
import TextShow
import qualified TextShow.Generic as TSG

-- | A command, paremeterised over its context
data Command (m :: Type -> Type) (c :: Type) (a :: Type) = forall p.
  Command
  { names :: NonEmpty T.Text
  , parent :: Maybe (Group m c a)
  , -- | If this command is hidden
    hidden :: Bool
  , -- | A list of checks that must pass for this command to be invoked
    checks :: [Check m c]
  , -- | A list of parameter metadata
    params :: [ParameterInfo]
  , -- | A function producing the \'help\' for the command.
    help :: c -> T.Text
  , -- | A function that parses the context for the command, producing the input
    -- @a@ for the command.
    parser :: c -> m (Either CommandError p)
  , -- | A function that given the context and the input (@p@) of the command,
    -- performs the action of the command.
    callback :: (c, p) -> m (Either T.Text a)
  }

data CommandS = CommandS
  { names :: NonEmpty T.Text
  , params :: [ParameterInfo]
  , parent :: Maybe T.Text
  , checks :: [T.Text]
  , hidden :: Bool
  }
  deriving (Generic, Show)
  deriving (TextShow) via TSG.FromGeneric CommandS

instance Show (Command m c a) where
  showsPrec d Command{names, params, parent, checks, hidden} =
    showsPrec d $
      CommandS
        names
        params
        (NE.head <$> parent ^? _Just . #names)
        (checks ^.. traverse . #name)
        hidden

instance TextShow (Command m c a) where
  showbPrec d Command{names, params, parent, checks, hidden} =
    showbPrec d $
      CommandS
        names
        params
        (NE.head <$> parent ^? _Just . #names)
        (checks ^.. traverse . #name)
        hidden
