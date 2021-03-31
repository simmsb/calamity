-- | A command handler
module Calamity.Commands.Handler
    ( CommandHandler(..) ) where

import           Calamity.Commands.AliasType
import           Calamity.Commands.Command
import           Calamity.Commands.Group

import qualified Data.HashMap.Lazy         as LH
import qualified Data.Text                 as S

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic          as TSG

data CommandHandler = CommandHandler
  { groups   :: LH.HashMap S.Text (Group, AliasType)
    -- ^ Top level groups
  , commands :: LH.HashMap S.Text (Command, AliasType)
    -- ^ Top level commands
  }
  deriving ( Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric CommandHandler
