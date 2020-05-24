-- | A command handler
module Calamity.Commands.Handler
    ( CommandHandler(..) ) where

import           Calamity.Commands.Command
import           Calamity.Commands.Group

import qualified Data.HashMap.Lazy         as LH
import qualified Data.Text                 as S

import           GHC.Generics

data CommandHandler = CommandHandler
  { groups   :: LH.HashMap S.Text Group
    -- ^ Top level groups
  , commands :: LH.HashMap S.Text Command
    -- ^ Top level commands
  }
  deriving ( Generic )
