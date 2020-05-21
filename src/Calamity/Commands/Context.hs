-- | Command invokation context
module Calamity.Commands.Context
    ( Context(..) ) where

import {-# SOURCE #-} Calamity.Commands.Command
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.Guild
import           Calamity.Types.Model.User

import qualified Data.Text.Lazy               as L

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic             as TSG

data Context = Context
  { message        :: Message
  , guild          :: Maybe Guild
  , member         :: Maybe Member
  , channel        :: Channel
  , user           :: User
  , command        :: Command
  , prefix         :: L.Text
  , unparsedParams :: L.Text
    -- ^ The message remaining after consuming the prefix
  }
  deriving ( Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric Context
