-- | Command invokation context
module Calamity.Commands.Context
    ( Context(..) ) where

import {-# SOURCE #-} Calamity.Commands.Command
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.Guild
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake
import           Calamity.Types.Tellable

import qualified Data.Text.Lazy               as L

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic             as TSG

-- | Invokation context for commands
data Context = Context
  { message        :: Message
    -- ^ The message that the command was invoked from
  , guild          :: Maybe Guild
    -- ^ If the command was sent in a guild, this will be present
  , member         :: Maybe Member
    -- ^ The member that invoked the command, if in a guild
  , channel        :: Channel
    -- ^ The channel the command was invoked from
  , user           :: User
    -- ^ The user that invoked the command
  , command        :: Command
    -- ^ The command that was invoked
  , prefix         :: L.Text
    -- ^ The prefix that was used to invoke the command
  , unparsedParams :: L.Text
    -- ^ The message remaining after consuming the prefix
  }
  deriving ( Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric Context

instance Tellable Context where
  getChannel Context { channel } = pure . getID $ channel
