-- | Calamity commands
-- This module only exports the DSL and core types for using commands
module Calamity.Commands
    ( module Calamity.Commands.Dsl
    , module Calamity.Commands.Error
    , module Calamity.Commands.Handler
    , module Calamity.Commands.Utils
    , module Calamity.Commands.ParsePrefix
    , module Calamity.Commands.Parser
    , module Calamity.Commands.Help
    , module Calamity.Commands.Context
    -- * Commands
    -- $commandDocs
    ) where

import           Calamity.Commands.Dsl
import           Calamity.Commands.Error
import           Calamity.Commands.Handler
import           Calamity.Commands.Help
import           Calamity.Commands.ParsePrefix
import           Calamity.Commands.Parser
import           Calamity.Commands.Utils
import           Calamity.Commands.Context ( Context )

-- $commandDocs
--
-- This module provides abstractions for writing declarative commands, that
-- support grouping, pre-invokation checks, and automatic argument parsing by
-- using a type level list of parameter types.
--
-- A DSL is provided in 'Calamity.Commands.Dsl' for constructing commands
-- declaratively.
--
-- You can implement 'Parser' for your own types to allow them to be used in the
-- parameter list of commands.
--
-- A default help command is provided in 'Calamity.Commands.Help' which can be
-- added just by using 'helpCommand' inside the command declaration DSL.
--
--
-- ==== Custom Events
--
-- The event handler registered by 'addCommands' will fire the following custom events:
--
--     1. @"command-error" ('Context', 'CommandError')@
--
--         Fired when a command returns an error.
--
--     2. @"command-not-found" ('Calamity.Types.Model.Channel.Message', ['Data.Text.Lazy.Text'])@
--
--         Fired when a valid prefix is used, but the command is not found.
--
--     3. @"command-invoked" 'Context'@
--
--         Fired when a command is successfully invoked.
--
-- ==== Registered Metrics
--
--     1. Counter: @"commands_invoked" [name]@
--
--         Incremented on each command invokation, the parameter @name@ is the
--         path/name of the invoked command.
--
--
-- ==== Examples
--
-- An example of using commands:
--
-- @
-- 'addCommands' $ do
--   'helpCommand'
--   'command' \@\'['Calamity.Types.Model.User.User'] "utest" $ \ctx u \-\> do
--     'Control.Monad.void' $ 'Calamity.Types.Tellable.tell' ctx $ "got user: " '<>' 'TextShow.showtl' u
--   'group' "admin" $ do
--     'command' \@'[] "bye" $ \ctx -> do
--       'Control.Monad.void' $ 'Calamity.Types.Tellable.tell' ctx "bye!"
--       'Calamity.Client.stopBot'
-- @
