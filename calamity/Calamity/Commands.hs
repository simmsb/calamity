{- | Calamity commands
 This module exports the DSL and core types for using commands
-}
module Calamity.Commands (
  module Calamity.Commands.Dsl,
  module CalamityCommands.Error,
  module Calamity.Commands.Help,
  module CalamityCommands.Parser,
  module Calamity.Commands.Utils,
  module Calamity.Commands.Types,

  -- * Commands
  -- $commandDocs
) where

import Calamity.Commands.CalamityParsers ()
import Calamity.Commands.Dsl
import Calamity.Commands.Help
import Calamity.Commands.Types
import Calamity.Commands.Utils
import CalamityCommands.Error
import CalamityCommands.Parser

{- $commandDocs

 This module provides abstractions for writing declarative commands, that
 support grouping, pre-invokation checks, and automatic argument parsing by
 using a type level list of parameter types.

 A DSL is provided in "Calamity.Commands.Dsl" for constructing commands
 declaratively.

 You can implement 'ParameterParser' for your own types to allow them to be used
 in the parameter list of commands.

 A default help command is provided in "Calamity.Commands.Help" which can be
 added just by using 'helpCommand' inside the command declaration DSL.

 This module is pretty much a wrapper/re-export of the package
 "CalamityCommands", look there for more documentation.

 To decide which context type you want to use, and how the command prefix
 should be parsed, you need to handle the following effects:

     1. 'CalamityCommands.ParsePrefix.ParsePrefix'

         Handles parsing prefixes, the
         'Calamity.Commands.Utils.useConstantPrefix' function handles constant
         prefixes.

     2. 'CalamityCommands.Context.ConstructContext'

         Handles constructing the context and also decides which context is
         going to be used, calamity offers
         'Calamity.Commands.Context.useFullContext' which makes the context
         'Calamity.Commands.Context.FullContext', and
         'Calamity.Commands.Context.useLightContext' which makes the context
         'Calamity.Commands.Context.LightContext'.

 ==== Custom Events

 The event handler registered by 'addCommands' will fire the following custom events:

     1. 'Calamity.Commands.Utils.CtxCommandError'

         Fired when a command returns an error.

     2. 'Calamity.Commands.Utils.CommandNotFound'

         Fired when a valid prefix is used, but the command is not found.

     3. 'Calamity.Commands.Utils.CommandInvoked'

         Fired when a command is successfully invoked.


 ==== Registered Metrics

     1. Counter: @"commands_invoked" [name]@

         Incremented on each command invokation, the parameter @name@ is the
         path/name of the invoked command.


 ==== Examples

 An example of using commands:

 @
 'addCommands' $ do
   'helpCommand'
   'command' \@\'['Calamity.Types.Model.User.User'] "utest" $ \\ctx u \-\> do
     'Control.Monad.void' $ 'Calamity.Types.Tellable.tell' ctx $ "got user: " '<>' 'TextShow.showt' u
   'group' "admin" $ do
     'command' \@'[] "bye" $ \\ctx -> do
       'Control.Monad.void' $ 'Calamity.Types.Tellable.tell' ctx "bye!"
       'Calamity.Client.stopBot'
 @
-}
