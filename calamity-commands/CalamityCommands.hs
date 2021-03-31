{- | CalamityCommands commands
 This module exports the DSL and core types for using commands
-}
module CalamityCommands (
    module CalamityCommands.Context,
    module CalamityCommands.Dsl,
    module CalamityCommands.Error,
    module CalamityCommands.Handler,
    module CalamityCommands.Utils,
    module CalamityCommands.ParsePrefix,
    module CalamityCommands.Help,

    -- * Parameter parsers

    module CalamityCommands.Parser,

    -- * Commands
    -- $commandDocs
) where

import CalamityCommands.Context
import CalamityCommands.Dsl
import CalamityCommands.Error
import CalamityCommands.Handler
import CalamityCommands.Help
import CalamityCommands.ParsePrefix
import CalamityCommands.Parser (Named, KleeneStarConcat, KleenePlusConcat, ParameterParser (..))
import CalamityCommands.Utils

{- $commandDocs

 This module provides abstractions for writing declarative commands, that
 support grouping, pre-invokation checks, and automatic argument parsing by
 using a type level list of parameter types.

 A DSL is provided in "CalamityCommands.Dsl" for constructing commands
 declaratively.

 You can implement 'ParameterParser' for your own types to allow them to be used in the
 parameter list of commands.

 A default help command is provided in "CalamityCommands.Help" which can be
 added just by using 'helpCommand' inside the command declaration DSL.

 Commands are parameterised over three types:

   - @m@, the base monad of the command processor. This is used because all
     commands and checks run in their base monad in the current implementation,
     as a result, all commands will run with the monadic state remaining the
     same as when they were created. In the future this design decision may be
     revised to remove this constraint. For pure commands this may be
     'Data.Functor.Identity.Identity' and for IO commands this may be 'IO'.

   - @c@, the context that is provided to each command invokation. The default
     context: 'BasicContext' stores the prefix, command, and unparsed parameters
     to the command. The context in use is decided by the interpreter for
     'ConstructContext'.

   - @a@, the result of a command invokation, for commands performing IO actions
     this is usually just @()@.

 ==== Examples

 Make a command handler, we don't actually use the context and therefore this
 handler is generic over the context used:

 @
 h' :: 'CommandContext' 'Data.Functor.Identity.Identity' c ('Either' 'Data.Text.Lazy.Text' 'Int') => 'CommandHandler' 'Data.Functor.Identity.Identity' c ('Either' 'Data.Text.Lazy.Text' 'Int')
 h' = 'Data.Functor.Identity.runIdentity' . 'Polysemy.runFinal' $ do
   (h, _) <- 'buildCommands' $ do
     'command' \@\'[Int, Int] "add" $ \ctx a b -> 'pure' $ 'Right' (a '+' b)
     'command' \@\'[Int, Int] "mul" $ \ctx a b -> 'pure' $ 'Right' (a '*' b)
     'helpCommand' ('pure' . 'Left')
   pure h
 @

 To use the commands we need to provide the interpreters for
 'ConstructContext' and 'ParsePrefix', the default provided ones are being
 used here: 'useBasicContext' which makes @ctx ~ 'BasicContext'@, and
 @'useConstantPrefix' "!"@ which treats any input starting with @!@ as a
 command.


 The 'processCommands' function can then be used to parse and invoke commands,
 since commands are generic over the monad they run in we use
 @'Data.Functor.Identity.runIdentity' . 'Polysemy.runFinal' .
 'Polysemy.embedToFinal'@ to have the commands interpreted purely.


 This function 'r' takes an input string such as "!add 1 2", and then looks up
 the invoked command and runs it, returning the result.

 @
 r :: 'Data.Text.Lazy.Text' -> 'Maybe' ('Either'
                     ('CmdInvokeFailReason' ('BasicContext' 'Data.Functor.Identity.Identity' ('Either' 'Data.Text.Lazy.Text' 'Int')))
                     ('BasicContext' 'Data.Functor.Identity.Identity' ('Either' 'Data.Text.Lazy.Text' 'Int'), 'Either' 'Data.Text.Lazy.Text' 'Int'))
 r = 'Data.Functor.Identity.runIdentity' . 'Polysemy.runFinal' . 'Polysemy.embedToFinal' . 'useBasicContext' . 'useConstantPrefix' "!" . 'processCommands' h'
 @

 Then to display the result of processing the command nicely, we can use a
 something like this function, which prints the result of a command if one was
 invoked successfully, and prints the error nicely if not.

 @
 rm :: 'Data.Text.Lazy.Text' -> IO ()
 rm s = case r s of
             Just (Right (_, Right r)) ->
             print r

             Just (Right (_, Left h)) ->
             'Data.Text.Lazy.IO.putStrLn' h

             Just (Left ('CommandInvokeError' _ ('ParseError' t r))) ->
             'Data.Text.Lazy.IO.putStrLn' ("Parsing parameter " '<>' 'Data.Text.Lazy.fromStrict' t '<>' " failed with reason: " '<>' r)

              _ -> pure ()
 @

 >>> rm "!add 1 1"
 2

 >>> rm "!add blah 1"
  Parsing parameter :Int failed with reason: 1:2:
    |
  1 |  blah 1
    |  ^
  unexpected 'b'
  expecting '+', '-', integer, or white space

 >>> rm "!help"
 ```
 The following commands exist:
 - mul :Int, :Int
 - help :[Text]
 - add :Int, :Int
 ```

 >>> rm "!help add"
 Help for command `add`:
 ```
 Usage: !add :Int, :Int
 This command or group has no help.
 ```
-}
