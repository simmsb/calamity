{-# LANGUAGE RecursiveDo #-}

{- | A DSL for generating commands and groups

 This is effectively just a re-export of "CalamityCommands.Dsl" but with
 documentation more tuned for usage with Calamity.
-}
module Calamity.Commands.Dsl (
    -- * Commands DSL
    -- $dslTutorial
    command,
    command',
    commandA,
    commandA',
    hide,
    help,
    requires,
    requires',
    requiresPure,
    group,
    group',
    groupA,
    groupA',
    DSLState,
    fetchHandler,
) where

import qualified CalamityCommands.Dsl as CC
import CalamityCommands.ParameterInfo

import Calamity.Commands.Types

import qualified Data.Text as S
import qualified Data.Text.Lazy as L

import CalamityCommands.CommandUtils (CommandForParsers, TypedCommandC)
import qualified CalamityCommands.Context as CC
import CalamityCommands.Error (CommandError)
import qualified Polysemy as P
import qualified Polysemy.Fail as P
import qualified Polysemy.Reader as P
import qualified Polysemy.Tagged as P

{- $dslTutorial

 This module provides a way of constructing bot commands in a declarative way.

 The main component of this is the 'Calamity.Commands.Dsl.command' function,
 which takes a type-level list of command parameters, the name, and the callback
 and produces a command. There are also the alternatives
 'Calamity.Commands.Dsl.command'', 'commandA' and 'commandA'', for when you want
 to handle parsing of the input yourself, and/or want aliases of the command.

 The functions: 'hide', 'help', 'requires', and 'group' can be used to change
 attributes of any commands declared inside the monadic action passed to them,
 for example:

 @
 'hide' '$' do
   'Calamity.Commands.Dsl.command' \@'[] "test" \\ctx -> 'pure' ()
 @

 In the above block, any command declared inside 'hide' will have it's
 \'hidden\' flag set and will not be shown by the default help command:
 'Calamity.Commands.Help.helpCommand'

 The 'Calamity.Commands.Help.helpCommand' function can be used to create a
 help command for the commands DSL action it is used in, read it's doc page
 for more information on how it functions.

 The 'Calamity.Commands.Utils.addCommands' function creates the command
 handler for the commands registered in the passed action, it is what reads a
 message to determine what command was invoked. It should be used to register the
 commands with the bot by using it inside the setup action, for example:

 @
 'Calamity.Client.runBotIO' ('Calamity.BotToken' token)
   $ 'Calamity.Commands.Utils.addCommands' $ do
     'Calamity.Commands.Help.helpCommand'
     'Calamity.Commands.Dsl.command' \@'[] "test" \\ctx ->
       'Control.Monad.void' $ 'Calamity.Types.Tellable.tell' \@'L.Text' ctx "hi"
 @

 The above block will create a command with no parameters named \'test\',
 along with a help command.
-}

{- | Given the command name and parameter names, @parser@ and @callback@ for a
 command in the 'P.Sem' monad, build a command by transforming the Polysemy
 actions into IO actions. Then register the command.

 The parent group, visibility, checks, and command help are drawn from the
 reader context.
-}
command' ::
    P.Member (P.Final IO) r =>
    -- | The name of the command
    S.Text ->
    -- | The command's parameters
    [ParameterInfo] ->
    -- | The parser for this command
    (c -> P.Sem r (Either CommandError a)) ->
    -- | The callback for this command
    ((c, a) -> P.Sem (P.Fail ': r) ()) ->
    P.Sem (DSLState c r) (Command c)
command' = CC.command'

{- | Given the command name, aliases, and parameter names, @parser@ and
 @callback@ for a command in the 'P.Sem' monad, build a command by
 transforming the Polysemy actions into IO actions. Then register the command.

 The parent group, visibility, checks, and command help are drawn from the
 reader context.
-}
commandA' ::
    P.Member (P.Final IO) r =>
    -- | The name of the command
    S.Text ->
    -- | The aliases for the command
    [S.Text] ->
    -- | The command's parameters
    [ParameterInfo] ->
    -- | The parser for this command
    (c -> P.Sem r (Either CommandError a)) ->
    -- | The callback for this command
    ((c, a) -> P.Sem (P.Fail ': r) ()) ->
    P.Sem (DSLState c r) (Command c)
commandA' = CC.commandA'

{- | Given the name of a command and a callback, and a type level list of
 the parameters, build and register a command.

 The parent group, visibility, checks, and command help are drawn from the
 reader context.

 Command parameters are parsed by first invoking
 'CalamityCommands.Parser.parse' for the first
 'CalamityCommands.Parser.ParameterParser', then running the next parser on the
 remaining input, and so on.

 ==== Examples

 Building a command that bans a user by id.

 @
 'Calamity.Commands.Dsl.command' \@\'['CalamityCommands.Parser.Named' "user" ('Calamity.Types.Snowflake' 'Calamity.Types.Model.User'),
                'CalamityCommands.Parser.Named' "reason" ('CalamityCommands.Parser.KleeneStarConcat' 'S.Text')]
    "ban" $ \\ctx uid r -> case (ctx 'Control.Lens.^.' #guild) of
      'Just' guild -> do
        'Control.Monad.void' . 'Calamity.HTTP.invoke' $ 'Calamity.HTTP.Guild.CreateGuildBan' guild uid ('Calamity.HTTP.Guild.CreateGuildBanData' 'Nothing' $ 'Just' r)
        'Control.Monad.void' $ 'Calamity.Types.Tellable.tell' ctx ("Banned user `" '<>' 'TextShow.showt' uid '<>' "` with reason: " '<>' r)
      'Nothing' -> 'void' $ 'Calamity.Types.Tellable.tell' @'L.Text' ctx "Can only ban users from guilds."
 @
-}
command ::
    forall ps c r.
    ( P.Member (P.Final IO) r
    , CC.CommandContext IO c ()
    , TypedCommandC ps c () r
    ) =>
    -- | The name of the command
    S.Text ->
    -- | The callback for this command
    (c -> CommandForParsers ps r ()) ->
    P.Sem (DSLState c r) (Command c)
command = CC.command @ps

{- | Given the name and aliases of a command and a callback, and a type level list of
 the parameters, build and register a command.

 The parent group, visibility, checks, and command help are drawn from the
 reader context.

 ==== Examples

 Building a command that bans a user by id.

 @
 'commandA' \@\'['CalamityCommands.Parser.Named' "user" ('Calamity.Types.Snowflake' 'Calamity.Types.Model.User'),
                'CalamityCommands.Parser.Named' "reason" ('CalamityCommands.Parser.KleeneStarConcat' 'S.Text')]
    "ban" [] $ \\ctx uid r -> case (ctx 'Control.Lens.^.' #guild) of
      'Just' guild -> do
        'Control.Monad.void' . 'Calamity.HTTP.invoke' $ 'Calamity.HTTP.Guild.CreateGuildBan' guild uid ('Calamity.HTTP.Guild.CreateGuildBanData' 'Nothing' $ 'Just' r)
        'Control.Monad.void' $ 'Calamity.Types.Tellable.tell' ctx ("Banned user `" '<>' 'TextShow.showt' uid '<>' "` with reason: " '<>' r)
      'Nothing' -> 'void' $ 'Calamity.Types.Tellable.tell' @'L.Text' ctx "Can only ban users from guilds."
 @
-}
commandA ::
    forall ps c r.
    ( P.Member (P.Final IO) r
    , CC.CommandContext IO c ()
    , TypedCommandC ps c () r
    ) =>
    -- | The name of the command
    S.Text ->
    -- | The aliases for the command
    [S.Text] ->
    -- | The callback for this command
    (c -> CommandForParsers ps r ()) ->
    P.Sem (DSLState c r) (Command c)
commandA = CC.commandA @ps

{- | Set the visibility of any groups or commands registered inside the given
 action to hidden.
-}
hide ::
    P.Member (P.Tagged "hidden" (P.Reader Bool)) r =>
    P.Sem r a ->
    P.Sem r a
hide = CC.hide

-- | Set the help for any groups or commands registered inside the given action.
help ::
    P.Member (P.Reader (c -> L.Text)) r =>
    (c -> L.Text) ->
    P.Sem r a ->
    P.Sem r a
help = CC.help

{- | Add to the list of checks for any commands registered inside the given
 action.
-}
requires ::
    [Check c] ->
    P.Sem (DSLState c r) a ->
    P.Sem (DSLState c r) a
requires = CC.requires

{- | Construct a check and add it to the list of checks for any commands
 registered inside the given action.

 Refer to 'CalamityCommands.Check.Check' for more info on checks.
-}
requires' ::
    P.Member (P.Final IO) r =>
    -- | The name of the check
    S.Text ->
    -- | The callback for the check
    (c -> P.Sem r (Maybe L.Text)) ->
    P.Sem (DSLState c r) a ->
    P.Sem (DSLState c r) a
requires' = CC.requires'

{- | Construct some pure checks and add them to the list of checks for any
 commands registered inside the given action.

 Refer to 'CalamityCommands.Check.Check' for more info on checks.
-}
requiresPure ::
    [(S.Text, c -> Maybe L.Text)] ->
    -- A list of check names and check callbacks
    P.Sem (DSLState c r) a ->
    P.Sem (DSLState c r) a
requiresPure = CC.requiresPure

{- | Construct a group and place any commands registered in the given action
 into the new group.

 This also resets the @help@ function back to it's original value, use
 'group'' if you don't want that (i.e. your help function is context aware).
-}
group ::
    P.Member (P.Final IO) r =>
    -- | The name of the group
    S.Text ->
    P.Sem (DSLState c r) a ->
    P.Sem (DSLState c r) a
group = CC.group

{- | Construct a group with aliases and place any commands registered in the
 given action into the new group.

 The parent group, visibility, checks, and command help are drawn from the
 reader context.

 This also resets the @help@ function back to it's original value, use
 'group'' if you don't want that (i.e. your help function is context aware).
-}
groupA ::
    P.Member (P.Final IO) r =>
    -- | The name of the group
    S.Text ->
    -- | The aliases of the group
    [S.Text] ->
    P.Sem (DSLState c r) a ->
    P.Sem (DSLState c r) a
groupA = CC.groupA

{- | Construct a group and place any commands registered in the given action
 into the new group.

 The parent group, visibility, checks, and command help are drawn from the
 reader context.

 Unlike 'help' this doesn't reset the @help@ function back to it's original
 value.
-}
group' ::
    P.Member (P.Final IO) r =>
    -- | The name of the group
    S.Text ->
    P.Sem (DSLState c r) a ->
    P.Sem (DSLState c r) a
group' = CC.group'

{- | Construct a group with aliases and place any commands registered in the given action
 into the new group.

 The parent group, visibility, checks, and command help are drawn from the
 reader context.

 Unlike 'help' this doesn't reset the @help@ function back to it's original
 value.
-}
groupA' ::
    P.Member (P.Final IO) r =>
    -- | The name of the group
    S.Text ->
    -- | The aliases of the group
    [S.Text] ->
    P.Sem (DSLState c r) a ->
    P.Sem (DSLState c r) a
groupA' = CC.groupA'

-- | Retrieve the final command handler for this block
fetchHandler :: P.Sem (DSLState c r) (CommandHandler c)
fetchHandler = P.ask
