{-# LANGUAGE RecursiveDo #-}

-- | A DSL for generating commands and groups
module CalamityCommands.Dsl (
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
    DSLC,
    raiseDSL,
    fetchHandler,
) where

import CalamityCommands.AliasType
import CalamityCommands.Check
import CalamityCommands.Command hiding (help)
import CalamityCommands.CommandUtils
import CalamityCommands.Context
import CalamityCommands.Error
import CalamityCommands.Group hiding (help)
import CalamityCommands.Handler
import CalamityCommands.ParameterInfo
import CalamityCommands.Internal.LocalWriter
import qualified Data.HashMap.Lazy as LH
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty (..))
import qualified Polysemy as P
import qualified Polysemy.Fail as P
import qualified Polysemy.Fixpoint as P
import qualified Polysemy.Reader as P
import qualified Polysemy.Tagged as P

{- $dslTutorial

 This module provides a way of constructing bot commands in a declarative way.

 The main component of this is the 'command' function, which takes a
 type-level list of command parameters, the name, and the callback and
 produces a command. There are also the alternatives 'command'', 'commandA'
 and 'commandA'', for when you want to handle parsing of the input yourself,
 and/or want aliases of the command.

 The functions: 'hide', 'help', 'requires', and 'group' can be used to change
 attributes of any commands declared inside the monadic action passed to them,
 for example:

 @
 'hide' '$' do
   'command' \@\'[] "test" \\ctx -> 'pure' ()
 @

 In the above block, any command declared inside 'hide' will have its
 \'hidden\' flag set and will not be shown by the default help command:
 'CalamityCommands.Help.helpCommand'

 The 'CalamityCommands.Help.helpCommand' function can be used to create a
 help command for the commands DSL action it is used in, read its doc page
 for more information on how it functions.

 The 'CalamityCommands.Utils.buildCommands' function is used to
 construct a 'CommandHandler' which can then be used with
 'CalamityCommands.Utils.processCommands' or
 'CalamityCommands.Utils.handleCommands' to process a command.
-}

type DSLState m c a r =
    ( LocalWriter (LH.HashMap T.Text (Command m c a, AliasType))
        ': LocalWriter (LH.HashMap T.Text (Group m c a, AliasType))
            ': P.Reader (Maybe (Group m c a))
                ': P.Tagged "hidden" (P.Reader Bool)
                    ': P.Reader (c -> T.Text)
                        ': P.Tagged "original-help" (P.Reader (c -> T.Text))
                            ': P.Reader [Check m c]
                                ': P.Reader (CommandHandler m c a)
                                    ': P.Fixpoint
                                        ': r
    )

type DSLC m c a r = P.Members [
  LocalWriter (LH.HashMap T.Text (Command m c a, AliasType))
        , LocalWriter (LH.HashMap T.Text (Group m c a, AliasType))
            , P.Reader (Maybe (Group m c a))
                , P.Tagged "hidden" (P.Reader Bool)
                    , P.Reader (c -> T.Text)
                        , P.Tagged "original-help" (P.Reader (c -> T.Text))
                            , P.Reader [Check m c]
                                , P.Reader (CommandHandler m c a)
                                    , P.Fixpoint
                              ] r

raiseDSL :: P.Sem r x -> P.Sem (DSLState m c a r) x
raiseDSL = P.raise . P.raise . P.raise . P.raise . P.raise . P.raise . P.raise . P.raise . P.raise

{- | Given the command name and parameter names, @parser@ and @callback@ for a
 command in the 'P.Sem' monad, build a command by transforming the Polysemy
 actions into @m@ actions. Then register the command.

 The parent group, visibility, checks, and command help are drawn from the
 reader context.
-}
command' ::
    (Monad m, P.Member (P.Final m) r, DSLC m c a r) =>
    -- | The name of the command
    T.Text ->
    -- | The command's parameter metadata
    [ParameterInfo] ->
    -- | The parser for this command
    (c -> P.Sem r (Either CommandError p)) ->
    -- | The callback for this command
    ((c, p) -> P.Sem (P.Fail ': r) a) ->
    P.Sem r (Command m c a)
command' name params parser cb = commandA' name [] params parser cb

{- | Given the command name, aliases, and parameter names, @parser@ and @callback@
 for a command in the 'P.Sem' monad, build a command by transforming the
 Polysemy actions into @m@ actions. Then register the command.

 The parent group, visibility, checks, and command help are drawn from the
 reader context.
-}
commandA' ::
    forall p c a m r.
    (Monad m, P.Member (P.Final m) r, DSLC m c a r) =>
    -- | The name of the command
    T.Text ->
    -- | The aliases for the command
    [T.Text] ->
    -- | The command's parameter metadata
    [ParameterInfo] ->
    -- | The parser for this command
    (c -> P.Sem r (Either CommandError p)) ->
    -- | The callback for this command
    ((c, p) -> P.Sem (P.Fail ': r) a) ->
    P.Sem r (Command m c a)
commandA' name aliases params parser cb = do
    parent <- P.ask @(Maybe (Group m c a))
    hidden <- P.tag $ P.ask @Bool
    checks <- P.ask @[Check m c]
    help' <- P.ask @(c -> T.Text)
    cmd <- buildCommand' (name :| aliases) parent hidden checks params help' parser cb
    ltell $ LH.singleton name (cmd, Original)
    ltell $ LH.fromList [(name, (cmd, Alias)) | name <- aliases]
    pure cmd

{- | Given the name of a command and a callback, and a type level list of
 the parameters, build and register a command.

 The parent group, visibility, checks, and command help are drawn from the
 reader context.

 Command parameters are parsed by first invoking
 'CalamityCommands.Parser.parse' for the first
 'CalamityCommands.Parser.Parser', then running the next parser on the
 remaining input, and so on.

 ==== Examples

 Building a command that adds two numbers.

 @
 'command' \@\'['CalamityCommands.Parser.Named' "a" 'Int', 'CalamityCommands.Parser.Named' "b" 'Int']
   "add" $ \\ctx a b -> 'pure' '$' 'Right' (a '+' b)
 @
-}
command ::
    forall ps c a m r.
    ( Monad m
    , P.Member (P.Final m) r
    , DSLC m c a r
    , TypedCommandC ps c a r
    , CommandContext m c a
    ) =>
    -- | The name of the command
    T.Text ->
    -- | The callback for this command
    (c -> CommandForParsers ps r a) ->
    P.Sem r (Command m c a)
command name cmd = commandA @ps name [] cmd

{- | Given the name and aliases of a command and a callback, and a type level list of
 the parameters, build and register a command.

 The parent group, visibility, checks, and command help are drawn from the
 reader context.

 ==== Examples


 Building a command that adds two numbers.

 @
 'commandA' \@\'['CalamityCommands.Parser.Named' "a" 'Int', 'CalamityCommands.Parser.Named' "b" 'Int']
   "add" [] $ \\ctx a b -> 'pure' '$' 'Right' (a '+' b)
 @
-}
commandA ::
    forall ps c a m r.
    ( Monad m
    , P.Member (P.Final m) r
    , DSLC m c a r
    , TypedCommandC ps c a r
    , CommandContext m c a
    ) =>
    -- | The name of the command
    T.Text ->
    -- | The aliases for the command
    [T.Text] ->
    -- | The callback for this command
    (c -> CommandForParsers ps r a) ->
    P.Sem r (Command m c a)
commandA name aliases cmd = do
    parent <- P.ask @(Maybe (Group m c a))
    hidden <- P.tag $ P.ask @Bool
    checks <- P.ask @[Check m c]
    help' <- P.ask @(c -> T.Text)
    cmd' <- buildCommand @ps (name :| aliases) parent hidden checks help' cmd
    ltell $ LH.singleton name (cmd', Original)
    ltell $ LH.fromList [(name, (cmd', Alias)) | name <- aliases]
    pure cmd'

{- | Set the visibility of any groups or commands registered inside the given
 action to hidden.
-}
hide ::
    P.Member (P.Tagged "hidden" (P.Reader Bool)) r =>
    P.Sem r x ->
    P.Sem r x
hide = P.tag @"hidden" . P.local @Bool (const True) . P.raise

{- | Set the help for any groups or commands registered inside the given action.

 ==== Examples

 @
 'help' ('const' "Add two integers") $
   'command' \@\'['CalamityCommands.Parser.Named' "a" 'Int', 'CalamityCommands.Parser.Named' "b" 'Int']
     "add" $ \\ctx a b -> 'pure' '$' 'Right' (a '+' b)
 @
-}
help ::
    P.Member (P.Reader (c -> T.Text)) r =>
    (c -> T.Text) ->
    P.Sem r a ->
    P.Sem r a
help = P.local . const

{- | Add to the list of checks for any commands registered inside the given
 action.
-}
requires :: DSLC m c a r =>
    [Check m c] ->
    P.Sem r x ->
    P.Sem r x
requires = P.local . (<>)

{- | Construct a check and add it to the list of checks for any commands
 registered inside the given action.

 Refer to 'CalamityCommands.Check.Check' for more info on checks.
-}
requires' ::
    (Monad m, P.Member (P.Final m) r, DSLC m c a r) =>
    -- | The name of the check
    T.Text ->
    -- | The callback for the check
    (c -> P.Sem r (Maybe T.Text)) ->
    P.Sem r x ->
    P.Sem r x
requires' name cb m = do
    check <- buildCheck name cb
    requires [check] m

{- | Construct some pure checks and add them to the list of checks for any
 commands registered inside the given action.

 Refer to 'CalamityCommands.Check.Check' for more info on checks.

 ==== Examples

 @
 'requiresPure' [("always ok", 'const' 'Nothing')] $
   'command' \@\'['CalamityCommands.Parser.Named' "a" 'Int', 'CalamityCommands.Parser.Named' "b" 'Int']
     "add" $ \\ctx a b -> 'pure' '$' 'Right' (a '+' b)
 @
-}
requiresPure ::
    (Monad m, DSLC m c a r) =>
    [(T.Text, c -> Maybe T.Text)] ->
    -- A list of check names and check callbacks
    P.Sem r x ->
    P.Sem r x
requiresPure checks = requires $ map (uncurry buildCheckPure) checks

{- | Construct a group and place any commands registered in the given action
 into the new group.

 This also resets the @help@ function back to its original value, use
 'group'' if you don't want that (i.e. your help function is context aware).
-}
group ::
    (Monad m, P.Member (P.Final m) r, DSLC m c a r) =>
    -- | The name of the group
    T.Text ->
    P.Sem r x ->
    P.Sem r x
group name m = groupA name [] m

{- | Construct a group with aliases and place any commands registered in the
 given action into the new group.

 The parent group, visibility, checks, and command help are drawn from the
 reader context.

 This also resets the @help@ function back to its original value, use
 'group'' if you don't want that (i.e. your help function is context aware).
-}
groupA ::
    forall x c m a r.
    (Monad m, P.Member (P.Final m) r, DSLC m c a r) =>
    -- | The name of the group
    T.Text ->
    -- | The aliases of the group
    [T.Text] ->
    P.Sem r x ->
    P.Sem r x
groupA name aliases m = mdo
    parent <- P.ask @(Maybe (Group m c a))
    hidden <- P.tag $ P.ask @Bool
    checks <- P.ask @[Check m c]
    help' <- P.ask @(c -> T.Text)
    origHelp <- fetchOrigHelp
    let group' = Group (name :| aliases) parent hidden commands children help' checks
    (children, (commands, res)) <-
        llisten @(LH.HashMap T.Text (Group m c a, AliasType)) $
            llisten @(LH.HashMap T.Text (Command m c a, AliasType)) $
                P.local @(Maybe (Group m c a)) (const $ Just group') $
                    P.local @(c -> T.Text) (const origHelp) m
    ltell $ LH.singleton name (group', Original)
    ltell $ LH.fromList [(name, (group', Alias)) | name <- aliases]
    pure res

fetchOrigHelp :: P.Member (P.Tagged "original-help" (P.Reader (c -> T.Text))) r => P.Sem r (c -> T.Text)
fetchOrigHelp = P.tag P.ask

{- | Construct a group and place any commands registered in the given action
 into the new group.

 The parent group, visibility, checks, and command help are drawn from the
 reader context.

 Unlike 'help' this doesn't reset the @help@ function back to its original
 value.
-}
group' ::
    (P.Member (P.Final m) r, DSLC m c a r) =>
    -- | The name of the group
    T.Text ->
    P.Sem r x ->
    P.Sem r x
group' name m = groupA' name [] m

{- | Construct a group with aliases and place any commands registered in the given action
 into the new group.

 The parent group, visibility, checks, and command help are drawn from the
 reader context.

 Unlike 'help' this doesn't reset the @help@ function back to its original
 value.
-}
groupA' ::
    forall x c m a r.
    (P.Member (P.Final m) r, DSLC m c a r) =>
    -- | The name of the group
    T.Text ->
    -- | The aliases of the group
    [T.Text] ->
    P.Sem r x ->
    P.Sem r x
groupA' name aliases m = mdo
    parent <- P.ask @(Maybe (Group m c a))
    hidden <- P.tag $ P.ask @Bool
    checks <- P.ask @[Check m c]
    help' <- P.ask @(c -> T.Text)
    let group' = Group (name :| aliases) parent hidden commands children help' checks
    (children, (commands, res)) <-
        llisten @(LH.HashMap T.Text (Group m c a, AliasType)) $
            llisten @(LH.HashMap T.Text (Command m c a, AliasType)) $
                P.local @(Maybe (Group m c a)) (const $ Just group') m
    ltell $ LH.singleton name (group', Original)
    ltell $ LH.fromList [(name, (group', Alias)) | name <- aliases]
    pure res

-- | Retrieve the final command handler for this block
fetchHandler :: DSLC m c a r => P.Sem r (CommandHandler m c a)
fetchHandler = P.ask
