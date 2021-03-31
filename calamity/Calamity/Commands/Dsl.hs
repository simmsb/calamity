{-# LANGUAGE RecursiveDo #-}

-- | A DSL for generating commands and groups
module Calamity.Commands.Dsl
    ( -- * Commands DSL
      -- $dslTutorial
      command
    , command'
    , commandA
    , commandA'
    , hide
    , help
    , requires
    , requires'
    , requiresPure
    , group
    , group'
    , groupA
    , groupA'
    , DSLState
    , raiseDSL
    , fetchHandler ) where

import           Calamity.Commands.AliasType
import           Calamity.Commands.Check
import           Calamity.Commands.Command     hiding ( help )
import           Calamity.Commands.CommandUtils
import           Calamity.Commands.Context     hiding ( command )
import           Calamity.Commands.Error
import           Calamity.Commands.Group       hiding ( help )
import           Calamity.Commands.Handler
import           Calamity.Internal.LocalWriter

import qualified Data.HashMap.Lazy             as LH
import qualified Data.Text                     as S
import qualified Data.Text.Lazy                as L

import qualified Polysemy                      as P
import qualified Polysemy.Fail                 as P
import qualified Polysemy.Tagged               as P
import qualified Polysemy.Fixpoint             as P
import qualified Polysemy.Reader               as P
import Data.List.NonEmpty (NonEmpty(..))

-- $dslTutorial
--
-- This module provides a way of constructing bot commands in a declarative way.
--
-- The main component of this is the 'command' function, which takes a
-- type-level list of command parameters, the name, and the callback and
-- produces a command. There are also the alternatives 'command'', 'commandA'
-- and 'commandA'', for when you want to handle parsing of the input yourself,
-- and/or want aliases of the command.
--
-- The functions: 'hide', 'help', 'requires', and 'group' can be used to change
-- attributes of any commands declared inside the monadic action passed to them,
-- for example:
--
-- @
-- 'hide' '$' do
--   'command' \@'[] "test" \\ctx -> 'pure' ()
-- @
--
-- In the above block, any command declared inside 'hide' will have it's
-- \'hidden\' flag set and will not be shown by the default help command:
-- 'Calamity.Commands.Help.helpCommand'
--
-- The 'Calamity.Commands.Help.helpCommand' function can be used to create a
-- help command for the commands DSL action it is used in, read it's doc page
-- for more information on how it functions.
--
-- The 'Calamity.Commands.Utils.addCommands' function creates the command
-- handler for the commands registered in the passed action, it is what reads a
-- message to determine what command was invoked. It should be used to register the
-- commands with the bot by using it inside the setup action, for example:
--
-- @
-- 'Calamity.Client.runBotIO' ('Calamity.BotToken' token)
--   $ 'Calamity.Commands.Utils.addCommands' $ do
--     'Calamity.Commands.Help.helpCommand'
--     'Calamity.Commands.Dsl.command' \@'[] "test" \\ctx ->
--       'Control.Monad.void' $ 'Calamity.Types.Tellable.tell' \@'L.Text' ctx "hi"
-- @
--
-- The above block will create a command with no parameters named \'test\',
-- along with a help command.

type DSLState r =
  ( LocalWriter (LH.HashMap S.Text (Command, AliasType))
      ': LocalWriter (LH.HashMap S.Text (Group, AliasType))
      ': P.Reader (Maybe Group)
      ': P.Tagged "hidden" (P.Reader Bool)
      ': P.Reader (Context -> L.Text)
      ': P.Tagged "original-help" (P.Reader (Context -> L.Text))
      ': P.Reader [Check]
      ': P.Reader CommandHandler
      ': P.Fixpoint
      ': r
  )

raiseDSL :: P.Sem r a -> P.Sem (DSLState r) a
raiseDSL = P.raise . P.raise . P.raise . P.raise . P.raise . P.raise . P.raise . P.raise . P.raise

-- | Given the command name and parameter names, @parser@ and @callback@ for a
-- command in the 'P.Sem' monad, build a command by transforming the Polysemy
-- actions into IO actions. Then register the command.
--
-- The parent group, visibility, checks, and command help are drawn from the
-- reader context.
command'
  :: P.Member (P.Final IO) r
  => S.Text
  -- ^ The name of the command
  -> [S.Text]
  -- ^ The names of the command's parameters
  -> (Context -> P.Sem r (Either CommandError a))
  -- ^ The parser for this command
  -> ((Context, a) -> P.Sem (P.Fail ': r) ())
  -- ^ The callback for this command
  -> P.Sem (DSLState r) Command
command' name params parser cb = commandA' name [] params parser cb

-- | Given the command name, aliases, and parameter names, @parser@ and
-- @callback@ for a command in the 'P.Sem' monad, build a command by
-- transforming the Polysemy actions into IO actions. Then register the command.
--
-- The parent group, visibility, checks, and command help are drawn from the
-- reader context.
commandA'
  :: P.Member (P.Final IO) r
  => S.Text
  -- ^ The name of the command
  -> [S.Text]
  -- ^ The aliases for the command
  -> [S.Text]
  -- ^ The names of the command's parameters
  -> (Context -> P.Sem r (Either CommandError a))
  -- ^ The parser for this command
  -> ((Context, a) -> P.Sem (P.Fail ': r) ())
  -- ^ The callback for this command
  -> P.Sem (DSLState r) Command
commandA' name aliases params parser cb = do
  parent <- P.ask @(Maybe Group)
  hidden <- P.tag $ P.ask @Bool
  checks <- P.ask @[Check]
  help' <- P.ask @(Context -> L.Text)
  cmd <- raiseDSL $ buildCommand' (name :| aliases) parent hidden checks params help' parser cb
  ltell $ LH.singleton name (cmd, Original)
  ltell $ LH.fromList [(name, (cmd, Alias)) | name <- aliases]
  pure cmd

-- | Given the name of a command and a callback, and a type level list of
-- the parameters, build and register a command.
--
-- The parent group, visibility, checks, and command help are drawn from the
-- reader context.
--
-- Command parameters are parsed by first invoking
-- 'Calamity.Commands.Parser.parse' for the first
-- 'Calamity.Commands.Parser.Parser', then running the next parser on the
-- remaining input, and so on.
--
-- ==== Examples
--
-- Building a command that bans a user by id.
--
-- @
-- 'command' \@\'['Calamity.Commands.Parser.Named' "user" ('Calamity.Types.Snowflake' 'Calamity.Types.Model.User'),
--                'Calamity.Commands.Parser.Named' "reason" ('Calamity.Commands.Parser.KleeneStarConcat' 'S.Text')]
--    "ban" $ \\ctx uid r -> case (ctx 'Control.Lens.^.' #guild) of
--      'Just' guild -> do
--        'Control.Monad.void' . 'Calamity.HTTP.invoke' $ 'Calamity.HTTP.Guild.CreateGuildBan' guild uid ('Calamity.HTTP.Guild.CreateGuildBanData' 'Nothing' $ 'Just' r)
--        'Control.Monad.void' $ 'Calamity.Types.Tellable.tell' ctx ("Banned user `" '<>' 'TextShow.showt' uid '<>' "` with reason: " '<>' r)
--      'Nothing' -> 'void' $ 'Calamity.Types.Tellable.tell' @'L.Text' ctx "Can only ban users from guilds."
-- @
command :: forall ps r.
        ( P.Member (P.Final IO) r,
          TypedCommandC ps r)
        => S.Text
        -- ^ The name of the command
        -> (Context -> CommandForParsers ps r)
        -- ^ The callback for this command
        -> P.Sem (DSLState r) Command
command name cmd = commandA @ps name [] cmd

-- | Given the name and aliases of a command and a callback, and a type level list of
-- the parameters, build and register a command.
--
-- The parent group, visibility, checks, and command help are drawn from the
-- reader context.
--
-- ==== Examples
--
-- Building a command that bans a user by id.
--
-- @
-- 'commandA' \@\'['Calamity.Commands.Parser.Named' "user" ('Calamity.Types.Snowflake' 'Calamity.Types.Model.User'),
--                'Calamity.Commands.Parser.Named' "reason" ('Calamity.Commands.Parser.KleeneStarConcat' 'S.Text')]
--    "ban" [] $ \\ctx uid r -> case (ctx 'Control.Lens.^.' #guild) of
--      'Just' guild -> do
--        'Control.Monad.void' . 'Calamity.HTTP.invoke' $ 'Calamity.HTTP.Guild.CreateGuildBan' guild uid ('Calamity.HTTP.Guild.CreateGuildBanData' 'Nothing' $ 'Just' r)
--        'Control.Monad.void' $ 'Calamity.Types.Tellable.tell' ctx ("Banned user `" '<>' 'TextShow.showt' uid '<>' "` with reason: " '<>' r)
--      'Nothing' -> 'void' $ 'Calamity.Types.Tellable.tell' @'L.Text' ctx "Can only ban users from guilds."
-- @
commandA :: forall ps r.
        ( P.Member (P.Final IO) r,
          TypedCommandC ps r)
        => S.Text
        -- ^ The name of the command
        -> [S.Text]
        -- ^ The aliases for the command
        -> (Context -> CommandForParsers ps r)
        -- ^ The callback for this command
        -> P.Sem (DSLState r) Command
commandA name aliases cmd = do
  parent <- P.ask @(Maybe Group)
  hidden <- P.tag $ P.ask @Bool
  checks <- P.ask @[Check]
  help' <- P.ask @(Context -> L.Text)
  cmd' <- raiseDSL $ buildCommand @ps (name :| aliases) parent hidden checks help' cmd
  ltell $ LH.singleton name (cmd', Original)
  ltell $ LH.fromList [(name, (cmd', Alias)) | name <- aliases]
  pure cmd'

-- | Set the visibility of any groups or commands registered inside the given
-- action to hidden.
hide :: P.Member (P.Tagged "hidden" (P.Reader Bool)) r
     => P.Sem r a
     -> P.Sem r a
hide = P.tag @"hidden" . P.local @Bool (const True) . P.raise

-- | Set the help for any groups or commands registered inside the given action.
help :: P.Member (P.Reader (Context -> L.Text)) r
     => (Context -> L.Text)
     -> P.Sem r a
     -> P.Sem r a
help = P.local . const

-- | Add to the list of checks for any commands registered inside the given
-- action.
requires :: [Check]
         -> P.Sem (DSLState r) a
         -> P.Sem (DSLState r) a
requires = P.local . (<>)

-- | Construct a check and add it to the list of checks for any commands
-- registered inside the given action.
--
-- Refer to 'Calamity.Commands.Check.Check' for more info on checks.
requires' :: P.Member (P.Final IO) r
          => S.Text
          -- ^ The name of the check
          -> (Context -> P.Sem r (Maybe L.Text))
          -- ^ The callback for the check
          -> P.Sem (DSLState r) a
          -> P.Sem (DSLState r) a
requires' name cb m = do
  check <- raiseDSL $ buildCheck name cb
  requires [check] m

-- | Construct some pure checks and add them to the list of checks for any
-- commands registered inside the given action.
--
-- Refer to 'Calamity.Commands.Check.Check' for more info on checks.
requiresPure :: [(S.Text, Context -> Maybe L.Text)]
             -- A list of check names and check callbacks
             -> P.Sem (DSLState r) a
             -> P.Sem (DSLState r) a
requiresPure checks = requires $ map (uncurry buildCheckPure) checks

-- | Construct a group and place any commands registered in the given action
-- into the new group.
--
-- This also resets the @help@ function back to it's original value, use
-- 'group'' if you don't want that (i.e. your help function is context aware).
group :: P.Member (P.Final IO) r
      => S.Text
      -- ^ The name of the group
      -> P.Sem (DSLState r) a
      -> P.Sem (DSLState r) a
group name m = groupA name [] m

-- | Construct a group with aliases and place any commands registered in the
-- given action into the new group.
--
-- The parent group, visibility, checks, and command help are drawn from the
-- reader context.
--
-- This also resets the @help@ function back to it's original value, use
-- 'group'' if you don't want that (i.e. your help function is context aware).
groupA :: P.Member (P.Final IO) r
       => S.Text
       -- ^ The name of the group
       -> [S.Text]
       -- ^ The aliases of the group
       -> P.Sem (DSLState r) a
       -> P.Sem (DSLState r) a
groupA name aliases m = mdo
  parent <- P.ask @(Maybe Group)
  hidden <- P.tag $ P.ask @Bool
  checks <- P.ask @[Check]
  help'  <- P.ask @(Context -> L.Text)
  origHelp <- fetchOrigHelp
  let group' = Group (name :| aliases) parent hidden commands children help' checks
  (children, (commands, res)) <- llisten @(LH.HashMap S.Text (Group, AliasType)) $
                                 llisten @(LH.HashMap S.Text (Command, AliasType)) $
                                 P.local @(Maybe Group) (const $ Just group') $
                                 P.local @(Context -> L.Text) (const origHelp) m
  ltell $ LH.singleton name (group', Original)
  ltell $ LH.fromList [(name, (group', Alias)) | name <- aliases]
  pure res

fetchOrigHelp :: P.Member (P.Tagged "original-help" (P.Reader (Context -> L.Text))) r => P.Sem r (Context -> L.Text)
fetchOrigHelp = P.tag P.ask

-- | Construct a group and place any commands registered in the given action
-- into the new group.
--
-- The parent group, visibility, checks, and command help are drawn from the
-- reader context.
--
-- Unlike 'help' this doesn't reset the @help@ function back to it's original
-- value.
group' :: P.Member (P.Final IO) r
         => S.Text
         -- The name of the group
         -> P.Sem (DSLState r) a
         -> P.Sem (DSLState r) a
group' name m = groupA' name [] m

-- | Construct a group with aliases and place any commands registered in the given action
-- into the new group.
--
-- The parent group, visibility, checks, and command help are drawn from the
-- reader context.
--
-- Unlike 'help' this doesn't reset the @help@ function back to it's original
-- value.
groupA' :: P.Member (P.Final IO) r
         => S.Text
         -- ^ The name of the group
         -> [S.Text]
         -- ^ The aliases of the group
         -> P.Sem (DSLState r) a
         -> P.Sem (DSLState r) a
groupA' name aliases m = mdo
  parent <- P.ask @(Maybe Group)
  hidden <- P.tag $ P.ask @Bool
  checks <- P.ask @[Check]
  help'  <- P.ask @(Context -> L.Text)
  let group' = Group (name :| aliases) parent hidden commands children help' checks
  (children, (commands, res)) <- llisten @(LH.HashMap S.Text (Group, AliasType)) $
                                 llisten @(LH.HashMap S.Text (Command, AliasType)) $
                                 P.local @(Maybe Group) (const $ Just group') m
  ltell $ LH.singleton name (group', Original)
  ltell $ LH.fromList [(name, (group', Alias)) | name <- aliases]
  pure res

-- | Retrieve the final command handler for this block
fetchHandler :: P.Sem (DSLState r) CommandHandler
fetchHandler = P.ask
