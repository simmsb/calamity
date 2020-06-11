-- | A default help command implementation
module Calamity.Commands.Help
    ( helpCommand'
    , helpCommand ) where

import           Calamity.Client.Types
import           Calamity.Commands.AliasType
import           Calamity.Commands.Check
import           Calamity.Commands.Command
import           Calamity.Commands.CommandUtils
import           Calamity.Commands.Context
import           Calamity.Commands.Dsl
import           Calamity.Commands.Group
import           Calamity.Commands.Handler
import           Calamity.Internal.LocalWriter
import           Calamity.Types.Tellable

import           Control.Applicative
import           Control.Lens hiding ( Context(..) )
import           Control.Monad

import qualified Data.HashMap.Lazy              as LH
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty             as NE
import qualified Data.Text                      as S
import qualified Data.Text.Lazy                 as L

import qualified Polysemy                       as P
import qualified Polysemy.Fail                  as P
import qualified Polysemy.Reader                as P
import Data.Maybe (mapMaybe)

data CommandOrGroup
  = Command' Command
  | Group' Group [S.Text]

helpCommandHelp :: Context -> L.Text
helpCommandHelp _ = "Show help for a command or group."

helpForCommand :: Context -> Command -> L.Text
helpForCommand ctx (cmd@Command { names, checks, help }) = "```\nUsage: " <> prefix' <> path' <> " " <> params' <> "\n" <>
                                                           aliasesFmt <>
                                                           checksFmt <>
                                                           help ctx <> "\n```"
  where prefix' = ctx ^. #prefix
        path'   = L.unwords . map L.fromStrict $ commandPath cmd
        params' = commandParams cmd
        aliases = map L.fromStrict $ NE.tail names
        checks' = map L.fromStrict . map (^. #name) $ checks
        aliasesFmt = if null aliases then "" else  "Aliases: " <> L.unwords aliases <> "\n"
        checksFmt = if null checks' then "" else "Checks: " <> L.unwords checks' <> "\n\n"

fmtCommandWithParams :: Command -> L.Text
fmtCommandWithParams cmd@Command { names } = formatWithAliases names <> " " <> commandParams cmd

formatWithAliases :: NonEmpty S.Text -> L.Text
formatWithAliases (name :| aliases) = L.fromStrict name <> aliasesFmt
  where
    aliasesFmt = case aliases of
      [] -> ""
      aliases' -> "[" <> L.intercalate "|" (map L.fromStrict aliases') <> "]"

onlyOriginals :: [(a, AliasType)] -> [a]
onlyOriginals = mapMaybe inner
  where inner (_, Alias) = Nothing
        inner (a, Original) = Just a

helpForGroup :: Context -> Group -> L.Text
helpForGroup ctx grp = "```\nGroup: " <> path' <> "\n" <>
                       aliasesFmt <>
                       checksFmt <>
                       (grp ^. #help) ctx <> "\n" <>
                       groupsMsg <> commandsMsg <> "\n```"
  where path' = L.fromStrict . S.unwords $ groupPath grp
        groups =  onlyOriginals . LH.elems $ grp ^. #children
        commands = onlyOriginals . LH.elems $ grp ^. #commands
        groupsFmt = map formatWithAliases (groups ^.. traverse . #names)
        groupsMsg = if null groups then "" else "The following child groups exist:\n" <> (L.unlines . map ("- " <>) $ groupsFmt)
        commandsMsg = if null commands then "" else "\nThe following child commands exist:\n" <> (L.unlines . map ("- " <>) . map fmtCommandWithParams $ commands)
        aliases = map L.fromStrict . NE.tail $ grp ^. #names
        checks' = map L.fromStrict . map (^. #name) $ grp ^. #checks
        aliasesFmt = if null aliases then "" else  "Aliases: " <> L.unwords aliases <> "\n"
        checksFmt = if null checks' then "" else "Checks: " <> L.unwords checks' <> "\n\n"

rootHelp :: CommandHandler -> L.Text
rootHelp handler = "```\n" <> groupsMsg <> commandsMsg <> "\n```"
  where groups =  onlyOriginals . LH.elems $ handler ^. #groups
        commands = onlyOriginals . LH.elems $ handler ^. #commands
        groupsFmt = map formatWithAliases (groups ^.. traverse . #names)
        groupsMsg = if null groups then "" else "The following groups exist:\n" <> (L.unlines . map ("- " <>) $ groupsFmt)
        commandsMsg = if null commands then "" else "\nThe following commands exist:\n" <> (L.unlines . map ("- " <>) . map fmtCommandWithParams $ commands)

helpCommandCallback :: BotC r => CommandHandler -> Context -> [S.Text] -> P.Sem (P.Fail ': r) ()
helpCommandCallback handler ctx path = do
  case findCommandOrGroup handler path of
    Just (Command' cmd@Command { names }) ->
      void $ tell @L.Text ctx $ "Help for command `" <> L.fromStrict (NE.head names) <> "`: \n" <> helpForCommand ctx cmd
    Just (Group' grp@Group { names } remainingPath) ->
      let failedMsg = if null remainingPath
            then ""
            else "No command or group with the path: `" <> L.fromStrict (S.unwords path) <> "` exists for the group: `" <> L.fromStrict (NE.head names) <> "`\n"
      in void $ tell @L.Text ctx $ failedMsg <> "Help for group `" <> L.fromStrict (NE.head names) <> "`: \n" <> helpForGroup ctx grp
    Nothing -> let failedMsg = if null path
                     then ""
                     else "No command or group with the path: `" <> L.fromStrict (S.unwords path) <> "` was found.\n"
               in void $ tell @L.Text ctx $ failedMsg <> rootHelp handler

-- | Given a 'CommandHandler', optionally a parent 'Group', and a list of 'Check's,
-- construct a help command that will provide help for all the commands and
-- groups in the passed 'CommandHandler'.
helpCommand' :: BotC r => CommandHandler -> Maybe Group -> [Check] -> P.Sem r Command
helpCommand' handler parent checks = buildCommand @'[[S.Text]] ("help" :| []) parent checks helpCommandHelp
  (helpCommandCallback handler)

-- | Create and register the default help command for all the commands
-- registered in the commands DSL this is used in.
helpCommand :: BotC r => P.Sem (DSLState r) Command
helpCommand = do
  handler <- P.ask @CommandHandler
  parent <- P.ask @(Maybe Group)
  checks <- P.ask @[Check]
  cmd <- raiseDSL $ helpCommand' handler parent checks
  ltell $ LH.singleton "help" (cmd, Original)
  pure cmd

findCommandOrGroup :: CommandHandler -> [S.Text] -> Maybe CommandOrGroup
findCommandOrGroup handler path = go (handler ^. #commands, handler ^. #groups) path
  where go :: (LH.HashMap S.Text (Command, AliasType), LH.HashMap S.Text (Group, AliasType))
           -> [S.Text]
           -> Maybe CommandOrGroup
        go (commands, groups) (x : xs) =
          case LH.lookup x commands of
            Just (cmd, _) -> Just (Command' cmd)
            Nothing       -> case LH.lookup x groups of
              Just (group, _) -> go (group ^. #commands, group ^. #children) xs <|> Just (Group' group xs)
              Nothing         -> Nothing
        go _ [] = Nothing
