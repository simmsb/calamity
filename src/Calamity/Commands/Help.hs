-- | A default help command implementation
module Calamity.Commands.Help
    ( helpCommand'
    , helpCommand ) where

import           Calamity.Client.Types
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
import qualified Data.Text                      as S
import qualified Data.Text.Lazy                 as L

import qualified Polysemy                       as P
import qualified Polysemy.Fail                  as P
import qualified Polysemy.Reader                as P

data CommandOrGroup
  = Command' Command
  | Group' Group [S.Text]

helpCommandHelp :: Context -> L.Text
helpCommandHelp _ = "Show help for a command or group."

groupPath :: Group -> [S.Text]
groupPath grp = maybe [] groupPath (grp ^. #parent) ++ [grp ^. #name]

commandParams :: Command -> L.Text
commandParams Command { params } = L.fromStrict $ S.intercalate " " params

helpForCommand :: Context -> Command -> L.Text
helpForCommand ctx (cmd@Command { name, parent, help }) = "```\nUsage: " <> prefix' <> path' <> " " <> params' <> "\n\n" <> help ctx <> "\n```"
  where prefix' = ctx ^. #prefix
        path'   = L.fromStrict . S.intercalate " " $ maybe [] groupPath parent ++ [name]
        params' = commandParams cmd

fmtCommandWithParams :: Command -> L.Text
fmtCommandWithParams cmd@Command { name } = L.fromStrict name <> " " <> commandParams cmd

helpForGroup :: Context -> Group -> L.Text
helpForGroup ctx grp = "```\nGroup: " <> path' <> "\n\n" <> (grp ^. #help) ctx <> "\n" <> groupsMsg <> commandsMsg <> "\n```"
  where path' = L.fromStrict . S.intercalate " " $ groupPath grp
        groups = LH.keys $ grp ^. #children
        commands = LH.elems $ grp ^. #commands
        groupsMsg = if null groups then "" else "The following child groups exist:\n" <> L.fromStrict (S.unlines . map ("- " <>) $ groups)
        commandsMsg = if null commands then "" else "\nThe following child commands exist:\n" <> (L.unlines . map ("- " <>) . map fmtCommandWithParams $ commands)

rootHelp :: CommandHandler -> L.Text
rootHelp handler = "```\n" <> groupsMsg <> commandsMsg <> "\n```"
  where groups = LH.keys $ handler ^. #groups
        commands = LH.keys $ handler ^. #commands
        groupsMsg = if null groups then "" else "The following groups exist:\n" <> L.fromStrict (S.unlines . map ("- " <>) $ groups)
        commandsMsg = if null commands then "" else "\nThe following commands exist:\n" <> L.fromStrict (S.unlines . map ("- " <>) $ commands)

-- TODO: process checks

helpCommandCallback :: BotC r => CommandHandler -> Context -> [S.Text] -> P.Sem (P.Fail ': r) ()
helpCommandCallback handler ctx path = do
  case findCommandOrGroup handler path of
    Just (Command' cmd@Command { name }) ->
      void $ tell @L.Text ctx $ "Help for command `" <> L.fromStrict name <> "`: \n" <> helpForCommand ctx cmd
    Just (Group' grp@Group { name } remainingPath) ->
      let failedMsg = if null remainingPath
            then ""
            else "No command or group with the path: `" <> L.fromStrict (S.intercalate " " path) <> "` exists for the group: `" <> L.fromStrict name <> "`\n"
      in void $ tell @L.Text ctx $ failedMsg <> "Help for group `" <> L.fromStrict name <> "`: \n" <> helpForGroup ctx grp
    Nothing -> let failedMsg = if null path
                     then ""
                     else "No command or group with the path: `" <> L.fromStrict (S.intercalate " " path) <> "` was found.\n"
               in void $ tell @L.Text ctx $ failedMsg <> rootHelp handler

helpCommand' :: BotC r => CommandHandler -> Maybe Group -> [Check] -> P.Sem r Command
helpCommand' handler parent checks = buildCommand @'[[S.Text]] "help" parent checks helpCommandHelp
  (helpCommandCallback handler)

helpCommand :: BotC r => P.Sem (DSLState r) Command
helpCommand = do
  handler <- P.ask @CommandHandler
  parent <- P.ask @(Maybe Group)
  checks <- P.ask @[Check]
  cmd <- raiseDSL $ helpCommand' handler parent checks
  ltell $ LH.singleton "help" cmd
  pure cmd

findCommandOrGroup :: CommandHandler -> [S.Text] -> Maybe CommandOrGroup
findCommandOrGroup handler path = go (handler ^. #commands, handler ^. #groups) path
  where go :: (LH.HashMap S.Text Command, LH.HashMap S.Text Group) -> [S.Text] -> Maybe CommandOrGroup
        go (commands, groups) (x : xs) =
          case LH.lookup x commands of
            Just cmd -> Just (Command' cmd)
            Nothing  -> case LH.lookup x groups of
              Just group -> go (group ^. #commands, group ^. #children) xs <|> Just (Group' group xs)
              Nothing    -> Nothing
        go _ [] = Nothing
