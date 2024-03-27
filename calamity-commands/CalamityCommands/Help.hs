{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

-- | A default help command implementation
module CalamityCommands.Help (
  helpCommand',
  helpCommand,
) where

import CalamityCommands.AliasType
import CalamityCommands.Check
import CalamityCommands.Command
import CalamityCommands.CommandUtils
import CalamityCommands.Context
import CalamityCommands.Dsl
import CalamityCommands.Group
import CalamityCommands.Handler
import CalamityCommands.Internal.LocalWriter
import CalamityCommands.ParameterInfo
import Control.Applicative
import Data.HashMap.Lazy qualified as LH
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Optics
import Polysemy qualified as P
import Polysemy.Fail qualified as P
import Polysemy.Reader qualified as P

data CommandOrGroup m c a
  = Command' (Command m c a)
  | Group' (Group m c a) [T.Text]

parameterTypeHelp :: [ParameterInfo] -> T.Text
parameterTypeHelp pinfo =
  let dedup = LH.toList . LH.fromList $ map (\(ParameterInfo _ t d) -> (t, d)) pinfo
      typeDescs = T.unlines ["- " <> T.pack (show t) <> ": " <> d | (t, d) <- dedup]
   in if null dedup
        then ""
        else "Types:\n" <> typeDescs <> "\n"

helpCommandHelp :: c -> T.Text
helpCommandHelp _ = "Show help for a command or group."

helpForCommand :: (CommandContext m c a) => c -> Command m c a -> T.Text
helpForCommand ctx cmd@Command {names, checks, help, params} =
  "Usage: "
    <> prefix'
    <> path'
    <> " "
    <> params'
    <> "\n"
    <> aliasesFmt
    <> checksFmt
    <> parameterTypeHelp params
    <> help ctx
  where
    prefix' = ctxPrefix ctx
    path' = T.unwords $ commandPath cmd
    params' = commandParams cmd
    aliases = NE.tail names
    checks' = map (^. #name) checks
    aliasesFmt =
      if null aliases
        then ""
        else "Aliases: " <> T.unwords aliases <> "\n"
    checksFmt =
      if null checks'
        then ""
        else "Checks: " <> T.unwords checks' <> "\n\n"

fmtCommandWithParams :: Command m c a -> T.Text
fmtCommandWithParams cmd@Command {names} = formatWithAliases names <> " " <> commandParams cmd

formatWithAliases :: NonEmpty T.Text -> T.Text
formatWithAliases (name :| aliases) = name <> aliasesFmt
  where
    aliasesFmt = case aliases of
      [] -> ""
      aliases' -> "[" <> T.intercalate "|" aliases' <> "]"

onlyOriginals :: [(a, AliasType)] -> [a]
onlyOriginals = mapMaybe inner
  where
    inner (_, Alias) = Nothing
    inner (a, Original) = Just a

onlyVisibleC :: [Command m c a] -> [Command m c a]
onlyVisibleC = mapMaybe notHiddenC

onlyVisibleG :: [Group m c a] -> [Group m c a]
onlyVisibleG = mapMaybe notHiddenG

helpForGroup :: (CommandContext m c a) => c -> Group m c a -> T.Text
helpForGroup ctx grp =
  "Group: "
    <> path'
    <> "\n"
    <> aliasesFmt
    <> checksFmt
    <> (grp ^. #help) ctx
    <> "\n"
    <> groupsMsg
    <> commandsMsg
  where
    path' = T.unwords $ groupPath grp
    groups = onlyVisibleG . onlyOriginals . LH.elems $ grp ^. #children
    commands = onlyVisibleC . onlyOriginals . LH.elems $ grp ^. #commands
    groupsFmt = map formatWithAliases (groups ^.. traversed % #names)
    groupsMsg =
      if null groups
        then ""
        else "The following child groups exist:\n" <> (T.unlines . map ("- " <>) $ groupsFmt)
    commandsMsg =
      if null commands
        then ""
        else "\nThe following child commands exist:\n" <> (T.unlines . map (("- " <>) . fmtCommandWithParams) $ commands)
    aliases = NE.tail $ grp ^. #names
    checks' = map (^. #name) $ grp ^. #checks
    aliasesFmt =
      if null aliases
        then ""
        else "Aliases: " <> T.unwords aliases <> "\n"
    checksFmt =
      if null checks'
        then ""
        else "Checks: " <> T.unwords checks' <> "\n\n"

rootHelp :: CommandHandler m c a -> T.Text
rootHelp handler = groupsMsg <> commandsMsg
  where
    groups = onlyVisibleG . onlyOriginals . LH.elems $ handler ^. #groups
    commands = onlyVisibleC . onlyOriginals . LH.elems $ handler ^. #commands
    groupsFmt = map formatWithAliases (groups ^.. traversed % #names)
    groupsMsg =
      if null groups
        then ""
        else "The following groups exist:\n" <> (T.unlines . map ("- " <>) $ groupsFmt)
    commandsMsg =
      if null commands
        then ""
        else "\nThe following commands exist:\n" <> (T.unlines . map (("- " <>) . fmtCommandWithParams) $ commands)

renderHelp :: (CommandContext m c a) => CommandHandler m c a -> c -> [T.Text] -> T.Text
renderHelp handler ctx path =
  case findCommandOrGroup handler path of
    Just (Command' cmd@Command {names}) ->
      "Help for command `" <> NE.head names <> "`: \n" <> helpForCommand ctx cmd
    Just (Group' grp@Group {names} remainingPath) ->
      let failedMsg =
            if null remainingPath
              then ""
              else "No command or group with the path: `" <> T.unwords remainingPath <> "` exists for the group: `" <> NE.head names <> "`\n"
       in failedMsg <> "Help for group `" <> NE.head names <> "`: \n" <> helpForGroup ctx grp
    Nothing ->
      let failedMsg =
            if null path
              then ""
              else "No command or group with the path: `" <> T.unwords path <> "` was found.\n"
       in failedMsg <> rootHelp handler

{- | Given a 'CommandHandler', optionally a parent 'Group', and a list of 'Check's,
 construct a help command that will provide help for all the commands and
 groups in the passed 'CommandHandler'.
-}
helpCommand' ::
  (Monad m, P.Member (P.Final m) r, CommandContext m c a) =>
  CommandHandler m c a ->
  Maybe (Group m c a) ->
  [Check m c] ->
  (c -> T.Text -> P.Sem (P.Fail ': r) a) ->
  P.Sem r (Command m c a)
helpCommand' handler parent checks render =
  buildCommand @'[[T.Text]]
    ("help" :| [])
    parent
    False
    checks
    helpCommandHelp
    (\ctx path -> render ctx $ renderHelp handler ctx path)

{- | Create and register the default help command for all the commands registered
 in the commands DSL this is used in. The @render@ parameter is used so you can
 determine how the help should be rendered, for example it could be
 @'putStrLn'@, or a pure function such as @'pure' . 'Left'@

 The registered command will have the name \'help\', called with no parameters
 it will render help for the top-level groups and commands, for example:

 @
 The following groups exist:
 - reanimate
 - prefix[prefixes]
 - alias[aliases]
 - remind[reminder|reminders]

 The following commands exist:
 - help :[Text]
 @

 Both commands and groups are listed in the form: @\<name\>[\<alias 0\>|\<alias 1\>]@,
 commands also have their parameter list shown.

 If a path to a group is passed, the help, aliases, and pre-invokation checks
 will be listed, along with the subgroups and commands, For example:

 @
 Help for group remind:
 Group: remind
 Aliases: reminder reminders
 Commands related to making reminders

 The following child commands exist:
 - list
 - remove reminder_id:Text
 - add :KleenePlusConcat Text
 @

 If a command path is passed, the usage, checks and help for the command are
 shown, for example:

 @
 Help for command add:
 Usage: c!prefix add prefix:Text
 Checks: prefixLimit guildOnly

 Add a new prefix
 @
-}
helpCommand ::
  forall c m a r.
  (Monad m, P.Member (P.Final m) r, CommandContext m c a) =>
  (c -> T.Text -> P.Sem (P.Fail ': r) a) ->
  P.Sem (DSLState m c a r) (Command m c a)
helpCommand render = do
  handler <- P.ask @(CommandHandler m c a)
  parent <- P.ask @(Maybe (Group m c a))
  checks <- P.ask @[Check m c]
  cmd <- raiseDSL $ helpCommand' handler parent checks render
  ltell $ LH.singleton "help" (cmd, Original)
  pure cmd

notHiddenC :: Command m c a -> Maybe (Command m c a)
notHiddenC c@Command {hidden} = if hidden then Nothing else Just c

notHiddenG :: Group m c a -> Maybe (Group m c a)
notHiddenG g@Group {hidden} = if hidden then Nothing else Just g

findCommandOrGroup :: CommandHandler m c a -> [T.Text] -> Maybe (CommandOrGroup m c a)
findCommandOrGroup handler = go (handler ^. #commands, handler ^. #groups)
  where
    go ::
      (LH.HashMap T.Text (Command m c a, AliasType), LH.HashMap T.Text (Group m c a, AliasType)) ->
      [T.Text] ->
      Maybe (CommandOrGroup m c a)
    go (commands, groups) (x : xs) =
      case LH.lookup x commands of
        Just (notHiddenC -> Just cmd, _) -> Just (Command' cmd)
        _ -> case LH.lookup x groups of
          Just (notHiddenG -> Just group, _) -> go (group ^. #commands, group ^. #children) xs <|> Just (Group' group xs)
          _ -> Nothing
    go _ [] = Nothing
