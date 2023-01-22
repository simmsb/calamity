-- | A default help command implementation
module Calamity.Commands.Help (
  helpCommand',
  helpCommand,
) where

import Calamity.Client.Types (BotC)
import Calamity.Commands.Types
import Calamity.Types.Tellable
import CalamityCommands.Help qualified as CC
import Control.Monad (void)
import Polysemy qualified as P

{- | Given a 'CommandHandler', optionally a parent 'Group', and a list of 'Check's,
 construct a help command that will provide help for all the commands and
 groups in the passed 'CommandHandler'.
-}
helpCommand' :: (Tellable c, BotC r, CommandContext c) => CommandHandler c -> Maybe (Group c) -> [Check c] -> P.Sem r (Command c)
helpCommand' handler parent checks = CC.helpCommand' handler parent checks (\a b -> void $ tell a b)

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
helpCommand :: (Tellable c, BotC r, CommandContext c) => P.Sem (DSLState c r) (Command c)
helpCommand = CC.helpCommand (\a b -> void $ tell a b)
