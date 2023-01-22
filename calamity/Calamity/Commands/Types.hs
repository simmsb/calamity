{- | "CalamityCommands" types with their types filled in

 If you're importing "CalamityCommands" make sure these get used instead of
 the more generic variants.
-}
module Calamity.Commands.Types (
  type Command,
  type Group,
  type CommandHandler,
  type Check,
  type DSLState,
  type DSLC,
  type CommandContext,
) where

import CalamityCommands.Check qualified as CC
import CalamityCommands.Command qualified as CC
import CalamityCommands.Context qualified as CC
import CalamityCommands.Dsl qualified as CC
import CalamityCommands.Group qualified as CC
import CalamityCommands.Handler qualified as CC

type Command c = CC.Command IO c ()
type Group c = CC.Group IO c ()
type CommandHandler c = CC.CommandHandler IO c ()
type Check c = CC.Check IO c
type DSLState c r = CC.DSLState IO c () r
type DSLC c r = CC.DSLC IO c () r
type CommandContext c = CC.CommandContext IO c ()
