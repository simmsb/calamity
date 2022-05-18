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

import qualified CalamityCommands.Check as CC
import qualified CalamityCommands.Command as CC
import qualified CalamityCommands.Dsl as CC
import qualified CalamityCommands.Group as CC
import qualified CalamityCommands.Handler as CC
import qualified CalamityCommands.Context as CC

type Command c = CC.Command IO c ()
type Group c = CC.Group IO c ()
type CommandHandler c = CC.CommandHandler IO c ()
type Check c = CC.Check IO c
type DSLState c r = CC.DSLState IO c () r
type DSLC c r = CC.DSLC IO c () r
type CommandContext c = CC.CommandContext IO c ()
