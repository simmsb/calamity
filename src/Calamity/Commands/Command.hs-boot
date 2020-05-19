-- | Commands and stuff
module Calamity.Commands.Command
    ( Command ) where

import TextShow

data Command

instance Show Command
instance TextShow Command
