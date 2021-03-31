-- | All user facing types
module Calamity.Types
    ( module Calamity.Types.Model
    , module Calamity.Types.Partial
    , module Calamity.Types.Snowflake
    , module Calamity.Types.Token
    , module Calamity.Types.Tellable
    , module Calamity.Types.Upgradeable
    , module Calamity.Types.UnixTimestamp
    -- * Types
    -- $typesDocs
    ) where

import           Calamity.Types.Model
import           Calamity.Types.Partial
import           Calamity.Types.Snowflake
import           Calamity.Types.Token
import           Calamity.Types.Tellable
import           Calamity.Types.Upgradeable
import           Calamity.Types.UnixTimestamp

-- $typesDocs
--
-- This module collects all discord models, and other useful types together.
--
-- The 'Tellable' class allows you to construct and send
-- messages to things to you can send messages to in a neat way.
--
-- The 'Upgradeable' class allows you to upgrade a snowflake to the full value
-- it refers to.
