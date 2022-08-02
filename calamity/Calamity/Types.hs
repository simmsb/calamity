-- | All user facing types
module Calamity.Types (
  module Calamity.Types.Model,
  module Calamity.Types.Partial,
  module Calamity.Types.Snowflake,
  module Calamity.Types.Token,
  module Calamity.Types.Tellable,
  module Calamity.Types.Upgradeable,
  module Calamity.Types.CDNAsset,
  module Calamity.Types.TokenEff,
  module Calamity.Types.LogEff,

  -- * Types
  -- $typesDocs
) where

import Calamity.Types.CDNAsset
import Calamity.Types.LogEff
import Calamity.Types.Model
import Calamity.Types.Partial
import Calamity.Types.Snowflake
import Calamity.Types.Tellable
import Calamity.Types.Token
import Calamity.Types.TokenEff
import Calamity.Types.Upgradeable

{- $typesDocs

 This module collects all discord models, and other useful types together.

 The 'Tellable' class allows you to construct and send
 messages to things to you can send messages to in a neat way.

 The 'Upgradeable' class allows you to upgrade a snowflake to the full value
 it refers to.

 The 'CDNAsset' class allows you to fetch assets from the discord CDN.
-}
