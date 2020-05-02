-- | Guild ban objects
module Calamity.Types.Model.Guild.Ban
    ( BanData(..) ) where

import           Calamity.Internal.AesonThings
import           Calamity.Internal.Utils          ()
import           Calamity.Types.Model.Guild.Guild
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Data.Aeson

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic                 as TSG

data BanData = BanData
  { guildID :: Snowflake Guild
  , user    :: User
  }
  deriving ( Show, Generic )
  deriving ( FromJSON ) via CalamityJSON BanData
  deriving ( TextShow ) via TSG.FromGeneric BanData
  deriving ( HasID Guild ) via HasIDField "guildID" BanData
  deriving ( HasID User ) via HasIDField "user" BanData
