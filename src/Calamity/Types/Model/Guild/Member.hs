-- | Guild Members
module Calamity.Types.Model.Guild.Member (Member (..)) where

import Calamity.Internal.AesonThings
import Calamity.Internal.Utils ()
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import Calamity.Types.Model.Guild.Role
import Calamity.Types.Model.User
import Calamity.Types.Snowflake
import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Text.Lazy (Text)
import Data.Time
import Data.Vector.Unboxing (Vector)
import Data.Word (Word64)
import GHC.Generics
import TextShow
import qualified TextShow.Generic as TSG

data Member = Member
  { id :: Snowflake User
  , username :: Text
  , discriminator :: Text
  , bot :: Maybe Bool
  , avatar :: Maybe Text
  , mfaEnabled :: Maybe Bool
  , verified :: Maybe Bool
  , email :: Maybe Text
  , flags :: Maybe Word64
  , premiumType :: Maybe Word64
  , guildID :: Snowflake Guild
  , nick :: Maybe Text
  , roles :: Vector (Snowflake Role)
  , joinedAt :: UTCTime
  , deaf :: Bool
  , mute :: Bool
  }
  deriving (Eq, Show, Generic, NFData)
  deriving (TextShow) via TSG.FromGeneric Member
  deriving
    (FromJSON)
    via WithSpecialCases
          '[ "user"
              `ExtractFields` [ "id"
                              , "username"
                              , "discriminator"
                              , "bot"
                              , "avatar"
                              , "mfa_enabled"
                              , "verified"
                              , "email"
                              , "flags"
                              , "premium_type"
                              ]
           ]
          Member
  deriving (HasID Guild) via HasIDField "guildID" Member
  deriving (HasID Member) via HasIDFieldCoerce "id" Member User
  deriving (HasID User) via HasIDField "id" Member
