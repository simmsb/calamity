-- | Channel webhooks
module Calamity.Types.Model.Channel.Webhook
    ( Webhook(..) ) where

import           Calamity.Internal.AesonThings
import {-# SOURCE #-} Calamity.Types.Model.Channel
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Data.Aeson
import           Data.Text.Lazy                       ( Text )

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic                     as TSG

data Webhook = Webhook
  { id        :: Snowflake Webhook
  , type_     :: Integer
  , guildID   :: Maybe (Snowflake Guild)
  , channelID :: Maybe (Snowflake Channel)
  , user      :: Maybe (Snowflake User)
  , name      :: Text
  , avatar    :: Text
  , token     :: Maybe Text
  }
  deriving ( Eq, Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric Webhook
  deriving ( FromJSON ) via WithSpecialCases '["user" `ExtractField` "id"] Webhook
  deriving ( HasID Webhook ) via HasIDField "id" Webhook
