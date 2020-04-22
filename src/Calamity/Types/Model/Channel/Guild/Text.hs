-- | Text channels
module Calamity.Types.Model.Channel.Guild.Text
    ( TextChannel(..) ) where

import           Calamity.Internal.AesonThings
import           Calamity.Internal.Utils                     ()
import {-# SOURCE #-} Calamity.Types.Model.Channel
import {-# SOURCE #-} Calamity.Types.Model.Channel.Guild.Category
import {-# SOURCE #-} Calamity.Types.Model.Channel.Message
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import           Calamity.Types.Model.Guild.Overwrite
import           Calamity.Types.Snowflake

import           Data.Aeson
import           Data.Text.Lazy                              ( Text )
import           Data.Time

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic                            as TSG

data TextChannel = TextChannel
  { id                   :: Snowflake TextChannel
  , guildID              :: Snowflake Guild
  , position             :: Int
  , permissionOverwrites :: [Overwrite]
  , name                 :: Text
  , topic                :: Maybe Text
  , nsfw                 :: Bool
  , lastMessageID        :: Maybe (Snowflake Message)
  , lastPinTimestamp     :: Maybe UTCTime
  , rateLimitPerUser     :: Maybe Int
  , parentID             :: Maybe (Snowflake Category)
  }
  deriving ( Show, Eq, Generic )
  deriving ( TextShow ) via TSG.FromGeneric TextChannel
  deriving ( ToJSON ) via CalamityJSON TextChannel
  deriving ( FromJSON ) via WithSpecialCases '[IfNoneThen "nsfw" DefaultToFalse]
      TextChannel
  deriving ( HasID TextChannel ) via HasIDField "id" TextChannel
  deriving ( HasID Channel ) via HasIDFieldCoerce' "id" TextChannel
  deriving ( HasID Guild ) via HasIDField "guildID" TextChannel
