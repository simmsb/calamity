-- | The route type

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module YAHDL.HTTP.Route
  ( mkRouteBuilder
  , giveChanID
  , giveGuildID
  , buildRoute
  , addStringFragment
  , addChanIDFragment
  , addGuildIDFragment
  , RouteBuilder
  , Route
  , RouteMethod(..)
  )
where

import           Data.Maybe                     ( fromJust )
import           Data.Singletons.Prelude
import           Data.Singletons.TH

import           YAHDL.Types.Snowflake
import           YAHDL.Types.General

data RouteFragment
  = SFragment Text
  | ChanID
  | GuildID
  deriving (Generic, Show, Eq)

instance Hashable RouteFragment

data RouteMethod
  = GET
  | POST
  | DELETE
  | PUT
  deriving (Generic, Show, Eq)

instance Hashable RouteMethod

$(singletons [d|
  data RouteRequirement = NotNeeded | Required | Satisfied
    deriving (Generic, Show, Eq)
  |])

data RouteBuilder (cIDState :: RouteRequirement) (gIDState :: RouteRequirement) = UnsafeMkRouteBuilder
  { method  :: RouteMethod
  , route   :: [RouteFragment]
  , chanID  :: Maybe (Snowflake Channel)
  , guildID :: Maybe (Snowflake Guild)
  } deriving (Generic, Show)

mkRouteBuilder :: RouteMethod -> RouteBuilder 'NotNeeded 'NotNeeded
mkRouteBuilder method = UnsafeMkRouteBuilder method [] Nothing Nothing

giveChanID
  :: RouteBuilder 'Required gids
  -> Snowflake Channel
  -> RouteBuilder 'Satisfied gids
giveChanID (UnsafeMkRouteBuilder method route _ guildID) chanID =
  UnsafeMkRouteBuilder method route (Just chanID) guildID

giveGuildID
  :: RouteBuilder cids 'Required
  -> Snowflake Channel
  -> RouteBuilder cids 'Satisfied
giveGuildID (UnsafeMkRouteBuilder method route chanID _) guildID =
  UnsafeMkRouteBuilder method route chanID (Just guildID)

$(singletons [d|
  isFulfilled :: RouteRequirement -> Bool
  isFulfilled NotNeeded = True
  isFulfilled Satisfied = True
  isFulfilled Required  = False

  addRequired :: RouteRequirement -> RouteRequirement
  addRequired NotNeeded = Required
  addRequired Required  = Required
  addRequired Satisfied = Satisfied
  |])

data Route = Route RouteMethod [Text]
  deriving (Generic, Show, Eq)

instance Hashable Route

buildRoute
  :: (IsFulfilled cids ~ 'True, IsFulfilled gids ~ 'True)
  => RouteBuilder cids gids
  -> Route
buildRoute (UnsafeMkRouteBuilder method route chanID guildID) = Route
  method
  (map go route)
 where
  go (SFragment t) = t
  go ChanID        = show . fromSnowflake . fromJust $ chanID
  go GuildID       = show . fromSnowflake . fromJust $ guildID


addStringFragment :: Text -> RouteBuilder cids gids -> RouteBuilder cids gids
addStringFragment t (UnsafeMkRouteBuilder m r c g) =
  UnsafeMkRouteBuilder m (SFragment t : r) c g

addChanIDFragment
  :: (IsFulfilled cids ~ 'False)
  => RouteBuilder cids gids
  -> RouteBuilder (AddRequired cids) gids
addChanIDFragment (UnsafeMkRouteBuilder m r c g) =
  UnsafeMkRouteBuilder m (ChanID : r) c g

addGuildIDFragment
  :: (IsFulfilled gids ~ 'False)
  => RouteBuilder cids gids
  -> RouteBuilder cids (AddRequired gids)
addGuildIDFragment (UnsafeMkRouteBuilder m r c g) =
  UnsafeMkRouteBuilder m (GuildID : r) c g
