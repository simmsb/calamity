-- | Things that can be upgraded from snowflakes to their full data
module Calamity.Types.Upgradeable
    ( Upgradeable(..) ) where

import           Calamity.Cache.Eff
import           Calamity.Client.Types
import           Calamity.HTTP             as H
import           Calamity.Internal.Utils
import           Calamity.Types.Model.User
import           Calamity.Types.Model.Guild
import           Calamity.Types.Model.Channel
import           Calamity.Types.Snowflake

import           Control.Applicative

import qualified Polysemy                  as P
import qualified Polysemy.Fail             as P
import qualified Polysemy.NonDet           as P
import Data.Generics.Sum.Constructors
import Control.Lens

class Upgradeable a where
  type SourceIDS a
  type SourceIDS a = Snowflake a
  upgrade :: BotC r => SourceIDS a -> P.Sem r (Maybe a)

maybeToAlt :: Alternative f => Maybe a -> f a
maybeToAlt (Just x) = pure x
maybeToAlt Nothing = empty

instance Upgradeable User where
  upgrade uid = P.runNonDetMaybe ((getUser uid >>= maybeToAlt) <|> gethttp)
    where
      gethttp = P.failToNonDet $ do
        Right u <- invoke $ H.GetUser uid
        setUser u
        pure u

instance Upgradeable Member where
  type SourceIDS Member = (Snowflake Guild, Snowflake Member)

  upgrade (gid, mid) = P.runNonDetMaybe (getcache <|> gethttp)
    where
      getcache = P.failToNonDet $ do
        Just g <- getGuild gid
        Just m <- pure (g ^. #members . at mid)
        pure m
      gethttp = P.failToNonDet $ do
        Right m <- invoke $ H.GetGuildMember gid (coerceSnowflake @_ @User mid)
        -- getcache could have failed becuase the member wasn't cached
        updateGuild gid (#members . at mid ?~ m)
        pure m

instance Upgradeable Guild where
  upgrade gid = P.runNonDetMaybe ((getGuild gid >>= maybeToAlt) <|> gethttp)
    where
      gethttp = P.failToNonDet $ do
        Right g <- invoke $ H.GetGuild gid
        pure g

insertChannel :: BotC r => Channel -> P.Sem r ()
insertChannel (DMChannel' dm) = setDM dm
insertChannel (GuildChannel' ch) =
  updateGuild (getID ch) (#channels . at (getID @GuildChannel ch) ?~ ch)
insertChannel _ = pure ()

instance Upgradeable Channel where
  upgrade cid = P.runNonDetMaybe (getcacheDM <|> getcacheGuild <|> gethttp)
    where
      getcacheDM = DMChannel' <<$>> getDM (coerceSnowflake cid) >>= maybeToAlt
      getcacheGuild = GuildChannel' <<$>> getGuildChannel (coerceSnowflake cid) >>= maybeToAlt
      gethttp = P.failToNonDet $ do
        Right c <- invoke $ H.GetChannel cid
        insertChannel c
        pure c

instance Upgradeable GuildChannel where
  upgrade cid = P.runNonDetMaybe (getcache <|> gethttp)
    where
      getcache = getGuildChannel (coerceSnowflake cid) >>= maybeToAlt
      gethttp = P.failToNonDet $ do
        Right c <- invoke $ H.GetChannel (coerceSnowflake @_ @Channel cid)
        insertChannel c
        maybeToAlt (c ^? _Ctor @"GuildChannel'")

instance Upgradeable Emoji where
  type SourceIDS Emoji = (Snowflake Guild, Snowflake Emoji)

  upgrade (gid, eid) = P.runNonDetMaybe (getcache <|> gethttp)
    where
      getcache = P.failToNonDet $ do
        Just g <- getGuild gid
        Just m <- pure (g ^. #emojis . at eid)
        pure m
      gethttp = P.failToNonDet $ do
        Right e <- invoke $ H.GetGuildEmoji gid eid
        updateGuild gid (#emojis . at eid ?~ e)
        pure e
