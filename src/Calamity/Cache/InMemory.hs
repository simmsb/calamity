-- | A 'Cache' handler that operates in memory
module Calamity.Cache.InMemory
    ( runCacheInMemory ) where

import           Calamity.Cache.Eff
import           Calamity.Internal.BoundedStore
import qualified Calamity.Internal.SnowflakeMap as SM
import           Calamity.Internal.Utils
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.Guild
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Control.Lens
import           Control.Monad.State.Strict

import qualified Data.HashMap.Lazy as LH

import           Data.Default.Class
import           Data.Foldable
import qualified Data.HashSet                   as LS
import           Data.IORef

import           GHC.Generics

import qualified Polysemy                       as P
import qualified Polysemy.AtomicState           as P

data Cache = Cache
  { user              :: Maybe User
  , guilds            :: SM.SnowflakeMap Guild
  , dms               :: SM.SnowflakeMap DMChannel
  , guildChannels     :: LH.HashMap (Snowflake GuildChannel) Guild
  , users             :: SM.SnowflakeMap User
  , unavailableGuilds :: LS.HashSet (Snowflake Guild)
  , messages          :: BoundedStore Message
  }
  deriving ( Generic, Show )

emptyCache :: Cache
emptyCache = Cache Nothing SM.empty SM.empty LH.empty SM.empty LS.empty def

runCacheInMemory :: P.Member (P.Embed IO) r => P.Sem (CacheEff ': r) a -> P.Sem r a
runCacheInMemory m = do
  var <- P.embed $ newIORef emptyCache
  P.runAtomicStateIORef var $ P.reinterpret runCache' m

runCache' :: P.Member (P.AtomicState Cache) r => CacheEff m a -> P.Sem r a
runCache' act = P.atomicState' ((swap .) . runState $ runCache act)

runCache :: CacheEff m a -> State Cache a

runCache (SetBotUser u) = #user ?= u
runCache GetBotUser     = use #user

runCache (SetGuild g)   = do
  #guilds %= SM.insert g
  #guildChannels %= LH.filter (\v -> getID @Guild v /= getID @Guild g)
  #guildChannels %= LH.union (LH.fromList $ map (,g) (SM.keys (g ^. #channels)))
runCache (GetGuild gid) = use (#guilds . at gid)
runCache (GetGuildChannel cid) = use (#guildChannels . at cid) <&> (>>= (^. #channels . at cid))
runCache GetGuilds      = SM.elems <$> use #guilds
runCache (DelGuild gid) = do
  #guilds %= sans gid
  #guildChannels %= LH.filter (\v -> getID @Guild v /= gid)

runCache (SetDM dm)  = #dms %= SM.insert dm
runCache (GetDM did) = use (#dms . at did)
runCache GetDMs      = SM.elems <$> use #dms
runCache (DelDM did) = #dms %= sans did

runCache (SetUser u)   = #users %= SM.insert u
runCache (GetUser uid) = use (#users . at uid)
runCache GetUsers      = SM.elems <$> use #users
runCache (DelUser uid) = #users %= sans uid

runCache (SetUnavailableGuild gid) = #unavailableGuilds %= LS.insert gid
runCache (IsUnavailableGuild gid)  = use (#unavailableGuilds . contains gid)
runCache GetUnavailableGuilds      = LS.toList <$> use #unavailableGuilds
runCache (DelUnavailableGuild gid) = #unavailableGuilds %= sans gid

runCache (SetMessage m)   = #messages %= addItem m
runCache (GetMessage mid) = use (#messages . at mid)
runCache GetMessages      = toList <$> use #messages
runCache (DelMessage mid) = #messages %= sans mid
