-- | A 'Cache' handler that operates in memory
module Calamity.Cache.InMemory
    ( runCacheInMemory ) where

import           Calamity.Cache.Eff
import           Calamity.Internal.MessageStore
import qualified Calamity.Internal.SnowflakeMap as SM
import           Calamity.Internal.Utils
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.Guild
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Control.Lens
import           Control.Monad.State.Strict

import           Data.Default.Class
import qualified Data.HashSet                   as LS
import           Data.IORef

import           GHC.Generics

import qualified Polysemy                       as P
import qualified Polysemy.AtomicState           as P

data Cache = Cache
  { user              :: Maybe User
  , guilds            :: SM.SnowflakeMap Guild
  , dms               :: SM.SnowflakeMap DMChannel
  , channels          :: SM.SnowflakeMap GuildChannel
  , users             :: SM.SnowflakeMap User
  , unavailableGuilds :: LS.HashSet (Snowflake Guild)
  , messages          :: MessageStore
  }
  deriving ( Generic, Show )

emptyCache :: Cache
emptyCache = Cache Nothing SM.empty SM.empty SM.empty SM.empty LS.empty def

runCacheInMemory :: P.Member (P.Embed IO) r => P.Sem (CacheEff ': r) a -> P.Sem r a
runCacheInMemory m = do
  var <- P.embed $ newIORef emptyCache
  P.runAtomicStateIORef var $ P.reinterpret updateCache' m

updateCache' :: P.Member (P.AtomicState Cache) r => CacheEff m a -> P.Sem r a
updateCache' act = P.atomicState' ((swap .) . runState $ updateCache act)

updateCache :: CacheEff m a -> State Cache a

updateCache (SetBotUser u) = #user ?= u
updateCache GetBotUser     = use #user

updateCache (SetGuild g)   = #guilds %= SM.insert g
updateCache (GetGuild gid) = use (#guilds . at gid)
updateCache (DelGuild gid) = #guilds %= sans gid


updateCache (SetDM dm)   = #dms %= SM.insert dm
updateCache (GetDM did) = use (#dms . at did)
updateCache (DelDM did) = #dms %= sans did

updateCache (SetUser u)   = #users %= SM.insert u
updateCache (GetUser uid) = use (#users . at uid)
updateCache (DelUser uid) = #users %= sans uid

updateCache (SetUnavailableGuild gid) = #unavailableGuilds %= LS.insert gid
updateCache (IsUnavailableGuild gid) = use (#unavailableGuilds . contains gid)
updateCache (DelUnavailableGuild gid) = #unavailableGuilds %= sans gid

updateCache (SetMessage m)   = #messages %= addMessage m
updateCache (GetMessage mid) = use (#messages . at mid)
updateCache (DelMessage mid) = #messages %= sans mid
