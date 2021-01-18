-- | A 'Cache' handler that operates in memory
module Calamity.Cache.InMemory (
  runCacheInMemory,
  runCacheInMemory',
  runCacheInMemoryNoMsg,
) where

import Calamity.Cache.Eff
import qualified Calamity.Internal.BoundedStore as BS
import qualified Calamity.Internal.SnowflakeMap as SM
import Calamity.Internal.Utils
import Calamity.Types.Model.Channel
import Calamity.Types.Model.Guild
import Calamity.Types.Model.User
import Calamity.Types.Snowflake

import Control.Lens
import Control.Monad.State.Strict

import Data.Foldable
import qualified Data.HashMap.Strict as SH
import qualified Data.HashSet as HS
import Data.IORef

import Control.DeepSeq
import GHC.Generics

import qualified Polysemy as P
import qualified Polysemy.AtomicState as P

data Cache f = Cache
  { user :: Maybe User
  , guilds :: SM.SnowflakeMap Guild
  , dms :: SM.SnowflakeMap DMChannel
  , guildChannels :: SH.HashMap (Snowflake GuildChannel) Guild
  , users :: SM.SnowflakeMap User
  , unavailableGuilds :: HS.HashSet (Snowflake Guild)
  , messages :: f (BS.BoundedStore Message)
  }
  deriving (Generic)

instance NFData (f (BS.BoundedStore Message)) => NFData (Cache f)

type CacheWithMsg = Cache Identity
type CacheNoMsg = Cache (Const ())

emptyCache :: CacheWithMsg
emptyCache = Cache Nothing SM.empty SM.empty SH.empty SM.empty HS.empty (Identity $ BS.empty 1000)

emptyCacheNoMsg :: CacheNoMsg
emptyCacheNoMsg = Cache Nothing SM.empty SM.empty SH.empty SM.empty HS.empty (Const ())

emptyCache' :: Int -> CacheWithMsg
emptyCache' msgLimit = Cache Nothing SM.empty SM.empty SH.empty SM.empty HS.empty (Identity $ BS.empty msgLimit)

-- | Run the cache in memory with a default message cache size of 1000
runCacheInMemory :: P.Member (P.Embed IO) r => P.Sem (CacheEff ': r) a -> P.Sem r a
runCacheInMemory m = do
  var <- P.embed $ newIORef emptyCache
  P.runAtomicStateIORef var $ P.reinterpret runCache' m

-- | Run the cache in memory with no messages being cached
runCacheInMemoryNoMsg :: P.Member (P.Embed IO) r => P.Sem (CacheEff ': r) a -> P.Sem r a
runCacheInMemoryNoMsg m = do
  var <- P.embed $ newIORef emptyCacheNoMsg
  P.runAtomicStateIORef var $ P.reinterpret runCache' m

-- | Run the cache in memory with a configurable message cache limit
runCacheInMemory' :: P.Member (P.Embed IO) r => Int -> P.Sem (CacheEff ': r) a -> P.Sem r a
runCacheInMemory' msgLimit m = do
  var <- P.embed $ newIORef (emptyCache' msgLimit)
  P.runAtomicStateIORef var $ P.reinterpret runCache' m

runCache' :: (MessageMod (Cache t), P.Member (P.AtomicState (Cache t)) r) => CacheEff m a -> P.Sem r a
runCache' act = P.atomicState' ((swap .) . force . runState $ runCache act)

class MessageMod t where
  setMessage' :: Message -> State t ()
  getMessage' :: Snowflake Message -> State t (Maybe Message)
  getMessages' :: State t [Message]
  delMessage' :: Snowflake Message -> State t ()

instance MessageMod CacheWithMsg where
  setMessage' m = #messages . _Wrapped %= BS.addItem m
  getMessage' mid = use (#messages . _Wrapped . at mid)
  getMessages' = toList <$> use (#messages . _Wrapped)
  delMessage' mid = #messages . _Wrapped %= sans mid

instance MessageMod CacheNoMsg where
  setMessage' _ = pure ()
  getMessage' _ = pure Nothing
  getMessages' = pure []
  delMessage' _ = pure ()

runCache :: MessageMod (Cache t) => CacheEff m a -> State (Cache t) a
runCache (SetBotUser u) = #user ?= u
runCache GetBotUser = use #user
runCache (SetGuild g) = do
  #guilds %= SM.insert g
  #guildChannels %= SH.filter (\v -> getID @Guild v /= getID @Guild g)
  #guildChannels %= SH.union (SH.fromList $ map (,g) (SM.keys (g ^. #channels)))
runCache (GetGuild gid) = use (#guilds . at gid)
runCache (GetGuildChannel cid) = use (#guildChannels . at cid) <&> (>>= (^. #channels . at cid))
runCache GetGuilds = SM.elems <$> use #guilds
runCache (DelGuild gid) = do
  #guilds %= sans gid
  #guildChannels %= SH.filter (\v -> getID @Guild v /= gid)
runCache (SetDM dm) = #dms %= SM.insert dm
runCache (GetDM did) = use (#dms . at did)
runCache GetDMs = SM.elems <$> use #dms
runCache (DelDM did) = #dms %= sans did
runCache (SetUser u) = #users %= SM.insert u
runCache (GetUser uid) = use (#users . at uid)
runCache GetUsers = SM.elems <$> use #users
runCache (DelUser uid) = #users %= sans uid
runCache (SetUnavailableGuild gid) = #unavailableGuilds %= HS.insert gid
runCache (IsUnavailableGuild gid) = use (#unavailableGuilds . contains gid)
runCache GetUnavailableGuilds = HS.toList <$> use #unavailableGuilds
runCache (DelUnavailableGuild gid) = #unavailableGuilds %= sans gid
runCache (SetMessage m) = setMessage' m
runCache (GetMessage mid) = getMessage' mid
runCache GetMessages = getMessages'
runCache (DelMessage mid) = delMessage' mid
