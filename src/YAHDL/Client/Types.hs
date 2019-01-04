-- | Types for the client

{-# OPTIONS_GHC -Wno-orphans #-}

module YAHDL.Client.Types
  ( ClientState(..)
  , BotM(..)
  , CacheState(..)
  )
where


import           Control.Monad.Catch            ( MonadMask )
import           Control.Concurrent.STM.TVar
import           Control.Monad.Log              ( LogT(..)
                                                , MonadLog
                                                , runLogTSafe
                                                )
import qualified Streamly                      as S
import qualified StmContainers.Map             as SC

import           YAHDL.Gateway.Shard
import           YAHDL.HTTP.Ratelimit
import           YAHDL.Types.General
import           YAHDL.Types.Snowflake
import           YAHDL.Types.DispatchEvents
-- import           YAHDL.Client.ShardManager

data CacheState = CacheState
  { guilds  :: SC.Map (Snowflake Guild) Guild
  , members :: SC.Map (Snowflake User) User
  } deriving (Generic)

data ClientState = ClientState
  { shards      :: TVar [(Shard, Async ())]
  , token       :: Token
  , rlState     :: RateLimitState
  , eventStream :: S.Serial DispatchData
  } deriving (Generic)


newtype BotM a = BotM
  { unBotM :: LogT Text (ReaderT ClientState IO) a
  } deriving (Applicative, Monad, MonadIO, MonadLog Text,
              Functor, MonadReader ClientState)

instance (MonadMask m, MonadIO m, MonadReader r m) => MonadReader r (LogT env m) where
  ask       = lift ask
  local f m = LogT $ \env -> local f (runLogTSafe env m)
  reader    = lift . reader
