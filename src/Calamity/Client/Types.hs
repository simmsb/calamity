-- | Types for the client
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Calamity.Client.Types
    ( Client(..)
    , BotM(..)
    , EventHandlers(..)
    , EventHandler(..)
    , EHType
    , runBotM
    , EventM
    , runEventM
    , runEventMCurrent
    , syncEventM
    , Cache(..)
    , HandlersM(..)
    , HasClient(..)
    ) where

import           Calamity.Gateway.DispatchEvents
import           Calamity.Gateway.Shard
import           Calamity.HTTP.Internal.Ratelimit
import           Calamity.Types.General
import           Calamity.Types.MessageStore
import qualified Calamity.Types.RefCountedSnowflakeMap as RSM
import           Calamity.Types.Snowflake
import qualified Calamity.Types.SnowflakeMap           as SM
import           Calamity.Types.UnixTimestamp

import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Control.Monad.Catch
import           Control.Monad.Fail
import           Control.Monad.Trans.Reader            ( runReaderT )
import           Control.Monad.Writer.Lazy

import           Data.Default
import qualified Data.HashSet                          as LS
import           Data.String                           ( String )
import           Data.Text.Strict.Lens
import           Data.Time
import qualified Data.TypeRepMap                       as TM
import           Data.TypeRepMap                       ( TypeRepMap, WrapTypeable(..) )

import           GHC.Exts                              ( fromList )
import qualified GHC.TypeLits                          as TL

import qualified StmContainers.Set                     as TS

import qualified Streamly                              as S

data Cache = Cache
  { user              :: Maybe User
  , guilds            :: SM.SnowflakeMap Guild
  , dms               :: SM.SnowflakeMap DMChannel
  , channels          :: SM.SnowflakeMap Channel
  , users             :: RSM.RefCountedSnowflakeMap User
  , unavailableGuilds :: LS.HashSet (Snowflake Guild)
  , messages          :: MessageStore
  }
  deriving ( Generic, Show )

data Client = Client
  { shards        :: TVar [(Shard, Async ())] -- TODO: migrate this to a set of Shard (make Shard hash to it's shardThread)
  , numShards     :: MVar Int
  , token         :: Token
  , rlState       :: RateLimitState
  , eventStream   :: S.Serial DispatchData
  , eventQueue    :: TQueue DispatchData -- ^ for shards to take
  , cache         :: TVar Cache
  , activeTasks   :: TS.Set (Async ()) -- ^ events currently being handled
  , eventHandlers :: EventHandlers
  }
  deriving ( Generic )

newtype BotM a = BotM
  { unBotM :: LogT (ReaderT Client IO) a
  }
  deriving ( Functor )
  deriving newtype ( Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadLog, MonadReader Client )

-- | Let's us have `MonadReader Client`
instance {-# OVERLAPPABLE #-}MonadReader a m => MonadReader a (LogT m) where
  ask = lift ask

  local f m = do
    b <- ask
    lift . local f $ runReaderT (runLogT m) b


runBotM :: Client -> Log -> BotM a -> IO a
runBotM cstate logEnv = (`runReaderT` cstate) . withLog logEnv . unBotM

-- | EventM is an event handler that contains a snapshot of the cache state
-- At the time of the invokation
newtype EventM a = EventM
  { unEventM :: ExceptT String (StateT Cache BotM) a
  }
  deriving ( Functor )
  deriving newtype ( Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadLog, MonadState Cache
                   , MonadReader Client )

instance MonadFail EventM where
  fail str = EventM $ throwE str

runEventM :: Cache -> EventM a -> BotM ()
runEventM cache e = do
  r <- (`evalStateT` cache) . runExceptT . unEventM $ e
  case r of
    Left e ->
      error $ e ^. packed
    _ -> pure ()

runEventMCurrent :: EventM a -> BotM ()
runEventMCurrent e = do
  client <- ask
  cache <- liftIO . readTVarIO . cache $ client
  runEventM cache e

-- | Sync the internal cache of an EventM
syncEventM :: EventM ()
syncEventM = do
  client <- ask
  cache <- liftIO . readTVarIO . cache $ client
  put cache

class (Functor m) => HasClient m where
  getClient :: m Client

  asksClient :: (Client -> a) -> m a
  asksClient = (<$> getClient)

instance HasClient EventM where
  getClient = ask

instance HasClient BotM where
  getClient = ask

newtype HandlersM a = HandlersM
  { unHandlersM :: Writer EventHandlers a
  }
  deriving ( Functor )
  deriving newtype ( Monad, Applicative, MonadWriter EventHandlers )

newtype EventHandlers = EventHandlers (TypeRepMap EventHandler)

newtype EventHandler d = EH
  { unwrapEventHandler :: [EHType d]
  }
  deriving newtype ( Semigroup, Monoid )

type family EHType d where
  EHType "ready"                    = ReadyData                          -> EventM ()
  EHType "channelcreate"            = Channel                            -> EventM ()
  EHType "channelupdate"            = Channel -> Channel                 -> EventM ()
  EHType "channeldelete"            = Channel                            -> EventM ()
  EHType "channelpinsupdate"        = Channel -> Maybe UTCTime           -> EventM ()
  EHType "guildcreate"              = Guild -> Bool                      -> EventM ()
  EHType "guildupdate"              = Guild -> Guild                     -> EventM ()
  EHType "guilddelete"              = Guild -> Bool                      -> EventM ()
  EHType "guildbanadd"              = Guild -> User                      -> EventM ()
  EHType "guildbanremove"           = Guild -> User                      -> EventM ()
  EHType "guildemojisupdate"        = Guild -> [Emoji]                   -> EventM ()
  EHType "guildintegrationsupdate"  = Guild                              -> EventM ()
  EHType "guildmemberadd"           = Member                             -> EventM ()
  EHType "guildmemberremove"        = Member                             -> EventM ()
  EHType "guildmemberupdate"        = Member -> Member                   -> EventM ()
  EHType "guildmemberschunk"        = Guild -> [Member]                  -> EventM ()
  EHType "guildrolecreate"          = Guild -> Role                      -> EventM ()
  EHType "guildroleupdate"          = Guild -> Role -> Role              -> EventM ()
  EHType "guildroledelete"          = Guild -> Role                      -> EventM ()
  EHType "messagecreate"            = Message                            -> EventM ()
  EHType "messageupdate"            = Message -> Message                 -> EventM ()
  EHType "messagedelete"            = Message                            -> EventM ()
  EHType "messagedeletebulk"        = [Message]                          -> EventM ()
  EHType "messagereactionadd"       = Message -> Reaction                -> EventM ()
  EHType "messagereactionremove"    = Message -> Reaction                -> EventM ()
  EHType "messagereactionremoveall" = Message                            -> EventM ()
  EHType "typingstart"              = Channel -> Member -> UnixTimestamp -> EventM ()
  EHType "userupdate"               = User -> User                       -> EventM ()
  EHType _ = TL.TypeError ('TL.Text "Unknown event name")
  -- EHType "voicestateupdate"         = VoiceStateUpdateData -> EventM ()
  -- EHType "voiceserverupdate"        = VoiceServerUpdateData -> EventM ()
  -- EHType "webhooksupdate"           = WebhooksUpdateData -> EventM ()


instance Default EventHandlers where
  def = EventHandlers $ fromList [ WrapTypeable $ EH @"ready" []
                                 , WrapTypeable $ EH @"channelcreate" []
                                 , WrapTypeable $ EH @"channelupdate" []
                                 , WrapTypeable $ EH @"channeldelete" []
                                 , WrapTypeable $ EH @"channelpinsupdate" []
                                 , WrapTypeable $ EH @"guildcreate" []
                                 , WrapTypeable $ EH @"guildupdate" []
                                 , WrapTypeable $ EH @"guilddelete" []
                                 , WrapTypeable $ EH @"guildbanadd" []
                                 , WrapTypeable $ EH @"guildbanremove" []
                                 , WrapTypeable $ EH @"guildemojisupdate" []
                                 , WrapTypeable $ EH @"guildintegrationsupdate" []
                                 , WrapTypeable $ EH @"guildmemberadd" []
                                 , WrapTypeable $ EH @"guildmemberremove" []
                                 , WrapTypeable $ EH @"guildmemberupdate" []
                                 , WrapTypeable $ EH @"guildrolecreate" []
                                 , WrapTypeable $ EH @"guildroleupdate" []
                                 , WrapTypeable $ EH @"guildroledelete" []
                                 , WrapTypeable $ EH @"messagecreate" []
                                 , WrapTypeable $ EH @"messageupdate" []
                                 , WrapTypeable $ EH @"messagedelete" []
                                 , WrapTypeable $ EH @"messagedeletebulk" []
                                 , WrapTypeable $ EH @"messagereactionadd" []
                                 , WrapTypeable $ EH @"messagereactionremove" []
                                 , WrapTypeable $ EH @"messagereactionremoveall" []
                                 , WrapTypeable $ EH @"typingstart" []
                                 , WrapTypeable $ EH @"userupdate" []
                                 -- , WrapTypeable $ EH @"voicestateupdate" []
                                 -- , WrapTypeable $ EH @"voiceserverupdate" []
                                 -- , WrapTypeable $ EH @"webhooksupdate" []
                                 ]

instance Semigroup EventHandlers where
  (EventHandlers a) <> (EventHandlers b) = EventHandlers $ TM.unionWith (<>) a b

instance Monoid EventHandlers where
  mempty = def
