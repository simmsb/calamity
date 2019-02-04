-- | Types for the client

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module YAHDL.Client.Types
  ( Client(..)
  , BotM(..)
  , EventHandlers(..)
  , EventHandler(..)
  , EHType
  , runBotM
  , EventM
  , runEventM
  , syncEventM
  , Cache(..)
  )
where

-- import           Control.Monad.Trans.Control
-- import           Control.Monad.Base
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM.TChan
import qualified StmContainers.Set             as TS
import           Control.Monad.Catch            ( MonadMask )
import           Control.Monad.Log              ( Logger
                                                , LogT(..)
                                                , MonadLog
                                                , runLogTSafe
                                                )
import           Data.Default
import qualified Data.HashMap.Lazy             as LH
import           Data.Time
import           Data.TypeRepMap                ( TypeRepMap
                                                , WrapTypeable(..)
                                                )
import           GHC.Exts                       ( fromList )
import qualified Streamly                      as S

import           YAHDL.Gateway.Shard
import           YAHDL.HTTP.Ratelimit
import           YAHDL.Types.General
import           YAHDL.Types.Snowflake
import           YAHDL.Types.DispatchEvents


data Cache = Cache
  { user    :: Maybe User
  , guilds  :: LH.HashMap (Snowflake Guild) Guild
  , members :: LH.HashMap (Snowflake User) User
  , dms     :: LH.HashMap (Snowflake DM) DM
  } deriving (Generic)

data Client = Client
  { shards        :: TVar [(Shard, Async ())]
  , numShards     :: MVar Int
  , token         :: Token
  , rlState       :: RateLimitState
  , eventStream   :: S.Serial DispatchData
  , eventChan     :: TChan DispatchData -- ^ for shards to take
  , cache         :: TVar Cache
  , activeTasks   :: TS.Set (Async ()) -- ^ events currently being handled
  , eventHandlers :: EventHandlers
  } deriving (Generic)

newtype BotM a = BotM
  { unBotM :: LogT Text (ReaderT Client IO) a
  } deriving (Applicative, Monad, MonadIO, MonadLog Text,
              Functor, MonadReader Client)

instance (MonadMask m, MonadIO m, MonadReader r m) => MonadReader r (LogT env m) where
  ask       = lift ask
  local f m = LogT $ \env -> local f (runLogTSafe env m)
  reader    = lift . reader

-- TODO: maybe come back and complete this impl sometime
-- so that we don't have to manually pull and insert data into the BotM monad for clientLoop
--
-- instance MonadTransControl (LogT Text) where
--   type StT (LogT Text) a =

-- instance MonadBase b m => MonadBase b (LogT Text m) where
--   liftBase = lift . liftBase

-- instance MonadBaseControl b m => MonadBaseControl b (LogT Text m) where
--   type StM (LogT Text m) a = ComposeSt (LogT Text) m a
--   liftBaseWith = defaultLiftBaseWith
--   restoreM = defaultRestoreM
--   {-# INLINABLE liftBaseWith #-}
--   {-# INLINABLE restoreM #-}


-- instance MonadBase IO BotM where
--   liftBase = liftIO

-- newtype StMBotM a = StMBotM { unStMBotM :: StM (LogT Text (ReaderT Client IO)) a }

-- instance MonadBaseControl IO BotM where
--   type StM BotM a = StMBotM a

--   liftBaseWith f = BotM $ liftBaseWith (\run -> f (liftM StMBotM . run . unBotM))

--   restoreM = BotM . restoreM . unStMBotM
--   {-# INLINABLE liftBaseWith #-}
--   {-# INLINABLE restoreM #-}

runBotM :: Client -> Logger Text -> BotM a -> IO a
runBotM cstate logEnv = (`runReaderT` cstate) . runLogTSafe logEnv . unBotM

-- | EventM is an event handler that contains a snapshot of the cache state
-- | At the time of the invokation
newtype EventM a = EventM
  { unEventM :: StateT Cache BotM a
  } deriving (Applicative, Monad, MonadIO, MonadLog Text,
              Functor, MonadReader Client, MonadState Cache)

runEventM :: Cache -> EventM a -> BotM a
runEventM cache event = (`evalStateT` cache) . unEventM $ event

-- | Sync the internal cache of an EventM
syncEventM :: EventM ()
syncEventM = do
  client <- ask
  cache  <- liftIO . readTVarIO . cache $ client
  put cache

newtype EventHandlers = EventHandlers (TypeRepMap EventHandler)

newtype EventHandler d = EH { unwrapEventHandler :: [EHType d] }

type family EHType d where
  EHType "ready"                    = ReadyData -> EventM ()
  EHType "channelcreate"            = Channel   -> EventM ()
  EHType "channelupdate"            = Channel   -> Channel -> EventM ()
  EHType "channeldelete"            = Channel   -> EventM ()
  EHType "channelpinsupdate"        = Channel   -> UTCTime -> EventM ()
  EHType "guildcreate"              = Guild     -> EventM ()
  EHType "guildupdate"              = Guild     -> Guild   -> EventM ()
  EHType "guilddelete"              = Guild     -> Bool    -> EventM ()
  EHType "guildbanadd"              = Guild     -> User    -> EventM ()
  EHType "guildbanremove"           = Guild     -> User    -> EventM ()
  EHType "guildemojisupdate"        = Guild     -> [Emoji] -> EventM ()
  EHType "guildintegrationsupdate"  = Guild     -> EventM ()
  EHType "guildmemberadd"           = Member    -> EventM ()
  EHType "guildmemberremove"        = Member    -> EventM ()
  -- TODO: Member update includes presence update events
  EHType "guildmemberupdate"        = Member    -> Member  -> EventM ()
  EHType "guildrolecreate"          = Role      -> EventM ()
  EHType "guildroleupdate"          = Role      -> Role    -> EventM ()
  EHType "guildroledelete"          = Role      -> EventM ()
  EHType "messagecreate"            = Message   -> EventM ()
  EHType "messageupdate"            = Message   -> Message -> EventM ()
  EHType "messagedelete"            = Message   -> EventM ()
  EHType "messagedeletebulk"        = [Message] -> EventM ()
  EHType "messagereactionadd"       = Reaction  -> EventM ()
  EHType "messagereactionremove"    = Reaction  -> EventM ()
  EHType "messagereactionremoveall" = Message   -> EventM ()
  EHType "typingstart"              = Member    -> EventM ()
  EHType "userupdate"               = User      -> EventM ()
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
