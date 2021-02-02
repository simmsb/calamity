-- | Types for the client
module Calamity.Client.Types
    ( Client(..)
    , StartupError(..)
    , EventType(..)
    , GuildCreateStatus(..)
    , GuildDeleteStatus(..)
    , EHType
    , BotC
    , SetupEff
    , ReactConstraints
    , EventHandlers(..)
    , InsertEventHandler(..)
    , RemoveEventHandler(..)
    , getEventHandlers
    , getCustomEventHandlers ) where

import           Calamity.Cache.Eff
import           Calamity.Gateway.DispatchEvents ( CalamityEvent(..), InviteCreateData, InviteDeleteData, ReadyData )
import           Calamity.Gateway.Types          ( ControlMessage )
import           Calamity.HTTP.Internal.Types
import           Calamity.Metrics.Eff
import           Calamity.Types.LogEff
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.Channel.UpdatedMessage
import           Calamity.Types.Model.Guild
import           Calamity.Types.Model.User
import           Calamity.Types.Model.Voice
import           Calamity.Types.Token
import           Calamity.Types.UnixTimestamp
import           Calamity.Types.Snowflake

import           Control.Concurrent.Async
import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.MVar
import           Control.Concurrent.STM.TVar

import           Data.Default.Class
import           Data.Dynamic
import qualified Data.HashMap.Lazy               as LH
import           Data.IORef
import           Data.Maybe
import           Data.Time
import qualified Data.TypeRepMap                 as TM
import           Data.TypeRepMap                 ( TypeRepMap, WrapTypeable(..) )
import           Data.Typeable
import           Data.Void

import           GHC.Exts                        ( fromList )
import           GHC.Generics

import qualified Polysemy                        as P
import qualified Polysemy.Async                  as P
import qualified Polysemy.AtomicState            as P
import qualified Polysemy.Reader                 as P

import qualified TextShow.Generic                as TSG
import TextShow
import qualified Df1 as Df1
import qualified Di.Core as DC

data Client = Client
  { shards              :: TVar [(InChan ControlMessage, Async (Maybe ()))]
  , numShards           :: MVar Int
  , token               :: Token
  , rlState             :: RateLimitState
  , eventsIn            :: InChan CalamityEvent
  , eventsOut           :: OutChan CalamityEvent
  , ehidCounter         :: IORef Integer
  , initialDi           :: Maybe (DC.Di Df1.Level Df1.Path Df1.Message)
  }
  deriving ( Generic )

type BotC r =
  ( P.Members '[LogEff, MetricEff, CacheEff, P.Reader Client,
  P.AtomicState EventHandlers, P.Embed IO, P.Final IO, P.Async] r
  , Typeable r)

-- | A concrete effect stack used inside the bot
type SetupEff r = (P.Reader Client ': P.AtomicState EventHandlers ': P.Async ': r)

-- | Some constraints that 'Calamity.Client.Client.react' needs to work. Don't
-- worry about these since they are satisfied for any type @s@ can be
type ReactConstraints s =
  ( InsertEventHandler s
  , RemoveEventHandler s
  )

newtype StartupError = StartupError String
  deriving ( Show )

-- | A Data Kind used to fire custom events
data EventType
  = ReadyEvt
  | ChannelCreateEvt
  | ChannelUpdateEvt
  | ChannelDeleteEvt
  | ChannelpinsUpdateEvt
  | GuildCreateEvt
  | GuildUpdateEvt
  | GuildDeleteEvt
  | GuildBanAddEvt
  | GuildBanRemoveEvt
  | GuildEmojisUpdateEvt
  | GuildIntegrationsUpdateEvt
  | GuildMemberAddEvt
  | GuildMemberRemoveEvt
  | GuildMemberUpdateEvt
  | GuildMembersChunkEvt
  | GuildRoleCreateEvt
  | GuildRoleUpdateEvt
  | GuildRoleDeleteEvt
  | InviteCreateEvt
  | InviteDeleteEvt
  | MessageCreateEvt
  | MessageUpdateEvt
  -- ^ Fired when a cached message is updated, use 'RawMessageUpdateEvt' to see
  -- updates of uncached messages
  | RawMessageUpdateEvt
  -- ^ Fired when a message is updated
  | MessageDeleteEvt
  -- ^ Fired when a cached message is deleted, use 'RawMessageDeleteEvt' to see
  -- deletes of uncached messages.
  --
  -- Does not include messages deleted through bulk deletes, use
  -- 'MessageDeleteBulkEvt' for those
  | RawMessageDeleteEvt
  -- ^ Fired when a message is deleted.
  --
  -- Does not include messages deleted through bulk deletes, use
  -- 'RawMessageDeleteBulkEvt' for those
  | MessageDeleteBulkEvt
  -- ^ Fired when messages are bulk deleted. Only includes cached messages, use
  -- 'RawMessageDeleteBulkEvt' to see deletes of uncached messages.
  | RawMessageDeleteBulkEvt
  -- ^ Fired when messages are bulk deleted.
  | MessageReactionAddEvt
  -- ^ Fired when a reaction is added to a cached message, use
  -- 'RawMessageReactionAddEvt' to see reactions on uncached messages.
  | RawMessageReactionAddEvt
  -- ^ Fired when a reaction is added to a message.
  | MessageReactionRemoveEvt
  -- ^ Fired when a reaction is removed from a cached message, use
  -- 'RawMessageReactionRemoveEvt' to see reactions on uncached messages.
  | RawMessageReactionRemoveEvt
  -- ^ Fired when a reaction is removed from a message.
  | MessageReactionRemoveAllEvt
  -- ^ Fired when all reactions are removed from a cached message, use
  -- 'RawMessageReactionRemoveEvt' to see reactions on uncached messages.
  --
  -- The message passed will contain the removed events.
  | RawMessageReactionRemoveAllEvt
  -- ^ Fired when all reactions are removed from a message.
  | TypingStartEvt
  | UserUpdateEvt
  | VoiceStateUpdateEvt
  -- ^ Sent when someone joins/leaves/moves voice channels
  | forall s a. CustomEvt s a
  -- ^ A custom event, @s@ is the name and @a@ is the data sent to the handler

data GuildCreateStatus
  = GuildCreateNew -- ^ The guild was just joined
  | GuildCreateAvailable -- ^ The guild is becoming available
  deriving ( Generic, Show )
  deriving ( TextShow ) via TSG.FromGeneric GuildCreateStatus

data GuildDeleteStatus
  = GuildDeleteUnavailable -- ^ The guild became unavailable
  | GuildDeleteRemoved -- ^ The bot was removed from the guild
  deriving ( Generic, Show )
  deriving ( TextShow ) via TSG.FromGeneric GuildDeleteStatus

-- | A type family to decide what the parameters for an event handler should be
-- determined by the type of event it is handling.
type family EHType (d :: EventType) where
  EHType 'ReadyEvt                    = ReadyData
  EHType 'ChannelCreateEvt            = Channel
  EHType 'ChannelUpdateEvt            = (Channel, Channel)
  EHType 'ChannelDeleteEvt            = Channel
  EHType 'ChannelpinsUpdateEvt        = (Channel, Maybe UTCTime)
  EHType 'GuildCreateEvt              = (Guild, GuildCreateStatus)
  EHType 'GuildUpdateEvt              = (Guild, Guild)
  EHType 'GuildDeleteEvt              = (Guild, GuildDeleteStatus)
  EHType 'GuildBanAddEvt              = (Guild, User)
  EHType 'GuildBanRemoveEvt           = (Guild, User)
  EHType 'GuildEmojisUpdateEvt        = (Guild, [Emoji])
  EHType 'GuildIntegrationsUpdateEvt  = Guild
  EHType 'GuildMemberAddEvt           = Member
  EHType 'GuildMemberRemoveEvt        = Member
  EHType 'GuildMemberUpdateEvt        = (Member, Member)
  EHType 'GuildMembersChunkEvt        = (Guild, [Member])
  EHType 'GuildRoleCreateEvt          = (Guild, Role)
  EHType 'GuildRoleUpdateEvt          = (Guild, Role, Role)
  EHType 'GuildRoleDeleteEvt          = (Guild, Role)
  EHType 'InviteCreateEvt             = InviteCreateData
  EHType 'InviteDeleteEvt             = InviteDeleteData
  EHType 'MessageCreateEvt            = Message
  EHType 'MessageUpdateEvt            = (Message, Message)
  EHType 'MessageDeleteEvt            = Message
  EHType 'MessageDeleteBulkEvt        = [Message]
  EHType 'MessageReactionAddEvt       = (Message, Reaction)
  EHType 'MessageReactionRemoveEvt    = (Message, Reaction)
  EHType 'MessageReactionRemoveAllEvt = Message
  EHType 'RawMessageUpdateEvt            = UpdatedMessage
  EHType 'RawMessageDeleteEvt            = Snowflake Message
  EHType 'RawMessageDeleteBulkEvt        = [Snowflake Message]
  EHType 'RawMessageReactionAddEvt       = Reaction
  EHType 'RawMessageReactionRemoveEvt    = Reaction
  EHType 'RawMessageReactionRemoveAllEvt = Snowflake Message
  EHType 'TypingStartEvt              = (Channel, Snowflake User, UnixTimestamp)
  EHType 'UserUpdateEvt               = (User, User)
  EHType 'VoiceStateUpdateEvt         = (Maybe VoiceState, VoiceState)
  EHType ('CustomEvt s a)             = a

type StoredEHType t = EHType t -> IO ()

newtype EventHandlers = EventHandlers (TypeRepMap EventHandler)

data EventHandlerWithID a = EventHandlerWithID
  { ehID :: Integer
  , eh   :: a
  }

type family EHStorageType (t :: EventType) where
  EHStorageType ('CustomEvt s a) = LH.HashMap TypeRep (LH.HashMap TypeRep [EventHandlerWithID (StoredEHType ('CustomEvt s a))])
  EHStorageType t                = [EventHandlerWithID (StoredEHType t)]

newtype EventHandler (t :: EventType) = EH
  { unwrapEventHandler :: (Semigroup (EHStorageType t), Monoid (EHStorageType t)) => EHStorageType t
  }

instance Semigroup (EventHandler t) where
  EH a <> EH b = EH $ a <> b

instance Monoid (EventHandler t) where
  mempty = EH mempty

instance Default EventHandlers where
  def = EventHandlers $ fromList [ WrapTypeable $ EH @'ReadyEvt []
                                 , WrapTypeable $ EH @'ChannelCreateEvt []
                                 , WrapTypeable $ EH @'ChannelUpdateEvt []
                                 , WrapTypeable $ EH @'ChannelDeleteEvt []
                                 , WrapTypeable $ EH @'ChannelpinsUpdateEvt []
                                 , WrapTypeable $ EH @'GuildCreateEvt []
                                 , WrapTypeable $ EH @'GuildUpdateEvt []
                                 , WrapTypeable $ EH @'GuildDeleteEvt []
                                 , WrapTypeable $ EH @'GuildBanAddEvt []
                                 , WrapTypeable $ EH @'GuildBanRemoveEvt []
                                 , WrapTypeable $ EH @'GuildEmojisUpdateEvt []
                                 , WrapTypeable $ EH @'GuildIntegrationsUpdateEvt []
                                 , WrapTypeable $ EH @'GuildMemberAddEvt []
                                 , WrapTypeable $ EH @'GuildMemberRemoveEvt []
                                 , WrapTypeable $ EH @'GuildMemberUpdateEvt []
                                 , WrapTypeable $ EH @'GuildMembersChunkEvt []
                                 , WrapTypeable $ EH @'GuildRoleCreateEvt []
                                 , WrapTypeable $ EH @'GuildRoleUpdateEvt []
                                 , WrapTypeable $ EH @'GuildRoleDeleteEvt []
                                 , WrapTypeable $ EH @'MessageCreateEvt []
                                 , WrapTypeable $ EH @'MessageUpdateEvt []
                                 , WrapTypeable $ EH @'MessageDeleteEvt []
                                 , WrapTypeable $ EH @'MessageDeleteBulkEvt []
                                 , WrapTypeable $ EH @'MessageReactionAddEvt []
                                 , WrapTypeable $ EH @'MessageReactionRemoveEvt []
                                 , WrapTypeable $ EH @'MessageReactionRemoveAllEvt []
                                 , WrapTypeable $ EH @'TypingStartEvt []
                                 , WrapTypeable $ EH @'UserUpdateEvt []
                                 , WrapTypeable $ EH @('CustomEvt Void Dynamic) LH.empty
                                 ]

instance Semigroup EventHandlers where
  (EventHandlers a) <> (EventHandlers b) = EventHandlers $ TM.unionWith (<>) a b

instance Monoid EventHandlers where
  mempty = def

-- not sure what to think of this

type family EHInstanceSelector (d :: EventType) :: Bool where
  EHInstanceSelector ('CustomEvt _ _) = 'True
  EHInstanceSelector _                = 'False


fromDynamicJust :: forall a. Typeable a => Dynamic -> a
fromDynamicJust d = case fromDynamic d of
  Just x -> x
  Nothing -> error $ "Extracting dynamic failed, wanted: " <> (show . typeRep $ Proxy @a) <> ", got: " <> (show $ dynTypeRep d)


-- | A helper typeclass that is used to decide how to register regular
-- events, and custom events which require storing in a map at runtime.
class InsertEventHandler a where
  makeEventHandlers :: Proxy a -> Integer -> StoredEHType a -> EventHandlers

instance (EHInstanceSelector a ~ flag, InsertEventHandler' flag a) => InsertEventHandler a where
  makeEventHandlers = makeEventHandlers' (Proxy @flag)

class InsertEventHandler' (flag :: Bool) a where
  makeEventHandlers' :: Proxy flag -> Proxy a -> Integer -> StoredEHType a -> EventHandlers

intoDynFn :: forall a. Typeable a => (a -> IO ()) -> (Dynamic -> IO ())
intoDynFn fn = \d -> fn $ fromDynamicJust d

instance (Typeable a, Typeable s, Typeable (StoredEHType ('CustomEvt s a)), (EHType ('CustomEvt s a) -> IO ()) ~ (a -> IO ()))
  => InsertEventHandler' 'True ('CustomEvt s a) where
  makeEventHandlers' _ _ id' handler = EventHandlers . TM.one $ EH @('CustomEvt Void Dynamic)
    (LH.singleton (typeRep $ Proxy @s) (LH.singleton (typeRep $ Proxy @a) [EventHandlerWithID id' (intoDynFn handler)]))

instance (Typeable s, EHStorageType s ~ [EventHandlerWithID (StoredEHType s)], Typeable (StoredEHType s)) => InsertEventHandler' 'False s where
  makeEventHandlers' _ _ id' handler = EventHandlers . TM.one $ EH @s [EventHandlerWithID id' handler]


class GetEventHandlers a where
  getEventHandlers :: EventHandlers -> [StoredEHType a]

instance (EHInstanceSelector a ~ flag, GetEventHandlers' flag a) => GetEventHandlers a where
  getEventHandlers = getEventHandlers' (Proxy @a) (Proxy @flag)

class GetEventHandlers' (flag :: Bool) a where
  getEventHandlers' :: Proxy a -> Proxy flag -> EventHandlers -> [StoredEHType a]

instance GetEventHandlers' 'True ('CustomEvt s a) where
  getEventHandlers' _ _ _ = error "use getCustomEventHandlers instead"

instance (Typeable s, Typeable (StoredEHType s), EHStorageType s ~ [EventHandlerWithID (StoredEHType s)]) => GetEventHandlers' 'False s where
  getEventHandlers' _ _ (EventHandlers handlers) =
    let theseHandlers = unwrapEventHandler @s $ fromMaybe mempty (TM.lookup handlers :: Maybe (EventHandler s))
    in map eh theseHandlers


class RemoveEventHandler a where
  removeEventHandler :: Proxy a -> Integer -> EventHandlers -> EventHandlers

instance (EHInstanceSelector a ~ flag, RemoveEventHandler' flag a) => RemoveEventHandler a where
  removeEventHandler = removeEventHandler' (Proxy @flag)

class RemoveEventHandler' (flag :: Bool) a where
  removeEventHandler' :: Proxy flag -> Proxy a -> Integer -> EventHandlers -> EventHandlers

instance (Typeable s, Typeable a) => RemoveEventHandler' 'True ('CustomEvt s a) where
  removeEventHandler' _ _ id' (EventHandlers handlers) = EventHandlers $ TM.adjust @('CustomEvt Void Dynamic)
    (\(EH ehs) -> EH (LH.update (Just . LH.update (Just . filter ((/= id') . ehID)) (typeRep $ Proxy @a))
                      (typeRep $ Proxy @s) ehs)) handlers

instance (Typeable s, Typeable (StoredEHType s), EHStorageType s ~ [EventHandlerWithID (StoredEHType s)])
  => RemoveEventHandler' 'False s where
  removeEventHandler' _ _ id' (EventHandlers handlers) = EventHandlers $ TM.adjust @s
    (\(EH ehs) -> EH $ filter ((/= id') . ehID) ehs) handlers


getCustomEventHandlers :: TypeRep -> TypeRep -> EventHandlers -> [Dynamic -> IO ()]
getCustomEventHandlers s a (EventHandlers handlers) =
    let handlerMap = unwrapEventHandler @('CustomEvt Void Dynamic) $ fromMaybe mempty
          (TM.lookup handlers :: Maybe (EventHandler ('CustomEvt Void Dynamic)))
    in map eh . concat $ LH.lookup s handlerMap >>= LH.lookup a
