-- | Types for the client
module Calamity.Client.Types
    ( Client(..)
    , EventType(..)
    , EHType
    , BotC
    , SetupEff
    , EventHandlers(..)
    , InsertEventHandler(..)
    , getEventHandlers
    , getCustomEventHandlers ) where

import           Calamity.Cache.Eff
import           Calamity.Gateway.DispatchEvents ( CalamityEvent(..), ReadyData )
import           Calamity.Gateway.Types          ( ControlMessage )
import           Calamity.HTTP.Internal.Types
import           Calamity.Metrics.Eff
import           Calamity.Types.LogEff
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.Guild
import           Calamity.Types.Model.User
import           Calamity.Types.Token
import           Calamity.Types.UnixTimestamp

import           Control.Concurrent.Async
import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.MVar
import           Control.Concurrent.STM.TVar

import           Data.Default.Class
import           Data.Dynamic
import           Data.Functor
import qualified Data.HashMap.Lazy               as LH
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

data Client = Client
  { shards        :: TVar [(InChan ControlMessage, Async (Maybe ()))]
  , numShards     :: MVar Int
  , token         :: Token
  , rlState       :: RateLimitState
  , eventsIn      :: InChan CalamityEvent
  , eventsOut     :: OutChan CalamityEvent
  }
  deriving ( Generic )

type BotC r =
  ( P.Members '[LogEff, MetricEff, CacheEff, P.Reader Client,
  P.AtomicState EventHandlers, P.Embed IO, P.Final IO, P.Async] r
  , Typeable r)

-- | A concrete effect stack used inside the bot
type SetupEff r = P.Sem (LogEff ': P.Reader Client ': P.AtomicState EventHandlers ': P.Async ': r) ()

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
  | MessageCreateEvt
  | MessageUpdateEvt
  | MessageDeleteEvt
  | MessageDeleteBulkEvt
  | MessageReactionAddEvt
  | MessageReactionRemoveEvt
  | MessageReactionRemoveAllEvt
  | TypingStartEvt
  | UserUpdateEvt
  | forall s a. CustomEvt s a
  -- ^ A custom event, @s@ is the name and @a@ is the data sent to the handler

-- | A type family to decide what the parameters for an event handler should be
-- determined by the type of event it is handling.
type family EHType (d :: EventType) m where
  EHType 'ReadyEvt                    m = ReadyData                                   -> m ()
  EHType 'ChannelCreateEvt            m = Channel                                     -> m ()
  EHType 'ChannelUpdateEvt            m = Channel   -> Channel                        -> m ()
  EHType 'ChannelDeleteEvt            m = Channel                                     -> m ()
  EHType 'ChannelpinsUpdateEvt        m = Channel   -> Maybe UTCTime                  -> m ()
  EHType 'GuildCreateEvt              m = Guild     -> Bool                           -> m ()
  EHType 'GuildUpdateEvt              m = Guild     -> Guild                          -> m ()
  EHType 'GuildDeleteEvt              m = Guild     -> Bool                           -> m ()
  EHType 'GuildBanAddEvt              m = Guild     -> User                           -> m ()
  EHType 'GuildBanRemoveEvt           m = Guild     -> User                           -> m ()
  EHType 'GuildEmojisUpdateEvt        m = Guild     -> [Emoji]                        -> m ()
  EHType 'GuildIntegrationsUpdateEvt  m = Guild                                       -> m ()
  EHType 'GuildMemberAddEvt           m = Member                                      -> m ()
  EHType 'GuildMemberRemoveEvt        m = Member                                      -> m ()
  EHType 'GuildMemberUpdateEvt        m = Member    -> Member                         -> m ()
  EHType 'GuildMembersChunkEvt        m = Guild     -> [Member]                       -> m ()
  EHType 'GuildRoleCreateEvt          m = Guild     -> Role                           -> m ()
  EHType 'GuildRoleUpdateEvt          m = Guild     -> Role          -> Role          -> m ()
  EHType 'GuildRoleDeleteEvt          m = Guild     -> Role                           -> m ()
  EHType 'MessageCreateEvt            m = Message                                     -> m ()
  EHType 'MessageUpdateEvt            m = Message   -> Message                        -> m ()
  EHType 'MessageDeleteEvt            m = Message                                     -> m ()
  EHType 'MessageDeleteBulkEvt        m = [Message]                                   -> m ()
  EHType 'MessageReactionAddEvt       m = Message   -> Reaction                       -> m ()
  EHType 'MessageReactionRemoveEvt    m = Message   -> Reaction                       -> m ()
  EHType 'MessageReactionRemoveAllEvt m = Message                                     -> m ()
  EHType 'TypingStartEvt              m = Channel   -> Maybe Member  -> UnixTimestamp -> m ()
  EHType 'UserUpdateEvt               m = User      -> User                           -> m ()
  EHType ('CustomEvt s a)             m = a                                           -> m ()

type family EHType' (d :: EventType) where
  EHType' 'ReadyEvt                    = Dynamic
  EHType' 'ChannelCreateEvt            = Dynamic
  EHType' 'ChannelUpdateEvt            = Dynamic
  EHType' 'ChannelDeleteEvt            = Dynamic
  EHType' 'ChannelpinsUpdateEvt        = Dynamic
  EHType' 'GuildCreateEvt              = Dynamic
  EHType' 'GuildUpdateEvt              = Dynamic
  EHType' 'GuildDeleteEvt              = Dynamic
  EHType' 'GuildBanAddEvt              = Dynamic
  EHType' 'GuildBanRemoveEvt           = Dynamic
  EHType' 'GuildEmojisUpdateEvt        = Dynamic
  EHType' 'GuildIntegrationsUpdateEvt  = Dynamic
  EHType' 'GuildMemberAddEvt           = Dynamic
  EHType' 'GuildMemberRemoveEvt        = Dynamic
  EHType' 'GuildMemberUpdateEvt        = Dynamic
  EHType' 'GuildMembersChunkEvt        = Dynamic
  EHType' 'GuildRoleCreateEvt          = Dynamic
  EHType' 'GuildRoleUpdateEvt          = Dynamic
  EHType' 'GuildRoleDeleteEvt          = Dynamic
  EHType' 'MessageCreateEvt            = Dynamic
  EHType' 'MessageUpdateEvt            = Dynamic
  EHType' 'MessageDeleteEvt            = Dynamic
  EHType' 'MessageDeleteBulkEvt        = Dynamic
  EHType' 'MessageReactionAddEvt       = Dynamic
  EHType' 'MessageReactionRemoveEvt    = Dynamic
  EHType' 'MessageReactionRemoveAllEvt = Dynamic
  EHType' 'TypingStartEvt              = Dynamic
  EHType' 'UserUpdateEvt               = Dynamic
  EHType' ('CustomEvt _ _)             = Dynamic

newtype EventHandlers = EventHandlers (TypeRepMap EventHandler)

type family EHStorageType t where
  EHStorageType ('CustomEvt s a) = LH.HashMap TypeRep (LH.HashMap TypeRep [Dynamic])
  EHStorageType t                = [EHType' t]

newtype EventHandler t = EH
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
                                 , WrapTypeable $ EH @('CustomEvt Void Void) LH.empty
                                 ]

instance Semigroup EventHandlers where
  (EventHandlers a) <> (EventHandlers b) = EventHandlers $ TM.unionWith (<>) a b

instance Monoid EventHandlers where
  mempty = def

-- not sure what to think of this

type family EHInstanceSelector (d :: EventType) :: Bool where
  EHInstanceSelector ('CustomEvt _ _) = 'True
  EHInstanceSelector _                = 'False

-- | A helper typeclass that is used to decide how to register regular
-- events, and custom events which require storing in a map at runtime.
class InsertEventHandler a m where
  makeEventHandlers :: Proxy a -> Proxy m -> EHType a m -> EventHandlers

instance (EHInstanceSelector a ~ flag, InsertEventHandler' flag a m) => InsertEventHandler a m where
  makeEventHandlers = makeEventHandlers' (Proxy @flag)

class InsertEventHandler' (flag :: Bool) a m where
  makeEventHandlers' :: Proxy flag -> Proxy a -> Proxy m -> EHType a m -> EventHandlers

instance (Typeable a, Typeable s, Typeable (EHType ('CustomEvt s a) m))
  => InsertEventHandler' 'True ('CustomEvt s a) m where
  makeEventHandlers' _ _ _ handler = EventHandlers . TM.one $ EH @('CustomEvt Void Void)
    (LH.singleton (typeRep $ Proxy @s) (LH.singleton (typeRep $ Proxy @a) [toDyn handler]))

instance (Typeable s, EHStorageType s ~ [Dynamic], Typeable (EHType s m)) => InsertEventHandler' 'False s m where
  makeEventHandlers' _ _ _ handler = EventHandlers . TM.one $ EH @s [toDyn handler]


class GetEventHandlers a m where
  getEventHandlers :: EventHandlers -> [EHType a m]

instance (EHInstanceSelector a ~ flag, GetEventHandlers' flag a m) => GetEventHandlers a m where
  getEventHandlers = getEventHandlers' (Proxy @a) (Proxy @m) (Proxy @flag)

class GetEventHandlers' (flag :: Bool) a m where
  getEventHandlers' :: Proxy a -> Proxy m -> Proxy flag -> EventHandlers -> [EHType a m]

instance (Typeable a, Typeable s, Typeable (EHType ('CustomEvt s a) m)) => GetEventHandlers' 'True ('CustomEvt s a) m where
  getEventHandlers' _ _ _ (EventHandlers handlers) =
    let handlerMap = unwrapEventHandler @('CustomEvt Void Void) $ fromJust
          (TM.lookup handlers :: Maybe (EventHandler ('CustomEvt Void Void)))
    in concat $ LH.lookup (typeRep $ Proxy @s) handlerMap >>= LH.lookup (typeRep $ Proxy @a) <&> map
       (fromJust . fromDynamic)

instance (Typeable s, Typeable (EHType s m), EHStorageType s ~ [Dynamic]) => GetEventHandlers' 'False s m where
  getEventHandlers' _ _ _ (EventHandlers handlers) =
    let theseHandlers = unwrapEventHandler @s $ fromJust (TM.lookup handlers :: Maybe (EventHandler s))
    in map (fromJust . fromDynamic) theseHandlers


getCustomEventHandlers :: TypeRep -> TypeRep -> EventHandlers -> [Dynamic]
getCustomEventHandlers s a (EventHandlers handlers) =
    let handlerMap = unwrapEventHandler @('CustomEvt Void Void) $ fromJust
          (TM.lookup handlers :: Maybe (EventHandler ('CustomEvt Void Void)))
    in concat $ LH.lookup s handlerMap >>= LH.lookup a
