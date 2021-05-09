-- | Types for the client
module Calamity.Client.Types (
  Client (..),
  StartupError (..),
  EventType (..),
  GuildCreateStatus (..),
  GuildDeleteStatus (..),
  EHType,
  BotC,
  SetupEff,
  ReactConstraints,
  EventHandlers (..),
  InsertEventHandler (..),
  RemoveEventHandler (..),
  getEventHandlers,
  getCustomEventHandlers,
) where

import Calamity.Cache.Eff
import Calamity.Gateway.DispatchEvents (CalamityEvent (..), InviteCreateData, InviteDeleteData, ReactionEvtData, ReadyData)
import Calamity.Gateway.Types (ControlMessage)
import Calamity.HTTP.Internal.Types
import Calamity.Metrics.Eff
import Calamity.Types.LogEff
import Calamity.Types.Model.Channel
import Calamity.Types.Model.Channel.UpdatedMessage
import Calamity.Types.Model.Guild
import Calamity.Types.Model.User
import Calamity.Types.Model.Voice
import Calamity.Types.Snowflake
import Calamity.Types.Token
import Calamity.Types.UnixTimestamp

import Control.Concurrent.Async
import Control.Concurrent.Chan.Unagi
import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar

import Data.Default.Class
import Data.Dynamic
import Data.IORef
import Data.Maybe
import Data.Time
import Data.TypeRepMap (TypeRepMap, WrapTypeable (..))
import qualified Data.TypeRepMap as TM
import Data.Typeable

import GHC.Exts (fromList)
import GHC.Generics

import qualified Polysemy as P
import qualified Polysemy.Async as P
import qualified Polysemy.AtomicState as P
import qualified Polysemy.Reader as P

import qualified Df1
import qualified Di.Core as DC
import TextShow
import qualified TextShow.Generic as TSG
import Data.Kind (Type)
import Data.Void (Void)

data Client = Client
  { shards :: TVar [(InChan ControlMessage, Async (Maybe ()))]
  , numShards :: MVar Int
  , token :: Token
  , rlState :: RateLimitState
  , eventsIn :: InChan CalamityEvent
  , eventsOut :: OutChan CalamityEvent
  , ehidCounter :: IORef Integer
  , initialDi :: Maybe (DC.Di Df1.Level Df1.Path Df1.Message)
  }
  deriving (Generic)

-- | Constraints required by the bot client
type BotC r =
  ( P.Members
      '[ LogEff
       , MetricEff
       , CacheEff
       , P.Reader Client
       , P.AtomicState EventHandlers
       , P.Embed IO
       , P.Final IO
       , P.Async
       ]
      r
  , Typeable r
  )

-- | A concrete effect stack used inside the bot
type SetupEff r = (P.Reader Client ': P.AtomicState EventHandlers ': P.Async ': r)

{- | Some constraints that 'Calamity.Client.Client.react' needs to work. Don't
 worry about these since they are satisfied for any type @s@ can be
-}
type ReactConstraints s =
  ( InsertEventHandler s
  , RemoveEventHandler s
  )

newtype StartupError = StartupError String
  deriving (Show)

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
  | -- | Fired when a cached message is updated, use 'RawMessageUpdateEvt' to see
    -- updates of uncached messages
    MessageUpdateEvt
  | -- | Fired when a message is updated
    RawMessageUpdateEvt
  | -- | Fired when a cached message is deleted, use 'RawMessageDeleteEvt' to see
    -- deletes of uncached messages.
    --
    -- Does not include messages deleted through bulk deletes, use
    -- 'MessageDeleteBulkEvt' for those
    MessageDeleteEvt
  | -- | Fired when a message is deleted.
    --
    -- Does not include messages deleted through bulk deletes, use
    -- 'RawMessageDeleteBulkEvt' for those
    RawMessageDeleteEvt
  | -- | Fired when messages are bulk deleted. Only includes cached messages, use
    -- 'RawMessageDeleteBulkEvt' to see deletes of uncached messages.
    MessageDeleteBulkEvt
  | -- | Fired when messages are bulk deleted.
    RawMessageDeleteBulkEvt
  | -- | Fired when a reaction is added to a cached message, use
    -- 'RawMessageReactionAddEvt' to see reactions on uncached messages.
    MessageReactionAddEvt
  | -- | Fired when a reaction is added to a message.
    RawMessageReactionAddEvt
  | -- | Fired when a reaction is removed from a cached message, use
    -- 'RawMessageReactionRemoveEvt' to see reactions on uncached messages.
    MessageReactionRemoveEvt
  | -- | Fired when a reaction is removed from a message.
    RawMessageReactionRemoveEvt
  | -- | Fired when all reactions are removed from a cached message, use
    -- 'RawMessageReactionRemoveEvt' to see reactions on uncached messages.
    --
    -- The message passed will contain the removed events.
    MessageReactionRemoveAllEvt
  | -- | Fired when all reactions are removed from a message.
    RawMessageReactionRemoveAllEvt
  | TypingStartEvt
  | UserUpdateEvt
  | -- | Sent when someone joins/leaves/moves voice channels
    VoiceStateUpdateEvt
  | -- | A custom event, @a@ is the data sent to the handler and should probably
    -- be a newtype to disambiguate events
    forall (a :: Type). CustomEvt a

data GuildCreateStatus
  = -- | The guild was just joined
    GuildCreateNew
  | -- | The guild is becoming available
    GuildCreateAvailable
  deriving (Generic, Show)
  deriving (TextShow) via TSG.FromGeneric GuildCreateStatus

data GuildDeleteStatus
  = -- | The guild became unavailable
    GuildDeleteUnavailable
  | -- | The bot was removed from the guild
    GuildDeleteRemoved
  deriving (Generic, Show)
  deriving (TextShow) via TSG.FromGeneric GuildDeleteStatus

{- | A type family to decide what the parameters for an event handler should be
 determined by the type of event it is handling.
-}
type family EHType (d :: EventType) where
  EHType 'ReadyEvt = ReadyData
  EHType 'ChannelCreateEvt = Channel
  EHType 'ChannelUpdateEvt = (Channel, Channel)
  EHType 'ChannelDeleteEvt = Channel
  EHType 'ChannelpinsUpdateEvt = (Channel, Maybe UTCTime)
  EHType 'GuildCreateEvt = (Guild, GuildCreateStatus)
  EHType 'GuildUpdateEvt = (Guild, Guild)
  EHType 'GuildDeleteEvt = (Guild, GuildDeleteStatus)
  EHType 'GuildBanAddEvt = (Guild, User)
  EHType 'GuildBanRemoveEvt = (Guild, User)
  EHType 'GuildEmojisUpdateEvt = (Guild, [Emoji])
  EHType 'GuildIntegrationsUpdateEvt = Guild
  EHType 'GuildMemberAddEvt = Member
  EHType 'GuildMemberRemoveEvt = Member
  EHType 'GuildMemberUpdateEvt = (Member, Member)
  EHType 'GuildMembersChunkEvt = (Guild, [Member])
  EHType 'GuildRoleCreateEvt = (Guild, Role)
  EHType 'GuildRoleUpdateEvt = (Guild, Role, Role)
  EHType 'GuildRoleDeleteEvt = (Guild, Role)
  EHType 'InviteCreateEvt = InviteCreateData
  EHType 'InviteDeleteEvt = InviteDeleteData
  EHType 'MessageCreateEvt = Message
  EHType 'MessageUpdateEvt = (Message, Message)
  EHType 'MessageDeleteEvt = Message
  EHType 'MessageDeleteBulkEvt = [Message]
  EHType 'MessageReactionAddEvt = (Message, User, Channel, RawEmoji)
  EHType 'MessageReactionRemoveEvt = (Message, User, Channel, RawEmoji)
  EHType 'MessageReactionRemoveAllEvt = Message
  EHType 'RawMessageUpdateEvt = UpdatedMessage
  EHType 'RawMessageDeleteEvt = Snowflake Message
  EHType 'RawMessageDeleteBulkEvt = [Snowflake Message]
  EHType 'RawMessageReactionAddEvt = ReactionEvtData
  EHType 'RawMessageReactionRemoveEvt = ReactionEvtData
  EHType 'RawMessageReactionRemoveAllEvt = Snowflake Message
  EHType 'TypingStartEvt = (Channel, Snowflake User, UnixTimestamp)
  EHType 'UserUpdateEvt = (User, User)
  EHType 'VoiceStateUpdateEvt = (Maybe VoiceState, VoiceState)
  EHType ( 'CustomEvt a) = a

type StoredEHType t = EHType t -> IO ()

newtype EventHandlers = EventHandlers (TypeRepMap EventHandler)

data EventHandlerWithID (a :: Type) = EventHandlerWithID
  { ehID :: Integer
  , eh :: a
  }

newtype CustomEHTypeStorage (a :: Type) = CustomEHTypeStorage
  { unwrapCustomEHTypeStorage :: [EventHandlerWithID (a -> IO ())]
  }
  deriving newtype ( Monoid, Semigroup )

type family EHStorageType (t :: EventType) where
  EHStorageType ( 'CustomEvt _) = TypeRepMap CustomEHTypeStorage
  EHStorageType t = [EventHandlerWithID (StoredEHType t)]

newtype EventHandler (t :: EventType) = EH
  { unwrapEventHandler :: (Semigroup (EHStorageType t), Monoid (EHStorageType t)) => EHStorageType t
  }

instance Semigroup (EventHandler t) where
  EH a <> EH b = EH $ a <> b

instance Monoid (EventHandler t) where
  mempty = EH mempty

instance Default EventHandlers where
  def =
    EventHandlers $
      fromList
        [ WrapTypeable $ EH @'ReadyEvt []
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
        , WrapTypeable $ EH @('CustomEvt Void) TM.empty
        ]

instance Semigroup EventHandlers where
  (EventHandlers a) <> (EventHandlers b) = EventHandlers $ TM.unionWith (<>) a b

instance Monoid EventHandlers where
  mempty = def

-- not sure what to think of this

type family EHInstanceSelector (d :: EventType) :: Bool where
  EHInstanceSelector ( 'CustomEvt _) = 'True
  EHInstanceSelector _ = 'False

{- | A helper typeclass that is used to decide how to register regular
 events, and custom events which require storing in a map at runtime.
-}
class InsertEventHandler (a :: EventType) where
  makeEventHandlers :: Proxy a -> Integer -> StoredEHType a -> EventHandlers

instance (EHInstanceSelector a ~ flag, InsertEventHandler' flag a) => InsertEventHandler a where
  makeEventHandlers = makeEventHandlers' (Proxy @flag)

class InsertEventHandler' (flag :: Bool) a where
  makeEventHandlers' :: Proxy flag -> Proxy a -> Integer -> StoredEHType a -> EventHandlers

instance forall (x :: Type). (Typeable (EHType ('CustomEvt x))) => InsertEventHandler' 'True ( 'CustomEvt x) where
  makeEventHandlers' _ _ id' handler =
    EventHandlers . TM.one $
      EH @( 'CustomEvt Void)
        (TM.one @x $ CustomEHTypeStorage [EventHandlerWithID id' handler])

instance (Typeable s, EHStorageType s ~ [EventHandlerWithID (StoredEHType s)], Typeable (StoredEHType s)) => InsertEventHandler' 'False s where
  makeEventHandlers' _ _ id' handler = EventHandlers . TM.one $ EH @s [EventHandlerWithID id' handler]

class GetEventHandlers a where
  getEventHandlers :: EventHandlers -> [StoredEHType a]

instance (EHInstanceSelector a ~ flag, GetEventHandlers' flag a) => GetEventHandlers a where
  getEventHandlers = getEventHandlers' (Proxy @a) (Proxy @flag)

class GetEventHandlers' (flag :: Bool) a where
  getEventHandlers' :: Proxy a -> Proxy flag -> EventHandlers -> [StoredEHType a]

instance GetEventHandlers' 'True ( 'CustomEvt a) where
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

instance forall (a :: Type). (Typeable a) => RemoveEventHandler' 'True ( 'CustomEvt a) where
  removeEventHandler' _ _ id' (EventHandlers handlers) =
    EventHandlers $
      TM.adjust @( 'CustomEvt Void)
        ( \(EH ehs) ->
            EH
              ( TM.adjust @a
                  (CustomEHTypeStorage . filter ((/= id') . ehID) . unwrapCustomEHTypeStorage)
                  ehs
              )
        )
        handlers

instance
  (Typeable s, Typeable (StoredEHType s), EHStorageType s ~ [EventHandlerWithID (StoredEHType s)]) =>
  RemoveEventHandler' 'False s
  where
  removeEventHandler' _ _ id' (EventHandlers handlers) =
    EventHandlers $
      TM.adjust @s
        (\(EH ehs) -> EH $ filter ((/= id') . ehID) ehs)
        handlers

getCustomEventHandlers :: forall a. Typeable a => EventHandlers -> [a -> IO ()]
getCustomEventHandlers (EventHandlers handlers) =
  let handlerMap =
        unwrapEventHandler @( 'CustomEvt Void) $
          fromMaybe mempty (TM.lookup handlers :: Maybe (EventHandler ( 'CustomEvt Void)))
   in maybe mempty (map eh . unwrapCustomEHTypeStorage) $ TM.lookup @a handlerMap
