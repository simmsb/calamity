-- | Types for the client
module Calamity.Client.Types
    ( Client(..)
    , BotC
    , SetupEff
    , EHType
    , EHType'
    , EventHandlers(..)
    , EventHandler(..) ) where

import           Calamity.Metrics.Eff
import           Calamity.Cache.Eff
import           Calamity.Gateway.DispatchEvents
import           Calamity.Gateway.Shard
import           Calamity.HTTP.Internal.Types
import           Calamity.LogEff
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.Guild
import           Calamity.Types.Model.User
import           Calamity.Types.Token
import           Calamity.Types.UnixTimestamp

import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar

import           Data.Default.Class
import           Data.Dynamic
import           Data.Time
import qualified Data.TypeRepMap                 as TM
import           Data.TypeRepMap                 ( TypeRepMap, WrapTypeable(..) )

import           GHC.Exts                        ( fromList )
import           GHC.Generics
import qualified GHC.TypeLits                    as TL

import qualified Polysemy                        as P
import qualified Polysemy.Async                  as P
import qualified Polysemy.AtomicState            as P
import qualified Polysemy.Reader                 as P

data Client = Client
  { shards        :: TVar [(Shard, Async (Maybe ()))]
  , numShards     :: MVar Int
  , token         :: Token
  , rlState       :: RateLimitState
  , eventQueue    :: TQueue DispatchMessage
  }
  deriving ( Generic )

type BotC r =
  ( P.Members '[LogEff, MetricEff, CacheEff, P.Reader Client,
  P.AtomicState EventHandlers, P.Embed IO, P.Final IO, P.Async] r
  , Typeable r)

type SetupEff r = P.Sem (LogEff ': P.Reader Client ': P.AtomicState EventHandlers ': P.Async ': r) ()

type family EHType d m where
  EHType "ready"                    m = ReadyData                                   -> m ()
  EHType "channelcreate"            m = Channel                                     -> m ()
  EHType "channelupdate"            m = Channel   -> Channel                        -> m ()
  EHType "channeldelete"            m = Channel                                     -> m ()
  EHType "channelpinsupdate"        m = Channel   -> Maybe UTCTime                  -> m ()
  EHType "guildcreate"              m = Guild     -> Bool                           -> m ()
  EHType "guildupdate"              m = Guild     -> Guild                          -> m ()
  EHType "guilddelete"              m = Guild     -> Bool                           -> m ()
  EHType "guildbanadd"              m = Guild     -> User                           -> m ()
  EHType "guildbanremove"           m = Guild     -> User                           -> m ()
  EHType "guildemojisupdate"        m = Guild     -> [Emoji]                        -> m ()
  EHType "guildintegrationsupdate"  m = Guild                                       -> m ()
  EHType "guildmemberadd"           m = Member                                      -> m ()
  EHType "guildmemberremove"        m = Member                                      -> m ()
  EHType "guildmemberupdate"        m = Member    -> Member                         -> m ()
  EHType "guildmemberschunk"        m = Guild     -> [Member]                       -> m ()
  EHType "guildrolecreate"          m = Guild     -> Role                           -> m ()
  EHType "guildroleupdate"          m = Guild     -> Role          -> Role          -> m ()
  EHType "guildroledelete"          m = Guild     -> Role                           -> m ()
  EHType "messagecreate"            m = Message                                     -> m ()
  EHType "messageupdate"            m = Message   -> Message                        -> m ()
  EHType "messagedelete"            m = Message                                     -> m ()
  EHType "messagedeletebulk"        m = [Message]                                   -> m ()
  EHType "messagereactionadd"       m = Message   -> Reaction                       -> m ()
  EHType "messagereactionremove"    m = Message   -> Reaction                       -> m ()
  EHType "messagereactionremoveall" m = Message                                     -> m ()
  EHType "typingstart"              m = Channel   -> Maybe Member  -> UnixTimestamp -> m ()
  EHType "userupdate"               m = User      -> User                           -> m ()
  EHType s _ = TL.TypeError ('TL.Text "Unknown event name: " 'TL.:<>: 'TL.ShowType s)
  -- EHType "voicestateupdate"         = VoiceStateUpdateData -> EventM ()
  -- EHType "voiceserverupdate"        = VoiceServerUpdateData -> EventM ()
  -- EHType "webhooksupdate"           = WebhooksUpdateData -> EventM ()

type family EHType' d where
  EHType' "ready"                    = Dynamic
  EHType' "channelcreate"            = Dynamic
  EHType' "channelupdate"            = Dynamic
  EHType' "channeldelete"            = Dynamic
  EHType' "channelpinsupdate"        = Dynamic
  EHType' "guildcreate"              = Dynamic
  EHType' "guildupdate"              = Dynamic
  EHType' "guilddelete"              = Dynamic
  EHType' "guildbanadd"              = Dynamic
  EHType' "guildbanremove"           = Dynamic
  EHType' "guildemojisupdate"        = Dynamic
  EHType' "guildintegrationsupdate"  = Dynamic
  EHType' "guildmemberadd"           = Dynamic
  EHType' "guildmemberremove"        = Dynamic
  EHType' "guildmemberupdate"        = Dynamic
  EHType' "guildmemberschunk"        = Dynamic
  EHType' "guildrolecreate"          = Dynamic
  EHType' "guildroleupdate"          = Dynamic
  EHType' "guildroledelete"          = Dynamic
  EHType' "messagecreate"            = Dynamic
  EHType' "messageupdate"            = Dynamic
  EHType' "messagedelete"            = Dynamic
  EHType' "messagedeletebulk"        = Dynamic
  EHType' "messagereactionadd"       = Dynamic
  EHType' "messagereactionremove"    = Dynamic
  EHType' "messagereactionremoveall" = Dynamic
  EHType' "typingstart"              = Dynamic
  EHType' "userupdate"               = Dynamic
  EHType' s = TL.TypeError ('TL.Text "Unknown event name: " 'TL.:<>: 'TL.ShowType s)

newtype EventHandlers = EventHandlers (TypeRepMap EventHandler)

newtype EventHandler d = EH
  { unwrapEventHandler :: [EHType' d]
  }
  deriving newtype ( Semigroup, Monoid )

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
                                 -- , WrapTypeable $ EH m @"voicestateupdate" []
                                 -- , WrapTypeable $ EH m @"voiceserverupdate" []
                                 -- , WrapTypeable $ EH m @"webhooksupdate" []
                                 ]

instance Semigroup EventHandlers where
  (EventHandlers a) <> (EventHandlers b) = EventHandlers $ TM.unionWith (<>) a b

instance Monoid EventHandlers where
  mempty = def
