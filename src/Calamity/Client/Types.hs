-- | Types for the client
module Calamity.Client.Types
    ( Client(..)
    , Cache(..)
    , BotC
    , EHType
    , EHType'
    , EventHandlers(..)
    , EventHandler(..) ) where

import           Calamity.Gateway.DispatchEvents
import           Calamity.Gateway.Shard
import           Calamity.HTTP.Internal.Types
import           Calamity.Internal.MessageStore
import qualified Calamity.Internal.SnowflakeMap  as SM
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.Guild
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake
import           Calamity.Types.Token
import           Calamity.Types.UnixTimestamp

import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar

import           Data.Default.Class
import           Data.Dynamic
import qualified Data.HashSet                    as LS
import           Data.Time
import qualified Data.TypeRepMap                 as TM
import           Data.TypeRepMap                 ( TypeRepMap, WrapTypeable(..) )

import           GHC.Exts                        ( fromList )
import qualified GHC.TypeLits                    as TL

import qualified Polysemy                        as P
import qualified Polysemy.Async                  as P
import qualified Polysemy.AtomicState            as P
import qualified Polysemy.Reader                 as P

import qualified StmContainers.Set               as TS


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

data Client = Client
  { shards        :: TVar [(Shard, Async (Maybe ()))] -- TODO: migrate this to a set of Shard (make Shard hash to it's shardThread)
  , numShards     :: MVar Int
  , token         :: Token
  , rlState       :: RateLimitState
  , eventQueue    :: TQueue DispatchData
  , cache         :: TVar Cache
  , activeTasks   :: TS.Set (Async ()) -- ^ events currently being handled
  }
  deriving ( Generic )

type BotC r = (LogC r, P.Members '[P.Reader Client, P.AtomicState EventHandlers, P.Embed IO, P.Final IO, P.Async] r, Typeable r)

type family EHType d m where
  EHType "ready"                    m = ReadyData -> Cache                                   -> m ()
  EHType "channelcreate"            m = Channel   -> Cache                                   -> m ()
  EHType "channelupdate"            m = Channel   -> Channel       -> Cache                  -> m ()
  EHType "channeldelete"            m = Channel   -> Cache                                   -> m ()
  EHType "channelpinsupdate"        m = Channel   -> Maybe UTCTime -> Cache                  -> m ()
  EHType "guildcreate"              m = Guild     -> Bool          -> Cache                  -> m ()
  EHType "guildupdate"              m = Guild     -> Guild         -> Cache                  -> m ()
  EHType "guilddelete"              m = Guild     -> Bool          -> Cache                  -> m ()
  EHType "guildbanadd"              m = Guild     -> User          -> Cache                  -> m ()
  EHType "guildbanremove"           m = Guild     -> User          -> Cache                  -> m ()
  EHType "guildemojisupdate"        m = Guild     -> [Emoji]       -> Cache                  -> m ()
  EHType "guildintegrationsupdate"  m = Guild     -> Cache                                   -> m ()
  EHType "guildmemberadd"           m = Member    -> Cache                                   -> m ()
  EHType "guildmemberremove"        m = Member    -> Cache                                   -> m ()
  EHType "guildmemberupdate"        m = Member    -> Member        -> Cache                  -> m ()
  EHType "guildmemberschunk"        m = Guild     -> [Member]      -> Cache                  -> m ()
  EHType "guildrolecreate"          m = Guild     -> Role          -> Cache                  -> m ()
  EHType "guildroleupdate"          m = Guild     -> Role          -> Role          -> Cache -> m ()
  EHType "guildroledelete"          m = Guild     -> Role          -> Cache                  -> m ()
  EHType "messagecreate"            m = Message   -> Cache                                   -> m ()
  EHType "messageupdate"            m = Message   -> Message       -> Cache                  -> m ()
  EHType "messagedelete"            m = Message   -> Cache                                   -> m ()
  EHType "messagedeletebulk"        m = [Message] -> Cache                                   -> m ()
  EHType "messagereactionadd"       m = Message   -> Reaction      -> Cache                  -> m ()
  EHType "messagereactionremove"    m = Message   -> Reaction      -> Cache                  -> m ()
  EHType "messagereactionremoveall" m = Message   -> Cache                                   -> m ()
  EHType "typingstart"              m = Channel   -> Maybe Member  -> UnixTimestamp -> Cache -> m ()
  EHType "userupdate"               m = User      -> User          -> Cache                  -> m ()
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
