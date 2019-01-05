-- | Types for the client

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module YAHDL.Client.Types
  ( Client(..)
  , BotM(..)
  , CacheState(..)
  )
where

import           GHC.Exts                       ( fromList )
import           Data.Default
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Control.Monad.Catch            ( MonadMask )
import           Control.Monad.Log              ( LogT(..)
                                                , MonadLog
                                                , runLogTSafe
                                                )
import           Data.Time
import           Data.TypeRepMap                ( TypeRepMap
                                                , WrapTypeable(..)
                                                )
import qualified StmContainers.Map             as SC
import qualified Streamly                      as S

import           YAHDL.Gateway.Shard
import           YAHDL.HTTP.Ratelimit
import           YAHDL.Types.General
import           YAHDL.Types.Snowflake
import           YAHDL.Types.DispatchEvents
-- import           YAHDL.Client.ShardManager

data CacheState = CacheState
  { user    :: TMVar User
  , guilds  :: SC.Map (Snowflake Guild) Guild
  , members :: SC.Map (Snowflake User) User
  , dms     :: SC.Map (Snowflake DM) DM
  } deriving (Generic)

data Client = Client
  { shards      :: TVar [(Shard, Async ())]
  , token       :: Token
  , rlState     :: RateLimitState
  , eventStream :: S.Serial DispatchData
  , cache       :: CacheState
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

newtype EventHandler d = EH (EHType d)

newtype EventHandlers = EventHandlers (TypeRepMap EventHandler)

type family EHType d where
  EHType "ready"                    = [ReadyData -> BotM ()]
  EHType "channelcreate"            = [Channel -> BotM ()]
  EHType "channelupdate"            = [Channel -> Channel -> BotM ()]
  EHType "channeldelete"            = [Channel -> BotM ()]
  EHType "channelpinsupdate"        = [Channel -> UTCTime -> BotM ()]
  EHType "guildcreate"              = [GuildCreateData -> BotM ()] -- TODO: functions on and below this line
  EHType "guildupdate"              = [GuildUpdateData -> BotM ()]
  EHType "guilddelete"              = [GuildDeleteData -> BotM ()]
  EHType "guildbanadd"              = [GuildBanAddData -> BotM ()]
  EHType "guildbanremove"           = [GuildBanRemoveData -> BotM ()]
  EHType "guildemojisupdate"        = [GuildEmojisUpdateData -> BotM ()]
  EHType "guildintegrationsupdate"  = [GuildIntegrationsUpdateData -> BotM ()]
  EHType "guildmemberadd"           = [GuildMemberAddData -> BotM ()]
  EHType "guildmemberremove"        = [GuildMemberRemoveData -> BotM ()]
  EHType "guildmemberupdate"        = [GuildMemberUpdateData -> BotM ()]
  EHType "guildmemberschunk"        = [GuildMembersChunkData -> BotM ()]
  EHType "guildrolecreate"          = [GuildRoleCreateData -> BotM ()]
  EHType "guildroleupdate"          = [GuildRoleUpdateData -> BotM ()]
  EHType "guildroledelete"          = [GuildRoleDeleteData -> BotM ()]
  EHType "messagecreate"            = [MessageCreateData -> BotM ()]
  EHType "messageupdate"            = [MessageUpdateData -> BotM ()]
  EHType "messagedelete"            = [MessageDeleteData -> BotM ()]
  EHType "messagedeletebulk"        = [MessageDeleteBulkData -> BotM ()]
  EHType "messagereactionadd"       = [MessageReactionAddData -> BotM ()]
  EHType "messagereactionremove"    = [MessageReactionRemoveData -> BotM ()]
  EHType "messagereactionremoveall" = [MessageReactionRemoveAllData -> BotM ()]
  EHType "presenceupdate"           = [PresenceUpdateData -> BotM ()]
  EHType "typingstart"              = [TypingStartData -> BotM ()]
  EHType "userupdate"               = [UserUpdateData -> BotM ()]
  EHType "voicestateupdate"         = [VoiceStateUpdateData -> BotM ()]
  EHType "voiceserverupdate"        = [VoiceServerUpdateData -> BotM ()]
  EHType "webhooksupdate"           = [WebhooksUpdateData -> BotM ()]


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
                                 , WrapTypeable $ EH @"guildmemberschunk" []
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
                                 , WrapTypeable $ EH @"presenceupdate" []
                                 , WrapTypeable $ EH @"typingstart" []
                                 , WrapTypeable $ EH @"userupdate" []
                                 , WrapTypeable $ EH @"voicestateupdate" []
                                 , WrapTypeable $ EH @"voiceserverupdate" []
                                 , WrapTypeable $ EH @"webhooksupdate" []
                                 ]
