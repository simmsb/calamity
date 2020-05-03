-- | Guild endpoints
module Calamity.HTTP.Guild
    ( GuildRequest(..)
    , CreateGuildData(..)
    , ModifyGuildData(..)
    , ChannelCreateData(..)
    , ChannelPosition(..)
    , ListMembersOptions(..)
    , AddGuildMemberData(..)
    , ModifyGuildMemberData(..)
    , CreateGuildBanData(..)
    , ModifyGuildRoleData(..)
    , ModifyGuildRolePositionsData(..) ) where

import           Calamity.HTTP.Internal.Request
import           Calamity.HTTP.Internal.Route
import           Calamity.Internal.AesonThings
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.Guild
import           Calamity.Types.Model.User
import           Calamity.Types.Model.Voice
import           Calamity.Types.Snowflake

import           Control.Arrow
import           Control.Lens                   hiding ( (.=) )

import           Data.Aeson
import           Data.Default.Class
import           Data.Maybe
import           Data.Text                      ( Text )

import           GHC.Generics

import           Network.Wreq

import           TextShow

data CreateGuildData = CreateGuildData
  { name                        :: Text
  , region                      :: Text
  , icon                        :: Text
  , verificationLevel           :: Integer -- TODO: enums for these
  , defaultMessageNotifications :: Integer
  , explicitContentFilter       :: Integer
  , roles                       :: [Role]
  , channels                    :: [Partial Channel]
  }
  deriving ( Show, Generic )
  deriving ( ToJSON ) via CalamityJSON CreateGuildData

data ModifyGuildData = ModifyGuildData
  { name                        :: Maybe Text
  , region                      :: Maybe Text
  , icon                        :: Maybe Text
  , verificationLevel           :: Maybe Integer -- TODO: enums for these
  , defaultMessageNotifications :: Maybe Integer
  , explicitContentFilter       :: Maybe Integer
  , afkChannelID                :: Maybe (Snowflake GuildChannel)
  , afkTimeout                  :: Maybe Integer
  , ownerID                     :: Maybe (Snowflake User)
  , splash                      :: Maybe Text
  , banner                      :: Maybe Text
  , systemChannelID             :: Maybe (Snowflake GuildChannel)
  }
  deriving ( Show, Generic, Default )
  deriving ( ToJSON ) via CalamityJSON ModifyGuildData

data ChannelCreateData = ChannelCreateData
  { name                 :: Text
  , type_                :: Maybe ChannelType
  , topic                :: Maybe Text
  , bitrate              :: Maybe Integer
  , userLimit            :: Maybe Integer
  , rateLimitPerUser     :: Maybe Integer
  , position             :: Maybe Integer
  , permissionOverwrites :: Maybe [Overwrite]
  , parentID             :: Maybe (Snowflake Category)
  , nsfw                 :: Maybe Bool
  }
  deriving ( Show, Generic )
  deriving ( ToJSON ) via CalamityJSON ChannelCreateData

data ChannelPosition = ChannelPosition
  { id       :: Snowflake GuildChannel
  , position :: Maybe Integer
  }
  deriving ( Show, Generic )
  deriving ( ToJSON ) via CalamityJSON ChannelPosition

data ListMembersOptions = ListMembersOptions
  { limit :: Maybe Integer
  , after :: Maybe (Snowflake User)
  }
  deriving ( Show, Generic, Default )

data AddGuildMemberData = AddGuildMemberData
  { accessToken :: Text
  , nick        :: Maybe Text
  , roles       :: Maybe [Snowflake Role]
  , mute        :: Maybe Bool
  , deaf        :: Maybe Bool
  }
  deriving ( Show, Generic )
  deriving ( ToJSON ) via CalamityJSON AddGuildMemberData

data ModifyGuildMemberData = ModifyGuildMemberData
  { nick      :: Maybe Text
  , roles     :: Maybe [Snowflake Role]
  , mute      :: Maybe Bool
  , deaf      :: Maybe Bool
  , channelID :: Maybe (Snowflake VoiceChannel)
  }
  deriving ( Show, Generic, Default )
  deriving ( ToJSON ) via CalamityJSON ModifyGuildMemberData

data CreateGuildBanData = CreateGuildBanData
  { deleteMessageDays :: Maybe Integer
  , reason            :: Maybe Text
  }
  deriving ( Show, Generic, Default )

data ModifyGuildRoleData = ModifyGuildRoleData
  { name        :: Maybe Text
  , permissions :: Maybe Integer
  , color       :: Maybe Integer
  , hoist       :: Maybe Bool
  , mentionable :: Maybe Bool
  }
  deriving ( Show, Generic, Default )
  deriving ( ToJSON ) via CalamityJSON ModifyGuildRoleData

data ModifyGuildRolePositionsData = ModifyGuildRolePositionsData
  { id       :: Snowflake Role
  , position :: Maybe Integer
  }
  deriving ( Show, Generic )
  deriving ( ToJSON ) via CalamityJSON ModifyGuildRolePositionsData

data GuildRequest a where
  CreateGuild                 :: CreateGuildData ->                                                                  GuildRequest Guild
  GetGuild                    :: HasID Guild g =>                               g ->                                 GuildRequest Guild
  ModifyGuild                 :: HasID Guild g =>                               g -> ModifyGuildData ->              GuildRequest Guild
  DeleteGuild                 :: HasID Guild g =>                               g ->                                 GuildRequest ()
  GetGuildChannels            :: HasID Guild g =>                               g ->                                 GuildRequest [Channel]
  CreateGuildChannel          :: HasID Guild g =>                               g -> ChannelCreateData ->            GuildRequest Channel
  ModifyGuildChannelPositions :: HasID Guild g =>                               g -> [ChannelPosition] ->            GuildRequest ()
  GetGuildMember              :: (HasID Guild g, HasID User u) =>               g -> u ->                            GuildRequest Member
  ListGuildMembers            :: HasID Guild g =>                               g -> ListMembersOptions ->           GuildRequest [Member]
  AddGuildMember              :: (HasID Guild g, HasID User u) =>               g -> u -> AddGuildMemberData ->      GuildRequest (Maybe Member)
  ModifyGuildMember           :: (HasID Guild g, HasID User u) =>               g -> u -> ModifyGuildMemberData ->   GuildRequest ()
  ModifyCurrentUserNick       :: HasID Guild g =>                               g -> Maybe Text ->                   GuildRequest ()
  AddGuildMemberRole          :: (HasID Guild g, HasID User u, HasID Role r) => g -> u -> r ->                       GuildRequest ()
  RemoveGuildMemberRole       :: (HasID Guild g, HasID User u, HasID Role r) => g -> u -> r ->                       GuildRequest ()
  RemoveGuildMember           :: (HasID Guild g, HasID User u) =>               g -> u ->                            GuildRequest ()
  GetGuildBans                :: HasID Guild g =>                               g ->                                 GuildRequest [BanData]
  GetGuildBan                 :: (HasID Guild g, HasID User u) =>               g -> u ->                            GuildRequest BanData
  CreateGuildBan              :: (HasID Guild g, HasID User u) =>               g -> u -> CreateGuildBanData ->      GuildRequest ()
  RemoveGuildBan              :: (HasID Guild g, HasID User u) =>               g -> u ->                            GuildRequest ()
  GetGuildRoles               :: HasID Guild g =>                               g ->                                 GuildRequest [Role]
  CreateGuildRole             :: HasID Guild g =>                               g -> ModifyGuildRoleData ->          GuildRequest Role
  ModifyGuildRolePositions    :: HasID Guild g =>                               g -> ModifyGuildRolePositionsData -> GuildRequest [Role]
  ModifyGuildRole             :: (HasID Guild g, HasID Role r) =>               g -> r -> ModifyGuildRoleData ->     GuildRequest Role
  DeleteGuildRole             :: (HasID Guild g, HasID Role r) =>               g -> r ->                            GuildRequest ()
  GetGuildPruneCount          :: HasID Guild g =>                               g -> Integer ->                      GuildRequest Integer
  BeginGuildPrune             :: HasID Guild g =>                               g -> Integer -> Bool ->              GuildRequest (Maybe Integer)
  GetGuildVoiceRegions        :: HasID Guild g =>                               g ->                                 GuildRequest [VoiceRegion]
  GetGuildInvites             :: HasID Guild g =>                               g ->                                 GuildRequest [Invite]

baseRoute :: Snowflake Guild -> RouteBuilder _
baseRoute id = mkRouteBuilder // S "guilds" // ID @Guild
  & giveID id

instance Request (GuildRequest a) a where
  toRoute (CreateGuild _) = mkRouteBuilder // S "guilds"
    & buildRoute
  toRoute (GetGuild (getID -> gid)) = baseRoute gid
    & buildRoute
  toRoute (ModifyGuild (getID -> gid) _) = baseRoute gid
    & buildRoute
  toRoute (DeleteGuild (getID -> gid)) = baseRoute gid
    & buildRoute
  toRoute (GetGuildChannels (getID -> gid)) = baseRoute gid // S "channels"
    & buildRoute
  toRoute (CreateGuildChannel (getID -> gid) _) = baseRoute gid // S "channels"
    & buildRoute
  toRoute (ModifyGuildChannelPositions (getID -> gid) _) = baseRoute gid // S "channels"
    & buildRoute
  toRoute (GetGuildMember (getID -> gid) (getID @User -> uid)) = baseRoute gid // S "members" // ID @User
    & giveID uid
    & buildRoute
  toRoute (ListGuildMembers (getID -> gid) _) = baseRoute gid // S "members"
    & buildRoute
  toRoute (AddGuildMember (getID -> gid) (getID @User -> uid) _) = baseRoute gid // S "members" // ID @User
    & giveID uid
    & buildRoute
  toRoute (ModifyGuildMember (getID -> gid) (getID @User -> uid) _) = baseRoute gid // S "members" // ID @User
    & giveID uid
    & buildRoute
  toRoute (ModifyCurrentUserNick (getID -> gid) _) = baseRoute gid // S "members" // S "@me" // S "nick"
    & buildRoute
  toRoute (AddGuildMemberRole (getID -> gid) (getID @User -> uid) (getID @Role -> rid)) =
    baseRoute gid // S "members" // ID @User // S "roles" // ID @Role
    & giveID uid
    & giveID rid
    & buildRoute
  toRoute (RemoveGuildMemberRole (getID -> gid) (getID @User -> uid) (getID @Role -> rid)) =
    baseRoute gid // S "members" // ID @User // S "roles" // ID @Role
    & giveID uid
    & giveID rid
    & buildRoute
  toRoute (RemoveGuildMember (getID -> gid) (getID @User -> uid)) = baseRoute gid // S "members" // ID @User
    & giveID uid
    & buildRoute
  toRoute (GetGuildBans (getID -> gid)) = baseRoute gid // S "bans"
    & buildRoute
  toRoute (GetGuildBan (getID -> gid) (getID @User -> uid)) = baseRoute gid // S "bans" // ID @User
    & giveID uid
    & buildRoute
  toRoute (CreateGuildBan (getID -> gid) (getID @User -> uid) _) = baseRoute gid // S "bans" // ID @User
    & giveID uid
    & buildRoute
  toRoute (RemoveGuildBan (getID -> gid) (getID @User -> uid)) = baseRoute gid // S "bans" // ID @User
    & giveID uid
    & buildRoute
  toRoute (GetGuildRoles (getID -> gid)) = baseRoute gid // S "roles"
    & buildRoute
  toRoute (CreateGuildRole (getID -> gid) _) = baseRoute gid // S "roles"
    & buildRoute
  toRoute (ModifyGuildRolePositions (getID -> gid) _) = baseRoute gid // S "roles"
    & buildRoute
  toRoute (ModifyGuildRole (getID -> gid) (getID @Role -> rid) _) = baseRoute gid // S "roles" // ID @Role
    & giveID rid
    & buildRoute
  toRoute (DeleteGuildRole (getID -> gid) (getID @Role -> rid)) = baseRoute gid // S "roles" // ID @Role
    & giveID rid
    & buildRoute
  toRoute (GetGuildPruneCount (getID -> gid) _) = baseRoute gid // S "prune"
    & buildRoute
  toRoute (BeginGuildPrune (getID -> gid) _ _) = baseRoute gid // S "prune"
    & buildRoute
  toRoute (GetGuildVoiceRegions (getID -> gid)) = baseRoute gid // S "regions"
    & buildRoute
  toRoute (GetGuildInvites (getID -> gid)) = baseRoute gid // S "invites"
    & buildRoute

  toAction (CreateGuild o) = postWith' (toJSON o)
  toAction (GetGuild _) = getWith
  toAction (ModifyGuild _ o) = patchWith' (toJSON o)
  toAction (DeleteGuild _) = deleteWith
  toAction (GetGuildChannels _) = getWith
  toAction (CreateGuildChannel _ o) = postWith' (toJSON o)
  toAction (ModifyGuildChannelPositions _ o) = postWith' (toJSON o)
  toAction (GetGuildMember _ _) = getWith
  toAction (ListGuildMembers _ ListMembersOptions { limit, after }) = getWithP
    (param "limit" .~ maybeToList (showt <$> limit) >>> param "after" .~ maybeToList (showt <$> after))
  toAction (AddGuildMember _ _ o) = putWith' (toJSON o)
  toAction (ModifyGuildMember _ _ o) = patchWith' (toJSON o)
  toAction (ModifyCurrentUserNick _ nick) = patchWith' (object ["nick" .= nick])
  toAction (AddGuildMemberRole {}) = putEmpty
  toAction (RemoveGuildMemberRole {}) = deleteWith
  toAction (RemoveGuildMember _ _) = deleteWith
  toAction (GetGuildBans _) = getWith
  toAction (GetGuildBan _ _) = getWith
  toAction (CreateGuildBan _ _ CreateGuildBanData { deleteMessageDays, reason }) = putEmptyP
    (param "delete-message-days" .~ maybeToList (showt <$> deleteMessageDays) >>> param "reason" .~ maybeToList
     (showt <$> reason))
  toAction (RemoveGuildBan _ _) = deleteWith
  toAction (GetGuildRoles _) = getWith
  toAction (CreateGuildRole _ o) = postWith' (toJSON o)
  toAction (ModifyGuildRolePositions _ o) = patchWith' (toJSON o)
  toAction (ModifyGuildRole _ _ o) = patchWith' (toJSON o)
  toAction (DeleteGuildRole _ _) = deleteWith
  toAction (GetGuildPruneCount _ d) = getWithP (param "days" .~ [showt d])
  toAction (BeginGuildPrune _ d r) = postEmptyP (param "days" .~ [showt d] >>> param "compute_prune_count" .~ [showt r])
  toAction (GetGuildVoiceRegions _) = getWith
  toAction (GetGuildInvites _) = getWith
