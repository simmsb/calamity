-- | Guild endpoints
module Calamity.HTTP.Guild (
    GuildRequest (..),
    CreateGuildData (..),
    ModifyGuildData (..),
    ChannelCreateData (..),
    ChannelPosition (..),
    ListMembersOptions (..),
    AddGuildMemberData (..),
    ModifyGuildMemberData (..),
    modifyGuildMemberNick,
    modifyGuildMemberRoles,
    modifyGuildMemberMute,
    modifyGuildMemberDeaf,
    modifyGuildMemberChannelID,
    CreateGuildBanData (..),
    ModifyGuildRoleData (..),
    modifyGuildRoleName,
    modifyGuildRolePermissions,
    modifyGuildRoleColour,
    modifyGuildRoleHoist,
    modifyGuildRoleMentionable,
    ModifyGuildRolePositionsData (..),
) where

import Calamity.HTTP.Internal.Request
import Calamity.HTTP.Internal.Route
import Calamity.Internal.AesonThings
import Calamity.Internal.IntColour
import Calamity.Types.Model.Channel
import Calamity.Types.Model.Guild
import Calamity.Types.Model.User
import Calamity.Types.Model.Voice
import Calamity.Types.Snowflake
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import Data.Colour (Colour)
import Data.Default.Class
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Req
import TextShow

data CreateGuildData = CreateGuildData
  { name :: Text
  , region :: Text
  , icon :: Text
  , verificationLevel :: Integer -- TODO: enums for these
  , defaultMessageNotifications :: Integer
  , explicitContentFilter :: Integer
  , roles :: [Role]
  , channels :: [Partial Channel]
  }
  deriving (Show, Generic)
  deriving (ToJSON) via CalamityJSON CreateGuildData

data ModifyGuildData = ModifyGuildData
  { name :: Maybe Text
  , region :: Maybe Text
  , icon :: Maybe Text
  , verificationLevel :: Maybe Integer -- TODO: enums for these
  , defaultMessageNotifications :: Maybe Integer
  , explicitContentFilter :: Maybe Integer
  , afkChannelID :: Maybe (Snowflake GuildChannel)
  , afkTimeout :: Maybe Integer
  , ownerID :: Maybe (Snowflake User)
  , splash :: Maybe Text
  , banner :: Maybe Text
  , systemChannelID :: Maybe (Snowflake GuildChannel)
  }
  deriving (Show, Generic, Default)
  deriving (ToJSON) via CalamityJSON ModifyGuildData

data ChannelCreateData = ChannelCreateData
  { name :: Text
  , type_ :: Maybe ChannelType
  , topic :: Maybe Text
  , bitrate :: Maybe Integer
  , userLimit :: Maybe Integer
  , rateLimitPerUser :: Maybe Integer
  , position :: Maybe Integer
  , permissionOverwrites :: Maybe [Overwrite]
  , parentID :: Maybe (Snowflake Category)
  , nsfw :: Maybe Bool
  }
  deriving (Show, Generic)
  deriving (ToJSON) via CalamityJSON ChannelCreateData

data ChannelPosition = ChannelPosition
  { id :: Snowflake GuildChannel
  , position :: Maybe Integer
  }
  deriving (Show, Generic)
  deriving (ToJSON) via CalamityJSON ChannelPosition

data ListMembersOptions = ListMembersOptions
  { limit :: Maybe Integer
  , after :: Maybe (Snowflake User)
  }
  deriving (Show, Generic, Default)

data AddGuildMemberData = AddGuildMemberData
  { accessToken :: Text
  , nick :: Maybe Text
  , roles :: Maybe [Snowflake Role]
  , mute :: Maybe Bool
  , deaf :: Maybe Bool
  }
  deriving (Show, Generic)
  deriving (ToJSON) via CalamityJSON AddGuildMemberData

-- | Parameters to the Modify Guild Member endpoint.
--
-- Use the provided methods (@modifyGuildMemberX@) to create a value with the
-- field set, use the Semigroup instance to union the values.
--
-- ==== Examples
--
-- >>> encode $ modifyGuildMemberNick (Just "test") <> modifyGuildMemberDeaf Nothing
-- "{\"nick\":\"test\",\"deaf\":null}"
newtype ModifyGuildMemberData = ModifyGuildMemberData Object
    deriving (Show, Generic)
    deriving newtype (ToJSON, Semigroup, Monoid)

modifyGuildMemberNick :: Maybe Text -> ModifyGuildMemberData
modifyGuildMemberNick v = ModifyGuildMemberData $ H.fromList [("nick", toJSON v)]

modifyGuildMemberRoles :: Maybe [Snowflake Role] -> ModifyGuildMemberData
modifyGuildMemberRoles v = ModifyGuildMemberData $ H.fromList [("roles", toJSON v)]

modifyGuildMemberMute :: Maybe Bool -> ModifyGuildMemberData
modifyGuildMemberMute v = ModifyGuildMemberData $ H.fromList [("mute", toJSON v)]

modifyGuildMemberDeaf :: Maybe Bool -> ModifyGuildMemberData
modifyGuildMemberDeaf v = ModifyGuildMemberData $ H.fromList [("deaf", toJSON v)]

modifyGuildMemberChannelID :: Maybe (Snowflake VoiceChannel) -> ModifyGuildMemberData
modifyGuildMemberChannelID v = ModifyGuildMemberData $ H.fromList [("channel_id", toJSON v)]

data CreateGuildBanData = CreateGuildBanData
  { deleteMessageDays :: Maybe Integer
  , reason :: Maybe Text
  }
  deriving (Show, Generic, Default)

-- | Parameters to the Modify Guild Role endpoint.
--
-- Use the provided methods (@modifyGuildRoleX@) to create a value with the
-- field set, use the Semigroup instance to union the values.
--
-- ==== Examples
--
-- >>> encode $ modifyGuildRoleName (Just "test") <> modifyGuildRolePermissions Nothing
-- "{\"name\":\"test\",\"permissions\":null}"
newtype ModifyGuildRoleData = ModifyGuildRoleData Object
    deriving (Show, Generic)
    deriving newtype (ToJSON, Semigroup, Monoid)


modifyGuildRoleName :: Maybe Text -> ModifyGuildRoleData
modifyGuildRoleName v = ModifyGuildRoleData $ H.fromList [("name", toJSON v)]

modifyGuildRolePermissions :: Maybe Permissions -> ModifyGuildRoleData
modifyGuildRolePermissions v = ModifyGuildRoleData $ H.fromList [("permissions", toJSON v)]

modifyGuildRoleColour :: Maybe (Colour Double) -> ModifyGuildRoleData
modifyGuildRoleColour v = ModifyGuildRoleData $ H.fromList [("colour", toJSON (IntColour <$> v))]

modifyGuildRoleHoist :: Maybe Bool -> ModifyGuildRoleData
modifyGuildRoleHoist v = ModifyGuildRoleData $ H.fromList [("hoist", toJSON v)]

modifyGuildRoleMentionable :: Maybe Bool -> ModifyGuildRoleData
modifyGuildRoleMentionable v = ModifyGuildRoleData $ H.fromList [("mentionable", toJSON v)]

data ModifyGuildRolePositionsData = ModifyGuildRolePositionsData
  { id :: Snowflake Role
  , position :: Maybe Integer
  }
  deriving (Show, Generic)
  deriving (ToJSON) via CalamityJSON ModifyGuildRolePositionsData

data GuildRequest a where
  CreateGuild :: CreateGuildData -> GuildRequest Guild
  GetGuild :: HasID Guild g => g -> GuildRequest Guild
  ModifyGuild :: HasID Guild g => g -> ModifyGuildData -> GuildRequest Guild
  DeleteGuild :: HasID Guild g => g -> GuildRequest ()
  GetGuildChannels :: HasID Guild g => g -> GuildRequest [Channel]
  CreateGuildChannel :: HasID Guild g => g -> ChannelCreateData -> GuildRequest Channel
  ModifyGuildChannelPositions :: HasID Guild g => g -> [ChannelPosition] -> GuildRequest ()
  GetGuildMember :: (HasID Guild g, HasID User u) => g -> u -> GuildRequest Member
  ListGuildMembers :: HasID Guild g => g -> ListMembersOptions -> GuildRequest [Member]
  AddGuildMember :: (HasID Guild g, HasID User u) => g -> u -> AddGuildMemberData -> GuildRequest (Maybe Member)
  ModifyGuildMember :: (HasID Guild g, HasID User u) => g -> u -> ModifyGuildMemberData -> GuildRequest ()
  ModifyCurrentUserNick :: HasID Guild g => g -> Maybe Text -> GuildRequest ()
  AddGuildMemberRole :: (HasID Guild g, HasID User u, HasID Role r) => g -> u -> r -> GuildRequest ()
  RemoveGuildMemberRole :: (HasID Guild g, HasID User u, HasID Role r) => g -> u -> r -> GuildRequest ()
  RemoveGuildMember :: (HasID Guild g, HasID User u) => g -> u -> GuildRequest ()
  GetGuildBans :: HasID Guild g => g -> GuildRequest [BanData]
  GetGuildBan :: (HasID Guild g, HasID User u) => g -> u -> GuildRequest BanData
  CreateGuildBan :: (HasID Guild g, HasID User u) => g -> u -> CreateGuildBanData -> GuildRequest ()
  RemoveGuildBan :: (HasID Guild g, HasID User u) => g -> u -> GuildRequest ()
  GetGuildRoles :: HasID Guild g => g -> GuildRequest [Role]
  CreateGuildRole :: HasID Guild g => g -> ModifyGuildRoleData -> GuildRequest Role
  ModifyGuildRolePositions :: HasID Guild g => g -> ModifyGuildRolePositionsData -> GuildRequest [Role]
  ModifyGuildRole :: (HasID Guild g, HasID Role r) => g -> r -> ModifyGuildRoleData -> GuildRequest Role
  DeleteGuildRole :: (HasID Guild g, HasID Role r) => g -> r -> GuildRequest ()
  GetGuildPruneCount :: HasID Guild g => g -> Integer -> GuildRequest Integer
  BeginGuildPrune :: HasID Guild g => g -> Integer -> Bool -> GuildRequest (Maybe Integer)
  GetGuildVoiceRegions :: HasID Guild g => g -> GuildRequest [VoiceRegion]
  GetGuildInvites :: HasID Guild g => g -> GuildRequest [Invite]

baseRoute :: Snowflake Guild -> RouteBuilder _
baseRoute id =
  mkRouteBuilder // S "guilds" // ID @Guild
    & giveID id

instance Request (GuildRequest a) where
  type Result (GuildRequest a) = a

  route (CreateGuild _) =
    mkRouteBuilder // S "guilds"
      & buildRoute
  route (GetGuild (getID -> gid)) =
    baseRoute gid
      & buildRoute
  route (ModifyGuild (getID -> gid) _) =
    baseRoute gid
      & buildRoute
  route (DeleteGuild (getID -> gid)) =
    baseRoute gid
      & buildRoute
  route (GetGuildChannels (getID -> gid)) =
    baseRoute gid // S "channels"
      & buildRoute
  route (CreateGuildChannel (getID -> gid) _) =
    baseRoute gid // S "channels"
      & buildRoute
  route (ModifyGuildChannelPositions (getID -> gid) _) =
    baseRoute gid // S "channels"
      & buildRoute
  route (GetGuildMember (getID -> gid) (getID @User -> uid)) =
    baseRoute gid // S "members" // ID @User
      & giveID uid
      & buildRoute
  route (ListGuildMembers (getID -> gid) _) =
    baseRoute gid // S "members"
      & buildRoute
  route (AddGuildMember (getID -> gid) (getID @User -> uid) _) =
    baseRoute gid // S "members" // ID @User
      & giveID uid
      & buildRoute
  route (ModifyGuildMember (getID -> gid) (getID @User -> uid) _) =
    baseRoute gid // S "members" // ID @User
      & giveID uid
      & buildRoute
  route (ModifyCurrentUserNick (getID -> gid) _) =
    baseRoute gid // S "members" // S "@me" // S "nick"
      & buildRoute
  route (AddGuildMemberRole (getID -> gid) (getID @User -> uid) (getID @Role -> rid)) =
    baseRoute gid // S "members" // ID @User // S "roles" // ID @Role
      & giveID uid
      & giveID rid
      & buildRoute
  route (RemoveGuildMemberRole (getID -> gid) (getID @User -> uid) (getID @Role -> rid)) =
    baseRoute gid // S "members" // ID @User // S "roles" // ID @Role
      & giveID uid
      & giveID rid
      & buildRoute
  route (RemoveGuildMember (getID -> gid) (getID @User -> uid)) =
    baseRoute gid // S "members" // ID @User
      & giveID uid
      & buildRoute
  route (GetGuildBans (getID -> gid)) =
    baseRoute gid // S "bans"
      & buildRoute
  route (GetGuildBan (getID -> gid) (getID @User -> uid)) =
    baseRoute gid // S "bans" // ID @User
      & giveID uid
      & buildRoute
  route (CreateGuildBan (getID -> gid) (getID @User -> uid) _) =
    baseRoute gid // S "bans" // ID @User
      & giveID uid
      & buildRoute
  route (RemoveGuildBan (getID -> gid) (getID @User -> uid)) =
    baseRoute gid // S "bans" // ID @User
      & giveID uid
      & buildRoute
  route (GetGuildRoles (getID -> gid)) =
    baseRoute gid // S "roles"
      & buildRoute
  route (CreateGuildRole (getID -> gid) _) =
    baseRoute gid // S "roles"
      & buildRoute
  route (ModifyGuildRolePositions (getID -> gid) _) =
    baseRoute gid // S "roles"
      & buildRoute
  route (ModifyGuildRole (getID -> gid) (getID @Role -> rid) _) =
    baseRoute gid // S "roles" // ID @Role
      & giveID rid
      & buildRoute
  route (DeleteGuildRole (getID -> gid) (getID @Role -> rid)) =
    baseRoute gid // S "roles" // ID @Role
      & giveID rid
      & buildRoute
  route (GetGuildPruneCount (getID -> gid) _) =
    baseRoute gid // S "prune"
      & buildRoute
  route (BeginGuildPrune (getID -> gid) _ _) =
    baseRoute gid // S "prune"
      & buildRoute
  route (GetGuildVoiceRegions (getID -> gid)) =
    baseRoute gid // S "regions"
      & buildRoute
  route (GetGuildInvites (getID -> gid)) =
    baseRoute gid // S "invites"
      & buildRoute

  action (CreateGuild o) = postWith' (ReqBodyJson o)
  action (GetGuild _) = getWith
  action (ModifyGuild _ o) = patchWith' (ReqBodyJson o)
  action (DeleteGuild _) = deleteWith
  action (GetGuildChannels _) = getWith
  action (CreateGuildChannel _ o) = postWith' (ReqBodyJson o)
  action (ModifyGuildChannelPositions _ o) = postWith' (ReqBodyJson o)
  action (GetGuildMember _ _) = getWith
  action (ListGuildMembers _ ListMembersOptions{limit, after}) =
    getWithP
      ("limit" =:? limit <> "after" =:? (fromSnowflake <$> after))
  action (AddGuildMember _ _ o) = putWith' (ReqBodyJson o)
  action (ModifyGuildMember _ _ o) = patchWith' (ReqBodyJson o)
  action (ModifyCurrentUserNick _ nick) = patchWith' (ReqBodyJson $ object ["nick" .= nick])
  action AddGuildMemberRole{} = putEmpty
  action RemoveGuildMemberRole{} = deleteWith
  action (RemoveGuildMember _ _) = deleteWith
  action (GetGuildBans _) = getWith
  action (GetGuildBan _ _) = getWith
  action (CreateGuildBan _ _ CreateGuildBanData{deleteMessageDays, reason}) =
    putEmptyP
      ("delete-message-days" =:? deleteMessageDays <> "reason" =:? reason)
  action (RemoveGuildBan _ _) = deleteWith
  action (GetGuildRoles _) = getWith
  action (CreateGuildRole _ o) = postWith' (ReqBodyJson o)
  action (ModifyGuildRolePositions _ o) = patchWith' (ReqBodyJson o)
  action (ModifyGuildRole _ _ o) = patchWith' (ReqBodyJson o)
  action (DeleteGuildRole _ _) = deleteWith
  action (GetGuildPruneCount _ d) = getWithP ("days" =: d)
  action (BeginGuildPrune _ d r) = postEmptyP ("days" =: d <> "compute_prune_count" =: r)
  action (GetGuildVoiceRegions _) = getWith
  action (GetGuildInvites _) = getWith

  -- this is a bit of a hack
  -- TODO: add something to allow for contextual parsing
  modifyResponse (GetGuildMember (getID @Guild -> gid) _) = _Object . at "guild_id" ?~ _String # showt (fromSnowflake gid)
  modifyResponse (ListGuildMembers (getID @Guild -> gid) _) = values . _Object . at "guild_id" ?~ _String # showt (fromSnowflake gid)
  modifyResponse _ = Prelude.id
