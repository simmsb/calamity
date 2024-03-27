{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Guild endpoints
module Calamity.HTTP.Guild (
  GuildRequest (..),
  CreateGuildData (..),
  ModifyGuildData (..),
  ChannelCreateData (..),
  ChannelPosition (..),
  ListMembersOptions (..),
  AddGuildMemberData (..),
  ModifyGuildMemberData,
  modifyGuildMemberNick,
  modifyGuildMemberRoles,
  modifyGuildMemberMute,
  modifyGuildMemberDeaf,
  modifyGuildMemberChannelID,
  CreateGuildBanData (..),
  ModifyGuildRoleData,
  modifyGuildRoleName,
  modifyGuildRolePermissions,
  modifyGuildRoleColour,
  modifyGuildRoleHoist,
  modifyGuildRoleMentionable,
  ModifyGuildRolePositionsData (..),
) where

import Calamity.HTTP.Internal.Request
import Calamity.HTTP.Internal.Route
import Calamity.Internal.IntColour
import Calamity.Internal.Utils (CalamityToJSON (..), CalamityToJSON' (..), (.=), (.?=))
import Calamity.Types.Model.Channel
import Calamity.Types.Model.Guild
import Calamity.Types.Model.User
import Calamity.Types.Model.Voice
import Calamity.Types.Snowflake
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as K
import Data.Colour (Colour)
import Data.Default.Class
import Data.Text (Text)
import Network.HTTP.Req
import Optics

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
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON CreateGuildData

instance CalamityToJSON' CreateGuildData where
  toPairs CreateGuildData {..} =
    [ "name" .= name
    , "region" .= region
    , "icon" .= icon
    , "verification_level" .= verificationLevel
    , "default_message_notifications" .= defaultMessageNotifications
    , "explicit_content_filter" .= explicitContentFilter
    , "roles" .= roles
    , "channels" .= channels
    ]

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
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON ModifyGuildData

instance Default ModifyGuildData where
  def =
    ModifyGuildData
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing

-- TODO: Make this work properly, a la modifyguildmember
instance CalamityToJSON' ModifyGuildData where
  toPairs ModifyGuildData {..} =
    [ "name" .?= name
    , "region" .?= region
    , "icon" .?= icon
    , "verification_level" .?= verificationLevel
    , "default_message_notifications" .?= defaultMessageNotifications
    , "explicit_content_filter" .?= explicitContentFilter
    , "afk_timeout" .?= afkTimeout
    , "owner_id" .?= ownerID
    , "splash" .?= splash
    , "banner" .?= banner
    , "system_channel_id" .?= systemChannelID
    ]

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
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON ChannelCreateData

instance CalamityToJSON' ChannelCreateData where
  toPairs ChannelCreateData {..} =
    [ "name" .= name
    , "type" .?= type_
    , "topic" .?= topic
    , "bitrate" .?= bitrate
    , "user_limit" .?= userLimit
    , "rate_limit_per_user" .?= rateLimitPerUser
    , "position" .?= position
    , "permission_overwrites" .?= permissionOverwrites
    , "parent_id" .?= parentID
    , "nsfw" .?= nsfw
    ]

data ChannelPosition = ChannelPosition
  { id :: Snowflake GuildChannel
  , position :: Maybe Integer
  , lockPermissions :: Maybe Bool
  , parentID :: Snowflake GuildChannel
  }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON ChannelPosition

instance CalamityToJSON' ChannelPosition where
  toPairs ChannelPosition {..} =
    [ "id" .= id
    , "position" .= position
    , "lock_permissions" .= lockPermissions
    , "parent_id" .= parentID
    ]

data ListMembersOptions = ListMembersOptions
  { limit :: Maybe Integer
  , after :: Maybe (Snowflake User)
  }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON ListMembersOptions

instance Default ListMembersOptions where
  def = ListMembersOptions Nothing Nothing

instance CalamityToJSON' ListMembersOptions where
  toPairs ListMembersOptions {..} =
    [ "limit" .?= limit
    , "after" .?= after
    ]

data SearchMembersOptions = SearchMembersOptions
  { limit :: Maybe Integer
  , query :: Text
  }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON SearchMembersOptions

instance CalamityToJSON' SearchMembersOptions where
  toPairs SearchMembersOptions {..} =
    [ "limit" .?= limit
    , "query" .= query
    ]

data AddGuildMemberData = AddGuildMemberData
  { accessToken :: Text
  , nick :: Maybe Text
  , roles :: Maybe [Snowflake Role]
  , mute :: Maybe Bool
  , deaf :: Maybe Bool
  }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON AddGuildMemberData

instance CalamityToJSON' AddGuildMemberData where
  toPairs AddGuildMemberData {..} =
    [ "access_token" .= accessToken
    , "nick" .?= nick
    , "roles" .?= roles
    , "mute" .?= mute
    , "deaf" .?= deaf
    ]

{- | Parameters to the Modify Guild Member endpoint.

 Use the provided methods (@modifyGuildMemberX@) to create a value with the
 field set, use the Semigroup instance to combine modifications.

 ==== Examples

 >>> encode $ modifyGuildMemberNick (Just "test") <> modifyGuildMemberDeaf Nothing
 "{\"nick\":\"test\",\"deaf\":null}"
-}
newtype ModifyGuildMemberData = ModifyGuildMemberData Aeson.Object
  deriving stock (Show)
  deriving newtype (Aeson.ToJSON, Semigroup, Monoid)

modifyGuildMemberNick :: Maybe Text -> ModifyGuildMemberData
modifyGuildMemberNick v = ModifyGuildMemberData $ K.fromList [("nick", Aeson.toJSON v)]

modifyGuildMemberRoles :: Maybe [Snowflake Role] -> ModifyGuildMemberData
modifyGuildMemberRoles v = ModifyGuildMemberData $ K.fromList [("roles", Aeson.toJSON v)]

modifyGuildMemberMute :: Maybe Bool -> ModifyGuildMemberData
modifyGuildMemberMute v = ModifyGuildMemberData $ K.fromList [("mute", Aeson.toJSON v)]

modifyGuildMemberDeaf :: Maybe Bool -> ModifyGuildMemberData
modifyGuildMemberDeaf v = ModifyGuildMemberData $ K.fromList [("deaf", Aeson.toJSON v)]

modifyGuildMemberChannelID :: Maybe (Snowflake VoiceChannel) -> ModifyGuildMemberData
modifyGuildMemberChannelID v = ModifyGuildMemberData $ K.fromList [("channel_id", Aeson.toJSON v)]

data GetGuildBansOptions = GetGuildBansOptions
  { limit :: Maybe Int
  , before :: Maybe Int
  , after :: Maybe Int
  }
  deriving (Show)

instance Default GetGuildBansOptions where
  def = GetGuildBansOptions Nothing Nothing Nothing

data CreateGuildBanData = CreateGuildBanData
  { deleteMessageDays :: Maybe Integer
  , reason :: Maybe Text
  }
  deriving (Show)

instance Default CreateGuildBanData where
  def = CreateGuildBanData Nothing Nothing

{- | Parameters to the Modify Guild Role endpoint.

 Use the provided methods (@modifyGuildRoleX@) to create a value with the
 field set, use the Semigroup instance to combine parameters.

 ==== Examples

 >>> encode $ modifyGuildRoleName (Just "test") <> modifyGuildRolePermissions Nothing
 "{\"name\":\"test\",\"permissions\":null}"
-}
newtype ModifyGuildRoleData = ModifyGuildRoleData Aeson.Object
  deriving stock (Show)
  deriving newtype (Aeson.ToJSON, Semigroup, Monoid)

modifyGuildRoleName :: Maybe Text -> ModifyGuildRoleData
modifyGuildRoleName v = ModifyGuildRoleData $ K.fromList [("name", Aeson.toJSON v)]

modifyGuildRolePermissions :: Maybe Permissions -> ModifyGuildRoleData
modifyGuildRolePermissions v = ModifyGuildRoleData $ K.fromList [("permissions", Aeson.toJSON v)]

modifyGuildRoleColour :: Maybe (Colour Double) -> ModifyGuildRoleData
modifyGuildRoleColour v = ModifyGuildRoleData $ K.fromList [("colour", Aeson.toJSON (IntColour <$> v))]

modifyGuildRoleHoist :: Maybe Bool -> ModifyGuildRoleData
modifyGuildRoleHoist v = ModifyGuildRoleData $ K.fromList [("hoist", Aeson.toJSON v)]

modifyGuildRoleMentionable :: Maybe Bool -> ModifyGuildRoleData
modifyGuildRoleMentionable v = ModifyGuildRoleData $ K.fromList [("mentionable", Aeson.toJSON v)]

data ModifyGuildRolePositionsData = ModifyGuildRolePositionsData
  { id :: Snowflake Role
  , position :: Maybe Integer
  }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON ModifyGuildRolePositionsData

instance CalamityToJSON' ModifyGuildRolePositionsData where
  toPairs ModifyGuildRolePositionsData {..} =
    [ "id" .= id
    , "postition" .?= position
    ]

data GuildRequest a where
  CreateGuild :: CreateGuildData -> GuildRequest Guild
  GetGuild :: (HasID Guild g) => g -> GuildRequest Guild
  ModifyGuild :: (HasID Guild g) => g -> ModifyGuildData -> GuildRequest Guild
  DeleteGuild :: (HasID Guild g) => g -> GuildRequest ()
  GetGuildChannels :: (HasID Guild g) => g -> GuildRequest [Channel]
  CreateGuildChannel :: (HasID Guild g) => g -> ChannelCreateData -> GuildRequest Channel
  ModifyGuildChannelPositions :: (HasID Guild g) => g -> [ChannelPosition] -> GuildRequest ()
  GetGuildMember :: (HasID Guild g, HasID User u) => g -> u -> GuildRequest Member
  ListGuildMembers :: (HasID Guild g) => g -> ListMembersOptions -> GuildRequest [Member]
  SearchGuildMembers :: (HasID Guild g) => g -> SearchMembersOptions -> GuildRequest [Member]
  AddGuildMember :: (HasID Guild g, HasID User u) => g -> u -> AddGuildMemberData -> GuildRequest (Maybe Member)
  ModifyGuildMember :: (HasID Guild g, HasID User u) => g -> u -> ModifyGuildMemberData -> GuildRequest ()
  ModifyCurrentUserNick :: (HasID Guild g) => g -> Maybe Text -> GuildRequest ()
  AddGuildMemberRole :: (HasID Guild g, HasID User u, HasID Role r) => g -> u -> r -> GuildRequest ()
  RemoveGuildMemberRole :: (HasID Guild g, HasID User u, HasID Role r) => g -> u -> r -> GuildRequest ()
  RemoveGuildMember :: (HasID Guild g, HasID User u) => g -> u -> GuildRequest ()
  GetGuildBans :: (HasID Guild g) => g -> GetGuildBansOptions -> GuildRequest [BanData]
  GetGuildBan :: (HasID Guild g, HasID User u) => g -> u -> GuildRequest BanData
  CreateGuildBan :: (HasID Guild g, HasID User u) => g -> u -> CreateGuildBanData -> GuildRequest ()
  RemoveGuildBan :: (HasID Guild g, HasID User u) => g -> u -> GuildRequest ()
  GetGuildRoles :: (HasID Guild g) => g -> GuildRequest [Role]
  CreateGuildRole :: (HasID Guild g) => g -> ModifyGuildRoleData -> GuildRequest Role
  ModifyGuildRolePositions :: (HasID Guild g) => g -> ModifyGuildRolePositionsData -> GuildRequest [Role]
  ModifyGuildRole :: (HasID Guild g, HasID Role r) => g -> r -> ModifyGuildRoleData -> GuildRequest Role
  DeleteGuildRole :: (HasID Guild g, HasID Role r) => g -> r -> GuildRequest ()
  GetGuildPruneCount :: (HasID Guild g) => g -> Integer -> GuildRequest Integer
  BeginGuildPrune :: (HasID Guild g) => g -> Integer -> Bool -> GuildRequest (Maybe Integer)
  GetGuildVoiceRegions :: (HasID Guild g) => g -> GuildRequest [VoiceRegion]
  GetGuildInvites :: (HasID Guild g) => g -> GuildRequest [Invite]

baseRoute :: Snowflake Guild -> RouteBuilder _
baseRoute id =
  mkRouteBuilder
    // S "guilds"
    // ID @Guild
      & giveID id

instance Request (GuildRequest a) where
  type Result (GuildRequest a) = a

  route (CreateGuild _) =
    mkRouteBuilder
      // S "guilds"
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
    baseRoute gid
      // S "channels"
        & buildRoute
  route (CreateGuildChannel (getID -> gid) _) =
    baseRoute gid
      // S "channels"
        & buildRoute
  route (ModifyGuildChannelPositions (getID -> gid) _) =
    baseRoute gid
      // S "channels"
        & buildRoute
  route (GetGuildMember (getID -> gid) (getID @User -> uid)) =
    baseRoute gid
      // S "members"
      // ID @User
        & giveID uid
        & buildRoute
  route (ListGuildMembers (getID -> gid) _) =
    baseRoute gid
      // S "members"
        & buildRoute
  route (SearchGuildMembers (getID -> gid) _) =
    baseRoute gid
      // S "members"
      // S "search"
        & buildRoute
  route (AddGuildMember (getID -> gid) (getID @User -> uid) _) =
    baseRoute gid
      // S "members"
      // ID @User
        & giveID uid
        & buildRoute
  route (ModifyGuildMember (getID -> gid) (getID @User -> uid) _) =
    baseRoute gid
      // S "members"
      // ID @User
        & giveID uid
        & buildRoute
  route (ModifyCurrentUserNick (getID -> gid) _) =
    baseRoute gid
      // S "members"
      // S "@me"
      // S "nick"
        & buildRoute
  route (AddGuildMemberRole (getID -> gid) (getID @User -> uid) (getID @Role -> rid)) =
    baseRoute gid
      // S "members"
      // ID @User
      // S "roles"
      // ID @Role
        & giveID uid
        & giveID rid
        & buildRoute
  route (RemoveGuildMemberRole (getID -> gid) (getID @User -> uid) (getID @Role -> rid)) =
    baseRoute gid
      // S "members"
      // ID @User
      // S "roles"
      // ID @Role
        & giveID uid
        & giveID rid
        & buildRoute
  route (RemoveGuildMember (getID -> gid) (getID @User -> uid)) =
    baseRoute gid
      // S "members"
      // ID @User
        & giveID uid
        & buildRoute
  route (GetGuildBans (getID -> gid) _) =
    baseRoute gid
      // S "bans"
        & buildRoute
  route (GetGuildBan (getID -> gid) (getID @User -> uid)) =
    baseRoute gid
      // S "bans"
      // ID @User
        & giveID uid
        & buildRoute
  route (CreateGuildBan (getID -> gid) (getID @User -> uid) _) =
    baseRoute gid
      // S "bans"
      // ID @User
        & giveID uid
        & buildRoute
  route (RemoveGuildBan (getID -> gid) (getID @User -> uid)) =
    baseRoute gid
      // S "bans"
      // ID @User
        & giveID uid
        & buildRoute
  route (GetGuildRoles (getID -> gid)) =
    baseRoute gid
      // S "roles"
        & buildRoute
  route (CreateGuildRole (getID -> gid) _) =
    baseRoute gid
      // S "roles"
        & buildRoute
  route (ModifyGuildRolePositions (getID -> gid) _) =
    baseRoute gid
      // S "roles"
        & buildRoute
  route (ModifyGuildRole (getID -> gid) (getID @Role -> rid) _) =
    baseRoute gid
      // S "roles"
      // ID @Role
        & giveID rid
        & buildRoute
  route (DeleteGuildRole (getID -> gid) (getID @Role -> rid)) =
    baseRoute gid
      // S "roles"
      // ID @Role
        & giveID rid
        & buildRoute
  route (GetGuildPruneCount (getID -> gid) _) =
    baseRoute gid
      // S "prune"
        & buildRoute
  route (BeginGuildPrune (getID -> gid) _ _) =
    baseRoute gid
      // S "prune"
        & buildRoute
  route (GetGuildVoiceRegions (getID -> gid)) =
    baseRoute gid
      // S "regions"
        & buildRoute
  route (GetGuildInvites (getID -> gid)) =
    baseRoute gid
      // S "invites"
        & buildRoute

  action (CreateGuild o) = postWith' (ReqBodyJson o)
  action (GetGuild _) = getWith
  action (ModifyGuild _ o) = patchWith' (ReqBodyJson o)
  action (DeleteGuild _) = deleteWith
  action (GetGuildChannels _) = getWith
  action (CreateGuildChannel _ o) = postWith' (ReqBodyJson o)
  action (ModifyGuildChannelPositions _ o) = postWith' (ReqBodyJson o)
  action (GetGuildMember _ _) = getWith
  action (ListGuildMembers _ ListMembersOptions {limit, after}) =
    getWithP
      ("limit" =:? limit <> "after" =:? (fromSnowflake <$> after))
  action (SearchGuildMembers _ SearchMembersOptions {limit, query}) =
    getWithP
      ("limit" =:? limit <> "query" =: query)
  action (AddGuildMember _ _ o) = putWith' (ReqBodyJson o)
  action (ModifyGuildMember _ _ o) = patchWith' (ReqBodyJson o)
  action (ModifyCurrentUserNick _ nick) = patchWith' (ReqBodyJson $ Aeson.object ["nick" Aeson..= nick])
  action AddGuildMemberRole {} = putEmpty
  action RemoveGuildMemberRole {} = deleteWith
  action (RemoveGuildMember _ _) = deleteWith
  action (GetGuildBans _ GetGuildBansOptions {limit, before, after}) =
    getWithP
      ("limit" =:? limit <> "before" =:? before <> "after" =:? after)
  action (GetGuildBan _ _) = getWith
  action (CreateGuildBan _ _ CreateGuildBanData {deleteMessageDays}) =
    putEmptyP
      ("delete_message_days" =:? deleteMessageDays)
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
  modifyResponse _ = Prelude.id

$(makeFieldLabelsNoPrefix ''CreateGuildData)
$(makeFieldLabelsNoPrefix ''ModifyGuildData)
$(makeFieldLabelsNoPrefix ''ChannelCreateData)
$(makeFieldLabelsNoPrefix ''ChannelPosition)
$(makeFieldLabelsNoPrefix ''ListMembersOptions)
$(makeFieldLabelsNoPrefix ''AddGuildMemberData)
$(makeFieldLabelsNoPrefix ''ModifyGuildRolePositionsData)
$(makeFieldLabelsNoPrefix ''CreateGuildBanData)
