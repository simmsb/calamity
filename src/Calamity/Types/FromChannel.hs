-- | FromChannel instances
module Calamity.Types.FromChannel
    ( FromChannel(..) ) where

import           Calamity.Types.General
import           Calamity.Types.Snowflake
import           Calamity.Types.SnowflakeMap  ( SnowflakeMap(..) )

import           Data.Generics.Product.Fields

class FromChannel a where
  type FromRet a

  type FromRet a = Either Text a

  -- | Convert from a channel into a more specific channel type
  fromChannel :: Proxy a -> Channel -> FromRet a

  -- | Convert from a specific channel type back to the generic channel type
  toChannel :: a -> Channel

ensureChannelType :: ChannelType -> ChannelType -> Either Text ()
ensureChannelType a b
  | a == b = Right ()
ensureChannelType a b = Left $ "Channel type " +|| a ||+ " does not match expected " +|| b ||+ ""

ensureField :: Text -> Maybe a -> Either Text a
ensureField name = maybeToRight ("Missing field: " <> name)

type family TContains (a :: k) (l :: [k]) :: Constraint where
  TContains k (k:_) = ()
  TContains k (_:r) = TContains k r

defChannel :: TContains a '[Channel, GroupDM, SingleDM, VoiceChannel, TextChannel, Category,
  GuildChannel]
           => Snowflake a
           -> ChannelType
           -> Channel
defChannel s t = Channel (coerceSnowflake s) t Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance FromChannel SingleDM where
  fromChannel _ c = do
    ensureChannelType (c ^. field @"type_") DMType
    recipients <- ensureField "recipients" $ c ^. field @"recipients"
    pure $ SingleDM (coerceSnowflake $ c ^. field @"id") (c ^. field @"lastMessageID") recipients

  toChannel SingleDM { id, lastMessageID, recipients } = defChannel id DMType
    & field @"lastMessageID" .~ lastMessageID
    & field @"recipients" ?~ recipients

instance FromChannel GroupDM where
  fromChannel _ c = do
    ensureChannelType (c ^. field @"type_") GroupDMType
    owner <- ensureField "ownerID" $ c ^. field @"ownerID"
    recipients <- ensureField "recipients" $ c ^. field @"recipients"
    name <- ensureField "name" $ c ^. field @"name"
    pure $ GroupDM (coerceSnowflake $ c ^. field @"id") owner (c ^. field @"lastMessageID") (c ^. field @"icon")
      recipients name

  toChannel GroupDM { id, ownerID, lastMessageID, icon, recipients, name } = defChannel id GroupDMType
    & field @"ownerID" ?~ ownerID
    & field @"lastMessageID" .~ lastMessageID
    & field @"icon" .~ icon
    & field @"recipients" ?~ recipients
    & field @"name" ?~ name

instance FromChannel DMChannel where
  fromChannel _ c@Channel { type_ } = case type_ of
    DMType      -> Single <$> fromChannel (Proxy @SingleDM) c
    GroupDMType -> Group <$> fromChannel (Proxy @GroupDM) c
    _           -> Left "Channel was not one of DMType or GroupDMType"

  toChannel (Single dm) = toChannel dm
  toChannel (Group dm) = toChannel dm

instance FromChannel GuildChannel where
  fromChannel _ c@Channel { type_ } = case type_ of
    GuildTextType  -> GuildTextChannel <$> fromChannel (Proxy @TextChannel) c
    GuildVoiceType -> GuildVoiceChannel <$> fromChannel (Proxy @VoiceChannel) c
    _              -> Left "Channel was not one of GuildTextType, GuildVoiceType, or GuildCategoryType"

  toChannel (GuildTextChannel c) = toChannel c
  toChannel (GuildVoiceChannel c) = toChannel c

instance FromChannel Category where
  type FromRet Category = SnowflakeMap GuildChannel -> Either Text Category

  fromChannel _ c channels = do
    ensureChannelType (c ^. field @"type_") GuildCategoryType
    permissionOverwrites <- ensureField "permissionOverwrites" $ c ^. field @"permissionOverwrites"
    name <- ensureField "name" $ c ^. field @"name"
    let nsfw = fromMaybe False $ c ^. field @"nsfw"
    position <- ensureField "position" $ c ^. field @"position"
    guildID <- ensureField "guildID" $ c ^. field @"guildID"
    pure $ Category (coerceSnowflake $ c ^. field @"id") permissionOverwrites name nsfw position guildID channels

  toChannel Category { id, permissionOverwrites, name, nsfw, position, guildID } = defChannel id GuildCategoryType
    & field @"permissionOverwrites" ?~ permissionOverwrites
    & field @"name" ?~ name
    & field @"nsfw" ?~ nsfw
    & field @"position" ?~ position
    & field @"guildID" ?~ guildID

instance FromChannel TextChannel where
  fromChannel _ c = do
    ensureChannelType (c ^. field @"type_") GuildTextType
    let id = coerceSnowflake $ c ^. field @"id"
    guildID <- ensureField "guildID" $ c ^. field @"guildID"
    position <- ensureField "position" $ c ^. field @"position"
    permissionOverwrites <- ensureField "permissionOverwrites" $ c ^. field @"permissionOverwrites"
    name <- ensureField "name" $ c ^. field @"name"
    let topic = fromMaybe "" $ c ^. field @"topic"
    let nsfw = fromMaybe False $ c ^. field @"nsfw"
    pure $ TextChannel id guildID position permissionOverwrites name topic nsfw (c ^. field @"lastMessageID")
      (c ^. field @"rateLimitPerUser") (c ^. field @"parentID")

  toChannel TextChannel { id
                        , guildID
                        , position
                        , permissionOverwrites
                        , name
                        , topic
                        , nsfw
                        , lastMessageID
                        , rateLimitPerUser
                        , parentID } = defChannel id GuildTextType
    & field @"guildID" ?~ guildID
    & field @"position" ?~ position
    & field @"permissionOverwrites" ?~ permissionOverwrites
    & field @"name" ?~ name
    & field @"topic" ?~ topic
    & field @"nsfw" ?~ nsfw
    & field @"lastMessageID" .~ lastMessageID
    & field @"rateLimitPerUser" .~ rateLimitPerUser
    & field @"parentID" .~ parentID

instance FromChannel VoiceChannel where
  fromChannel _ c = do
    ensureChannelType (c ^. field @"type_") GuildVoiceType
    guildID <- ensureField "guildID" $ c ^. field @"guildID"
    position <- ensureField "position" $ c ^. field @"position"
    permissionOverwrites <- ensureField "permissionOverwrites" $ c ^. field @"permissionOverwrites"
    name <- ensureField "name" $ c ^. field @"name"
    bitrate <- ensureField "bitrate" $ c ^. field @"bitrate"
    userLimit <- ensureField "userLimit" $ c ^. field @"userLimit"
    pure $ VoiceChannel (coerceSnowflake $ c ^. field @"id") guildID position permissionOverwrites name bitrate
      userLimit

  toChannel VoiceChannel { id, guildID, position, permissionOverwrites, name, bitrate, userLimit } =
    defChannel id GuildVoiceType
    & field @"guildID" ?~ guildID
    & field @"position" ?~ position
    & field @"permissionOverwrites" ?~ permissionOverwrites
    & field @"name" ?~ name
    & field @"bitrate" ?~ bitrate
    & field @"userLimit" ?~ userLimit
