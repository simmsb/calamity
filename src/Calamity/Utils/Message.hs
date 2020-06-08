-- | Things for formatting things
module Calamity.Utils.Message
  ( codeblock,
    codeblock',
    codeline,
    escapeCodeblocks,
    escapeCodelines,
    zws,
    displayUser,
    Mentionable (..),
  )
where

import Calamity.Types.Model.Channel (Category, Channel, DMChannel, GuildChannel, TextChannel, VoiceChannel)
import Calamity.Types.Model.Guild (Member, Role)
import Calamity.Types.Model.User (User)
import Calamity.Types.Snowflake
import Control.Lens
import Data.Generics.Product.Fields
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import qualified Data.Text.Lazy as L
import TextShow (TextShow (showtl))

zws :: IsString s => s
zws = fromString "\x200b"

-- | Replaces all occurences of @```@ with @`<zws>`<zws>`@
escapeCodeblocks :: L.Text -> L.Text
escapeCodeblocks = L.replace "```" (L.intercalate zws $ replicate 3 "`")

-- | Replaces all occurences of @``@ with @`<zws>`@
escapeCodelines :: L.Text -> L.Text
escapeCodelines = L.replace "``" (L.intercalate zws $ replicate 2 "`")

-- | Formats a lang and content into a codeblock
--
-- >>> codeblock "hs" "x = y"
-- "```hs\nx = y\n```"
--
-- Any codeblocks in the @content@ are escaped
codeblock :: L.Text -- ^ language
          -> L.Text -- ^ content
          -> L.Text
codeblock lang = codeblock' (Just lang)

-- | Formats an optional lang and content into a codeblock
--
-- Any codeblocks in the @content@ are escaped
codeblock' :: Maybe L.Text -- ^ language
          -> L.Text -- ^ content
          -> L.Text
codeblock' lang content = "```" <> fromMaybe "" lang <> "\n" <>
                         escapeCodeblocks content <>
                         "\n```"

-- | Formats some content into a code line
--
-- This always uses @``@ code lines as they can be escaped
--
-- Any code lines in the content are escaped
codeline :: L.Text -> L.Text
codeline content = "``" <> escapeCodelines content <> "``"

-- | Format a 'User' or 'Member' into the format of @username#discriminator@
displayUser :: (HasField' "username" a L.Text, HasField' "discriminator" a L.Text) => a -> L.Text
displayUser u = (u ^. field' @"username") <> "#" <> (u ^. field' @"discriminator")

mentionSnowflake :: L.Text -> Snowflake a -> L.Text
mentionSnowflake tag s = "<" <> tag <> showtl s <> ">"

-- | Things that can be mentioned
class Mentionable a where
  mention :: a -> L.Text

instance Mentionable (Snowflake User) where
  mention = mentionSnowflake "@"

instance Mentionable (Snowflake Member) where
  mention = mentionSnowflake "@"

instance Mentionable (Snowflake Channel) where
  mention = mentionSnowflake "#"

instance Mentionable (Snowflake TextChannel) where
  mention = mentionSnowflake "#"

instance Mentionable (Snowflake VoiceChannel) where
  mention = mentionSnowflake "#"

instance Mentionable (Snowflake Category) where
  mention = mentionSnowflake "#"

instance Mentionable (Snowflake GuildChannel) where
  mention = mentionSnowflake "#"

instance Mentionable (Snowflake DMChannel) where
  mention = mentionSnowflake "#"

instance Mentionable (Snowflake Role) where
  mention = mentionSnowflake "@&"

instance Mentionable User where
  mention = mentionSnowflake "@" . getID @User

instance Mentionable Member where
  mention = mentionSnowflake "@" . getID @Member

instance Mentionable Channel where
  mention = mentionSnowflake "#" . getID @Channel

instance Mentionable TextChannel where
  mention = mentionSnowflake "#" . getID @TextChannel

instance Mentionable VoiceChannel where
  mention = mentionSnowflake "#" . getID @VoiceChannel

instance Mentionable Category where
  mention = mentionSnowflake "#" . getID @Category

instance Mentionable GuildChannel where
  mention = mentionSnowflake "#" . getID @GuildChannel

instance Mentionable DMChannel where
  mention = mentionSnowflake "#" . getID @DMChannel

instance Mentionable Role where
  mention = mentionSnowflake "@&" . getID @Role
