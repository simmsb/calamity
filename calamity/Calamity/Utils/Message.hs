-- | Things for formatting things
module Calamity.Utils.Message (
  codeblock,
  codeblock',
  codeline,
  escapeCodeblocks,
  escapeCodelines,
  escapeBold,
  escapeStrike,
  escapeUnderline,
  escapeSpoilers,
  escapeFormatting,
  bold,
  strike,
  underline,
  quote,
  quoteAll,
  spoiler,
  zws,
  fmtEmoji,
  displayUser,
  Mentionable (..),
  asReference,
) where

import Calamity.Types.Model.Channel (
  Category,
  Channel,
  DMChannel,
  GuildChannel,
  Message,
  MessageReference(MessageReference),
  TextChannel,
  VoiceChannel,
 )
import Calamity.Types.Model.Guild (Emoji (..), Member, Role)
import Calamity.Types.Model.User (User)
import Calamity.Types.Snowflake
import Control.Lens
import Data.Foldable (Foldable (foldl'))
import Data.Generics.Product.Fields
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import qualified Data.Text.Lazy as L
import TextShow (TextShow (showtl))

zws :: IsString s => s
zws = fromString "\x200b"

-- | Replaces all occurences of @\`\`\`@ with @\`\<zws\>\`\<zws\>\`@
escapeCodeblocks :: L.Text -> L.Text
escapeCodeblocks = L.replace "```" (L.intercalate zws $ replicate 3 "`")

-- | Replaces all occurences of @\`\`@ with @\`\<zws\>\`@
escapeCodelines :: L.Text -> L.Text
escapeCodelines = L.replace "``" (L.intercalate zws $ replicate 2 "`")

-- | Replaces all occurences of @\*\*@ with @\*\<zws\>\*@
escapeBold :: L.Text -> L.Text
escapeBold = L.replace "**" (L.intercalate zws $ replicate 2 "*")

-- | Replaces all occurences of @\~\~@ with @\~\<zws\>\~@
escapeStrike :: L.Text -> L.Text
escapeStrike = L.replace "~~" (L.intercalate zws $ replicate 2 "~")

-- | Replaces all occurences of @\_\_@ with @\_\<zws\>\_@
escapeUnderline :: L.Text -> L.Text
escapeUnderline = L.replace "__" (L.intercalate zws $ replicate 2 "_")

-- | Replaces all occurences of @\|\|@ with @\|\<zws\>\|@
escapeSpoilers :: L.Text -> L.Text
escapeSpoilers = L.replace "||" (L.intercalate zws $ replicate 2 "|")

-- | Escape all discord formatting
escapeFormatting :: L.Text -> L.Text
escapeFormatting = foldl' (.) Prelude.id [escapeCodelines, escapeCodeblocks, escapeBold, escapeStrike, escapeUnderline, escapeSpoilers, escapeFormatting]

{- | Formats a lang and content into a codeblock

 >>> codeblock "hs" "x = y"
 "```hs\nx = y\n```"

 Any codeblocks in the @content@ are escaped
-}
codeblock ::
  -- | language
  L.Text ->
  -- | content
  L.Text ->
  L.Text
codeblock lang = codeblock' (Just lang)

{- | Formats an optional lang and content into a codeblock

 Any codeblocks in the @content@ are escaped
-}
codeblock' ::
  -- | language
  Maybe L.Text ->
  -- | content
  L.Text ->
  L.Text
codeblock' lang content =
  "```" <> fromMaybe "" lang <> "\n"
    <> escapeCodeblocks content
    <> "\n```"

{- | Formats some content into a code line

 This always uses @``@ code lines as they can be escaped

 Any code lines in the content are escaped
-}
codeline :: L.Text -> L.Text
codeline content = "``" <> escapeCodelines content <> "``"

{- | Formats some text into its bolded form

 Any existing bolded text is escaped
-}
bold :: L.Text -> L.Text
bold content = "**" <> escapeBold content <> "**"

{- | Formats some text into its striked form

 Any existing striked text is escaped
-}
strike :: L.Text -> L.Text
strike content = "~~" <> escapeStrike content <> "~~"

{- | Formats some text into its underlined form

 Any existing underlined text is escaped
-}
underline :: L.Text -> L.Text
underline content = "__" <> escapeUnderline content <> "__"

-- | Quotes a section of text
quote :: L.Text -> L.Text
quote = ("> " <>)

-- | Quotes all remaining text
quoteAll :: L.Text -> L.Text
quoteAll = (">> " <>)

{- | Formats some text into its spoilered form

 Any existing spoilers are escaped
-}
spoiler :: L.Text -> L.Text
spoiler content = "||" <> escapeSpoilers content <> "||"

fmtEmoji :: Emoji -> L.Text
fmtEmoji Emoji{id, name, animated} = "<" <> ifanim <> ":" <> name <> ":" <> showtl id <> ">"
 where
  ifanim = if animated then "a" else ""

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

-- | Turn a regular 'Message' into a 'MessageReference'
asReference :: Message
  -- ^ The message to reply to
  -> Bool
  -- ^ If discord should error when replying to deleted messages
  -> MessageReference
asReference msg failIfNotExists =
  MessageReference
    (Just $ getID @Message msg)
    (Just $ getID @Channel msg)
    (msg ^. #guildID)
    failIfNotExists
