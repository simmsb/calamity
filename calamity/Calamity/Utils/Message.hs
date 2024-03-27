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
  MessageReference (MessageReference),
  TextChannel,
  VoiceChannel,
 )
import Calamity.Types.Model.Guild (Emoji (..), Member, Role)
import Calamity.Types.Model.User (User)
import Calamity.Types.Snowflake
import Data.Foldable (Foldable (foldl'))
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import Data.Text qualified as T
import GHC.Records (HasField (getField))
import Optics
import TextShow (TextShow (showt))

zws :: (IsString s) => s
zws = fromString "\x200b"

-- | Replaces all occurences of @\`\`\`@ with @\`\<zws\>\`\<zws\>\`@
escapeCodeblocks :: T.Text -> T.Text
escapeCodeblocks = T.replace "```" (T.intercalate zws $ replicate 3 "`")

-- | Replaces all occurences of @\`\`@ with @\`\<zws\>\`@
escapeCodelines :: T.Text -> T.Text
escapeCodelines = T.replace "``" (T.intercalate zws $ replicate 2 "`")

-- | Replaces all occurences of @\*\*@ with @\*\<zws\>\*@
escapeBold :: T.Text -> T.Text
escapeBold = T.replace "**" (T.intercalate zws $ replicate 2 "*")

-- | Replaces all occurences of @\~\~@ with @\~\<zws\>\~@
escapeStrike :: T.Text -> T.Text
escapeStrike = T.replace "~~" (T.intercalate zws $ replicate 2 "~")

-- | Replaces all occurences of @\_\_@ with @\_\<zws\>\_@
escapeUnderline :: T.Text -> T.Text
escapeUnderline = T.replace "__" (T.intercalate zws $ replicate 2 "_")

-- | Replaces all occurences of @\|\|@ with @\|\<zws\>\|@
escapeSpoilers :: T.Text -> T.Text
escapeSpoilers = T.replace "||" (T.intercalate zws $ replicate 2 "|")

-- | Escape all discord formatting
escapeFormatting :: T.Text -> T.Text
escapeFormatting = foldl' (.) Prelude.id [escapeCodelines, escapeCodeblocks, escapeBold, escapeStrike, escapeUnderline, escapeSpoilers, escapeFormatting]

{- | Formats a lang and content into a codeblock

 >>> codeblock "hs" "x = y"
 "```hs\nx = y\n```"

 Any codeblocks in the @content@ are escaped
-}
codeblock ::
  -- | language
  T.Text ->
  -- | content
  T.Text ->
  T.Text
codeblock lang = codeblock' (Just lang)

{- | Formats an optional lang and content into a codeblock

 Any codeblocks in the @content@ are escaped
-}
codeblock' ::
  -- | language
  Maybe T.Text ->
  -- | content
  T.Text ->
  T.Text
codeblock' lang content =
  "```"
    <> fromMaybe "" lang
    <> "\n"
    <> escapeCodeblocks content
    <> "\n```"

{- | Formats some content into a code line

 This always uses @``@ code lines as they can be escaped

 Any code lines in the content are escaped
-}
codeline :: T.Text -> T.Text
codeline content = "``" <> escapeCodelines content <> "``"

{- | Formats some text into its bolded form

 Any existing bolded text is escaped
-}
bold :: T.Text -> T.Text
bold content = "**" <> escapeBold content <> "**"

{- | Formats some text into its striked form

 Any existing striked text is escaped
-}
strike :: T.Text -> T.Text
strike content = "~~" <> escapeStrike content <> "~~"

{- | Formats some text into its underlined form

 Any existing underlined text is escaped
-}
underline :: T.Text -> T.Text
underline content = "__" <> escapeUnderline content <> "__"

-- | Quotes a section of text
quote :: T.Text -> T.Text
quote = ("> " <>)

-- | Quotes all remaining text
quoteAll :: T.Text -> T.Text
quoteAll = (">> " <>)

{- | Formats some text into its spoilered form

 Any existing spoilers are escaped
-}
spoiler :: T.Text -> T.Text
spoiler content = "||" <> escapeSpoilers content <> "||"

fmtEmoji :: Emoji -> T.Text
fmtEmoji Emoji {id, name, animated} = "<" <> ifanim <> ":" <> name <> ":" <> showt id <> ">"
  where
    ifanim = if animated then "a" else ""

-- | Format a 'User' or 'Member' into the format of @username#discriminator@
displayUser :: (HasField "username" a T.Text, HasField "discriminator" a T.Text) => a -> T.Text
displayUser u = getField @"username" u <> "#" <> getField @"discriminator" u

mentionSnowflake :: T.Text -> Snowflake a -> T.Text
mentionSnowflake tag s = "<" <> tag <> showt s <> ">"

-- | Things that can be mentioned
class Mentionable a where
  mention :: a -> T.Text

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
asReference ::
  -- | The message to reply to
  Message ->
  -- | If discord should error when replying to deleted messages
  Bool ->
  MessageReference
asReference msg =
  MessageReference
    (Just $ getID @Message msg)
    (Just $ getID @Channel msg)
    (msg ^. #guildID)
