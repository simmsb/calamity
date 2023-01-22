{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'ParameterParser' instances for calamity models
module Calamity.Commands.CalamityParsers () where

import Calamity.Cache.Eff
import Calamity.Commands.Context
import Calamity.Types.Model.Channel (Channel, GuildChannel)
import Calamity.Types.Model.Guild (Emoji, Guild, Member, Partial (PartialEmoji), RawEmoji (..), Role)
import Calamity.Types.Model.User (User)
import Calamity.Types.Partial
import Calamity.Types.Snowflake
import CalamityCommands.ParameterInfo
import CalamityCommands.Parser
import Control.Monad
import Control.Monad.Trans (lift)
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as T
import Data.Typeable
import Optics
import Polysemy qualified as P
import Polysemy.Reader qualified as P
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error.Builder (errFancy, fancy)

parserName :: forall a c r. ParameterParser a c r => T.Text
parserName =
  let ParameterInfo (fromMaybe "" -> name) type_ _ = parameterInfo @a @c @r
   in name <> ":" <> T.pack (show type_)

instance Typeable (Snowflake a) => ParameterParser (Snowflake a) c r where
  parse = parseMP (parserName @(Snowflake a)) snowflake
  parameterDescription = "discord id"

-- | Accepts both plain IDs and mentions
instance {-# OVERLAPS #-} ParameterParser (Snowflake User) c r where
  parse = parseMP (parserName @(Snowflake User)) (try (ping "@") <|> snowflake)
  parameterDescription = "user mention or id"

-- | Accepts both plain IDs and mentions
instance {-# OVERLAPS #-} ParameterParser (Snowflake Member) c r where
  parse = parseMP (parserName @(Snowflake Member)) (try (ping "@") <|> snowflake)
  parameterDescription = "user mention or id"

-- | Accepts both plain IDs and mentions
instance {-# OVERLAPS #-} ParameterParser (Snowflake Channel) c r where
  parse = parseMP (parserName @(Snowflake Channel)) (try (ping "#") <|> snowflake)
  parameterDescription = "channel mention or id"

-- | Accepts both plain IDs and mentions
instance {-# OVERLAPS #-} ParameterParser (Snowflake Role) c r where
  parse = parseMP (parserName @(Snowflake Role)) (try (ping "@&") <|> snowflake)
  parameterDescription = "role mention or id"

-- | Accepts both plain IDs and uses of emoji
instance {-# OVERLAPS #-} ParameterParser (Snowflake Emoji) c r where
  parse = parseMP (parserName @(Snowflake Emoji)) (try emoji <|> snowflake)
  parameterDescription = "emoji or id"

mapParserMaybeM :: (Monad m, Stream s) => ParsecT SpannedError s m a -> T.Text -> (a -> m (Maybe b)) -> ParsecT SpannedError s m b
mapParserMaybeM m e f = do
  offs <- getOffset
  r <- m >>= lift . f
  offe <- getOffset
  case r of
    Just r' -> pure r'
    Nothing -> parseError . errFancy offs . fancy . ErrorCustom $ SpannedError e offs offe

{- | ParameterParser for members in the guild the command was invoked in, this only looks
 in the cache. Use @'Snowflake' 'Member'@ and use
 'Calamity.Types.Upgradeable.upgrade' if you want to allow fetching from http.
-}
instance (P.Member CacheEff r, CalamityCommandContext c) => ParameterParser Member c r where
  parse =
    parseMP (parserName @Member @c @r) $
      mapParserMaybeM
        (try (ping "@") <|> snowflake)
        "Couldn't find a Member with this id"
        ( \mid -> do
            ctx <- P.ask
            guild <- join <$> getGuild `traverse` ctxGuildID ctx
            pure $ guild ^? _Just % #members % ix mid
        )
  parameterDescription = "user mention or id"

{- | ParameterParser for users, this only looks in the cache. Use @'Snowflake'
 'User'@ and use 'Calamity.Types.Upgradeable.upgrade' if you want to allow
 fetching from http.
-}
instance P.Member CacheEff r => ParameterParser User c r where
  parse =
    parseMP (parserName @User @c @r) $
      mapParserMaybeM
        (try (ping "@") <|> snowflake)
        "Couldn't find a User with this id"
        getUser
  parameterDescription = "user mention or id"

{- | ParameterParser for channels in the guild the command was invoked in, this only
 looks in the cache. Use @'Snowflake' 'Channel'@ and use
 'Calamity.Types.Upgradeable.upgrade' if you want to allow fetching from http.
-}
instance (P.Member CacheEff r, CalamityCommandContext c) => ParameterParser GuildChannel c r where
  parse =
    parseMP (parserName @GuildChannel @c @r) $
      mapParserMaybeM
        (try (ping "#") <|> snowflake)
        "Couldn't find a GuildChannel with this id"
        ( \cid -> do
            ctx <- P.ask
            guild <- join <$> getGuild `traverse` ctxGuildID ctx
            pure $ guild ^? _Just % #channels % ix cid
        )
  parameterDescription = "channel mention or id"

{- | ParameterParser for guilds, this only looks in the cache. Use @'Snowflake' 'Guild'@
 and use 'Calamity.Types.Upgradeable.upgrade' if you want to allow fetching
 from http.
-}
instance P.Member CacheEff r => ParameterParser Guild c r where
  parse =
    parseMP (parserName @Guild @c @r) $
      mapParserMaybeM
        snowflake
        "Couldn't find a Guild with this id"
        getGuild
  parameterDescription = "guild id"

{- | ParameterParser for emojis in the guild the command was invoked in, this only
 looks in the cache. Use @'Snowflake' 'Emoji'@ and use
 'Calamity.Types.Upgradeable.upgrade' if you want to allow fetching from http.
-}
instance (P.Member CacheEff r, CalamityCommandContext c) => ParameterParser Emoji c r where
  parse =
    parseMP (parserName @Emoji @c @r) $
      mapParserMaybeM
        (try emoji <|> snowflake)
        "Couldn't find an Emoji with this id"
        ( \eid -> do
            ctx <- P.ask
            guild <- join <$> getGuild `traverse` ctxGuildID ctx
            pure $ guild ^? _Just % #emojis % ix eid
        )
  parameterDescription = "emoji or id"

-- | Parses both discord emojis, and unicode emojis
instance ParameterParser RawEmoji c r where
  parse = parseMP (parserName @RawEmoji) (try parseCustomEmoji <|> UnicodeEmoji <$> takeP (Just "A unicode emoji") 1)
    where
      parseCustomEmoji = CustomEmoji <$> partialEmoji
  parameterDescription = "emoji"

{- | ParameterParser for roles in the guild the command was invoked in, this only
 looks in the cache. Use @'Snowflake' 'Role'@ and use
 'Calamity.Types.Upgradeable.upgrade' if you want to allow fetching from http.
-}
instance (P.Member CacheEff r, CalamityCommandContext c) => ParameterParser Role c r where
  parse =
    parseMP (parserName @Role @c @r) $
      mapParserMaybeM
        (try (ping "@&") <|> snowflake)
        "Couldn't find an Emoji with this id"
        ( \rid -> do
            ctx <- P.ask
            guild <- join <$> getGuild `traverse` ctxGuildID ctx
            pure $ guild ^? _Just % #roles % ix rid
        )
  parameterDescription = "role mention or id"

-- skipN :: (Stream s, Ord e) => Int -> ParsecT e s m ()
-- skipN n = void $ takeP Nothing n

ping :: MonadParsec e T.Text m => T.Text -> m (Snowflake a)
ping c = chunk ("<" <> c) *> optional (chunk "!") *> snowflake <* chunk ">"

ping' :: MonadParsec e T.Text m => m () -> m (Snowflake a)
ping' m = chunk "<" *> m *> snowflake <* chunk ">"

snowflake :: MonadParsec e T.Text m => m (Snowflake a)
snowflake = Snowflake <$> decimal

partialEmoji :: MonadParsec e T.Text m => m (Partial Emoji)
partialEmoji = do
  animated <- isJust <$> (chunk "<" *> optional (chunk "a"))
  name <- between (chunk ":") (chunk ":") (takeWhileP (Just "Emoji name") (/= ':'))
  id <- snowflake
  void $ chunk ">"
  pure (PartialEmoji id name animated)

emoji :: MonadParsec e T.Text m => m (Snowflake a)
emoji = ping' (optional (chunk "a") *> between (chunk ":") (chunk ":") (void $ takeWhileP Nothing (/= ':')))

-- trackOffsets :: MonadParsec e s m => m a -> m (a, Int)
-- trackOffsets m = do
--   offs <- getOffset
--   a <- m
--   offe <- getOffset
--   pure (a, offe - offs)

-- item :: MonadParsec e L.Text m => m L.Text
-- item = try quotedString <|> someNonWS

-- manySingle :: MonadParsec e s m => m (Tokens s)
-- manySingle = takeWhileP (Just "Any character") (const True)

-- someSingle :: MonadParsec e s m => m (Tokens s)
-- someSingle = takeWhile1P (Just "any character") (const True)

-- quotedString :: MonadParsec e L.Text m => m L.Text
-- quotedString = try (between (chunk "'") (chunk "'") (takeWhileP (Just "any character") (/= '\''))) <|>
--                between (chunk "\"") (chunk "\"") (takeWhileP (Just "any character") (/= '"'))

-- -- manyNonWS :: (Token s ~ Char, MonadParsec e s m) => m (Tokens s)
-- -- manyNonWS = takeWhileP (Just "Any Non-Whitespace") (not . isSpace)

-- someNonWS :: (Token s ~ Char, MonadParsec e s m) => m (Tokens s)
-- someNonWS = takeWhile1P (Just "any non-whitespace") (not . isSpace)
