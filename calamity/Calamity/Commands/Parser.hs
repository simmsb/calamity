-- | Something that can parse user input
module Calamity.Commands.Parser
    ( Parser(..)
    , Named
    , KleeneStarConcat
    , KleenePlusConcat
    , ParserEffs
    , runCommandParser
    -- * Utilities for writing your own parsers
    , parseMP
    , ParserCtxE
    , SpannedError(..) ) where

import           Calamity.Cache.Eff
import           Calamity.Commands.Context
import           Calamity.Commands.ParameterInfo
import           Calamity.Types.Model.Channel  ( Channel, GuildChannel )
import           Calamity.Types.Model.Guild    ( Emoji, RawEmoji(..), Partial(PartialEmoji), Guild, Member, Role )
import           Calamity.Types.Model.User     ( User )
import           Calamity.Types.Snowflake
import           Calamity.Types.Partial

import           Control.Lens                  hiding ( Context )
import           Control.Monad
import           Control.Monad.Trans           ( lift )

import           Data.Char                     ( isSpace )
import           Data.Kind
import           Data.List.NonEmpty            ( NonEmpty(..) )
import           Data.Maybe                    ( isJust, fromMaybe )
import           Data.Semigroup
import qualified Data.Text                     as S
import qualified Data.Text.Lazy                as L
import           Data.Typeable

import           GHC.Generics                  ( Generic )
import           GHC.TypeLits                  ( KnownSymbol, Symbol, symbolVal )

import qualified Polysemy                      as P
import qualified Polysemy.Error                as P
import qualified Polysemy.Reader               as P
import qualified Polysemy.State                as P

import           Text.Megaparsec               hiding ( parse )
import           Text.Megaparsec.Char
import           Text.Megaparsec.Error.Builder ( errFancy, fancy )
import Text.Megaparsec.Char.Lexer (float, decimal, signed)
import Numeric.Natural (Natural)

data SpannedError = SpannedError L.Text !Int !Int
  deriving ( Show, Eq, Ord )

data ParserState = ParserState
  { off :: Int
  , msg :: L.Text
  }
  deriving ( Show, Generic )

type ParserEffs r = P.State ParserState ': P.Error (S.Text, L.Text) ': P.Reader Context ': r
type ParserCtxE r = P.Reader Context ': r

runCommandParser :: Context -> L.Text -> P.Sem (ParserEffs r) a -> P.Sem r (Either (S.Text, L.Text) a)
runCommandParser ctx t = P.runReader ctx . P.runError . P.evalState (ParserState 0 t)

class Typeable a => Parser (a :: Type) r where
  type ParserResult a

  type ParserResult a = a

  parameterInfo :: ParameterInfo
  default parameterInfo :: ParameterInfo
  parameterInfo = ParameterInfo Nothing (typeRep $ Proxy @a) (parameterDescription @a @r)

  parameterDescription :: S.Text

  parse :: P.Sem (ParserEffs r) (ParserResult a)

-- | A named parameter, used to attach the name @s@ to a type in the command's
-- help output
data Named (s :: Symbol) (a :: Type)

instance (KnownSymbol s, Parser a r) => Parser (Named s a) r where
  type ParserResult (Named s a) = ParserResult a

  parameterInfo =
    let ParameterInfo _ type_ typeDescription = parameterInfo @a @r
    in ParameterInfo (Just . S.pack . symbolVal $ Proxy @s) type_ typeDescription

  parameterDescription = parameterDescription @a @r

  parse = mapE (_1 .~ parserName @(Named s a) @r) $ parse @a @r

parserName :: forall a r. Parser a r => S.Text
parserName = let ParameterInfo (fromMaybe "" -> name) type_ _ = parameterInfo @a @r
              in name <> ":" <> S.pack (show type_)

mapE :: P.Member (P.Error e) r => (e -> e) -> P.Sem r a -> P.Sem r a
mapE f m = P.catch m (P.throw . f)

-- | Use a megaparsec parser to parse a parameter
parseMP ::
  -- | The name of this parser
  S.Text ->
  -- | The megaparsec parser
  ParsecT SpannedError L.Text (P.Sem (ParserCtxE r)) a
  -> P.Sem (ParserEffs r) a
parseMP n m = do
  s <- P.get
  res <- P.raise . P.raise $ runParserT (skipN (s ^. #off) *> trackOffsets (space *> m)) "" (s ^. #msg)
  case res of
    Right (a, offset) -> do
      P.modify (#off +~ offset)
      pure a
    Left s  -> P.throw (n, L.pack $ errorBundlePretty s)

instance Parser L.Text r where
  parse = parseMP (parserName @L.Text) item
  parameterDescription = "word or quoted string"

instance Parser S.Text r where
  parse = parseMP (parserName @S.Text) (L.toStrict <$> item)
  parameterDescription = "word or quoted string"

instance Parser Integer r where
  parse = parseMP (parserName @Integer) (signed mempty decimal)
  parameterDescription = "number"

instance Parser Natural r where
  parse = parseMP (parserName @Natural) decimal
  parameterDescription = "number"

instance Parser Int r where
  parse = parseMP (parserName @Int) (signed mempty decimal)
  parameterDescription = "number"

instance Parser Word r where
  parse = parseMP (parserName @Word) decimal
  parameterDescription = "number"

instance Parser Float r where
  parse = parseMP (parserName @Float) (signed mempty (try float <|> decimal))
  parameterDescription = "number"

instance Parser a r => Parser (Maybe a) r where
  type ParserResult (Maybe a) = Maybe (ParserResult a)

  parse = P.catch (Just <$> parse @a) (const $ pure Nothing)
  parameterDescription = "optional " <> parameterDescription @a @r


instance (Parser a r, Parser b r) => Parser (Either a b) r where
  type ParserResult (Either a b) = Either (ParserResult a) (ParserResult b)

  parse = do
    l <- parse @(Maybe a) @r
    case l of
      Just l' -> pure (Left l')
      Nothing ->
        Right <$> parse @b @r
  parameterDescription = "either '" <> parameterDescription @a @r <> "' or '" <> parameterDescription @b @r <> "'"

instance Parser a r => Parser [a] r where
  type ParserResult [a] = [ParserResult a]

  parse = go []
    where go :: [ParserResult a] -> P.Sem (ParserEffs r) [ParserResult a]
          go l = P.catch (Just <$> parse @a) (const $ pure Nothing) >>= \case
            Just a -> go $ l <> [a]
            Nothing -> pure l

  parameterDescription = "zero or more '" <> parameterDescription @a @r <> "'"

instance (Parser a r, Typeable a) => Parser (NonEmpty a) r where
  type ParserResult (NonEmpty a) = NonEmpty (ParserResult a)

  parse = do
    a <- parse @a
    as <- parse @[a]
    pure $ a :| as

  parameterDescription = "one or more '" <> parameterDescription @a @r <> "'"

-- | A parser that consumes zero or more of @a@ then concatenates them together.
--
-- @'KleeneStarConcat' 'L.Text'@ therefore consumes all remaining input.
data KleeneStarConcat (a :: Type)

instance (Monoid (ParserResult a), Parser a r) => Parser (KleeneStarConcat a) r where
  type ParserResult (KleeneStarConcat a) = ParserResult a

  parse = mconcat <$> parse @[a]
  parameterDescription = "zero or more '" <> parameterDescription @a @r <> "'"

instance {-# OVERLAPS #-}Parser (KleeneStarConcat L.Text) r where
  type ParserResult (KleeneStarConcat L.Text) = ParserResult L.Text

  -- | consume rest on text just takes everything remaining
  parse = parseMP (parserName @(KleeneStarConcat L.Text)) manySingle
  parameterDescription = "the remaining input"

instance {-# OVERLAPS #-}Parser (KleeneStarConcat S.Text) r where
  type ParserResult (KleeneStarConcat S.Text) = ParserResult S.Text

  -- | consume rest on text just takes everything remaining
  parse = parseMP (parserName @(KleeneStarConcat S.Text)) (L.toStrict <$> manySingle)
  parameterDescription = "the remaining input"

-- | A parser that consumes one or more of @a@ then concatenates them together.
--
-- @'KleenePlusConcat' 'L.Text'@ therefore consumes all remaining input.
data KleenePlusConcat (a :: Type)

instance (Semigroup (ParserResult a), Parser a r) => Parser (KleenePlusConcat a) r where
  type ParserResult (KleenePlusConcat a) = ParserResult a

  parse = sconcat <$> parse @(NonEmpty a)
  parameterDescription = "one or more '" <> parameterDescription @a @r <> "'"

instance {-# OVERLAPS #-}Parser (KleenePlusConcat L.Text) r where
  type ParserResult (KleenePlusConcat L.Text) = ParserResult L.Text

  -- consume rest on text just takes everything remaining
  parse = parseMP (parserName @(KleenePlusConcat L.Text)) someSingle
  parameterDescription = "the remaining input"

instance {-# OVERLAPS #-}Parser (KleenePlusConcat S.Text) r where
  type ParserResult (KleenePlusConcat S.Text) = ParserResult S.Text

  -- consume rest on text just takes everything remaining
  parse = parseMP (parserName @(KleenePlusConcat S.Text)) (L.toStrict <$> someSingle)
  parameterDescription = "the remaining input"

instance Typeable (Snowflake a) => Parser (Snowflake a) r where
  parse = parseMP (parserName @(Snowflake a)) snowflake
  parameterDescription = "discord id"

-- | Accepts both plain IDs and mentions
instance {-# OVERLAPS #-}Parser (Snowflake User) r where
  parse = parseMP (parserName @(Snowflake User)) (try (ping "@") <|> snowflake)
  parameterDescription = "user mention or id"

-- | Accepts both plain IDs and mentions
instance {-# OVERLAPS #-}Parser (Snowflake Member) r where
  parse = parseMP (parserName @(Snowflake Member)) (try (ping "@") <|> snowflake)
  parameterDescription = "user mention or id"

-- | Accepts both plain IDs and mentions
instance {-# OVERLAPS #-}Parser (Snowflake Channel) r where
  parse = parseMP (parserName @(Snowflake Channel)) (try (ping "#") <|> snowflake)
  parameterDescription = "channel mention or id"

-- | Accepts both plain IDs and mentions
instance {-# OVERLAPS #-}Parser (Snowflake Role) r where
  parse = parseMP (parserName @(Snowflake Role)) (try (ping "@&") <|> snowflake)
  parameterDescription = "role mention or id"

-- | Accepts both plain IDs and uses of emoji
instance {-# OVERLAPS #-}Parser (Snowflake Emoji) r where
  parse = parseMP (parserName @(Snowflake Emoji)) (try emoji <|> snowflake)
  parameterDescription = "emoji or id"

-- mapParserMaybe :: Stream s => ParsecT SpannedError s m a -> Text -> (a -> Maybe b) -> ParsecT SpannedError s m b
-- mapParserMaybe m e f = do
--   offs <- getOffset
--   r <- f <$> m
--   offe <- getOffset
--   case r of
--     Just r' -> pure r'
--     _       -> parseError . errFancy offs . fancy . ErrorCustom $ SpannedError e offs offe

mapParserMaybeM :: (Monad m, Stream s) => ParsecT SpannedError s m a -> L.Text -> (a -> m (Maybe b)) -> ParsecT SpannedError s m b
mapParserMaybeM m e f = do
  offs <- getOffset
  r <- m >>= lift . f
  offe <- getOffset
  case r of
    Just r' -> pure r'
    Nothing -> parseError . errFancy offs . fancy . ErrorCustom $ SpannedError e offs offe

-- | Parser for members in the guild the command was invoked in, this only looks
-- in the cache. Use @'Snowflake' 'Member'@ and use
-- 'Calamity.Types.Upgradeable.upgrade' if you want to allow fetching from http.
instance Parser Member r where
  parse = parseMP (parserName @Member) $ mapParserMaybeM (try (ping "@") <|> snowflake)
          "Couldn't find a Member with this id"
          (\mid -> do
              ctx <- P.ask
              pure $ ctx ^? #guild . _Just . #members . ix mid)
  parameterDescription = "user mention or id"

-- | Parser for users, this only looks in the cache. Use @'Snowflake'
-- 'User'@ and use 'Calamity.Types.Upgradeable.upgrade' if you want to allow
-- fetching from http.
instance P.Member CacheEff r => Parser User r where
  parse = parseMP (parserName @User @r) $ mapParserMaybeM (try (ping "@") <|> snowflake)
          "Couldn't find a User with this id"
          getUser
  parameterDescription = "user mention or id"

-- | Parser for channels in the guild the command was invoked in, this only
-- looks in the cache. Use @'Snowflake' 'Channel'@ and use
-- 'Calamity.Types.Upgradeable.upgrade' if you want to allow fetching from http.
instance Parser GuildChannel r where
  parse = parseMP (parserName @GuildChannel @r) $ mapParserMaybeM (try (ping "#") <|> snowflake)
          "Couldn't find a GuildChannel with this id"
          (\cid -> do
              ctx <- P.ask
              pure $ ctx ^? #guild . _Just . #channels . ix cid)
  parameterDescription = "channel mention or id"

-- | Parser for guilds, this only looks in the cache. Use @'Snowflake' 'Guild'@
-- and use 'Calamity.Types.Upgradeable.upgrade' if you want to allow fetching
-- from http.
instance P.Member CacheEff r => Parser Guild r where
  parse = parseMP (parserName @Guild @r) $ mapParserMaybeM snowflake
          "Couldn't find a Guild with this id"
          getGuild
  parameterDescription = "guild id"

-- | Parser for emojis in the guild the command was invoked in, this only
-- looks in the cache. Use @'Snowflake' 'Emoji'@ and use
-- 'Calamity.Types.Upgradeable.upgrade' if you want to allow fetching from http.
instance Parser Emoji r where
  parse = parseMP (parserName @Emoji @r) $ mapParserMaybeM (try emoji <|> snowflake)
          "Couldn't find an Emoji with this id"
          (\eid -> do
              ctx <- P.ask
              pure $ ctx ^? #guild . _Just . #emojis . ix eid)
  parameterDescription = "emoji or id"

-- | Parses both discord emojis, and unicode emojis
instance Parser RawEmoji r where
  parse = parseMP (parserName @RawEmoji) (try parseCustomEmoji <|> (UnicodeEmoji <$> takeP (Just "A unicode emoji") 1))
    where parseCustomEmoji = CustomEmoji <$> partialEmoji
  parameterDescription = "emoji"

-- | Parser for roles in the guild the command was invoked in, this only
-- looks in the cache. Use @'Snowflake' 'Role'@ and use
-- 'Calamity.Types.Upgradeable.upgrade' if you want to allow fetching from http.
instance Parser Role r where
  parse = parseMP (parserName @Role @r) $ mapParserMaybeM (try (ping "@&") <|> snowflake)
          "Couldn't find an Emoji with this id"
          (\rid -> do
              ctx <- P.ask
              pure $ ctx ^? #guild . _Just . #roles . ix rid)
  parameterDescription = "role mention or id"

instance (Parser a r, Parser b r) => Parser (a, b) r where
  type ParserResult (a, b) = (ParserResult a, ParserResult b)

  parse = do
    a <- parse @a
    b <- parse @b
    pure (a, b)

  parameterDescription = "'" <> parameterDescription @a @r <> "' then '" <> parameterDescription @b @r <> "'"

instance Parser () r where
  parse = parseMP (parserName @()) space
  parameterDescription = "whitespace"

instance ShowErrorComponent SpannedError where
  showErrorComponent (SpannedError t _ _) = L.unpack t
  errorComponentLen (SpannedError _ s e) = max 1 $ e - s

skipN :: (Stream s, Ord e) => Int -> ParsecT e s m ()
skipN n = void $ takeP Nothing n

ping :: MonadParsec e L.Text m => L.Text -> m (Snowflake a)
ping c = chunk ("<" <> c) *> optional (chunk "!") *> snowflake <* chunk ">"

ping' :: MonadParsec e L.Text m => m () -> m (Snowflake a)
ping' m = chunk "<" *> m *> snowflake <* chunk ">"

snowflake :: MonadParsec e L.Text m => m (Snowflake a)
snowflake = Snowflake <$> decimal

partialEmoji :: MonadParsec e L.Text m => m (Partial Emoji)
partialEmoji = do
  animated <- isJust <$> (chunk "<" *> optional (chunk "a"))
  name <-  between (chunk ":") (chunk ":") (takeWhileP (Just "Emoji name") $ not . (== ':'))
  id <- snowflake
  void $ chunk ">"
  pure (PartialEmoji id name animated)

emoji :: MonadParsec e L.Text m => m (Snowflake a)
emoji = ping' (optional (chunk "a") *> between (chunk ":") (chunk ":") (void $ takeWhileP Nothing $ not . (== ':')))

trackOffsets :: MonadParsec e s m => m a -> m (a, Int)
trackOffsets m = do
  offs <- getOffset
  a <- m
  offe <- getOffset
  pure (a, offe - offs)

item :: MonadParsec e L.Text m => m L.Text
item = try quotedString <|> someNonWS

manySingle :: MonadParsec e s m => m (Tokens s)
manySingle = takeWhileP (Just "Any character") (const True)

someSingle :: MonadParsec e s m => m (Tokens s)
someSingle = takeWhile1P (Just "any character") (const True)

quotedString :: MonadParsec e L.Text m => m L.Text
quotedString = try (between (chunk "'") (chunk "'") (takeWhileP (Just "any character") $ not . (== '\''))) <|>
               between (chunk "\"") (chunk "\"") (takeWhileP (Just "any character") $ not . (== '"'))

-- manyNonWS :: (Token s ~ Char, MonadParsec e s m) => m (Tokens s)
-- manyNonWS = takeWhileP (Just "Any Non-Whitespace") (not . isSpace)

someNonWS :: (Token s ~ Char, MonadParsec e s m) => m (Tokens s)
someNonWS = takeWhile1P (Just "any non-whitespace") (not . isSpace)
