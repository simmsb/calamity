-- | Something that can parse user input
module Calamity.Commands.Parser
    ( Parser(..)
    , Named
    , KleeneConcat
    , runCommandParser ) where

import           Calamity.Cache.Eff
import           Calamity.Commands.Context
import           Calamity.Types.Model.Channel  ( Channel )
import           Calamity.Types.Model.Guild    ( Emoji, Member, Role )
import           Calamity.Types.Model.User     ( User )
import           Calamity.Types.Snowflake

import           Control.Lens                  hiding ( Context )
import           Control.Monad
import           Control.Monad.Trans           ( lift )

import           Data.Char                     ( isSpace )
import           Data.Kind
import           Data.List.NonEmpty            ( NonEmpty(..) )
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

data SpannedError = SpannedError L.Text !Int !Int
  deriving ( Show, Eq, Ord )

showTypeOf :: forall a. Typeable a => String
showTypeOf = show . typeRep $ Proxy @a

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

  parserName :: S.Text
  default parserName :: S.Text
  parserName = ":" <> S.pack (showTypeOf @a)

  parse :: P.Sem (ParserEffs r) (ParserResult a)

data Named (s :: Symbol) a

instance (KnownSymbol s, Parser a r) => Parser (Named s a) r where
  type ParserResult (Named s a) = ParserResult a

  parserName = (S.pack . symbolVal $ Proxy @s) <> parserName @a @r

  parse = mapE (_1 .~ parserName @(Named s a) @r) $ parse @a @r

mapE :: P.Member (P.Error e) r => (e -> e) -> P.Sem r a -> P.Sem r a
mapE f m = P.catch m (P.throw . f)

parseMP :: S.Text -> ParsecT SpannedError L.Text (P.Sem (ParserCtxE r)) a -> P.Sem (ParserEffs r) a
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

instance Parser S.Text r where
  parse = parseMP (parserName @S.Text) (L.toStrict <$> item)

instance Parser a r => Parser (Maybe a) r where
  type ParserResult (Maybe a) = Maybe (ParserResult a)

  parse = P.catch (Just <$> parse @a) (const $ pure Nothing)

instance Parser a r => Parser [a] r where
  type ParserResult [a] = [ParserResult a]

  parse = go []
    where go :: [ParserResult a] -> P.Sem (ParserEffs r) [ParserResult a]
          go l = P.catch ((: []) <$> parse @a) (const $ pure []) >>= go . (l ++)

instance (Parser a r, Typeable a) => Parser (NonEmpty a) r where
  type ParserResult (NonEmpty a) = NonEmpty (ParserResult a)

  parse = do
    a <- parse @a
    as <- parse @[a]
    pure $ a :| as

data KleeneConcat a

instance (Monoid (ParserResult a), Parser a r) => Parser (KleeneConcat a) r where
  type ParserResult (KleeneConcat a) = ParserResult a

  parse = mconcat <$> parse @[a]

instance {-# OVERLAPS #-}Parser (KleeneConcat L.Text) r where
  type ParserResult (KleeneConcat L.Text) = ParserResult L.Text

  -- consume rest on text just takes everything remaining
  parse = parseMP (parserName @(KleeneConcat L.Text)) someSingle

instance {-# OVERLAPS #-}Parser (KleeneConcat S.Text) r where
  type ParserResult (KleeneConcat S.Text) = ParserResult S.Text

  -- consume rest on text just takes everything remaining
  parse = parseMP (parserName @(KleeneConcat S.Text)) (L.toStrict <$> someSingle)

instance Typeable (Snowflake a) => Parser (Snowflake a) r where
  parse = parseMP (parserName @(Snowflake a)) snowflake

instance {-# OVERLAPS #-}Parser (Snowflake User) r where
  parse = parseMP (parserName @(Snowflake User)) (try (ping "@") <|> snowflake)

instance {-# OVERLAPS #-}Parser (Snowflake Member) r where
  parse = parseMP (parserName @(Snowflake Member)) (try (ping "@") <|> snowflake)

instance {-# OVERLAPS #-}Parser (Snowflake Channel) r where
  parse = parseMP (parserName @(Snowflake Channel)) (try (ping "#") <|> snowflake)

instance {-# OVERLAPS #-}Parser (Snowflake Role) r where
  parse = parseMP (parserName @(Snowflake Role)) (try (ping "@&") <|> snowflake)

instance {-# OVERLAPS #-}Parser (Snowflake Emoji) r where
  parse = parseMP (parserName @(Snowflake Emoji)) (try emoji <|> snowflake)

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
    _       -> parseError . errFancy offs . fancy . ErrorCustom $ SpannedError e offs offe

instance Parser Member r where
  parse = parseMP (parserName @Member) $ mapParserMaybeM (try (ping "@") <|> snowflake)
          "Couldn't find a Member with this id"
          (\mid -> do
              ctx <- P.ask
              pure $ ctx ^? #guild . _Just . #members . ix mid)

instance P.Member CacheEff r => Parser User r where
  parse = parseMP (parserName @User @r) $ mapParserMaybeM (try (ping "@") <|> snowflake)
          "Couldn't find a User with this id"
          getUser

instance (Parser a r, Parser b r) => Parser (a, b) r where
  type ParserResult (a, b) = (ParserResult a, ParserResult b)

  parse = do
    a <- parse @a
    b <- parse @b
    pure (a, b)

instance Parser () r where
  parse = parseMP (parserName @()) space

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
snowflake = (Snowflake . read) <$> some digitChar

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

-- manySingle :: MonadParsec e s m => m (Tokens s)
-- manySingle = takeWhileP (Just "Any character") (const True)

someSingle :: MonadParsec e s m => m (Tokens s)
someSingle = takeWhile1P (Just "any character") (const True)

quotedString :: MonadParsec e L.Text m => m L.Text
quotedString = try (between (chunk "'") (chunk "'") (takeWhileP (Just "any character") $ not . (== '\''))) <|>
               between (chunk "\"") (chunk "\"") (takeWhileP (Just "any character") $ not . (== '"'))

-- manyNonWS :: (Token s ~ Char, MonadParsec e s m) => m (Tokens s)
-- manyNonWS = takeWhileP (Just "Any Non-Whitespace") (not . isSpace)

someNonWS :: (Token s ~ Char, MonadParsec e s m) => m (Tokens s)
someNonWS = takeWhile1P (Just "any non-whitespace") (not . isSpace)
