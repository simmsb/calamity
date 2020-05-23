-- | Something that can parse user input
module Calamity.Commands.Parser
    ( Parser(..)
    , SpannedError(..)
    , KleeneConcat
    , runParserToCommandError ) where

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
import           Data.List.NonEmpty            ( NonEmpty, nonEmpty )
import           Data.Maybe                    ( fromJust )
import qualified Data.Text.Lazy                as L
import           Data.Text.Lazy                ( Text )
import           Data.Typeable

import qualified Polysemy                      as P

import           Text.Megaparsec               hiding ( parse )
import           Text.Megaparsec.Char
import           Text.Megaparsec.Error.Builder ( errFancy, fancy )

data SpannedError = SpannedError Text !Int !Int
  deriving (Show, Eq, Ord)

class Typeable a => Parser (a :: Type) r where
  type ParserResult a

  type ParserResult a = a

  parse :: Context -> ParsecT SpannedError Text (P.Sem r) (ParserResult a)

instance Parser Text r where
  parse _ctx = label "Text" $ item

showTypeOf :: forall a. Typeable a => String
showTypeOf = show . typeRep $ Proxy @a

instance Parser a r => Parser (Maybe a) r where
  type ParserResult (Maybe a) = Maybe (ParserResult a)

  parse ctx = optional . try $ parse @a ctx

instance Parser a r => Parser [a] r where
  type ParserResult [a] = [ParserResult a]

  parse ctx = label (showTypeOf @[a]) $ many $ parse @a ctx

instance (Parser a r, Typeable a) => Parser (NonEmpty a) r where
  type ParserResult (NonEmpty a) = NonEmpty (ParserResult a)

  parse ctx = label (showTypeOf @(NonEmpty a)) $ fromJust . nonEmpty <$> (some $ parse @a ctx)

data KleeneConcat a

instance (Monoid (ParserResult a), Parser a r) => Parser (KleeneConcat a) r where
  type ParserResult (KleeneConcat a) = ParserResult a

  parse ctx = label (showTypeOf @(KleeneConcat a)) $ mconcat <$> parse @[a] ctx

instance {-# OVERLAPS #-}Parser (KleeneConcat Text) r where
  type ParserResult (KleeneConcat Text) = ParserResult Text

  -- consume rest on text just takes everything remaining
  parse _ctx = label "KleeneConcat Text" someSingle

instance Typeable (Snowflake a) => Parser (Snowflake a) r where
  parse _ctx = label (showTypeOf @(Snowflake a)) snowflake

instance {-# OVERLAPS #-}Parser (Snowflake User) r where
  parse _ctx = label (showTypeOf @(Snowflake User)) $ try (ping "@") <|> snowflake

instance {-# OVERLAPS #-}Parser (Snowflake Member) r where
  parse _ctx = label (showTypeOf @(Snowflake Member)) $ try (ping "@") <|> snowflake

instance {-# OVERLAPS #-}Parser (Snowflake Channel) r where
  parse _ctx = label (showTypeOf @(Snowflake Channel)) $ try (ping "#") <|> snowflake

instance {-# OVERLAPS #-}Parser (Snowflake Role) r where
  parse _ctx = label (showTypeOf @(Snowflake Role)) $ try (ping "@&") <|> snowflake

instance {-# OVERLAPS #-}Parser (Snowflake Emoji) r where
  parse _ctx = label (showTypeOf @(Snowflake Emoji)) $ try emoji <|> snowflake

mapParserMaybe :: Stream s => ParsecT SpannedError s m a -> Text -> (a -> Maybe b) -> ParsecT SpannedError s m b
mapParserMaybe m e f = do
  offs <- getOffset
  r <- f <$> m
  offe <- getOffset
  case r of
    Just r' -> pure r'
    _       -> parseError . errFancy offs . fancy . ErrorCustom $ SpannedError e offs offe

mapParserMaybeM :: (Monad m, Stream s) => ParsecT SpannedError s m a -> Text -> (a -> m (Maybe b)) -> ParsecT SpannedError s m b
mapParserMaybeM m e f = do
  offs <- getOffset
  r <- m >>= lift . f
  offe <- getOffset
  case r of
    Just r' -> pure r'
    _       -> parseError . errFancy offs . fancy . ErrorCustom $ SpannedError e offs offe

instance Parser Member r where
  parse ctx = mapParserMaybe (parse @(Snowflake Member) ctx)
              "Couldn't find a Member with this id"
              (\mid -> ctx ^? #guild . _Just . #members . ix mid)

instance P.Member CacheEff r => Parser User r where
  parse ctx = mapParserMaybeM (parse @(Snowflake User) ctx)
              "Couldn't find a User with this id"
              getUser

instance (Parser a r, Parser b r) => Parser (a, b) r where
  type ParserResult (a, b) = (ParserResult a, ParserResult b)

  parse ctx = do
    space
    a <- parse @a ctx
    space
    b <- parse @b ctx
    pure (a, b)

instance Parser () r where
  parse _ctx = space

instance ShowErrorComponent SpannedError where
  showErrorComponent (SpannedError t _ _) = L.unpack t
  errorComponentLen (SpannedError _ s e) = max 1 $ e - s

runParserToCommandError :: Monad m => ParsecT SpannedError Text m a -> Text -> m (Either Text a)
runParserToCommandError m t = runParserT (space *> m) "" t <&> \case
  Right a -> Right a
  Left s  -> Left . L.pack . errorBundlePretty $ s

ping :: MonadParsec e Text m => Text -> m (Snowflake a)
ping c = chunk ("<" <> c) *> optional (chunk "!") *> snowflake <* chunk ">"

ping' :: MonadParsec e Text m => m () -> m (Snowflake a)
ping' m = chunk "<" *> m *> snowflake <* chunk ">"

snowflake :: MonadParsec e Text m => m (Snowflake a)
snowflake = (Snowflake . read) <$> some digitChar

emoji :: MonadParsec e Text m => m (Snowflake a)
emoji = ping' (optional (chunk "a") *> between (chunk ":") (chunk ":") (void $ takeWhileP Nothing $ not . (== ':')))

-- andRemaining :: MonadParsec e s m => m a -> m (a, Tokens s)
-- andRemaining m = do
--   a <- m
--   rest <- manySingle
--   pure (a, rest)

item :: MonadParsec e Text m => m Text
item = try quotedString <|> someNonWS

-- manySingle :: MonadParsec e s m => m (Tokens s)
-- manySingle = takeWhileP (Just "Any character") (const True)

someSingle :: MonadParsec e s m => m (Tokens s)
someSingle = takeWhile1P (Just "Any character") (const True)

quotedString :: MonadParsec e Text m => m Text
quotedString = try (between (chunk "'") (chunk "'") (takeWhileP Nothing $ not . (== '\''))) <|>
               between (chunk "\"") (chunk "\"") (takeWhileP Nothing $ not . (== '"'))

-- manyNonWS :: (Token s ~ Char, MonadParsec e s m) => m (Tokens s)
-- manyNonWS = takeWhileP (Just "Any Non-Whitespace") (not . isSpace)

someNonWS :: (Token s ~ Char, MonadParsec e s m) => m (Tokens s)
someNonWS = takeWhile1P (Just "Any Non-Whitespace") (not . isSpace)
