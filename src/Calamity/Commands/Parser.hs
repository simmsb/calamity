-- | Something that can parse user input
module Calamity.Commands.Parser
    ( Parser(..)
    , KleeneConcat
    , runParserToCommandError ) where

import           Calamity.Cache.Eff
import           Calamity.Commands.Context
import           Calamity.Types.Model.Channel ( Channel )
import           Calamity.Types.Model.Guild   ( Emoji, Member, Role )
import           Calamity.Types.Model.User    ( User )
import           Calamity.Types.Snowflake

import           Control.Lens                 hiding ( Context )
import           Control.Monad
import           Control.Monad.Trans          ( lift )

import           Data.Char                    ( isSpace )
import           Data.Kind
import           Data.List.NonEmpty           ( NonEmpty, nonEmpty )
import           Data.Maybe                   ( fromJust )
import qualified Data.Text.Lazy               as L
import           Data.Text.Lazy               ( Text )
import           Data.Typeable

import qualified Polysemy                     as P

import           Text.Megaparsec              hiding ( parse )
import           Text.Megaparsec.Char

class Parser (a :: Type) r where
  type ParserResult a

  type ParserResult a = a

  parse :: Context -> ParsecT Text Text (P.Sem r) (ParserResult a)

instance Parser Text r where
  parse _ctx = item

instance Parser a r => Parser (Maybe a) r where
  type ParserResult (Maybe a) = Maybe (ParserResult a)

  parse ctx = optional . try $ parse @a ctx

instance Parser a r => Parser [a] r where
  type ParserResult [a] = [ParserResult a]

  parse ctx = many $ parse @a ctx

instance (Parser a r, Typeable a) => Parser (NonEmpty a) r where
  type ParserResult (NonEmpty a) = NonEmpty (ParserResult a)

  parse ctx = fromJust . nonEmpty <$> (some $ parse @a ctx)

data KleeneConcat a

instance (Monoid (ParserResult a), Parser a r) => Parser (KleeneConcat a) r where
  type ParserResult (KleeneConcat a) = ParserResult a

  parse ctx = mconcat <$> parse @[a] ctx

instance {-# OVERLAPS #-}Parser (KleeneConcat Text) r where
  type ParserResult (KleeneConcat Text) = ParserResult Text

  -- consume rest on text just takes everything remaining
  parse _ctx = someSingle

instance Parser (Snowflake a) r where
  parse _ctx = snowflake

instance {-# OVERLAPS #-}Parser (Snowflake User) r where
  parse _ctx = try (ping "@") <|> snowflake

instance {-# OVERLAPS #-}Parser (Snowflake Member) r where
  parse _ctx = try (ping "@") <|> snowflake

instance {-# OVERLAPS #-}Parser (Snowflake Channel) r where
  parse _ctx = try (ping "#") <|> snowflake

instance {-# OVERLAPS #-}Parser (Snowflake Role) r where
  parse _ctx = try (ping "@&") <|> snowflake

instance {-# OVERLAPS #-}Parser (Snowflake Emoji) r where
  parse _ctx = try emoji <|> snowflake

instance Parser Member r where
  parse ctx = do
    mid <- parse @(Snowflake Member) ctx
    case ctx ^? #guild . _Just . #members . ix mid of
      Just member -> pure member
      _           -> customFailure "Couldn't find member with this id"

instance P.Member CacheEff r => Parser User r where
  parse ctx = do
    uid <- parse @(Snowflake User) ctx
    user <- lift $ getUser uid
    case user of
      Just user -> pure user
      Nothing   -> customFailure "Couldn't find a user with this id"

instance (Parser a r, Parser b r) => Parser (a, b) r where
  type ParserResult (a, b) = (ParserResult a, ParserResult b)

  parse ctx = do
    space
    a <- parse @a ctx
    space1
    b <- parse @b ctx
    pure (a, b)

instance Parser () r where
  parse _ctx = space

instance ShowErrorComponent Text where
  showErrorComponent = L.unpack
  errorComponentLen = fromIntegral . L.length

runParserToCommandError :: Monad m => ParsecT Text Text m a -> Text -> m (Either Text a)
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
