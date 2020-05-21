-- | Something that can parse user input
module Calamity.Commands.Parser
    ( Parser(..)
    , KleeneConcat ) where

import           Calamity.Cache.Eff
import           Calamity.Commands.Context
import           Calamity.Internal.Utils
import           Calamity.Types.Model.Channel ( Channel )
import           Calamity.Types.Model.Guild   ( Emoji, Member, Role )
import           Calamity.Types.Model.User    ( User )
import           Calamity.Types.Snowflake

import           Control.Lens                 hiding ( Context )
import           Control.Monad

import           Data.Bifunctor
import           Data.Char                    ( isSpace )
import           Data.Kind
import           Data.List.NonEmpty           ( NonEmpty, nonEmpty )
import qualified Data.Text.Lazy               as L
import           Data.Text.Lazy               ( Text )
import           Data.Typeable

import qualified Polysemy                     as P

import           Text.Megaparsec              hiding ( parse )
import           Text.Megaparsec.Char

import           TextShow

class Parser (a :: Type) r where
  type ParserResult a

  type ParserResult a = a

  parse :: (Context, Text) -> P.Sem r (Either Text (ParserResult a, Text))

instance Parser Text r where
  parse (_ctx, msg) = pure $ runParserToCommandError item msg

instance Parser a r => Parser [a] r where
  type ParserResult [a] = [ParserResult a]

  parse (ctx, msg) = Right <$> go msg []
    where
      go :: Text -> [ParserResult a] -> P.Sem r ([ParserResult a], Text)
      go t l = parse @a (ctx, t) >>= \case
        Left _        -> pure (l, t)
        Right (v, t') -> go t' (l <> [v])

instance (Parser a r, Typeable a) => Parser (NonEmpty a) r where
  type ParserResult (NonEmpty a) = NonEmpty (ParserResult a)

  parse (ctx, msg) = parse @[a] (ctx, msg)
    <&> (\case
           Right (res, rest) -> case nonEmpty res of
             Just res' -> Right (res', rest)
             Nothing   -> Left ("Couldn't parse at least one of " <> (L.pack . show . typeRep $ Proxy @a))
           Left e            -> Left e)

data KleeneConcat a

instance (Monoid (ParserResult a), Parser a r) => Parser (KleeneConcat a) r where
  type ParserResult (KleeneConcat a) = ParserResult a

  parse (ctx, msg) = (first mconcat) <<$>> parse @[a] (ctx, msg)

instance {-# OVERLAPS #-}Parser (KleeneConcat Text) r where
  type ParserResult (KleeneConcat Text) = ParserResult Text

  -- consume rest on text just takes everything remaining
  parse (_ctx, msg) = pure $ runParserToCommandError (someSingle) msg

instance Parser (Snowflake a) r where
  parse (_ctx, msg) = pure $ runParserToCommandError snowflake msg

instance {-# OVERLAPS #-}Parser (Snowflake User) r where
  parse (_ctx, msg) = pure $ runParserToCommandError (try (ping "@") <|> snowflake) msg

instance {-# OVERLAPS #-}Parser (Snowflake Member) r where
  parse (_ctx, msg) = pure $ runParserToCommandError (try (ping "@") <|> snowflake) msg

instance {-# OVERLAPS #-}Parser (Snowflake Channel) r where
  parse (_ctx, msg) = pure $ runParserToCommandError (try (ping "#") <|> snowflake) msg

instance {-# OVERLAPS #-}Parser (Snowflake Role) r where
  parse (_ctx, msg) = pure $ runParserToCommandError (try (ping "@&") <|> snowflake) msg

instance {-# OVERLAPS #-}Parser (Snowflake Emoji) r where
  parse (_ctx, msg) = pure $runParserToCommandError
    (try emoji <|> snowflake) msg


instance Parser Member r where
  parse (ctx, msg) = parse @(Snowflake Member) (ctx, msg)
    <&> (>>= \(mid, rest) -> case ctx ^? #guild . _Just . #members . ix mid of
           Just member -> Right (member, rest)
           _           -> Left ("Couldn't find member with id: " <> showtl mid))

instance P.Member CacheEff r => Parser User r where
  parse (ctx, msg) = do
    r <- parse @(Snowflake User) (ctx, msg)
    case r of
      Right (uid, rest) -> getUser uid <&> \case
        Just member -> Right (member, rest)
        _           -> Left ("Couldn't find user with id: " <> showtl uid)
      Left e            -> pure $ Left e

instance ShowErrorComponent Text where
  showErrorComponent = L.unpack
  errorComponentLen = fromIntegral . L.length

runParserToCommandError :: Parsec Text Text a -> Text -> Either Text (a, Text)
runParserToCommandError m t = case runParser (space *> andRemaining m) "" t of
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

andRemaining :: MonadParsec e s m => m a -> m (a, Tokens s)
andRemaining m = do
  a <- m
  rest <- manySingle
  pure (a, rest)

item :: MonadParsec e Text m => m Text
item = try quotedString <|> someNonWS

manySingle :: MonadParsec e s m => m (Tokens s)
manySingle = takeWhileP (Just "Any character") (const True)

someSingle :: MonadParsec e s m => m (Tokens s)
someSingle = takeWhile1P (Just "Any character") (const True)

quotedString :: MonadParsec e Text m => m Text
quotedString = try (between (chunk "'") (chunk "'") (takeWhileP Nothing $ not . (== '\''))) <|>
               between (chunk "\"") (chunk "\"") (takeWhileP Nothing $ not . (== '"'))

-- manyNonWS :: (Token s ~ Char, MonadParsec e s m) => m (Tokens s)
-- manyNonWS = takeWhileP (Just "Any Non-Whitespace") (not . isSpace)

someNonWS :: (Token s ~ Char, MonadParsec e s m) => m (Tokens s)
someNonWS = takeWhile1P (Just "Any Non-Whitespace") (not . isSpace)
