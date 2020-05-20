-- | Something that can parse user input
module Calamity.Commands.Parser
    ( Parser(..)
    , KleeneConcat ) where

import           Calamity.Cache.Eff
import           Calamity.Commands.Context
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.Guild
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Control.Lens                 hiding ( Context )
import           Control.Monad

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

data KleeneConcat a

instance (Monoid (ParserResult a), Parser a r) => Parser (KleeneConcat a) r where
  type ParserResult (KleeneConcat a) = ParserResult a

  parse (ctx, msg) = Right <$> go msg mempty
    where
      go :: Text -> (ParserResult a) -> P.Sem r (ParserResult a, Text)
      go t v = parse @a (ctx, t) >>= \case
        Left _         -> pure (v, t)
        Right (v', t') -> go t' (v <> v')

instance {-# OVERLAPS #-}Parser (KleeneConcat Text) r where
  type ParserResult (KleeneConcat Text) = ParserResult Text

  -- consume rest on text just takes everything remaining
  parse (_ctx, msg) = pure $ runParserToCommandError (space *> manySingle) msg

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

instance Parser (Snowflake a) r where
  parse (_ctx, msg) = pure $ runParserToCommandError snowflake msg

instance {-# OVERLAPS #-}Parser (Snowflake User) r where
  parse (_ctx, msg) = pure $ runParserToCommandError (ping "@" <|> snowflake) msg

instance {-# OVERLAPS #-}Parser (Snowflake Member) r where
  parse (_ctx, msg) = pure $ runParserToCommandError (ping "@" <|> snowflake) msg

instance {-# OVERLAPS #-}Parser (Snowflake Channel) r where
  parse (_ctx, msg) = pure $ runParserToCommandError (ping "#" <|> snowflake) msg

instance {-# OVERLAPS #-}Parser (Snowflake Role) r where
  parse (_ctx, msg) = pure $ runParserToCommandError (ping "@&" <|> snowflake) msg

instance {-# OVERLAPS #-}Parser (Snowflake Emoji) r where
  parse (_ctx, msg) = pure $runParserToCommandError
    (ping' (optional (chunk "a") *> between (chunk ":") (chunk ":") (void manyNonWS)) <|> snowflake) msg

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

runParserToCommandError :: Parsec Text Text a -> Text -> Either Text (a, Text)
runParserToCommandError m t = case runParser (andRemaining m) "" t of
  Right a -> Right a
  Left s  -> Left . L.pack . show $ s

ping :: MonadParsec e Text m => Text -> m (Snowflake a)
ping c = chunk ("<" <> c) *> optional (chunk "!") *> snowflake <* chunk ">"

ping' :: MonadParsec e Text m => m () -> m (Snowflake a)
ping' m = chunk "<" *> m *> snowflake <* chunk ">"

snowflake :: MonadParsec e Text m => m (Snowflake a)
snowflake = (Snowflake . read) <$> some digitChar

andRemaining :: MonadParsec e s m => m a -> m (a, Tokens s)
andRemaining m = do
  a <- m
  rest <- manySingle
  pure (a, rest)

item :: MonadParsec e Text m => m Text
item = space *> (betweenQuotes manySingle <|> manyNonWS)

manySingle :: MonadParsec e s m => m (Tokens s)
manySingle = takeWhileP Nothing (const True)

betweenQuotes :: MonadParsec e Text m => m a -> m a
betweenQuotes m = between (chunk "'") (chunk "'") m <|> between (chunk "\"") (chunk "\"") m

manyNonWS :: (Token s ~ Char, MonadParsec e s m) => m (Tokens s)
manyNonWS = takeWhileP Nothing (not . isSpace)
