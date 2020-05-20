-- | Something that can parse user input
module Calamity.Commands.Parser
    ( Parser(..)
    , KleeneConcat ) where

import           Calamity.Commands.Context

import           Data.Char                 ( isSpace )
import           Data.Kind
import qualified Data.Text.Lazy            as L
import           Data.Text.Lazy            ( Text )

import           Text.Megaparsec           hiding ( parse )

class Parser (a :: Type) where
  type ParserResult a

  type ParserResult a = a

  parse :: (Context, Text) -> Either Text (ParserResult a, Text)

instance Parser Text where
  parse (_ctx, msg) = runParserToCommandError item msg

data KleeneConcat a

instance (Monoid (ParserResult a), Parser a) => Parser (KleeneConcat a) where
  type ParserResult (KleeneConcat a) = ParserResult a

  parse (ctx, msg) = Right $ go msg mempty
    where
      go :: Text -> (ParserResult a) -> (ParserResult a, Text)
      go t v = case parse @a (ctx, t) of
        Left _         -> (v, t)
        Right (v', t') -> go t' (v <> v')

runParserToCommandError :: Parsec Text Text a -> Text -> Either Text (a, Text)
runParserToCommandError m t = case runParser (andRemaining m) "" t of
  Right a -> Right a
  Left s  -> Left . L.pack . show $ s

andRemaining :: MonadParsec e s m => m a -> m (a, Tokens s)
andRemaining m = do
  a <- m
  rest <- manySingle
  pure (a, rest)

item :: MonadParsec e Text m => m Text
item = manyWS *> (betweenQuotes manySingle <|> manyNonWS)

manySingle :: MonadParsec e s m => m (Tokens s)
manySingle = takeWhileP Nothing (const True)

betweenQuotes :: MonadParsec e Text m => m a -> m a
betweenQuotes m = between (chunk "'") (chunk "'") m <|> between (chunk "\"") (chunk "\"") m

manyWS :: (Token s ~ Char, MonadParsec e s m) => m (Tokens s)
manyWS = takeWhileP Nothing isSpace

manyNonWS :: (Token s ~ Char, MonadParsec e s m) => m (Tokens s)
manyNonWS = takeWhileP Nothing (not . isSpace)
