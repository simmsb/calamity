-- | Something that can parse user input
module CalamityCommands.Parser (
  ParameterParser (..),
  Named,
  KleeneStarConcat,
  KleenePlusConcat,
  ParserEffs,
  runCommandParser,
  ParserState (..),
  parseMP,
) where

import Control.Lens hiding (Context)
import Control.Monad

import Data.Char (isSpace)
import Data.Generics.Labels ()
import Data.Kind
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import Data.Typeable

import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import qualified Polysemy as P
import qualified Polysemy.Error as P
import qualified Polysemy.Reader as P
import qualified Polysemy.State as P

import Numeric.Natural (Natural)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal, float, signed)

data SpannedError = SpannedError L.Text !Int !Int
  deriving (Show, Eq, Ord)

showTypeOf :: forall a. Typeable a => String
showTypeOf = show . typeRep $ Proxy @a

{- | The current state of the parser, used so that the entire remaining input is
 available.

 This is used instead of just concatenating parsers to allow for more
 flexibility, for example, this could be used to construct flag-style parsers
 that parse a parameter from anywhere in the input message.
-}
data ParserState = ParserState
  { -- | The current offset, or where the next parser should start parsing at
    off :: Int
  , -- | The input message ot parse
    msg :: L.Text
  }
  deriving (Show, Generic)

type ParserEffs c r =
  ( P.State ParserState
      ': P.Error (S.Text, L.Text) -- the current parser state
        ': P.Reader c -- (failing parser name, error reason)
          ': r -- context
  )
type ParserCtxE c r = P.Reader c ': r

-- | Run a command parser, @ctx@ is the context, @t@ is the text input
runCommandParser :: c -> L.Text -> P.Sem (ParserEffs c r) a -> P.Sem r (Either (S.Text, L.Text) a)
runCommandParser ctx t = P.runReader ctx . P.runError . P.evalState (ParserState 0 t)

-- | A typeclass for things that can be parsed as parameters to commands.
--
-- Any type that is an instance of ParamerParser can be used in the type level
-- parameter @ps@ of 'CalamityCommands.Dsl.command',
-- 'CalamityCommands.CommandUtils.buildCommand', etc.
class Typeable a => ParameterParser (a :: Type) r where
  type ParserResult a

  type ParserResult a = a

  parserName :: S.Text
  default parserName :: S.Text
  parserName = ":" <> S.pack (showTypeOf @a)

  parse :: P.Sem (ParserEffs c r) (ParserResult a)

{- | A named parameter, used to attach the name @s@ to a type in the command's
 help output
-}
data Named (s :: Symbol) (a :: Type)

instance (KnownSymbol s, ParameterParser a r) => ParameterParser (Named s a) r where
  type ParserResult (Named s a) = ParserResult a

  parserName = (S.pack . symbolVal $ Proxy @s) <> parserName @a @r

  parse = mapE (_1 .~ parserName @(Named s a) @r) $ parse @a @r

mapE :: P.Member (P.Error e) r => (e -> e) -> P.Sem r a -> P.Sem r a
mapE f m = P.catch m (P.throw . f)

-- | Parse a paremeter using a MegaParsec parser.
--
-- On failure this constructs a nice-looking megaparsec error for the failed parameter.
parseMP :: S.Text -> ParsecT SpannedError L.Text (P.Sem (ParserCtxE c r)) a -> P.Sem (ParserEffs c r) a
parseMP n m = do
  s <- P.get
  res <- P.raise . P.raise $ runParserT (skipN (s ^. #off) *> trackOffsets (space *> m)) "" (s ^. #msg)
  case res of
    Right (a, offset) -> do
      P.modify (#off +~ offset)
      pure a
    Left s -> P.throw (n, L.pack $ errorBundlePretty s)

instance ParameterParser L.Text r where
  parse = parseMP (parserName @L.Text) item

instance ParameterParser S.Text r where
  parse = parseMP (parserName @S.Text) (L.toStrict <$> item)

instance ParameterParser Integer r where
  parse = parseMP (parserName @Integer) (signed mempty decimal)

instance ParameterParser Natural r where
  parse = parseMP (parserName @Natural) decimal

instance ParameterParser Int r where
  parse = parseMP (parserName @Int) (signed mempty decimal)

instance ParameterParser Word r where
  parse = parseMP (parserName @Word) decimal

instance ParameterParser Float r where
  parse = parseMP (parserName @Float) (signed mempty (try float <|> decimal))

instance ParameterParser a r => ParameterParser (Maybe a) r where
  type ParserResult (Maybe a) = Maybe (ParserResult a)

  parse = P.catch (Just <$> parse @a) (const $ pure Nothing)

instance (ParameterParser a r, ParameterParser b r) => ParameterParser (Either a b) r where
  type ParserResult (Either a b) = Either (ParserResult a) (ParserResult b)

  parse = do
    l <- parse @(Maybe a) @r
    case l of
      Just l' -> pure (Left l')
      Nothing ->
        Right <$> parse @b @r

instance ParameterParser a r => ParameterParser [a] r where
  type ParserResult [a] = [ParserResult a]

  parse = go []
   where
    go :: [ParserResult a] -> P.Sem (ParserEffs c r) [ParserResult a]
    go l =
      P.catch (Just <$> parse @a) (const $ pure Nothing) >>= \case
        Just a -> go $ l <> [a]
        Nothing -> pure l

instance (ParameterParser a r, Typeable a) => ParameterParser (NonEmpty a) r where
  type ParserResult (NonEmpty a) = NonEmpty (ParserResult a)

  parse = do
    a <- parse @a
    as <- parse @[a]
    pure $ a :| as

{- | A parser that consumes zero or more of @a@ then concatenates them together.

 @'KleeneStarConcat' 'L.Text'@ therefore consumes all remaining input.
-}
data KleeneStarConcat (a :: Type)

instance (Monoid (ParserResult a), ParameterParser a r) => ParameterParser (KleeneStarConcat a) r where
  type ParserResult (KleeneStarConcat a) = ParserResult a

  parse = mconcat <$> parse @[a]

instance {-# OVERLAPS #-} ParameterParser (KleeneStarConcat L.Text) r where
  type ParserResult (KleeneStarConcat L.Text) = ParserResult L.Text

  -- consume rest on text just takes everything remaining
  parse = parseMP (parserName @(KleeneStarConcat L.Text)) manySingle

instance {-# OVERLAPS #-} ParameterParser (KleeneStarConcat S.Text) r where
  type ParserResult (KleeneStarConcat S.Text) = ParserResult S.Text

  -- consume rest on text just takes everything remaining
  parse = parseMP (parserName @(KleeneStarConcat S.Text)) (L.toStrict <$> manySingle)

{- | A parser that consumes one or more of @a@ then concatenates them together.

 @'KleenePlusConcat' 'L.Text'@ therefore consumes all remaining input.
-}
data KleenePlusConcat (a :: Type)

instance (Semigroup (ParserResult a), ParameterParser a r) => ParameterParser (KleenePlusConcat a) r where
  type ParserResult (KleenePlusConcat a) = ParserResult a

  parse = sconcat <$> parse @(NonEmpty a)

instance {-# OVERLAPS #-} ParameterParser (KleenePlusConcat L.Text) r where
  type ParserResult (KleenePlusConcat L.Text) = ParserResult L.Text

  -- consume rest on text just takes everything remaining
  parse = parseMP (parserName @(KleenePlusConcat L.Text)) someSingle

instance {-# OVERLAPS #-} ParameterParser (KleenePlusConcat S.Text) r where
  type ParserResult (KleenePlusConcat S.Text) = ParserResult S.Text

  -- consume rest on text just takes everything remaining
  parse = parseMP (parserName @(KleenePlusConcat S.Text)) (L.toStrict <$> someSingle)

instance (ParameterParser a r, ParameterParser b r) => ParameterParser (a, b) r where
  type ParserResult (a, b) = (ParserResult a, ParserResult b)

  parse = do
    a <- parse @a
    b <- parse @b
    pure (a, b)

instance ParameterParser () r where
  parse = parseMP (parserName @()) space

instance ShowErrorComponent SpannedError where
  showErrorComponent (SpannedError t _ _) = L.unpack t
  errorComponentLen (SpannedError _ s e) = max 1 $ e - s

skipN :: (Stream s, Ord e) => Int -> ParsecT e s m ()
skipN n = void $ takeP Nothing n

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
quotedString =
  try (between (chunk "'") (chunk "'") (takeWhileP (Just "any character") (/= '\'')))
    <|> between (chunk "\"") (chunk "\"") (takeWhileP (Just "any character") (/= '"'))

someNonWS :: (Token s ~ Char, MonadParsec e s m) => m (Tokens s)
someNonWS = takeWhile1P (Just "any non-whitespace") (not . isSpace)
