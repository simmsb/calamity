{-# LANGUAGE TemplateHaskell #-}

-- | Command context typeclass
module CalamityCommands.Context (
  CommandContext (..),
  ConstructContext (..),
  constructContext,
  BasicContext (..),
  useBasicContext,
) where

import CalamityCommands.Command
import Data.Text qualified as T
import Optics.TH
import Polysemy qualified as P

class CommandContext m c a | c -> m, c -> a where
  -- | The prefix that was used to invoke the command
  ctxPrefix :: c -> T.Text

  -- | The command that was invoked
  ctxCommand :: c -> Command m c a

  -- | The message remaining after consuming the prefix
  ctxUnparsedParams :: c -> T.Text

-- | An effect for constructing the context for a command
data ConstructContext msg ctx m' a' m a where
  -- | Construct a context for a command invokation, returning Just @context@ on
  -- success, or Nothing if a context could not be constructed
  ConstructContext ::
    -- | The (prefix, command, remaining)
    (T.Text, Command m' ctx a', T.Text) ->
    -- | The message type to extract the context from
    msg ->
    ConstructContext msg ctx m' a' m (Maybe ctx)

P.makeSem ''ConstructContext

-- | A basic context that only knows the prefix used and the unparsed input
data BasicContext m a = BasicContext
  { bcPrefix :: T.Text
  , bcCommand :: Command m (BasicContext m a) a
  , bcUnparsedParams :: T.Text
  }
  deriving (Show)

instance CommandContext m (BasicContext m a) a where
  ctxPrefix = bcPrefix
  ctxCommand = bcCommand
  ctxUnparsedParams = bcUnparsedParams

-- | A default interpretation for 'ConstructContext' that constructs a BasicContext
useBasicContext :: P.Sem (ConstructContext msg (BasicContext m a') m a' ': r) a -> P.Sem r a
useBasicContext =
  P.interpret
    ( \case
        ConstructContext (pre, cmd, up) _ -> pure . Just $ BasicContext pre cmd up
    )

$(makeFieldLabelsNoPrefix ''BasicContext)
