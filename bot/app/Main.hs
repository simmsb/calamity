module Main (main) where

import Calamity
import Calamity.Cache.InMemory
import Calamity.Commands
import Calamity.Commands.Context (FullContext, useFullContext)
import Calamity.Metrics.Noop
import Control.Lens
import Control.Monad
import qualified Data.Text as T
import qualified Di
import qualified DiPolysemy as DiP
import qualified Polysemy as P
import qualified Polysemy.Async as P
import qualified Polysemy.AtomicState as P
import qualified Polysemy.Embed as P
import qualified Polysemy.Fail as P
import System.Environment (getEnv)
import Text.Pretty.Simple

main :: IO ()
main = do
  token <- T.pack <$> getEnv "BOT_TOKEN"
  Di.new $ \di ->
    void . P.runFinal . P.embedToFinal . DiP.runDiToIO di
      . runCacheInMemory
      . runMetricsNoop
      . useConstantPrefix "!"
      . useFullContext
      $ runBotIO (BotToken token) defaultIntents $ do
        addCommands $ do
          command @'[] "components" \ctx -> do
            void . tell ctx $
              ( intoMsg
                  [ button ButtonPrimary (CustomID "test")
                      & #label ?~ "test"
                  , button ButtonDanger (CustomID "test2")
                      & #label ?~ "test2"
                  ]
                  <> intoMsg
                    [select [sopt "a" "a", sopt "b" "b"] (CustomID "test3")]
              )
        react @ 'InteractionEvt \i -> do
          pPrint i
