{-# LANGUAGE ApplicativeDo #-}

module Main (main) where

import Control.Concurrent
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
import Data.Default.Class
import qualified Calamity.Interactions.View as V
import qualified Calamity.Interactions.Eff as I
import GHC.Generics
import qualified Polysemy.State as P

data MyViewState = MyViewState
  { numOptions :: Int
  , selected :: Maybe T.Text
  }
  deriving (Generic)

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
            -- let view = do
            --       x <- V.row $ do
            --         a <- V.button ButtonPrimary "a"
            --         b <- V.button ButtonPrimary "b"
            --         c <- V.button ButtonPrimary "c"
            --         pure (a, b, c)

            -- V.runView view (void . tell ctx) $ \s -> do
            --   void I.deferComponent
            --   P.embed $ print s
            let view options = do
                  ~(add, done) <- V.row $ do
                    add <- V.button ButtonPrimary "add"
                    done <- V.button ButtonPrimary "done"
                    pure (add, done)
                  s <- V.select options
                  pure (add, done, s)
            let initialState = MyViewState 1 Nothing
            s <- P.evalState initialState $ V.runView (view ["0"]) (void . tell ctx) $ \(add, done, s) -> do
              when add $ do
                n <- P.gets (^. #numOptions)
                let n' = n + 1
                P.modify' (#numOptions .~ n')
                let options = map (T.pack . show) [0..n]
                V.replaceView (view options) (void . I.edit)

              when done $ do
                finalSelected <- P.gets (^. #selected)
                V.endView finalSelected
                void I.deferComponent

              case s of
                Just s' -> do
                  P.modify' (#selected ?~ s')
                  void I.deferComponent
                Nothing -> pure ()
            P.embed $ print s
        -- react @ 'InteractionEvt \i -> do
        --   pPrint i
        --   let token = i ^. #token
        --   void . invoke $ CreateResponseDeferEdit i token
        --   P.embed $ threadDelay 1000000
        --   void . invoke $ CreateFollowupMessage i token (def & #content ?~ "Lol")
