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
import qualified Calamity.Interactions as I
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
            --       x <- I.row $ do
            --         a <- I.button ButtonPrimary "a"
            --         b <- I.button ButtonPrimary "b"
            --         c <- I.button ButtonPrimary "c"
            --         pure (a, b, c)

            -- I.runView view (void . tell ctx) $ \s -> do
            --   void I.deferComponent
            --   P.embed $ print s
            let view options = do
                  ~(add, done) <- I.row $ do
                    add <- I.button ButtonPrimary "add"
                    done <- I.button ButtonPrimary "done"
                    pure (add, done)
                  s <- I.select options
                  pure (add, done, s)
            let initialState = MyViewState 1 Nothing
            s <- P.evalState initialState $ I.runView (view ["0"]) (tell ctx) $ \(add, done, s) -> do
              when add $ do
                n <- P.gets (^. #numOptions)
                let n' = n + 1
                P.modify' (#numOptions .~ n')
                let options = map (T.pack . show) [0..n]
                I.replaceView (view options) (void . I.edit)

              when done $ do
                finalSelected <- P.gets (^. #selected)
                I.endView finalSelected
                I.deleteInitialMsg
                void . I.respond $ case finalSelected of
                  Just x -> "Thanks: " <> x
                  Nothing -> "Oopsie"

              case s of
                Just s' -> do
                  P.modify' (#selected ?~ s')
                  void I.deferComponent
                Nothing -> pure ()
            P.embed $ print s

          command @'[] "cresponses" \ctx -> do
            let view = I.row $ do
                  a <- I.button ButtonPrimary "defer"
                  b <- I.button ButtonPrimary "deferEph"
                  c <- I.button ButtonPrimary "deferComp"
                  d <- I.button ButtonPrimary "modal"
                  pure (a, b, c, d)

                modalView = do
                  a <- I.textInput TextInputShort "a"
                  b <- I.textInput TextInputParagraph "b"
                  pure (a, b)

            I.runView view (tell ctx) $ \(a, b, c, d) -> do
              when a $ do
                void I.defer
                P.embed $ threadDelay 1000000
                void $ I.followUp @T.Text "lol"

              when b $ do
                void I.deferEphemeral
                P.embed $ threadDelay 1000000
                void $ I.followUpEphemeral @T.Text "lol"

              when c $ do
                void I.deferComponent
                P.embed $ threadDelay 1000000
                void $ I.followUp @T.Text "lol"

              when d $ do
                void . P.async $ do
                  I.runView modalView (void . I.pushModal "lol") $ \(a, b) -> do
                    P.embed $ print (a, b)
                    void $ I.respond ("Thanks: " <> a <> " " <> b)
                    I.endView ()

              pure ()

        -- react @'InteractionEvt \i -> do
        --   pPrint i
        --   let token = i ^. #token
        --   void . invoke $ CreateResponseDeferEdit i token
        --   P.embed $ threadDelay 1000000
        --   void . invoke $ CreateFollowupMessage i token (def & #content ?~ "Lol")
