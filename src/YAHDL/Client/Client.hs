-- |

module YAHDL.Client.Client
  ( runBotM
  )
where

import           Control.Monad.Log              ( Logger
                                                , runLogTSafe
                                                )
import           YAHDL.Client.Types

runBotM :: ClientState -> Logger Text -> BotM a -> IO a
runBotM cstate logEnv = (`runReaderT` cstate) . runLogTSafe logEnv . unBotM
