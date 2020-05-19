-- | Command invokation context
module Calamity.Commands.Context
    ( Context(..)
    , buildContext ) where

import           Calamity.Cache.Eff
import           Calamity.Client.Types
import {-# SOURCE #-} Calamity.Commands.Command
import {-# SOURCE #-} Calamity.Commands.Handler
import           Calamity.Commands.ParsePrefix
import           Calamity.Internal.Utils
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.Guild
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Control.Lens                  hiding ( Context )
import           Control.Monad

import qualified Data.Text.Lazy                as L

import           GHC.Generics

import qualified Polysemy                      as P
import qualified Polysemy.Fail                 as P

import           TextShow
import qualified TextShow.Generic              as TSG

data Context = Context
  { message         :: Message
  , guild           :: Maybe Guild
  , member          :: Maybe Member
  , channel         :: Channel
  , user            :: User
  , command         :: Command
  , prefix          :: L.Text
  , unparsedMessage :: L.Text
    -- ^ The message remaining after consuming the prefix
  }
  deriving ( Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric Context

buildContext :: (BotC r, P.Member ParsePrefix r) => CommandHandler -> Message -> P.Sem r (Maybe Context)
buildContext handler msg = (rightToMaybe <$>) . P.runFail $ do
  Just (prefix, unparsedMessage) <- parsePrefix msg
  Just command <- pure $ findCommand handler unparsedMessage
  guild <- join <$> getGuild `traverse` (msg ^. #guildID)
  let member = guild ^? _Just . #members . ix (coerceSnowflake $ getID @User msg)
  let gchan = guild ^? _Just . #channels . ix (coerceSnowflake $ getID @Channel msg)
  Just channel <- case gchan of
    Just chan -> pure . pure $ GuildChannel' chan
    _         -> DMChannel' <<$>> getDM (coerceSnowflake $ getID @Channel msg)
  Just user <- getUser $ getID msg

  pure $ Context msg guild member channel user command prefix unparsedMessage

findCommand :: CommandHandler -> L.Text -> Maybe Command
findCommand handler unparsedMessage = Nothing -- TODO do this
