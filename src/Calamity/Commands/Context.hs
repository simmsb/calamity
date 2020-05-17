-- | Command invokation context
module Calamity.Commands.Context
    ( Context(..)
    , buildContext ) where

import           Calamity.Cache.Eff
import           Calamity.Client.Types
import           Calamity.Internal.Utils
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.Guild
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Control.Lens                 hiding ( Context )
import           Control.Monad

import           GHC.Generics

import qualified Polysemy                     as P

import           TextShow
import qualified TextShow.Generic             as TSG

data Context = Context
  { message :: Message
  , guild   :: Maybe Guild
  , member  :: Maybe Member
  , channel :: Channel
  , user    :: User
  }
  deriving ( Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric Context

buildContext :: BotC r => Message -> P.Sem r (Maybe Context)
buildContext msg = do
  guild <- join <$> getGuild `traverse` (msg ^. #guildID)
  let member = guild ^? _Just . #members . ix (coerceSnowflake $ getID @User msg)
  let gchan = guild ^? _Just . #channels . ix (coerceSnowflake $ getID @Channel msg)
  channel <- case gchan of
    Just chan -> pure . pure $ GuildChannel' chan
    _          -> DMChannel' <<$>> getDM (coerceSnowflake $ getID @Channel msg)
  user <- getUser $ getID msg

  pure $ Context msg guild member <$> channel <*> user
