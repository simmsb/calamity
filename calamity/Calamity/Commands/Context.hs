{-# LANGUAGE TemplateHaskell #-}

-- | Command invokation context
module Calamity.Commands.Context (
  CalamityCommandContext (..),
  FullContext (..),
  useFullContext,
  LightContext (..),
  useLightContext,
) where

import Calamity.Cache.Eff
import Calamity.Commands.Types
import Calamity.Internal.Utils
import Calamity.Types.Model.Channel
import Calamity.Types.Model.Guild
import Calamity.Types.Model.User
import Calamity.Types.Snowflake
import Calamity.Types.Tellable
import CalamityCommands.Context qualified as CC
import Control.Applicative
import Control.Monad
import Data.Text qualified as T
import Optics
import Polysemy qualified as P
import Polysemy.Fail qualified as P
import TextShow qualified

class (CommandContext c) => CalamityCommandContext c where
  -- | The id of the channel that invoked this command
  ctxChannelID :: c -> Snowflake Channel

  -- | The id of the guild the command was invoked in, if in a guild
  ctxGuildID :: c -> Maybe (Snowflake Guild)

  -- | The id of the user that invoked this command
  ctxUserID :: c -> Snowflake User

  -- | The message that triggered this command
  ctxMessage :: c -> Message

-- | Invokation context for commands
data FullContext = FullContext
  { message :: Message
  -- ^ The message that the command was invoked from
  , guild :: Maybe Guild
  -- ^ If the command was sent in a guild, this will be present
  , member :: Maybe Member
  -- ^ The member that invoked the command, if in a guild
  --
  -- Note: If discord sent a member with the message, this is used; otherwise
  -- we try to fetch the member from the cache.
  , channel :: Channel
  -- ^ The channel the command was invoked from
  , user :: User
  -- ^ The user that invoked the command
  , command :: Command FullContext
  -- ^ The command that was invoked
  , prefix :: T.Text
  -- ^ The prefix that was used to invoke the command
  , unparsedParams :: T.Text
  -- ^ The message remaining after consuming the prefix
  }
  deriving (Show)
  deriving (TextShow.TextShow) via TextShow.FromStringShow FullContext
  deriving (HasID Channel) via HasIDField "channel" FullContext
  deriving (HasID Message) via HasIDField "message" FullContext
  deriving (HasID User) via HasIDField "user" FullContext

$(makeFieldLabelsNoPrefix ''FullContext)

instance CC.CommandContext IO FullContext () where
  ctxPrefix = (^. #prefix)
  ctxCommand = (^. #command)
  ctxUnparsedParams = (^. #unparsedParams)

instance CalamityCommandContext FullContext where
  ctxChannelID = getID . (^. #channel)
  ctxGuildID c = getID <$> c ^. #guild
  ctxUserID = getID . (^. #user)
  ctxMessage = (^. #message)

instance Tellable FullContext where
  getChannel = pure . ctxChannelID

useFullContext :: (P.Member CacheEff r) => P.Sem (CC.ConstructContext (Message, User, Maybe Member) FullContext IO () ': r) a -> P.Sem r a
useFullContext =
  P.interpret
    ( \case
        CC.ConstructContext (pre, cmd, up) (msg, usr, mem) -> buildContext msg usr mem pre cmd up
    )

buildContext :: (P.Member CacheEff r) => Message -> User -> Maybe Member -> T.Text -> Command FullContext -> T.Text -> P.Sem r (Maybe FullContext)
buildContext msg usr mem prefix command unparsed = (rightToMaybe <$>) . P.runFail $ do
  guild <- join <$> getGuild `traverse` (msg ^. #guildID)
  let member = mem <|> guild ^? _Just % #members % ix (coerceSnowflake $ getID @User msg)
  let gchan = guild ^? _Just % #channels % ix (coerceSnowflake $ getID @Channel msg)
  Just channel <- case gchan of
    Just chan -> pure . pure $ GuildChannel' chan
    Nothing -> DMChannel' <<$>> getDM (coerceSnowflake $ getID @Channel msg)

  pure $ FullContext msg guild member channel usr command prefix unparsed

-- | A lightweight context that doesn't need any cache information
data LightContext = LightContext
  { message :: Message
  -- ^ The message that the command was invoked from
  , guildID :: Maybe (Snowflake Guild)
  -- ^ If the command was sent in a guild, this will be present
  , channelID :: Snowflake Channel
  -- ^ The channel the command was invoked from
  , user :: User
  -- ^ The user that invoked the command
  , member :: Maybe Member
  -- ^ The member that triggered the command.
  --
  -- Note: Only sent if discord sent the member object with the message.
  , command :: Command LightContext
  -- ^ The command that was invoked
  , prefix :: T.Text
  -- ^ The prefix that was used to invoke the command
  , unparsedParams :: T.Text
  -- ^ The message remaining after consuming the prefix
  }
  deriving (Show)
  deriving (TextShow.TextShow) via TextShow.FromStringShow LightContext
  deriving (HasID Channel) via HasIDField "channelID" LightContext
  deriving (HasID Message) via HasIDField "message" LightContext
  deriving (HasID User) via HasIDField "user" LightContext

$(makeFieldLabelsNoPrefix ''LightContext)

instance CC.CommandContext IO LightContext () where
  ctxPrefix = (^. #prefix)
  ctxCommand = (^. #command)
  ctxUnparsedParams = (^. #unparsedParams)

instance CalamityCommandContext LightContext where
  ctxChannelID = (^. #channelID)
  ctxGuildID = (^. #guildID)
  ctxUserID = (^. #user % #id)
  ctxMessage = (^. #message)

instance Tellable LightContext where
  getChannel = pure . ctxChannelID

useLightContext :: P.Sem (CC.ConstructContext (Message, User, Maybe Member) LightContext IO () ': r) a -> P.Sem r a
useLightContext =
  P.interpret
    ( \case
        CC.ConstructContext (pre, cmd, up) (msg, usr, mem) ->
          pure . Just $ LightContext msg (msg ^. #guildID) (msg ^. #channelID) usr mem cmd pre up
    )
