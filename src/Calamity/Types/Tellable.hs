-- | Things that are messageable
module Calamity.Types.Tellable
    ( ToMessage(..)
    , Tellable(..)
    , TFile(..)
    , tell ) where

import           Calamity.Client.Types
import           Calamity.HTTP
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.Guild
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Control.Lens

import           Data.ByteString.Lazy         ( ByteString )
import           Data.Default.Class
import           Data.Monoid
import           Data.Text                    ( Text )

import           GHC.Generics

import qualified Polysemy                     as P
import qualified Polysemy.Error               as P

-- | A wrapper type for sending files
newtype TFile = TFile ByteString
  deriving ( Show, Generic )

-- | Things that can be used to send a message
--
-- Can be used to compose text, embeds, and files. /e.g./
--
-- @
-- 'intoMsg' @'Text' "A message" '<>' 'intoMsg' @'Embed' ('def' '&' #description '?~' "Embed description")
-- @
class ToMessage a where
  -- | Turn @a@ into a 'CreateMessageOptions' builder
  intoMsg :: a -> Endo CreateMessageOptions

-- | Message content, '(<>)' concatenates the content
instance ToMessage Text where
  intoMsg t = Endo (#content %~ (<> Just t))

-- | Message embed, '(<>)' merges embeds using '(<>)'
instance ToMessage Embed where
  intoMsg e = Endo (#embed %~ (<> Just e))

-- | Message file, '(<>)' keeps the last added file
instance ToMessage TFile where
  intoMsg (TFile f) = Endo (#file %~ getLast . (<> Last (Just f)) . Last)

instance ToMessage (Endo CreateMessageOptions) where
  intoMsg = Prelude.id

instance ToMessage (CreateMessageOptions -> CreateMessageOptions) where
  intoMsg = Endo

instance ToMessage CreateMessageOptions where
  intoMsg = Endo . const

class Tellable a where
  getChannel :: (BotC r, P.Member (P.Error RestError) r) => a -> P.Sem r (Snowflake Channel)

runToMessage :: ToMessage a => a -> CreateMessageOptions
runToMessage = flip appEndo def . intoMsg

-- | Send a message to something that is messageable
--
-- To send a string literal you'll probably want to use @TypeApplication@ to
-- specify the type of @msg@
--
-- ==== Examples
--
-- Sending a string:
--
-- @
-- 'void' $ 'tell' @'Text' m ("Somebody told me to tell you about: " '<>' s)
-- @
tell :: forall msg r t. (BotC r, ToMessage msg, Tellable t) => t -> msg -> P.Sem r (Either RestError Message)
tell target (runToMessage -> msg) = P.runError $ do
  cid <- getChannel target
  r <- invoke $ CreateMessage cid msg
  P.fromEither r

instance Tellable DMChannel where
  getChannel = pure . getID

instance Tellable (Snowflake DMChannel) where
  getChannel = pure . coerceSnowflake

instance Tellable TextChannel where
  getChannel = pure . getID

instance Tellable (Snowflake TextChannel) where
  getChannel = pure . coerceSnowflake

instance Tellable Message where
  getChannel = pure . getID

messageUser :: (BotC r, P.Member (P.Error RestError) r, HasID User a) => a -> P.Sem r (Snowflake Channel)
messageUser (getID @User -> uid) = do
  c <- invoke $ CreateDM uid
  getID <$> P.fromEither c

instance Tellable (Snowflake Member) where
  getChannel = messageUser . coerceSnowflake @_ @User

instance Tellable Member where
  getChannel = messageUser

instance Tellable User where
  getChannel = messageUser

instance Tellable (Snowflake User) where
  getChannel = messageUser
