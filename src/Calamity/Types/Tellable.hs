-- | Things that are messageable
module Calamity.Types.Tellable
    ( ToMessage
    , Tellable(..)
    , TFile(..)
    , tell ) where

import           Calamity.Client.Types
import           Calamity.HTTP
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Control.Lens

import           Data.ByteString.Lazy         ( ByteString )
import           Data.Default.Class
import           Data.Monoid
import           Data.Text                    ( Text )

import           GHC.Generics

import qualified Polysemy                     as P
import qualified Polysemy.Error as P

-- | A wrapper type for sending files
newtype TFile = TFile ByteString
  deriving ( Show, Generic )

-- | Things that can be used to send a message
class ToMessage a where
  intoMsg :: a -> Endo CreateMessageOptions

instance ToMessage Text where
  intoMsg t = Endo (#content %~ (<> Just t))

instance ToMessage Embed where
  intoMsg e = Endo (#embed %~ (<> Just e))

instance ToMessage TFile where
  intoMsg (TFile f) = Endo (#file %~ getLast . (<> Last (Just f)) . Last)

instance ToMessage (Endo CreateMessageOptions) where
  intoMsg = Prelude.id

instance ToMessage (CreateMessageOptions -> CreateMessageOptions) where
  intoMsg = Endo

instance ToMessage CreateMessageOptions where
  intoMsg = Endo . const

class Tellable a where
  getChannel :: (BotC r, P.Member (P.Error RestError) r) => Snowflake a -> P.Sem r (Snowflake Channel)

runToMessage :: ToMessage a => a -> CreateMessageOptions
runToMessage = flip appEndo def . intoMsg

-- | Send a message to something that is messageable
tell :: forall r msg t. (BotC r, ToMessage msg, Tellable t, HasID' t) => t -> msg -> P.Sem r (Either RestError Message)
tell (getID @t -> tid) (runToMessage -> msg) = P.runError $ do
  cid <- getChannel tid
  r <- invokeRequest $ CreateMessage cid msg
  P.fromEither r

instance Tellable DMChannel where
  getChannel = pure . coerceSnowflake

instance Tellable TextChannel where
  getChannel = pure . coerceSnowflake

instance Tellable User where
  getChannel uid = do
    c <- invokeRequest $ CreateDM uid
    getID <$> P.fromEither c
