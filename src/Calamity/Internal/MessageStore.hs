-- | A thing for storing the last N messages
module Calamity.Internal.MessageStore
    ( MessageStore(..)
    , addMessage
    , getMessage
    , dropMessage ) where

import           Calamity.Internal.Utils
import           Calamity.Types.Model.Channel.Message
import           Calamity.Types.Snowflake

import           Control.Lens
import           Control.Monad.State.Lazy

import           Data.Default.Class
import           Data.Generics.Labels                 ()
import           Data.HashMap.Lazy                    ( HashMap )
import qualified Data.HashMap.Lazy                    as H

import qualified Deque.Lazy                           as DQ
import           Deque.Lazy                           ( Deque )

import           GHC.Generics

data MessageStore = MessageStore
  { messageQueue :: Deque (Snowflake Message)
  , messages     :: HashMap (Snowflake Message) Message
  , limit        :: Int
  , size         :: Int
  }
  deriving ( Show, Generic )

instance Default MessageStore where
  def = MessageStore mempty H.empty 1000 0

type instance (Index MessageStore) = Snowflake Message
type instance (IxValue MessageStore) = Message

instance Ixed MessageStore

instance At MessageStore where
  at k f m = f mv <&> \case
    Nothing -> maybe m (const (dropMessage k m)) mv
    Just v  -> addMessage v m
    where
      mv = getMessage k m

  {-# INLINE at #-}

addMessage :: Message -> MessageStore -> MessageStore
addMessage m = execState $ do
  unlessM (H.member (m ^. #id) <$> use #messages) do
    #messageQueue %= DQ.cons (getID m)
    #size += 1

  size <- use #size
  limit <- use #limit

  when (size > limit) do
    q <- use #messageQueue
    let Just (rid, q') = DQ.unsnoc q
    #messageQueue .= q'
    #messages %= sans rid
    #size -= 1

  #messages %= H.insert (getID m) m

{-# INLINE addMessage #-}

getMessage :: Snowflake Message -> MessageStore -> Maybe Message
getMessage id s = H.lookup id (s ^. #messages)

{-# INLINE getMessage #-}

dropMessage :: Snowflake Message -> MessageStore -> MessageStore
dropMessage id = execState $ do
  whenM (H.member id <$> use #messages) do
    #size -= 1

  #messageQueue %= DQ.filter (/= id)
  #messages %= H.delete id

{-# INLINE dropMessage #-}
