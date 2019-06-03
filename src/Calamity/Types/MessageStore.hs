-- | A thing for storing the last N messages
module Calamity.Types.MessageStore
    ( MessageStore(..)
    , addMessage
    , getMessage
    , dropMessage ) where

import           Calamity.Types.General
import           Calamity.Types.Snowflake

import           Data.Default
import qualified Data.PQueue.Prio.Min     as PQ
import           Data.PQueue.Prio.Min     ( MinPQueue )

data MessageStore = MessageStore
  { messages :: MinPQueue (Snowflake Message) Message
  , limit    :: Int
  }
  deriving ( Show, Generic )

instance Default MessageStore where
  def = MessageStore PQ.empty 1000

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
addMessage m s@MessageStore { messages, limit } =
  s { messages = messages
        & PQ.filterWithKey (\k _ -> k /= getID m) -- don't have duplicate messages
        & PQ.insert (getID m) m
        & PQ.drop (min 0 (PQ.size messages - limit)) }

{-# INLINE addMessage #-}

getMessage :: Snowflake Message -> MessageStore -> Maybe Message
getMessage id MessageStore { messages } = messages
  & PQ.assocsU
  & find (\(k, _) -> k == id)
  & fmap snd

{-# INLINE getMessage #-}

dropMessage :: Snowflake Message -> MessageStore -> MessageStore
dropMessage id s@MessageStore { messages } = s { messages = PQ.filterWithKey (\k _ -> k /= id) messages }

{-# INLINE dropMessage #-}
