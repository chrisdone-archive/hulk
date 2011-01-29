{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hulk.Types2 where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Map             (Map)
import Network.IRC          hiding (Channel)

data Ref = Ref deriving (Show,Ord,Eq)

data Error = Error String

data Env = Env {
   envClients :: Map Ref Client
  ,envNicks :: Map String Ref
  ,envChannels :: Map String Channel
}

data Channel = Channel {
      channelName :: String
    , channelTopic :: Maybe String
    , channelUsers :: [Ref]
} deriving Show

data User = Unregistered UnregUser | Registered RegUser
  deriving Show

data UnregUser = UnregUser {
   unregUserName :: Maybe String
  ,unregUserNick :: Maybe String
  ,unregUserUser :: Maybe String
  ,unregUserPass :: Maybe String
} deriving Show

data RegUser = RegUser {
   regUserName :: String
  ,regUserNick :: String
  ,regUserUser :: String
  ,regUserPass :: String
} deriving Show

data Client = Client {
      clientRef :: Ref
    , clientUser :: User
    , clientHostname :: String
    } deriving Show

data Conn = Conn {
   connRef :: Ref
  ,connHostname :: String
  ,connServerName :: String
} deriving Show

data Reply = ErrorReply Error | MessageReply Ref Message | LogReply String

newtype IRC m a = IRC { 
      runIRC :: ReaderT Conn (WriterT [Reply] (StateT Env m)) a
  }
  deriving (Monad
           ,Functor
           ,MonadWriter [Reply]
           ,MonadState Env
           ,MonadReader Conn)
