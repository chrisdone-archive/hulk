{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Hulk.Types where

import Data.Typeable
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity
import Data.Char
import Data.Function
import Data.Map             (Map)
import Network
import Network.IRC          hiding (Channel)
import System.IO

data Config = Config {
      configListen :: PortNumber
    , configHostname :: String
    , configMotd :: Maybe FilePath
    , configPreface :: Maybe FilePath
    , configPasswd :: FilePath
    , configPasswdKey :: FilePath
    } deriving (Show)

newtype Ref = Ref { unRef :: Handle } 
    deriving (Show,Eq)

instance Ord Ref where
  compare = on compare show

-- | Construct a Ref value.
newRef :: Handle -> Ref
newRef = Ref

data Error = Error String

data Env = Env {
   envClients :: Map Ref Client
  ,envNicks :: Map Nick Ref
  ,envChannels :: Map ChannelName Channel
} deriving (Typeable)

newtype Nick = Nick { unNick :: String } deriving Show

instance Ord Nick where
  compare = on compare (map toLower . unNick)
instance Eq Nick where
  (==) = on (==) (map toLower . unNick)

newtype ChannelName = ChannelName { unChanName :: String } deriving Show
  
instance Ord ChannelName where
  compare = on compare (map toLower . unChanName)
instance Eq ChannelName where
  (==) = on (==) (map toLower . unChanName)

data Channel = Channel {
      channelName :: ChannelName
    , channelTopic :: Maybe String
    , channelUsers :: [Ref]
} deriving Show

data User = Unregistered UnregUser | Registered RegUser
  deriving Show

data UnregUser = UnregUser {
   unregUserName :: Maybe String
  ,unregUserNick :: Maybe Nick
  ,unregUserUser :: Maybe String
  ,unregUserPass :: Maybe String
} deriving Show

data RegUser = RegUser {
   regUserName :: String
  ,regUserNick :: Nick
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
} deriving (Typeable,Show)

data Reply = MessageReply Ref Message | LogReply String | Close
  deriving Typeable

newtype IRC m a = IRC { 
    runIRC :: ReaderT Conn (WriterT [Reply] (StateT Env m)) a
  }
  deriving (Monad
           ,Functor
           ,MonadWriter [Reply]
           ,MonadState Env
           ,MonadReader Conn)

data Event = PASS | USER | NICK | PING | QUIT | TELL | JOIN | PART | PRIVMSG
           | NOTICE | CONNECT | DISCONNECT | NOTHING
  deriving (Read,Show)

data QuitType = RequestedQuit | SocketQuit deriving Eq

data ChannelReplyType = IncludeMe | ExcludeMe deriving Eq

class Monad m => MonadProvider m where
  providePreface   :: m (Maybe String)
  provideMotd      :: m (Maybe String)
  provideKey       :: m String
  providePasswords :: m String

newtype HulkIO a = HulkIO { runHulkIO :: ReaderT Config IO a }
 deriving (Monad,MonadReader Config,Functor,MonadIO,Typeable)

newtype HulkP a = HulkP { runHulkPure :: Identity a }
 deriving (Monad)

instance MonadTrans IRC where
  lift m = do
    s <- get
    IRC $ ReaderT $ \_ -> WriterT $ StateT $ \_ -> do
      a <- m
      return ((a,[]),s)

type Handler = Env -> Conn -> String -> HulkIO ([Reply],Env)
