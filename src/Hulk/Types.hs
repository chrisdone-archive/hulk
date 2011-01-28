{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Hulk.Types where
    
import Data.Function
import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader
import Data.Map
import Network
import System.IO

instance Monad x => Applicative (ReaderT a x) where
  (<*>) = ap; pure = return

data Env = Env {
      envConfig :: Config
    , envTellHandle :: MVar Handle
    , envLogColumn :: Int
    , envListenSock :: Socket
    , envClients :: MVar (Map Ref Client)
    , envNicks :: MVar (Map String Ref)
    , envChannels :: MVar (Map String Channel)
    }

data Config = Config {
      configListen :: PortNumber
    , configHostname :: String
    , configMotd :: Maybe FilePath
    , configPreface :: Maybe FilePath
    , configPasswd :: FilePath
    , configPasswdKey :: FilePath
    } deriving (Show)
    
data Channel = Channel {
      channelName :: String
    , channelTopic :: String
    , channelUsers :: [Ref]
} deriving Show

data User = User {
      userUser :: String
    , userName :: String
    , userNick :: String
    , userPass :: Maybe String
    , userRegistered :: Bool
    } deriving (Show,Eq,Ord)

data Client = Client {
      clientUser :: MVar (Maybe User)
    , clientHandle :: MVar Ref
    , clientHostName :: String
    , clientPort :: PortNumber
    } deriving Show
    
instance Show (MVar Ref) where show _ = "MVar Ref"
instance Show (MVar (Maybe User)) where show _ = "MVar (Maybe User)"
instance Show (MVar (Map String Ref)) where show _ = "MVar (Map String Ref)"

newtype Ref = Ref { unRef :: Handle }
  deriving (Eq,Show)

instance Ord Ref where compare = on compare show

newtype Hulk a = Hulk { runHulk :: ReaderT Env IO a }
  deriving (Functor,Applicative,Monad,MonadReader Env,MonadIO)

newtype IRC a = IRC { runIRC :: ReaderT Client Hulk a }
  deriving (Functor,Applicative,Monad,MonadReader Client,MonadIO)

class (MonadIO m,Monad m) => Loggable m where
  tell :: String -> m ()

liftHulk :: Hulk a -> IRC a
liftHulk m = IRC $ ReaderT $ \_ -> m
