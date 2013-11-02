{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Hulk.Types where

import           Control.Applicative
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Aeson
import           Data.Char
import           Data.Function
import           Data.Map               (Map)
import           Data.Set               (Set)
import           Data.Text              (Text)
import qualified Data.Text as T
import           Data.Time
import           GHC.Generics
import           Network
import           Network.IRC            hiding (Channel)
import           System.IO

-- | Server configuration.
data Config = Config
  { configListen    :: !PortNumber
  , configHostname  :: !Text
  , configMotd      :: !(Maybe FilePath)
  , configPreface   :: !(Maybe FilePath)
  , configPasswd    :: !FilePath
  , configPasswdKey :: !FilePath
  , configUserData  :: !FilePath
  , configLogFile   :: !FilePath
  , configLogChans  :: ![Text]
  }
  deriving (Show)

newtype Ref = Ref
  { unRef :: Handle
  }
  deriving (Show,Eq)

instance Ord Ref where
  compare = on compare show

-- | Construct a Ref value.
newRef :: Handle -> Ref
newRef = Ref

data Error = Error Text

data Env = Env {
   envClients  :: !(Map Ref Client)
  ,envNicks    :: !(Map Nick Ref)
  ,envChannels :: !(Map ChannelName Channel)
}

data UserData = UserData {
   userDataUser     :: !Text
  ,userDataLastSeen :: !UTCTime
} deriving (Generic)
instance ToJSON UserData
instance FromJSON UserData

newtype Nick = Nick { unNick :: Text } deriving Show

instance Ord Nick where
  compare = on compare (T.toLower . unNick)

instance Eq Nick where
  (==) = on (==) (T.toLower . unNick)

newtype ChannelName = ChannelName { unChanName :: Text } deriving Show

instance Ord ChannelName where
  compare = on compare (T.toLower . unChanName)
instance Eq ChannelName where
  (==) = on (==) (T.toLower . unChanName)

data Channel = Channel {
      channelName  :: !ChannelName
    , channelTopic :: !(Maybe Text)
    , channelUsers :: !(Set Ref)
} deriving Show

data User = Unregistered UnregUser | Registered RegUser
  deriving Show

data UnregUser = UnregUser {
   unregUserName :: !(Maybe Text)
  ,unregUserNick :: !(Maybe Nick)
  ,unregUserUser :: !(Maybe Text)
  ,unregUserPass :: !(Maybe Text)
} deriving Show

data RegUser = RegUser {
   regUserName :: !Text
  ,regUserNick :: !Nick
  ,regUserUser :: !Text
  ,regUserPass :: !Text
} deriving Show

data Client = Client {
      clientRef      :: !Ref
    , clientUser     :: !User
    , clientHostname :: !Text
    , clientLastPong :: !UTCTime
    } deriving Show

data Conn = Conn {
   connRef        :: !Ref
  ,connHostname   :: !Text
  ,connServerName :: !Text
  ,connTime       :: !UTCTime
} deriving Show

data Reply = MessageReply !Ref !Message
           | LogReply !Text
           | Close
           | Bump Ref

newtype IRC m a = IRC {
    runIRC :: ReaderT (UTCTime,Conn,Config) (WriterT [Reply] (StateT Env m)) a
  }
  deriving (Monad
           ,Functor
           ,MonadWriter [Reply]
           ,MonadState Env
           ,MonadReader (UTCTime,Conn,Config))

data Event = PASS | USER | NICK | PING | QUIT | TELL | JOIN | PART | PRIVMSG
           | NOTICE | ISON | WHOIS | TOPIC | CONNECT | DISCONNECT | PINGPONG
           | PONG | NAMES
           | NOTHING
  deriving (Read,Show)


data RPL = RPL_WHOISUSER
         | RPL_NICK
         | RPL_PONG
         | RPL_JOIN
         | RPL_QUIT
         | RPL_NOTICE
         | RPL_PART
         | RPL_PRIVMSG
         | RPL_ISON
         | RPL_JOINS
         | RPL_TOPIC
         | RPL_NAMEREPLY
         | RPL_ENDOFNAMES
         | ERR_NICKNAMEINUSE
         | RPL_WELCOME
         | RPL_MOTDSTART
         | RPL_MOTD
         | RPL_ENDOFMOTD
         | RPL_WHOISIDLE
         | RPL_ENDOFWHOIS
         | RPL_WHOISCHANNELS
         | ERR_NOSUCHNICK
         | ERR_NOSUCHCHANNEL
         | RPL_PING
  deriving (Generic)

instance ToJSON RPL
instance FromJSON RPL

fromRPL :: RPL -> Text
fromRPL RPL_WHOISUSER     = "311"
fromRPL RPL_NICK          = "NICK"
fromRPL RPL_PONG          = "PONG"
fromRPL RPL_QUIT          = "QUIT"
fromRPL RPL_JOIN          = "JOIN"
fromRPL RPL_NOTICE        = "NOTICE"
fromRPL RPL_PART          = "PART"
fromRPL RPL_PRIVMSG       = "PRIVMSG"
fromRPL RPL_ISON          = "303"
fromRPL RPL_JOINS         = "JOIN"
fromRPL RPL_TOPIC         = "TOPIC"
fromRPL RPL_NAMEREPLY     = "353"
fromRPL RPL_ENDOFNAMES    = "366"
fromRPL RPL_WELCOME       = "001"
fromRPL RPL_MOTDSTART     = "375"
fromRPL RPL_MOTD          = "372"
fromRPL RPL_ENDOFMOTD     = "376"
fromRPL RPL_WHOISIDLE     = "317"
fromRPL RPL_WHOISCHANNELS = "319"
fromRPL RPL_ENDOFWHOIS    = "318"
fromRPL ERR_NICKNAMEINUSE = "433"
fromRPL ERR_NOSUCHNICK    = "401"
fromRPL ERR_NOSUCHCHANNEL = "403"
fromRPL RPL_PING          = "PING"

data QuitType = RequestedQuit | SocketQuit deriving Eq

data ChannelReplyType = IncludeMe | ExcludeMe deriving Eq

class Monad m => MonadProvider m where
  providePreface   :: m (Maybe Text)
  provideMotd      :: m (Maybe Text)
  provideKey       :: m Text
  providePasswords :: m Text
  provideWriteUser :: UserData -> m ()
  provideUser      :: Text -> m UserData
  provideLogger    :: Text -> RPL -> [Text] -> m ()
  provideLog       :: m [(UTCTime,Text,RPL,[Text])]

newtype HulkIO a = HulkIO { runHulkIO :: ReaderT Config IO a }
 deriving (Monad,MonadReader Config,Functor,MonadIO)

newtype HulkP a = HulkP { runHulkPure :: Identity a }
 deriving (Monad)

instance MonadTrans IRC where
  lift m = do
    s <- get
    IRC $ ReaderT $ \_ -> WriterT $ StateT $ \_ -> do
      a <- m
      return ((a,[]),s)
