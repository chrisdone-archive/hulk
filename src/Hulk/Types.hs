{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Hulk.Types
  (Config (..)
  ,Nick (..) -- FIXME:
  ,nickText
  ,UserName (..) -- FIXME:
  ,userText
  ,ChannelName (..) -- FIXME:
  ,channelNameText
  ,Env (..)
  ,Channel (..)
  ,Client (..)
  ,User (..)
  ,UnregUser (..)
  ,RegUser (..)
  ,Ref (..)
  ,mkRef
  ,UserData (..)
  ,IRC (..)
  ,Conn (..)
  ,Reply (..)
  ,Event (..)
  ,RPL (..)
  ,QuitType (..)
  ,ChannelReplyType (..)
  ,MonadProvider (..)
  ,HulkIO (..)
  ,HulkP (..))
  where

import           Control.Applicative
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Aeson
import           Data.CaseInsensitive
import           Data.Char
import           Data.Function
import           Data.Map               (Map)
import           Data.Set               (Set)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time
import           GHC.Generics
import           Network
import           Network.FastIRC        (Message)
import           System.IO

--------------------------------------------------------------------------------
-- Configuration

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
  } deriving (Show)

--------------------------------------------------------------------------------
-- Fundamental IRC data types

-- | A case-insensitive nickname.
newtype Nick = NickName (CI Text)
  deriving (Show,Eq,Ord)

-- | Extract the text of a nickname for use in output.
nickText :: Nick -> Text
nickText (NickName ci) = original ci

-- | A case-insensitive username.
newtype UserName = UserName (CI Text)
  deriving (Show,Eq,Ord,Generic)

instance ToJSON UserName where
  toJSON (UserName ci) = toJSON (original ci)
instance FromJSON UserName where
  parseJSON = fmap (UserName . mk) . parseJSON

-- | Extract the text of a username for use in output.
userText :: UserName -> Text
userText (UserName ci) = original ci

-- | A case-insensitive channel name.
newtype ChannelName = ChannelName (CI Text)
  deriving (Show,Eq,Ord)

-- | Extract the text of a channelname for use in output.
channelNameText :: ChannelName -> Text
channelNameText (ChannelName ci) = original ci

--------------------------------------------------------------------------------
-- Server state types

-- | The server state.
data Env = Env
  { envClients  :: !(Map Ref Client)
  , envNicks    :: !(Map Nick Ref)
  , envChannels :: !(Map ChannelName Channel)
  } deriving (Show)

-- | A channel.
data Channel = Channel
  { channelName  :: !ChannelName
  , channelTopic :: !(Maybe Text)
  , channelUsers :: !(Set Ref)
  } deriving (Show)

--------------------------------------------------------------------------------
-- Client data types

-- | A connected client.
data Client = Client
  { clientRef      :: !Ref
  , clientUser     :: !User
  , clientHostname :: !Text
  , clientLastPong :: !UTCTime
  } deriving (Show)

-- | Some user, either unregistered or registered.
data User
  = Unregistered UnregUser
  | Registered RegUser
  deriving Show

-- | An unregistered user.
data UnregUser = UnregUser
  { unregUserName :: !(Maybe Text)
  , unregUserNick :: !(Maybe Nick)
  , unregUserUser :: !(Maybe UserName)
  , unregUserPass :: !(Maybe Text)
  } deriving (Show)

-- | A registered user.
data RegUser = RegUser
  { regUserName :: !Text
  , regUserNick :: !Nick
  , regUserUser :: !UserName
  , regUserPass :: !Text
  } deriving (Show)

-- | A reference for a client.
newtype Ref = Ref { unRef :: Handle }
  deriving (Show,Eq)

-- | Make a ref.
mkRef :: Handle -> Ref
mkRef = Ref

-- | Use for refs in maps.
instance Ord Ref where
  compare x y = if x == y then EQ else LT

-- | Data saved about a user for later actions like log recall.
data UserData = UserData
   { userDataUser     :: !UserName
   , userDataLastSeen :: !UTCTime
   } deriving (Show,Generic)

instance ToJSON UserData
instance FromJSON UserData

--------------------------------------------------------------------------------
-- Client handling types

-- | The client monad.
newtype IRC m a = IRC {
    runIRC :: ReaderT (UTCTime,Conn,Config) (WriterT [Reply] (StateT Env m)) a
  }
  deriving (Monad
           ,Functor
           ,MonadWriter [Reply]
           ,MonadState Env
           ,MonadReader (UTCTime,Conn,Config))

-- | Used when handling a line from a client.
data Conn = Conn
  { connRef        :: !Ref
  , connHostname   :: !Text
  , connServerName :: !Text
  , connTime       :: !UTCTime
  } deriving (Show)

-- | Replies are generated by the client after some messages.
data Reply
  = MessageReply !Ref !Message
  | LogReply !Text
  | Close
  | Bump !Ref
  deriving (Show)

-- | An incoming client message.
data Event
  = PASS
  | USER
  | NICK
  | PING
  | QUIT
  | TELL
  | JOIN
  | PART
  | PRIVMSG
  | NOTICE
  | ISON
  | WHOIS
  | TOPIC
  | CONNECT
  | DISCONNECT
  | PINGPONG
  | PONG
  | NAMES
  | NOTHING
  deriving (Show,Read)

-- | An outgoing server reply.
data RPL
  = RPL_WHOISUSER
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
  deriving (Show,Generic)

instance ToJSON RPL
instance FromJSON RPL

-- | When quitting it can either be due to user request, ping timeout,
-- or the socket was closed.
data QuitType
  = RequestedQuit
  | SocketQuit
  deriving (Show,Eq)

-- | When sending a channel reply, it can either include the current
-- client or exclude them (e.g. when the client sends a message, it's
-- no use echoing it back to that user).
data ChannelReplyType
  = IncludeMe
  | ExcludeMe
  deriving (Show,Eq)

--------------------------------------------------------------------------------
-- Providers

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
