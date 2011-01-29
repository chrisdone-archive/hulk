{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
module Hulk.Server2 where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Char
import qualified Data.Map             as M
import           Network.IRC
import           Prelude              hiding (log)

import           Hulk.Types2

-- Entry point handler

-- | Handle an incoming line, try to parse it and handle the message.
handleLine :: Monad m => String -> IRC m ()
handleLine line =
  case decode line of
    Just msg -> handleMsg line msg
    Nothing  -> errorReply $ "Unable to parse " ++ show line

-- | Handle an incoming message.
handleMsg :: Monad m => String -> Message -> IRC m ()
handleMsg line Message{..} =
  case (msg_command,msg_params) of
    ("PASS",[pass]) -> asUnregistered $ handlePass pass
    safeToLog -> do
      incoming line
      case safeToLog of
        ("USER",[user,_,_,realname]) -> 
            asUnregistered $ handleUser user realname
        ("NICK",[nick])   -> handleNick nick
        ("PING",[param])  -> handlePing param
        ("QUIT",[msg])    -> handleQuit msg
        ("TELL",[to,msg]) -> handleTell to msg
        mustBeReg'd -> do
          asRegistered $
           case mustBeReg'd of
             ("JOIN",(name:_))    -> handleJoin name
             ("PART",[chan,msg])  -> handlePart chan msg
             ("PRIVMSG",[to,msg]) -> handlePrivmsg to msg
             ("NOTICE",[to,msg])  -> handleNotice to msg
             _ -> errorReply $ "Invalid or unknown message type, or not" ++ 
                               " enough parameters: " ++ msg_command

-- Message handlers

-- | Handle the PASS message.
handlePass :: Monad m => String -> IRC m ()
handlePass pass = do
  modifyUnregistered $ \u -> u { unregUserPass = Just pass }
  notice "Received password."

-- | Handle the USER message.
handleUser :: Monad m => String -> String -> IRC m ()
handleUser user realname = do
  if validUser user
     then do modifyUnregistered $ \u -> u { unregUserUser = Just user
                                          , unregUserName = Just realname }
             notice "Recieved user details."
     else errorReply "Invalid user format."

-- | Handle the USER message.
handleNick :: Monad m => String -> IRC m ()
handleNick user = do
  modifyUnregistered

handlePing = undefined
handleQuit = undefined
handleTell = undefined
handleJoin = undefined
handlePart = undefined
handlePrivmsg = undefined
handleNotice = undefined

-- Client/user access functions

-- | Modify the current user if unregistered.
modifyUnregistered :: Monad m => (UnregUser -> UnregUser) -> IRC m ()
modifyUnregistered f = do
  modifyUser $ \user -> 
      case user of
        Unregistered user -> Unregistered (f user)
        u -> u

-- | Modify the current user.
modifyUser :: Monad m => (User -> User) -> IRC m ()
modifyUser f = do
  let modMe = undefined
  modify $ \env -> env { envClients = modMe (envClients env) }

-- | Only perform command if the client is registered.
asRegistered :: Monad m => IRC m () -> IRC m ()
asRegistered m = do
  registered <- isRegistered <$> getUser
  when registered m

-- | Only perform command if the client is registered.
asUnregistered :: Monad m => IRC m () -> IRC m ()
asUnregistered m = do
  registered <- isRegistered <$> getUser
  unless registered m

-- | Is a user registered?
isRegistered :: User -> Bool
isRegistered Registered{} = True
isRegistered _ = False

-- | Get the current client's user.
getUser :: Monad m => IRC m User
getUser = clientUser <$> getClient

-- | Get the current client.
getClient :: Monad m => IRC m Client
getClient = do
  ref <- asks connRef
  clients <- gets envClients
  case M.lookup ref clients of
    Just client -> return $ client
    Nothing -> makeNewClient
    
-- | Make a current client based on the current connection.
makeNewClient :: Monad m => IRC m Client
makeNewClient = do
  conn <- ask
  let client = Client { clientRef = connRef conn
                      , clientHostname = connHostname conn
                      , clientUser = newUnregisteredUser }
      addMe = M.insert (connRef conn) client
  modify $ \env -> env { envClients = addMe (envClients env) }
  return client

-- | Make a new unregistered user.
newUnregisteredUser :: User
newUnregisteredUser = Unregistered $ UnregUser {
   unregUserName = Nothing
  ,unregUserNick = Nothing
  ,unregUserUser = Nothing
  ,unregUserPass = Nothing
  }

-- Server replies

-- | Send a message reply.
notice :: Monad m => String -> IRC m ()
notice msg = do
  msg <- newServerMsg "NOTICE" [msg]
  tell . return . MessageReply $ msg

-- | Make a new IRC message from the server.
newServerMsg :: Monad m => String -> [String] -> IRC m Message
newServerMsg cmd ps = do
  hostname <- asks connServerName
  return $ Message {
    msg_prefix = Just $ Server hostname
   ,msg_command = cmd
   ,msg_params = ps
  }

-- Output functions

-- | Send an error reply.
errorReply :: Monad m => String -> IRC m ()
errorReply = tell . return . ErrorReply . Error

-- | Send a message reply.
reply :: Monad m => Message -> IRC m ()
reply = tell . return . MessageReply

-- | Log an incoming line.
incoming :: Monad m => String -> IRC m ()
incoming = log . (++"<- ")

-- | Log an outgoing line.
outgoing :: Monad m => String -> IRC m ()
outgoing = log . (++"-> ")

-- | Log a line.
log :: Monad m => String -> IRC m ()
log = tell . return . LogReply

-- Validation functions

-- | Is a username valid?
validUser :: String -> Bool
validUser = validNick

-- | Is a nickname valid? Digit/letter or one of these: -_/\\;()[]{}?`'
validNick :: String -> Bool
validNick = all ok where
  ok c = isDigit c || isLetter c || elem c "-_/\\;()[]{}?`'"
