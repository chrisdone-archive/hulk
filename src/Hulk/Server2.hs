{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
module Hulk.Server2 where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Char
import           Data.List.Split
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Maybe
import           Network.IRC          hiding (Channel)
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
handleNick nick = do
  modifyUnregistered $ \u -> u { unregUserNick = Just nick }
  thisClientReply "NICK" [nick]

-- | Handle the PING message.
handlePing :: Monad m => String -> IRC m ()
handlePing p = do
  hostname <- asks connServerName
  thisServerReply "PONG" [hostname,p]

handleQuit = undefined
handleTell = undefined

-- | Handle the JOIN message.
handleJoin :: Monad m => String -> IRC m ()
handleJoin chans = do
  ref <- asks connRef
  let names = splitWhen (==',') chans
  forM_ names $ flip withValidChanName $ \name -> do
      exists <- M.member name <$> gets envChannels
      unless exists $ insertChannel name
      joined <- inChannel name
      unless joined $ joinChannel name

-- | Handle the PART message.
handlePart :: Monad m => String -> String -> IRC m ()
handlePart name msg =
  withValidChanName name $ \name -> do
    ref <- asks connRef
    let remMe c = c { channelUsers = filter (==ref) (channelUsers c) }
    modifyChannels $ M.adjust remMe name
    channelReply name "PART" [name]

-- | Handle the PRIVMSG message.
handlePrivmsg :: Monad m => String -> String -> IRC m ()
handlePrivmsg name msg = sendMsgTo "PRIVMSG" name msg

handleNotice = undefined

-- Channel functions

-- | Join a channel.
-- Generic message functions

-- | Send a message to a user or a channel (it figures it out).
sendMsgTo :: Monad m => String -> String -> String -> IRC m ()
sendMsgTo typ name msg =
  if validChannel name
     then channelReply name typ [msg]
     else userReply name typ [msg]

joinChannel :: Monad m => String -> IRC m ()
joinChannel name = do
  ref <- asks connRef
  let addMe c = c { channelUsers = channelUsers c ++ [ref] }
  modifyChannels $ M.adjust addMe name
  channelReply name "JOIN" [name]
  withRegistered $ \RegUser{regUserNick=me} ->
    withChannel name $ \Channel{..} -> do
      clients <- catMaybes <$> mapM getClientByRef channelUsers
      let nicks = map regUserNick . catMaybes . map clientRegUser $ clients
      forM_ (splitEvery 10 nicks) $ \nicks ->
        thisServerReply "353" [me,"@",name,unwords nicks]
      thisServerReply "366" [me,name,"End of /NAMES list."]

-- | Am I in a channel?
inChannel :: Monad m => String -> IRC m Bool
inChannel name = do
  chan <- M.lookup name <$> gets envChannels
  case chan of
    Nothing -> return False
    Just Channel{..} -> (`elem` channelUsers) <$> asks connRef

-- | Insert a new channel.
insertChannel :: Monad m => String -> IRC m ()
insertChannel name = modifyChannels $ M.insert name newChan where
  newChan = Channel { channelName  = name
                    , channelTopic = Nothing
                    , channelUsers = []
                    }

-- | Modify the channel map.
modifyChannels :: Monad m => (Map String Channel -> Map String Channel) 
               -> IRC m ()
modifyChannels f = modify $ \e -> e { envChannels = f (envChannels e) }

-- | Perform an action with an existing channel, sends error if not exists.
withChannel :: Monad m => String -> (Channel -> IRC m ()) -> IRC m ()
withChannel name m = do
  chan <- M.lookup name <$> gets envChannels
  case chan of
    Nothing -> errorReply "Channel does not exist."
    Just chan -> m chan
    
withValidChanName :: Monad m => String -> (String -> IRC m ()) -> IRC m ()
withValidChanName name m 
    | validChannel name = m name
    | otherwise         = errorReply $ "Invalid channel name: " ++ name

-- Client/user access functions

-- | Send a client reply to a user.
userReply :: Monad m => String -> String -> [String] -> IRC m ()
userReply nick typ ps = do
  clients <- gets envClients
  client <- (M.lookup nick >=> (`M.lookup` clients)) <$> gets envNicks
  case client of
    Nothing -> errorReply "Unknown nick."
    Just Client{..} 
        | isRegistered clientUser -> clientReply clientRef typ ps
        | otherwise -> errorReply "Unknown user."

-- | Maybe get a registered user from a client.
clientRegUser :: Client -> Maybe RegUser
clientRegUser Client{..} = 
    case clientUser of
      Registered u -> Just u
      _ -> Nothing
  
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
  ref <- clientRef <$> getClient
  let modUser c = c { clientUser = f (clientUser c) }
      modClient = M.adjust modUser ref
  modify $ \env -> env { envClients = modClient (envClients env) }

-- | Only perform command if the client is registered.
asRegistered :: Monad m => IRC m () -> IRC m ()
asRegistered m = do
  registered <- isRegistered <$> getUser
  when registered m

-- | Perform command with a registered user.
withRegistered :: Monad m => (RegUser -> IRC m ()) -> IRC m ()
withRegistered m = do
  user <- getUser
  case user of
    Registered user -> m user
    _ -> return ()

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
getClientByRef :: Monad m => Ref -> IRC m (Maybe Client)
getClientByRef ref = do
  clients <- gets envClients
  return $ M.lookup ref clients

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

-- Channel replies

-- | Send a client reply to everyone in a channel.
channelReply :: Monad m => String -> String -> [String] -> IRC m ()
channelReply name cmd params = do
  withChannel name $ \Channel{..} -> do
    forM_ channelUsers $ \ref -> do
      clientReply ref cmd params

-- Client replies

-- | Send a client reply to the current client.
thisClientReply :: Monad m => String -> [String] -> IRC m ()
thisClientReply typ params = do
  ref <- asks connRef
  clientReply ref typ params

-- | Send a client reply of the given type with the given params, on
-- the given connection reference.
clientReply :: Monad m => Ref -> String -> [String] -> IRC m ()
clientReply ref typ params = do
  withRegistered $ \user -> do
    client <- getClient
    msg <- newClientMsg client user typ params
    tell . return . MessageReply ref $ msg

-- | Make a new IRC message from the current client.
newClientMsg :: Monad m => Client -> RegUser -> String -> [String] 
             -> IRC m Message
newClientMsg Client{..} RegUser{..} cmd ps = do
  let nickName = NickName regUserUser (Just regUserNick) (Just clientHostname)
  return $ Message {
    msg_prefix = Just $ nickName
   ,msg_command = cmd
   ,msg_params = ps
  }

-- Server replies

-- | Send a message reply.
notice :: Monad m => String -> IRC m ()
notice msg = thisServerReply "NOTICE" [msg]

-- | Send a server reply of the given type with the given params.
thisServerReply :: Monad m => String -> [String] -> IRC m ()
thisServerReply typ params = do
  ref <- asks connRef
  serverReply ref typ params

-- | Send a server reply of the given type with the given params.
serverReply :: Monad m => Ref -> String -> [String] -> IRC m ()
serverReply ref typ params = do
  msg <- newServerMsg typ params
  tell . return . MessageReply ref $ msg

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
reply :: Monad m => Ref -> Message -> IRC m ()
reply ref = tell . return . MessageReply ref

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

-- | Valid channel name?
validChannel :: String -> Bool
validChannel ('#':cs) = all ok cs where
  ok c = isDigit c || isLetter c || elem c "-_/\\;()[]{}?`'"
validChannel _ = False
