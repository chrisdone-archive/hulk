{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
module Hulk.Client 
    (handleLine)
    where

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

import           Hulk.Auth
import           Hulk.Event
import           Hulk.Types

-- Entry point handler

-- | Handle an incoming line, try to parse it and handle the message.
handleLine :: MonadProvider m => Env -> Conn -> String -> m ([Reply],Env)
handleLine env conn line = runClient env conn $
  case decode line of
    Just Message{..} -> handleMsg line (readEventType msg_command,msg_params)
    Nothing  -> errorReply $ "Unable to parse " ++ show line

-- | Run the client monad.    
runClient :: MonadProvider m => Env -> Conn -> IRC m () -> m ([Reply],Env)
runClient env conn m = do
  flip runStateT env $ 
    execWriterT $ 
      flip runReaderT conn $
        runIRC m

-- | Handle an incoming message.
handleMsg :: MonadProvider m => String -> (Event,[String]) -> IRC m ()
handleMsg line msg =
  case msg of
    (NOTHING,_) -> return ()
    (PASS,[pass]) -> asUnregistered $ handlePass pass
    safeToLog -> handleMsgSafeToLog line safeToLog
    
-- | Handle messages that are safe to log.
handleMsgSafeToLog :: MonadProvider m => String -> (Event,[String]) 
                   -> IRC m ()
handleMsgSafeToLog line safeToLog = do
  incoming line
  case safeToLog of
    (USER,[user,_,_,realname]) -> asUnregistered $ handleUser user realname
    (NICK,[nick])   -> handleNick nick
    (PING,[param])  -> handlePing param
    (QUIT,[msg])    -> handleQuit RequestedQuit msg
    (TELL,[to,msg]) -> handleTell to msg
    (DISCONNECT,[msg]) -> handleQuit SocketQuit msg
    mustBeReg'd -> handleMsgReg'd line mustBeReg'd

-- | Handle messages that can only be used when registered.
handleMsgReg'd :: Monad m => String -> (Event,[String]) -> IRC m ()
handleMsgReg'd _line mustBeReg'd =
  asRegistered $
   case mustBeReg'd of
     (JOIN,(name:_))    -> handleJoin name
     (PART,[chan,msg])  -> handlePart chan msg
     (PRIVMSG,[to,msg]) -> handlePrivmsg to msg
     (NOTICE,[to,msg])  -> handleNotice to msg
     _ -> errorReply $ "Invalid or unknown message type, or not" ++ 
                       " enough parameters: " ++ show (fst mustBeReg'd)

-- Message handlers

-- | Handle the PASS message.
handlePass :: MonadProvider m => String -> IRC m ()
handlePass pass = do
  modifyUnregistered $ \u -> u { unregUserPass = Just pass }
  notice "Received password."
  tryRegister

-- | Handle the USER message.
handleUser :: MonadProvider m => String -> String -> IRC m ()
handleUser user realname = do
  if validUser user
     then do modifyUnregistered $ \u -> u { unregUserUser = Just user
                                          , unregUserName = Just realname }
             notice "Recieved user details."
             tryRegister
     else errorReply "Invalid user format."

-- | Handle the USER message.
handleNick :: MonadProvider m => String -> IRC m ()
handleNick nick =
  withValidNick nick $ \nick ->
    ifUniqueNick nick $ do
      ref <- asks connRef
      modifyNicks $ M.insert nick ref
      modifyUnregistered $ \u -> u { unregUserNick = Just nick }
      tryRegister
      asRegistered $ do
        modifyRegistered $ \u -> u { regUserNick = nick }
        thisClientReply "NICK" [unNick nick]

-- | Handle the PING message.
handlePing :: Monad m => String -> IRC m ()
handlePing p = do
  hostname <- asks connServerName
  thisServerReply "PONG" [hostname,p]

-- | Handle the QUIT message.
handleQuit :: Monad m => QuitType -> String -> IRC m ()
handleQuit quitType msg = do
 (myChannels >>=) $ mapM_ $ \Channel{..} -> do
   channelReply channelName "QUIT" [msg] ExcludeMe
 withRegistered $ \RegUser{regUserNick=nick} -> do
   modifyNicks $ M.delete nick
 ref <- asks connRef
 modifyClients $ M.delete ref
 notice "Bye bye!"
 when (quitType == RequestedQuit) $ tell [Close]

-- | Handle the TELL message.
handleTell :: Monad m => String -> String -> IRC m ()
handleTell name msg = sendMsgTo "NOTICE" name msg

-- | Handle the JOIN message.
handleJoin :: Monad m => String -> IRC m ()
handleJoin chans = do
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
    channelReply name "PART" [msg] IncludeMe

-- | Handle the PRIVMSG message.
handlePrivmsg :: Monad m => String -> String -> IRC m ()
handlePrivmsg name msg = sendMsgTo "PRIVMSG" name msg

-- | Handle the NOTICE message.
handleNotice :: Monad m => String -> String -> IRC m ()
handleNotice name msg = sendMsgTo "NOTICE" name msg

-- Generic message functions

-- | Send a message to a user or a channel (it figures it out).
sendMsgTo :: Monad m => String -> String -> String -> IRC m ()
sendMsgTo typ name msg =
  if validChannel name
     then withValidChanName name $ \name -> 
            channelReply name typ [unChanName name,msg] ExcludeMe
     else userReply name typ [msg]

-- Channel functions

-- | Get channels that the current client is in.
myChannels :: Monad m => IRC m [Channel]
myChannels = do
  ref <- asks connRef
  filter (elem ref . channelUsers) . map snd . M.toList <$> gets envChannels

-- | Join a channel.
joinChannel :: Monad m => ChannelName -> IRC m ()
joinChannel name = do
  ref <- asks connRef
  let addMe c = c { channelUsers = channelUsers c ++ [ref] }
  modifyChannels $ M.adjust addMe name
  channelReply name "JOIN" [unChanName name] IncludeMe
  sendNamesList name

sendNamesList :: Monad m => ChannelName -> IRC m ()
sendNamesList name = do
  withRegistered $ \RegUser{regUserNick=me} ->
    withChannel name $ \Channel{..} -> do
      clients <- catMaybes <$> mapM getClientByRef channelUsers
      let nicks = map regUserNick . catMaybes . map clientRegUser $ clients
      forM_ (splitEvery 10 nicks) $ \nicks ->
        thisServerReply "353" [unNick me,"@",unChanName name
                              ,unwords $ map unNick nicks]
      thisServerReply "366" [unNick me,unChanName name,"End of /NAMES list."]

-- | Am I in a channel?
inChannel :: Monad m => ChannelName -> IRC m Bool
inChannel name = do
  chan <- M.lookup name <$> gets envChannels
  case chan of
    Nothing -> return False
    Just Channel{..} -> (`elem` channelUsers) <$> asks connRef

-- | Insert a new channel.
insertChannel :: Monad m => ChannelName -> IRC m ()
insertChannel name = modifyChannels $ M.insert name newChan where
  newChan = Channel { channelName  = name
                    , channelTopic = Nothing
                    , channelUsers = []
                    }

-- | Modify the channel map.
modifyChannels :: Monad m 
               => (Map ChannelName Channel -> Map ChannelName Channel) 
               -> IRC m ()
modifyChannels f = modify $ \e -> e { envChannels = f (envChannels e) }

-- | Perform an action with an existing channel, sends error if not exists.
withChannel :: Monad m => ChannelName -> (Channel -> IRC m ()) -> IRC m ()
withChannel name m = do
  chan <- M.lookup name <$> gets envChannels
  case chan of
    Nothing -> errorReply "Channel does not exist."
    Just chan -> m chan
    
withValidChanName :: Monad m => String -> (ChannelName -> IRC m ()) 
                  -> IRC m ()
withValidChanName name m 
    | validChannel name = m $ ChannelName name
    | otherwise         = errorReply $ "Invalid channel name: " ++ name

-- Client/user access functions

modifyNicks :: Monad m => (Map Nick Ref -> Map Nick Ref) -> IRC m ()
modifyNicks f = modify $ \env -> env { envNicks = f (envNicks env) }

-- | With a valid nickname, perform an action.
withValidNick :: Monad m => String -> (Nick -> IRC m ()) -> IRC m ()
withValidNick nick m
    | validNick nick = m (Nick nick)
    | otherwise      = errorReply $ "Invalid nick format: " ++ nick

-- | Perform an action if a nickname is unique, otherwise send error.
ifUniqueNick :: Monad m => Nick -> IRC m () -> IRC m ()
ifUniqueNick nick m = do
  clients <- gets envClients
  client <- (M.lookup nick >=> (`M.lookup` clients)) <$> gets envNicks
  case client of
    Nothing -> m
    Just{}  -> errorReply $ "That nick is already in use: " ++ unNick nick

-- | Try to register the user with the USER/NICK/PASS that have been given.
tryRegister :: MonadProvider m => IRC m ()
tryRegister =
  withUnegistered $ \UnregUser{..} -> do
    let details = (,,,) <$> unregUserName
                        <*> unregUserNick
                        <*> unregUserUser
                        <*> unregUserPass
    case details of
      Nothing -> return ()
      Just (name,nick,user,pass) -> do
        authentic <- lift $ authenticate user pass
        if not authentic
           then errorReply $ "Wrong user/pass."
           else do modifyUser $ \_ ->
                     Registered $ RegUser name nick user pass
                   sendWelcome
                   sendMotd

-- | Send the welcome message.
sendWelcome :: Monad m => IRC m ()
sendWelcome = do
  withRegistered $ \RegUser{..} -> do
    thisServerReply "001" [unNick regUserNick,"Welcome."]
    
sendMotd :: Monad m => IRC m ()
sendMotd = do
  withRegistered $ \RegUser{regUserNick=nick} -> do
    thisServerReply "375" [unNick nick,"MOTD"]
    -- TODO: MOTD.
    thisServerReply "376" [unNick nick,"/MOTD."]

-- | Send a client reply to a user.
userReply :: Monad m => String -> String -> [String] -> IRC m ()
userReply nick typ ps = 
  withValidNick nick $ \nick -> do
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

-- | Modify the current user if registered.
modifyRegistered :: Monad m => (RegUser -> RegUser) -> IRC m ()
modifyRegistered f = do
  modifyUser $ \user -> 
      case user of
        Registered user -> Registered (f user)
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

-- | Perform command with a registered user.
withUnegistered :: Monad m => (UnregUser -> IRC m ()) -> IRC m ()
withUnegistered m = do
  user <- getUser
  case user of
    Unregistered user -> m user
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
    
-- | Modify the clients table.
modifyClients :: Monad m => (Map Ref Client -> Map Ref Client) -> IRC m ()
modifyClients f = modify $ \env -> env { envClients = f (envClients env) }

-- | Make a current client based on the current connection.
makeNewClient :: Monad m => IRC m Client
makeNewClient = do
  conn <- ask
  let client = Client { clientRef = connRef conn
                      , clientHostname = connHostname conn
                      , clientUser = newUnregisteredUser }
  modifyClients $ M.insert (connRef conn) client
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
channelReply :: Monad m => ChannelName -> String -> [String] 
             -> ChannelReplyType
             -> IRC m ()
channelReply name cmd params typ = do
  withChannel name $ \Channel{..} -> do
    ref <- asks connRef
    forM_ channelUsers $ \theirRef -> do
      unless (typ == ExcludeMe && ref == theirRef) $ 
        clientReply theirRef cmd params

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
    reply ref msg

-- | Make a new IRC message from the current client.
newClientMsg :: Monad m => Client -> RegUser -> String -> [String] 
             -> IRC m Message
newClientMsg Client{..} RegUser{..} cmd ps = do
  let nickName = NickName (unNick regUserNick)
                          (Just regUserUser)
                          (Just clientHostname)
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
  reply ref msg

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
errorReply = notice . ("ERROR: " ++)

-- | Send a message reply.
reply :: Monad m => Ref -> Message -> IRC m ()
reply ref msg = do
  outgoing $ encode msg
  tell . return $ MessageReply ref msg

-- | Log an incoming line.
incoming :: Monad m => String -> IRC m ()
incoming = log . ("<- " ++)

-- | Log an outgoing line.
outgoing :: Monad m => String -> IRC m ()
outgoing = log . ("-> " ++)

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
