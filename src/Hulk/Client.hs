{-# LANGUAGE RecordWildCards, ViewPatterns #-}
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
    (NOTHING,_) -> invalidMessage line
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
    (CONNECT,_)        -> handleConnect
    mustBeReg'd -> handleMsgReg'd line mustBeReg'd

-- | Handle messages that can only be used when registered.
handleMsgReg'd :: Monad m => String -> (Event,[String]) -> IRC m ()
handleMsgReg'd line mustBeReg'd =
  asRegistered $
   case mustBeReg'd of
     (JOIN,(name:_))    -> handleJoin name
     (PART,[chan,msg])  -> handlePart chan msg
     (PRIVMSG,[to,msg]) -> handlePrivmsg to msg
     (TOPIC,[chan,topic]) -> handleTopic chan topic
     (NOTICE,[to,msg])  -> handleNotice to msg
     (WHOIS,(nick:_))   -> handleWhoIs nick
     (ISON,people)      -> handleIsOn people
     _                  -> invalidMessage line

-- | Log an invalid message.
invalidMessage :: Monad m => String -> IRC m ()
invalidMessage line = do
    incoming line
    errorReply $ "Invalid or unknown message type, or not" ++ 
                 " enough parameters: " ++ show line

-- Message handlers

-- | Handle the CONNECT event.
handleConnect :: MonadProvider m => IRC m ()
handleConnect = do
  motd <- fmap lines <$> lift providePreface
  case motd of
    Nothing -> return ()
    Just lines -> mapM_ notice lines

-- | Handle the PASS message.
handlePass :: MonadProvider m => String -> IRC m ()
handlePass pass = do
  modifyUnregistered $ \u -> u { unregUserPass = Just pass }
  notice "Received password."
  tryRegister

-- | Handle the USER message.
handleUser :: MonadProvider m => String -> String -> IRC m ()
handleUser user realname = do
  withSentPass $
    if validUser user
       then do modifyUnregistered $ \u -> u { unregUserUser = Just user
                                            , unregUserName = Just realname }
               notice "Recieved user details."
               tryRegister
       else errorReply "Invalid user format."

-- | Handle the USER message.
handleNick :: MonadProvider m => String -> IRC m ()
handleNick nick =
  withSentPass $
    withValidNick nick $ \nick ->
      ifUniqueNick nick $ do
        ref <- asks connRef
        modifyNicks $ M.insert nick ref
        modifyUnregistered $ \u -> u { unregUserNick = Just nick }
        tryRegister
        asRegistered $ do
          thisClientReply RPL_NICK [unNick nick]
          modifyRegistered $ \u -> u { regUserNick = nick }

-- | Handle the PING message.
handlePing :: Monad m => String -> IRC m ()
handlePing p = do
  hostname <- asks connServerName
  thisServerReply RPL_PONG [hostname,p]

-- | Handle the QUIT message.
handleQuit :: Monad m => QuitType -> String -> IRC m ()
handleQuit quitType msg = do
 (myChannels >>=) $ mapM_ $ \Channel{..} -> do
   channelReply channelName RPL_QUIT [msg] ExcludeMe
   removeFromChan channelName
 withRegistered $ \RegUser{regUserNick=nick} -> do
   modifyNicks $ M.delete nick
 ref <- asks connRef
 modifyClients $ M.delete ref
 notice "Bye bye!"
 when (quitType == RequestedQuit) $ tell [Close]

-- | Handle the TELL message.
handleTell :: Monad m => String -> String -> IRC m ()
handleTell name msg = sendMsgTo RPL_NOTICE name msg

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
    removeFromChan name
    channelReply name RPL_PART [msg] IncludeMe

-- | Remove a user from a channel.
removeFromChan :: Monad m => ChannelName -> IRC m ()
removeFromChan name = do
  ref <- asks connRef
  let remMe c = c { channelUsers = filter (/=ref) (channelUsers c) }
  modifyChannels $ M.adjust remMe name

-- | Handle the TOPIC message.
handleTopic :: Monad m => String -> String -> IRC m ()
handleTopic name topic =
  withValidChanName name $ \name -> do
    let setTopic c = c { channelTopic = Just topic }
    modifyChannels $ M.adjust setTopic name
    channelReply name RPL_TOPIC [unChanName name,topic] IncludeMe

-- | Handle the PRIVMSG message.
handlePrivmsg :: Monad m => String -> String -> IRC m ()
handlePrivmsg name msg = sendMsgTo RPL_PRIVMSG name msg

-- | Handle the NOTICE message.
handleNotice :: Monad m => String -> String -> IRC m ()
handleNotice name msg = sendMsgTo RPL_NOTICE name msg

-- | Handle WHOIS message.
handleWhoIs :: Monad m => String -> IRC m ()
handleWhoIs nick =
  withValidNick nick $ \nick ->
    withClientByNick nick $ \Client{..} ->
      withRegUserByNick nick $ \RegUser{..} -> do
        thisNickServerReply RPL_WHOISUSER 
                            [unNick regUserNick
                            ,regUserUser
                            ,clientHostname
                            ,"*"
                            ,regUserName]
        thisNickServerReply RPL_ENDOFWHOIS
                            [unNick regUserNick
                            ,"End of WHOIS list."]

-- | Handle the ISON ('is on?') message.
handleIsOn :: Monad m => [String] -> IRC m ()
handleIsOn (catMaybes . map readNick -> nicks) =
  asRegistered $ do
    online <- catMaybes <$> mapM regUserByNick nicks
    let nicks = unwords $ map (unNick.regUserNick) online
    unless (null nicks) $ thisNickServerReply RPL_ISON [nicks ++ " "]

-- Generic message functions

-- | Send a message to a user or a channel (it figures it out).
sendMsgTo :: Monad m => RPL -> String -> String -> IRC m ()
sendMsgTo typ name msg =
  if validChannel name
     then withValidChanName name $ \name -> 
            channelReply name typ [unChanName name,msg] ExcludeMe
     else userReply name typ [name,msg]

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
  channelReply name RPL_JOIN [unChanName name] IncludeMe
  sendNamesList name
  withChannel name $ \Channel{..} -> do
    case channelTopic of
      Just topic -> thisServerReply RPL_TOPIC [unChanName name,topic]
      Nothing -> return ()

-- | Send the names list of a channel.
sendNamesList :: Monad m => ChannelName -> IRC m ()
sendNamesList name = do
  asRegistered $
    withChannel name $ \Channel{..} -> do
      clients <- catMaybes <$> mapM getClientByRef channelUsers
      let nicks = map regUserNick . catMaybes . map clientRegUser $ clients
      forM_ (splitEvery 10 nicks) $ \nicks ->
        thisNickServerReply RPL_NAMEREPLY ["@",unChanName name
                                          ,unwords $ map unNick nicks]
      thisNickServerReply RPL_ENDOFNAMES [unChanName name
                                         ,"End of /NAMES list."]

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
    Nothing -> thisServerReply ERR_NOSUCHCHANNEL [unChanName name
                                                 ,"No such channel."]
    Just chan -> m chan
    
withValidChanName :: Monad m => String -> (ChannelName -> IRC m ()) 
                  -> IRC m ()
withValidChanName name m 
    | validChannel name = m $ ChannelName name
    | otherwise         = errorReply $ "Invalid channel name: " ++ name

-- Client/user access functions

-- | Read a valid nick.
readNick :: String -> Maybe Nick
readNick n | validNick n = Just $ Nick n
           | otherwise   = Nothing

-- | Modify the nicks mapping.
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
    Just{}  -> thisServerReply ERR_NICKNAMEINUSE 
                               [unNick nick,"Nick is already in use."]

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

-- | Send a client reply to a user.
userReply :: Monad m => String -> RPL -> [String] -> IRC m ()
userReply nick typ ps = 
  withValidNick nick $ \nick ->
    withClientByNick nick $ \Client{..} ->
      clientReply clientRef typ ps

-- | Perform an action with a client by nickname.
withClientByNick :: Monad m => Nick -> (Client -> IRC m ()) -> IRC m ()
withClientByNick nick m = do
    client <- clientByNick nick
    case client of
      Nothing -> sendNoSuchNick nick
      Just client@Client{..} 
          | isRegistered clientUser -> m client
          | otherwise -> sendNoSuchNick nick

-- | Perform an action with a registered user by its nickname.
withRegUserByNick :: Monad m => Nick -> (RegUser -> IRC m ()) -> IRC m ()
withRegUserByNick nick m = do
  user <- regUserByNick nick
  case user of
    Just user -> m user
    Nothing -> sendNoSuchNick nick
    
-- | Send the RPL_NOSUCHNICK reply.
sendNoSuchNick :: Monad m => Nick -> IRC m ()
sendNoSuchNick nick =
  thisServerReply ERR_NOSUCHNICK [unNick nick,"No such nick."]

-- | Get a registered user by nickname.
regUserByNick :: Monad m => Nick -> IRC m (Maybe RegUser)
regUserByNick nick = do
  c <- clientByNick nick
  case clientUser <$> c of
    Just (Registered u) -> return $ Just u
    _ -> return Nothing

-- | Get a client by nickname.
clientByNick :: Monad m => Nick -> IRC m (Maybe Client)
clientByNick nick = do
  clients <- gets envClients
  (M.lookup nick >=> (`M.lookup` clients)) <$> gets envNicks

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
    
-- | With sent pass.
withSentPass :: Monad m => IRC m () -> IRC m ()
withSentPass m = do
  asRegistered m
  withUnegistered $ \UnregUser{..} -> do
    case unregUserPass of
      Just{} -> m
      Nothing -> return ()

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
channelReply :: Monad m => ChannelName -> RPL -> [String] 
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
thisClientReply :: Monad m => RPL -> [String] -> IRC m ()
thisClientReply typ params = do
  ref <- asks connRef
  clientReply ref typ params

-- | Send a client reply of the given type with the given params, on
-- the given connection reference.
clientReply :: Monad m => Ref -> RPL -> [String] -> IRC m ()
clientReply ref typ params = do
  withRegistered $ \user -> do
    client <- getClient
    msg <- newClientMsg client user typ params
    reply ref msg

-- | Make a new IRC message from the current client.
newClientMsg :: Monad m => Client -> RegUser -> RPL -> [String] 
             -> IRC m Message
newClientMsg Client{..} RegUser{..} cmd ps = do
  let nickName = NickName (unNick regUserNick)
                          (Just regUserUser)
                          (Just clientHostname)
  return $ Message {
    msg_prefix = Just $ nickName
   ,msg_command = fromRPL cmd
   ,msg_params = ps
  }

-- Server replies

-- | Send the welcome message.
sendWelcome :: Monad m => IRC m ()
sendWelcome = do
  withRegistered $ \RegUser{..} -> do
    thisNickServerReply RPL_WELCOME ["Welcome."]

-- | Send the MOTD.    
sendMotd :: MonadProvider m => IRC m ()
sendMotd = do
  asRegistered $ do
    thisNickServerReply RPL_MOTDSTART ["MOTD"]
    motd <- fmap lines <$> lift provideMotd
    let motdLine line = thisNickServerReply RPL_MOTD [line]
    case motd of
      Nothing -> motdLine "None."
      Just lines -> mapM_ motdLine lines
    thisNickServerReply RPL_ENDOFMOTD ["/MOTD."]

-- | Send a message reply.
notice :: Monad m => String -> IRC m ()
notice msg = thisServerReply RPL_NOTICE ["*",msg]

thisNickServerReply :: Monad m => RPL -> [String] -> IRC m ()
thisNickServerReply typ params = do
  withRegistered $ \RegUser{regUserNick=Nick nick} ->
    thisServerReply typ (nick : params)

-- | Send a server reply of the given type with the given params.
thisServerReply :: Monad m => RPL -> [String] -> IRC m ()
thisServerReply typ params = do
  ref <- asks connRef
  serverReply ref typ params

-- | Send a server reply of the given type with the given params.
serverReply :: Monad m => Ref -> RPL -> [String] -> IRC m ()
serverReply ref typ params = do
  msg <- newServerMsg typ params
  reply ref msg

-- | Make a new IRC message from the server.
newServerMsg :: Monad m => RPL -> [String] -> IRC m Message
newServerMsg cmd ps = do
  hostname <- asks connServerName
  return $ Message {
    msg_prefix = Just $ Server hostname
   ,msg_command = fromRPL cmd
   ,msg_params = ps
  }

-- Output functions

-- | Send an error reply.
errorReply :: Monad m => String -> IRC m ()
errorReply m = do
  notice $ "ERROR: " ++ m
  log $ "ERROR: " ++ m

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
validNick s = all ok s && length s > 0 where
  ok c = isDigit c || isLetter c || elem c "-_/\\;()[]{}?`'"

-- | Valid channel name?
validChannel :: String -> Bool
validChannel ('#':cs) = all ok cs && length cs > 0 where
  ok c = isDigit c || isLetter c || elem c "-_/\\;()[]{}?`'"
validChannel _ = False
