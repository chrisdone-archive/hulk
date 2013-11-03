{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- | This module communicates and manages the state of clients
-- connected to the server.

module Hulk.Client
    where

import           Hulk.Auth
import           Hulk.Types

import           Control.Applicative
import           Control.Monad.RWS
import           Data.CaseInsensitive (mk)
import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Maybe
import qualified Data.Set             as S
import           Data.Text            (Text, pack, unpack)
import qualified Data.Text            as T
import           Data.Text.Encoding   (decodeUtf8,encodeUtf8)
import           Data.Time hiding (readTime)
import           Network.FastIRC      (Command (..), CommandArg, Message (..), UserSpec(..))
import qualified Network.FastIRC      as IRC
import           Prelude              hiding (log)

--------------------------------------------------------------------------------
-- * Top-level dispatchers

-- | Run the client monad.
handleCommand
  :: Config                        -- ^ Server configuration.
  -> HulkState                     -- ^ Server state.
  -> UTCTime                       -- ^ Current time.
  -> Conn                          -- ^ Current client connection.
  -> (String,String)               -- ^ Authorization info.
  -> Command                       -- ^ The command.
  -> ((), HulkState, [HulkWriter]) -- ^ The new transformed state and any instructions.
handleCommand config state now conn auth cmd = do
  runRWS (runHulk (handleCmd cmd))
         (HulkReader now conn config Nothing auth)
         state

-- | Handle an incoming command.
handleCmd :: Command -- ^ A command which shouldn't be logged (e.g. PASS).
          -> Hulk ()
handleCmd cmd = do
  case cmd of
    PassCmd (decodeUtf8 -> pass) -> do
      incoming cmd
      asUnregistered $ handlePass pass
    StringCmd "PINGPONG" _ -> do
      incoming cmd
      handlePingPong
    _ -> handleMsgSafeToLog cmd

-- | Handle commands that are safe to log.
handleMsgSafeToLog :: Command -- ^ A command which is safe to log
                              -- (PONG, NICK, etc.).
                   -> Hulk ()
handleMsgSafeToLog cmd = do
  incoming cmd
  updateLastPong
  case cmd of
    PongCmd{} -> handlePong
    NickCmd (decodeUtf8 -> nick) _ -> handleNick nick
    PingCmd (decodeUtf8 -> param) _ -> handlePing param
    UserCmd (decodeUtf8 -> user) _ _ (decodeUtf8 -> realname) ->
      asUnregistered $ handleUser user realname
    QuitCmd mmsg ->
      handleQuit RequestedQuit
                 (maybe "Quit (no message given)"
                        decodeUtf8
                        mmsg)
    StringCmd "DISCONNECT" _ ->
      handleQuit SocketQuit "Connection lost."
    _ -> handleMsgReg'd cmd

-- | Handle commands that can only be used when registered.
handleMsgReg'd :: Command -- ^ A command that users use after
                          -- registration (e.g. JOIN, PART, etc.).
               -> Hulk ()
handleMsgReg'd cmd =
  asRegistered $
   case cmd of
     JoinCmd names ->
       mapM_ handleJoin (map decodeUtf8 (M.keys names))
     PartCmd names mmsg  ->
       mapM_ (flip handlePart (maybe "" decodeUtf8 mmsg))
             (map decodeUtf8 (S.toList names))
     PrivMsgCmd targets msg ->
       mapM_ (flip handlePrivmsg (decodeUtf8 msg))
             (map decodeUtf8 (S.toList targets))
     TopicCmd chan (fmap decodeUtf8 -> Just topic) -> handleTopic (decodeUtf8 chan) topic
     NoticeCmd targets msg  ->
       mapM_ (flip handleNotice (decodeUtf8 msg))
             (map decodeUtf8 (S.toList targets))
     StringCmd "WHOIS" [nick] -> handleWhoIs (decodeUtf8 nick)
     StringCmd "ISON" people  -> handleIsOn (map decodeUtf8 people)
     StringCmd "NAMES" [chan] -> handleNames (decodeUtf8 chan)
     _ -> invalidCmd cmd

-- | Log an invalid cmd.
invalidCmd :: Command -- ^ The given command that we don't know how to handle.
           -> Hulk ()
invalidCmd cmd = do
  errorReply $ "Invalid or unknown message type, or not" <>
               " enough parameters: " <> decodeUtf8 (IRC.showCommand cmd)

--------------------------------------------------------------------------------
-- * Command handlers

-- | Handle the PONG command. This updates the user's “last seen”
-- value on file.
handlePong :: Hulk ()
handlePong = do
  now <- asks readTime
  withRegistered $ \RegUser{regUserUser=user} -> do
    tell [UpdateUserData UserData { userDataUser = user
                                  , userDataLastSeen = now
                                  }]

-- | Handle the PINGPONG event. Disconnect the client if timedout.
handlePingPong :: Hulk ()
handlePingPong = do
  lastPong <- clientLastPong <$> getClient
  now <- asks readTime
  let n = diffUTCTime now lastPong
  if n > 60*4
     then handleQuit RequestedQuit $ "Ping timeout: " <> pack (show n) <> " seconds"
     else do hostname <- asks (connServerName . readConn)
             thisCmdReply RPL_PING [hostname]

-- | Handle the PASS message.
handlePass :: Text -> Hulk ()
handlePass pass = do
  modifyUnregistered $ \u -> u { unregUserPass = Just pass }
  notice "Received password."
  tryRegister

-- | Handle the USER message.
handleUser :: Text -> Text -> Hulk ()
handleUser user realname = do
  withSentPass $
    if validUser user
       then do modifyUnregistered $ \u -> u { unregUserUser = Just (UserName (mk user))
                                            , unregUserName = Just realname }
               notice "Recieved user details."
               tryRegister
       else errorReply "Invalid user format."

-- | Handle the USER message.
handleNick :: Text -> Hulk ()
handleNick nick =
  withSentPass $
    withValidNick nick $ \nick ->
      ifNotMyNick nick $
        ifUniqueNick nick (updateNickAndTryRegistration nick)
                          (Just (tryBumpingSomeoneElseOff nick))

  where updateNickAndTryRegistration nick = do
          ref <- getRef
          withRegistered $ \RegUser{regUserNick=nick} ->
            modifyNicks $ M.delete nick
          withUnregistered $ \UnregUser{unregUserNick=nick} ->
            maybe (return ()) (modifyNicks . M.delete) nick
          modifyNicks $ M.insert nick ref
          modifyUnregistered $ \u -> u { unregUserNick = Just nick }
          tryRegister
          asRegistered $ do
            thisClientReply RPL_NICK [nickText nick]
            (myChannels >>=) $ mapM_ $ \Channel{..} -> do
              channelReply channelName RPL_NICK [nickText nick] ExcludeMe
            modifyRegistered $ \u -> u { regUserNick = nick }

        tryBumpingSomeoneElseOff nick = \error_reply -> do
          registered <- isRegistered <$> getUser
          if registered
             then bumpAndRegister nick error_reply
             else do
               withUnregistered $ \unreg -> do
                 (authentic,_) <- isAuthentic unreg { unregUserNick = Just nick }
                 if not authentic
                    then error_reply " (Registration not valid, can't bump off this user.)"
                    else bumpAndRegister nick error_reply

        bumpAndRegister nick error_reply = do
          username <- getUsername
          if fmap (mk.userText) username == Just (mk (nickText nick))
             then do bumpOff nick
                     updateNickAndTryRegistration nick
             else error_reply $
               let x = "username=" <> pack (show username) <> ", nick=" <> nickText nick
               in " (Can only bump the nick that matches your username. Debug: " <> x <> ")"

-- | Handle the PING message.
handlePing :: Text -> Hulk ()
handlePing p = do
  hostname <- asks (connServerName . readConn)
  thisServerReply RPL_PONG [hostname,p]

-- | Handle the QUIT message.
handleQuit :: QuitType -> Text -> Hulk ()
handleQuit quitType msg = do
  clearQuittedUser msg
  when (quitType == RequestedQuit) $
    tell [Close]

-- | Handle the TELL message.
handleTell :: Text -> Text -> Hulk ()
handleTell name msg = sendMsgTo RPL_NOTICE name msg

-- | Handle the NAMES list request.
handleNames :: Text -> Hulk ()
handleNames chan = do
  withValidChanName chan sendNamesList

-- | Handle the JOIN message.
handleJoin :: Text -> Hulk ()
handleJoin chans = do
  let names = T.split (==',') chans
  forM_ names $ flip withValidChanName $ \name -> do
      exists <- M.member name <$> gets stateChannels
      unless exists $ insertChannel name
      joined <- inChannel name
      unless joined $ joinChannel name

-- | Handle the PART message.
handlePart :: Text -> Text -> Hulk ()
handlePart name msg =
  withValidChanName name $ \name -> do
    removeFromChan name
    channelReply name RPL_PART [msg] IncludeMe

-- | Handle the TOPIC message.
handleTopic :: Text -> Text -> Hulk ()
handleTopic name topic =
  withValidChanName name $ \name -> do
    let setTopic c = c { channelTopic = Just topic }
    modifyChannels $ M.adjust setTopic name
    channelReply name RPL_TOPIC [channelNameText name,topic] IncludeMe

-- | Handle the PRIVMSG message.
handlePrivmsg :: Text -> Text -> Hulk ()
handlePrivmsg name msg = do
  sendMsgTo RPL_PRIVMSG name msg
  historyLog RPL_PRIVMSG [name,msg]

-- | Handle the NOTICE message.
handleNotice :: Text -> Text -> Hulk ()
handleNotice name msg = sendMsgTo RPL_NOTICE name msg

-- | Handle WHOIS message.
handleWhoIs :: Text -> Hulk ()
handleWhoIs nick =
  withValidNick nick $ \nick ->
    withClientByNick nick $ \Client{..} ->
      withRegUserByNick nick $ \RegUser{..} -> do
        thisNickServerReply RPL_WHOISUSER
                            [nickText regUserNick
                            ,userText regUserUser
                            ,clientHostname
                            ,"*"
                            ,regUserName]
        thisNickServerReply RPL_ENDOFWHOIS
                            [nickText regUserNick
                            ,"End of WHOIS list."]

-- | Handle the ISON ('is on?') message.
handleIsOn :: [Text] -> Hulk ()
handleIsOn (catMaybes . map readNick -> nicks) =
  asRegistered $ do
    online <- catMaybes <$> mapM regUserByNick nicks
    let nicks = T.unwords $ map (nickText.regUserNick) online
    unless (T.null nicks) $ thisNickServerReply RPL_ISON [nicks <> " "]

--------------------------------------------------------------------------------
-- * General actions

-- | Send a message to a user or a channel (it figures it out).
sendMsgTo :: RPL -> Text -> Text -> Hulk ()
sendMsgTo typ name msg =
  if validChannel name
     then withValidChanName name $ \name ->
            channelReply name typ [channelNameText name,msg] ExcludeMe
     else userReply name typ [name,msg]

--------------------------------------------------------------------------------
-- * Users

-- | Is a username valid?
validUser :: Text -> Bool
validUser = validNick

-- | Get the username.
getUsername :: Hulk (Maybe UserName)
getUsername = do
  user <- getUser
  return $ case user of
    Unregistered (UnregUser{unregUserUser=username}) -> username
    Registered (RegUser{regUserUser=username})       -> Just username

-- | Get the current connection ref.
getRef :: Hulk Ref
getRef = connRef <$> asks readConn

-- | Bump off the given nick.
bumpOff :: Nick -> Hulk ()
bumpOff nick = ifNotMyNick nick $ do
  notice $ "Bumping off user " <> nickText nick <> "…"
  withClientByNick nick $ \Client{clientRef=ref} ->
    local (\r -> r { readConn = (readConn r) { connRef = ref } }) $ do
      clearQuittedUser msg
      tell [Bump ref]

  where msg = "Bumped off."

-- | Clear a quitted user from channels and nick list, and notify
-- people in channels of their leaving.
clearQuittedUser :: Text -> Hulk ()
clearQuittedUser msg = do
  (myChannels >>=) $ mapM_ $ \Channel{..} -> do
    channelReply channelName RPL_QUIT [msg] ExcludeMe
    removeFromChan channelName
  withRegistered $ \RegUser{regUserNick=nick} -> do
    modifyNicks $ M.delete nick
  withUnregistered $ \UnregUser{unregUserNick=nick} -> do
    maybe (return ()) (modifyNicks . M.delete) nick
  ref <- getRef
  modifyClients $ M.delete ref
  notice msg

-- | Update the last pong reply time.
updateLastPong :: Hulk ()
updateLastPong = do
  ref <- getRef
  now <- asks readTime
  let adjust client@Client{..} = client { clientLastPong = now }
  modifyClients $ M.adjust adjust ref

-- | Send a client reply to a user.
userReply :: Text -> RPL -> [Text] -> Hulk ()
userReply nick typ ps =
  withValidNick nick $ \nick ->
    withClientByNick nick $ \Client{..} ->
      clientReply clientRef typ ps

-- | Perform an action with a registered user by its nickname.
withRegUserByNick :: Nick -> (RegUser -> Hulk ()) -> Hulk ()
withRegUserByNick nick m = do
  user <- regUserByNick nick
  case user of
    Just user -> m user
    Nothing -> sendNoSuchNick nick

-- | Send the RPL_NOSUCHNICK reply.
sendNoSuchNick :: Nick -> Hulk ()
sendNoSuchNick nick =
  thisServerReply ERR_NOSUCHNICK [nickText nick,"No such nick."]

-- | Modify the current user.
modifyUser :: (User -> User) -> Hulk ()
modifyUser f = do
  ref <- getRef
  let modUser c = c { clientUser = f (clientUser c) }
      modClient = M.adjust modUser ref
  modify $ \env -> env { stateClients = modClient (stateClients env) }

-- | Get the current client's user.
getUser :: Hulk User
getUser = clientUser <$> getClient

--------------------------------------------------------------------------------
-- * Clients

-- | Get the current client.
getClientByRef :: Ref -> Hulk (Maybe Client)
getClientByRef ref = do
  clients <- gets stateClients
  return $ M.lookup ref clients

-- | Get a client by nickname.
clientByNick :: Nick -> Hulk (Maybe Client)
clientByNick nick = do
  clients <- gets stateClients
  (M.lookup nick >=> (`M.lookup` clients)) <$> gets stateNicks

-- | Perform an action with a client by nickname.
withClientByNick :: Nick -> (Client -> Hulk ()) -> Hulk ()
withClientByNick nick m = do
    client <- clientByNick nick
    case client of
      Nothing -> sendNoSuchNick nick
      Just client@Client{..}
          | isRegistered clientUser -> m client
          | otherwise -> sendNoSuchNick nick

-- | Get the current client.
getClient :: Hulk Client
getClient = do
  ref <- getRef
  clients <- gets stateClients
  case M.lookup ref clients of
    Just client -> return $ client
    Nothing -> makeNewClient

-- | Modify the clients table.
modifyClients :: (Map Ref Client -> Map Ref Client) -> Hulk ()
modifyClients f = modify $ \env -> env { stateClients = f (stateClients env) }

-- | Make a current client based on the current connection.
makeNewClient :: Hulk Client
makeNewClient = do
  Conn{..} <- asks readConn
  let client = Client { clientRef = connRef
                      , clientHostname = connHostname
                      , clientUser = newUnregisteredUser
                      , clientLastPong = connTime }
  modifyClients $ M.insert connRef client
  return client


--------------------------------------------------------------------------------
-- * Registration

-- | Get a registered user by nickname.
regUserByNick :: Nick -> Hulk (Maybe RegUser)
regUserByNick nick = do
  c <- clientByNick nick
  case clientUser <$> c of
    Just (Registered u) -> return $ Just u
    _ -> return Nothing

-- | Maybe get a registered user from a client.
clientRegUser :: Client -> Maybe RegUser
clientRegUser Client{..} =
    case clientUser of
      Registered u -> Just u
      _ -> Nothing

-- | Modify the current user if unregistered.
modifyUnregistered :: (UnregUser -> UnregUser) -> Hulk ()
modifyUnregistered f = do
  modifyUser $ \user ->
      case user of
        Unregistered user -> Unregistered (f user)
        u -> u

-- | Modify the current user if registered.
modifyRegistered :: (RegUser -> RegUser) -> Hulk ()
modifyRegistered f = do
  modifyUser $ \user ->
      case user of
        Registered user -> Registered (f user)
        u -> u

-- | Only perform command if the client is registered.
asRegistered :: Hulk () -> Hulk ()
asRegistered m = do
  registered <- isRegistered <$> getUser
  when registered m

-- | Perform command with a registered user.
withRegistered :: (RegUser -> Hulk ()) -> Hulk ()
withRegistered m = do
  user <- getUser
  case user of
    Registered user -> m user
    _ -> return ()

-- | With sent pass.
withSentPass :: Hulk () -> Hulk ()
withSentPass m = do
  asRegistered m
  withUnregistered $ \UnregUser{..} -> do
    case unregUserPass of
      Just{} -> m
      Nothing -> return ()

-- | Perform command with a registered user.
withUnregistered :: (UnregUser -> Hulk ()) -> Hulk ()
withUnregistered m = do
  user <- getUser
  case user of
    Unregistered user -> m user
    _ -> return ()

-- | Only perform command if the client is registered.
asUnregistered :: Hulk () -> Hulk ()
asUnregistered m = do
  registered <- isRegistered <$> getUser
  unless registered m

-- | Is a user registered?
isRegistered :: User -> Bool
isRegistered Registered{} = True
isRegistered _ = False


-- | Make a new unregistered user.
newUnregisteredUser :: User
newUnregisteredUser = Unregistered $ UnregUser {
   unregUserName = Nothing
  ,unregUserNick = Nothing
  ,unregUserUser = Nothing
  ,unregUserPass = Nothing
  }

-- | Try to register the user with the USER/NICK/PASS that have been given.
tryRegister :: Hulk ()
tryRegister =
  withUnregistered $ \unreg -> do
    check <- isAuthentic unreg
    case check of
      (True,Just (name,user,nick)) -> do
        modifyUser $ \_ ->
          Registered $ RegUser name nick user ""
        sendWelcome
        sendMotd
        sendEvents
      (False,Just{}) -> errorReply $ "Wrong user/pass."
      _ -> return ()

isAuthentic :: UnregUser -> Hulk (Bool,Maybe (Text,UserName,Nick))
isAuthentic UnregUser{..} = do
  let details = (,,,) <$> unregUserName
                      <*> unregUserNick
                      <*> unregUserUser
                      <*> unregUserPass
  case details of
    Nothing -> return (False,Nothing)
    Just (name,nick,user,pass) -> do
      (keystr,passwords) <- asks readAuth
      let authentic = authenticate keystr passwords (userText user) pass
      return (authentic,Just (name,user,nick))

--------------------------------------------------------------------------------
-- * Nicknames

-- | Read a valid nick.
readNick :: Text -> Maybe Nick
readNick n | validNick n = Just $ NickName (mk n)
           | otherwise   = Nothing

-- | Modify the nicks mapping.
modifyNicks :: (Map Nick Ref -> Map Nick Ref) -> Hulk ()
modifyNicks f = modify $ \env -> env { stateNicks = f (stateNicks env) }

-- | With a valid nickname, perform an action.
withValidNick :: Text -> (Nick -> Hulk ()) -> Hulk ()
withValidNick nick m
    | validNick nick = m (NickName (mk nick))
    | otherwise      = errorReply $ "Invalid nick format: " <> nick

-- | Perform an action if a nickname is unique, otherwise send error.
ifUniqueNick :: Nick -> Hulk () -> Maybe ((Text -> Hulk ()) -> Hulk ()) -> Hulk ()
ifUniqueNick nick then_m else_m = do
  clients <- gets stateClients
  client <- (M.lookup nick >=> (`M.lookup` clients)) <$> gets stateNicks
  case client of
    Nothing -> then_m
    Just{}  -> do
      case else_m of
        Just else_m -> else_m error_reply
        Nothing -> error_reply ""

  where error_reply x = thisServerReply ERR_NICKNAMEINUSE
                                      [nickText nick,"Nick is already in use." <> x]


-- | Is a nickname valid? Digit/letter or one of these: -_/\\;()[]{}?`'
validNick :: Text -> Bool
validNick s = T.all ok s && T.length s > 0 where
  ok c = isDigit c || isLetter c || elem c "-_/\\;()[]{}?`'"

-- | If the given nick is not my nick name, ….
ifNotMyNick :: Nick -> Hulk () -> Hulk ()
ifNotMyNick nick m = do
  user <- getUser
  case user of
    Registered RegUser{..}     | regUserNick /= nick -> m
    Unregistered UnregUser{..} | unregUserNick /= Just nick -> m
    _ -> return ()

--------------------------------------------------------------------------------
-- * Channels

-- | Valid channel name?
validChannel :: Text -> Bool
validChannel (T.uncons -> Just ('#',cs)) = T.all ok cs && T.length cs > 0 where
  ok c = isDigit c || isLetter c || elem c "-_/\\;()[]{}?`'"
validChannel _ = False

-- | Remove a user from a channel.
removeFromChan :: ChannelName -> Hulk ()
removeFromChan name = do
  ref <- getRef
  let remMe c = c { channelUsers = S.delete ref (channelUsers c) }
  modifyChannels $ M.adjust remMe name

-- | Get channels that the current client is in.
myChannels :: Hulk [Channel]
myChannels = do
  ref <- getRef
  filter (S.member ref . channelUsers) . map snd . M.toList <$> gets stateChannels

-- | Join a channel.
joinChannel :: ChannelName -> Hulk ()
joinChannel name = do
  ref <- getRef
  let addMe c = c { channelUsers = S.insert ref (channelUsers c) }
  modifyChannels $ M.adjust addMe name
  channelReply name RPL_JOIN [channelNameText name] IncludeMe
  sendNamesList name
  withChannel name $ \Channel{..} -> do
    case channelTopic of
      Just topic -> thisServerReply RPL_TOPIC [channelNameText name,topic]
      Nothing -> return ()

-- | Send the names list of a channel.
sendNamesList :: ChannelName -> Hulk ()
sendNamesList name = do
  asRegistered $
    withChannel name $ \Channel{..} -> do
      clients <- catMaybes <$> mapM getClientByRef (S.toList channelUsers)
      let nicks = map regUserNick . catMaybes . map clientRegUser $ clients
      forM_ (chunksOf 10 nicks) $ \nicks ->
        thisNickServerReply RPL_NAMEREPLY ["@",channelNameText name
                                          ,T.unwords $ map nickText nicks]
      thisNickServerReply RPL_ENDOFNAMES [channelNameText name
                                         ,"End of /NAMES list."]

-- | Am I in a channel?
inChannel :: ChannelName -> Hulk Bool
inChannel name = do
  chan <- M.lookup name <$> gets stateChannels
  case chan of
    Nothing -> return False
    Just Channel{..} -> (`S.member` channelUsers) <$> getRef

-- | Insert a new channel.
insertChannel :: ChannelName -> Hulk ()
insertChannel name = modifyChannels $ M.insert name newChan where
  newChan = Channel { channelName  = name
                    , channelTopic = Nothing
                    , channelUsers = S.empty
                    }

-- | Modify the channel map.
modifyChannels :: (Map ChannelName Channel -> Map ChannelName Channel)
               -> Hulk ()
modifyChannels f = modify $ \e -> e { stateChannels = f (stateChannels e) }

withValidChanName :: Text -> (ChannelName -> Hulk ())
                  -> Hulk ()
withValidChanName name m
    | validChannel name = m $ ChannelName (mk name)
    | otherwise         = errorReply $ "Invalid channel name: " <> name

-- | Perform an action with an existing channel, sends error if not exists.
withChannel :: ChannelName -> (Channel -> Hulk ()) -> Hulk ()
withChannel name m = do
  chan <- M.lookup name <$> gets stateChannels
  case chan of
    Nothing -> thisServerReply ERR_NOSUCHCHANNEL [channelNameText name
                                                 ,"No such channel."]
    Just chan -> m chan

-- | Send a client reply to everyone in a channel.
channelReply :: ChannelName -> RPL -> [Text]
             -> ChannelReplyType
             -> Hulk ()
channelReply name cmd params typ = do
  withChannel name $ \Channel{..} -> do
    ref <- getRef
    forM_ (S.toList channelUsers) $ \theirRef -> do
      unless (typ == ExcludeMe && ref == theirRef) $
        clientReply theirRef cmd params

--------------------------------------------------------------------------------
-- * Client replies

-- | Send a client reply to the current client.
thisClientReply :: RPL -> [Text] -> Hulk ()
thisClientReply typ params = do
  ref <- getRef
  clientReply ref typ params

-- | Send a client reply of the given type with the given params, on
-- the given connection reference.
clientReply :: Ref -> RPL -> [Text] -> Hulk ()
clientReply ref typ params = do
  withRegistered $ \user -> do
    client <- getClient
    msg <- newClientMsg client user typ params
    reply ref msg

-- | Make a new IRC message from the current client.
newClientMsg :: Client -> RegUser -> RPL -> [Text]
             -> Hulk Message
newClientMsg Client{..} RegUser{..} cmd ps = do
  return (Message (Just (User (encodeUtf8 (nickText regUserNick))
                              (encodeUtf8 (userText regUserUser))
                              (encodeUtf8 clientHostname)))
                  (makeCommand cmd ps))

--------------------------------------------------------------------------------
-- * Server replies

-- | Send the welcome message.
sendWelcome :: Hulk ()
sendWelcome = do
  withRegistered $ \RegUser{..} -> do
    thisNickServerReply RPL_WELCOME ["Welcome."]

-- | Send the MOTD.
sendMotd :: Hulk ()
sendMotd = do
  asRegistered $ do
    thisNickServerReply RPL_MOTDSTART ["MOTD"]
    motd <- fmap (fmap T.lines) (asks readMotd)
    let motdLine line = thisNickServerReply RPL_MOTD [line]
    case motd of
      Nothing -> motdLine "None."
      Just lines -> mapM_ motdLine lines
    thisNickServerReply RPL_ENDOFMOTD ["/MOTD."]

-- | Send events that the user missed.
sendEvents :: Hulk ()
sendEvents = do
  chans <- configLogChans <$> asks readConfig
  unless (null chans) $ do
    withRegistered $ \RegUser{regUserUser=user} -> do
      ref <- getRef
      forM_ chans handleJoin
      tell [SendEvents ref user]

--------------------------------------------------------------------------------
-- * Output functions

-- | Send a message reply.
notice :: Text -> Hulk ()
notice msg = thisServerReply RPL_NOTICE ["*",msg]

thisNickServerReply :: RPL -> [Text] -> Hulk ()
thisNickServerReply typ params = do
  withRegistered $ \RegUser{regUserNick=nick} ->
    thisServerReply typ (nickText nick : params)

-- | Send a server reply of the given type with the given params.
thisServerReply :: RPL -> [Text] -> Hulk ()
thisServerReply typ params = do
  ref <- getRef
  serverReply ref typ params

-- | Send a server reply of the given type with the given params.
serverReply :: Ref -> RPL -> [Text] -> Hulk ()
serverReply ref typ params = do
  msg <- newServerMsg typ params
  reply ref msg

-- | Make a new IRC message from the server.
newServerMsg :: RPL -> [Text] -> Hulk Message
newServerMsg cmd ps = do
  hostname <- asks (connServerName.readConn)
  return (Message (Just (Nick (encodeUtf8 hostname)))
                  (makeCommand cmd ps))

-- | Send a cmd reply of the given type with the given params.
thisCmdReply :: RPL -> [Text] -> Hulk ()
thisCmdReply typ params = do
  ref <- getRef
  cmdReply ref typ params

-- | Send a cmd reply of the given type with the given params.
cmdReply :: Ref -> RPL -> [Text] -> Hulk ()
cmdReply ref typ params = do
  let msg = newCmdMsg typ params
  reply ref msg

-- | Send an error reply.
errorReply :: Text -> Hulk ()
errorReply m = do
  notice $ "ERROR: " <> m
  log $ "ERROR: " <> m

-- | Send a message reply.
reply :: Ref -> Message -> Hulk ()
reply ref msg = do
  outgoing msg
  tell . return $ MessageReply ref msg

-- | Log an incoming line.
incoming :: Command -> Hulk ()
incoming = log . ("<- " <>) . decodeUtf8 . IRC.showCommand

-- | Log an outgoing line.
outgoing :: Message -> Hulk ()
outgoing msg = do
  ref <- getRef
  tell [outgoingWriter ref msg]

-- | Log a line.
log :: Text -> Hulk ()
log line = do
  ref <- getRef
  tell . return . LogReply $ pack (show (unRef ref)) <> ": " <> line

-- | Make a writer reply.
outgoingWriter :: Ref -> Message -> HulkWriter
outgoingWriter ref =
  LogReply .
  (pack (show (unRef ref)) <>) .
  (": -> " <>) .
  decodeUtf8 .
  IRC.showMessage

historyLog :: RPL -> [Text] -> Hulk ()
historyLog rpl params = do
  chans <- asks (configLogChans . readConfig)
  unless (null chans) $ do
    withRegistered $ \RegUser{regUserUser=name} -> do
      let send = tell [SaveLog (userText name) rpl params]
      case (rpl,params) of
        (RPL_PRIVMSG,[chan])
          | chan `elem` chans -> send
          | otherwise         -> return ()
        _                     -> send

--------------------------------------------------------------------------------
-- * Command Construction

-- | Make a command.
makeCommand :: RPL -> [Text] -> Command
makeCommand rpl xs = fromRPL rpl (map encodeUtf8 xs)

-- | Convert from a reply to an appropriate protocol format.
fromRPL :: RPL -> ([CommandArg] -> Command)
fromRPL RPL_NICK          = StringCmd "NICK"
fromRPL RPL_PONG          = StringCmd "PONG"
fromRPL RPL_QUIT          = StringCmd "QUIT"
fromRPL RPL_JOIN          = StringCmd "JOIN"
fromRPL RPL_NOTICE        = StringCmd "NOTICE"
fromRPL RPL_PART          = StringCmd "PART"
fromRPL RPL_PRIVMSG       = StringCmd "PRIVMSG"
fromRPL RPL_JOINS         = StringCmd "JOIN"
fromRPL RPL_TOPIC         = StringCmd "TOPIC"
fromRPL RPL_PING          = StringCmd "PING"
fromRPL RPL_WHOISUSER     = NumericCmd 311
fromRPL RPL_ISON          = NumericCmd 303
fromRPL RPL_NAMEREPLY     = NumericCmd 353
fromRPL RPL_ENDOFNAMES    = NumericCmd 366
fromRPL RPL_WELCOME       = NumericCmd 001
fromRPL RPL_MOTDSTART     = NumericCmd 375
fromRPL RPL_MOTD          = NumericCmd 372
fromRPL RPL_ENDOFMOTD     = NumericCmd 376
fromRPL RPL_WHOISIDLE     = NumericCmd 317
fromRPL RPL_WHOISCHANNELS = NumericCmd 319
fromRPL RPL_ENDOFWHOIS    = NumericCmd 318
fromRPL ERR_NICKNAMEINUSE = NumericCmd 433
fromRPL ERR_NOSUCHNICK    = NumericCmd 401
fromRPL ERR_NOSUCHCHANNEL = NumericCmd 403

-- | Make a new IRC message from the cmd.
newCmdMsg :: RPL -> [Text] -> Message
newCmdMsg cmd ps = Message Nothing (makeCommand cmd ps)
