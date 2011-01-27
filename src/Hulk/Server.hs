{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
module Hulk.Server (start) where

import Data.List
import Data.List.Split
import Control.Applicative
import Control.Monad.Reader
import System.IO
import Data.Maybe
import Control.Monad.IO
import Control.Concurrent
import Network
import Data.Map (Map)
import qualified Data.Map as M
import Network.IRC hiding (Channel)
import Data.Char

import Hulk.Concurrent
import Hulk.Types
import Hulk.Log
import Hulk.Config

start :: Config -> IO ()
start config = do
  sto <- newMVar stdout
  clients <- newMVar M.empty
  nicks <- newMVar M.empty
  chans <- newMVar M.empty
  runReaderT (runHulk startHulk)
             Env { envConfig = config
                 , envTellHandle = sto -- TODO: Make configurable.
                 , envLogColumn = 0
                 , envListenSock = error "Listen socket not set."
                 , envClients = clients
                 , envNicks = nicks
                 , envChannels = chans
                 }

startHulk :: Hulk ()
startHulk = do
  performing "Starting Hulk server" $ do
    startListener

startListener :: Hulk ()
startListener = do
  port <- config configListen
  socket <- performing ("Attempting to listen on port " ++ show port) $
    io $ listenOn $ PortNumber port
  let setSocket env = env { envListenSock = socket }
  local setSocket $ do
    listenLoop

listenLoop :: Hulk ()
listenLoop = do
  socket <- asks envListenSock
  forever $ do
    (handle,hostname,portnumber) <- io $ accept socket
    io $ hSetBuffering handle NoBuffering
    hvar <- io $ newMVar $ Ref handle
    user <- io $ newMVar $ Nothing
    let client = Client { clientUser = user
                        , clientHandle = hvar
                        , clientHostName = hostname
                        , clientPort = portnumber
                        }
    insertClient client
    fork $ runReaderT (runIRC handleClient) client

insertClient :: Client -> Hulk ()
insertClient client@Client{..} = do
   handle <- io $ readMVar clientHandle
   modifyClients $ M.insert handle client

modifyClients :: (Map Ref Client -> Map Ref Client) -> Hulk ()
modifyClients f = do
  clients <- asks envClients
  io $ modifyMVar_ clients (return . f)

handleClient :: IRC ()
handleClient = do
  hvar <- asks clientHandle
  handle <- io $ unRef <$> readMVar hvar
  inprogress "Waiting for your user and nickname"
  let loop = do
        line <- io $ catch (Right <$> hGetLine handle) (return . Left)
        case line of
          Right line -> do incoming line; handleLine line; loop
          Left err -> handleQuit "Connection reset by peer."
  loop

handleLine :: String -> IRC ()
handleLine line = 
  case decode line of
    Just msg -> handleMsg msg
    Nothing  -> barf $ "Unable to parse " ++ show line

handleMsg :: Message -> IRC ()
handleMsg Message{..} = 
    case (msg_command,msg_params) of
      ("USER",[user,_,_,realname]) -> handleUser realname user
      ("NICK",[nick])              -> handleNick nick
      ("PART",[chan,msg])          -> chanSendLeave "PART" chan msg
      ("QUIT",[msg])               -> handleQuit msg
      ("JOIN",[name])              -> handleJoin name
      ("PRIVMSG",[to,msg])         -> handlePrivmsg to msg
      _ -> barf $ "Invalid or unknown message type, or not" ++ 
                  " enough parameters: " ++ msg_command

handlePrivmsg :: String -> String -> IRC ()
handlePrivmsg to msg = do
  case to of
    chan@('#':_) -> channelMsg "PRIVMSG" to [to,msg] True
    nick         -> nickMsg to msg
    
channelMsg typ to msg privmsg = do
  chans <- getChans
  case M.lookup to chans of
    Nothing -> notice "Unknown channel."
    Just chan -> do myRef <- getRef
                    forM_ (channelUsers chan) $ \ref -> do
                    when (ref /= myRef || not privmsg) $ do
                      userReply' ref typ msg

nickMsg to msg = do
  client <- liftHulk $ getClientByNick to
  case client of
    Just Client{..} -> do
       ref <- io $ readMVar clientHandle
       userReply' ref "PRIVMSG" [to,msg]

-- TODO: Only allow registration once.
-- TODO: User restrictions.
handleUser :: String -> String -> IRC ()
handleUser real user = do
  nick <- maybe "" userNick <$> getUser
  insertUser $ User { userUser = user
                    , userName = real
                    , userNick = nick
                    , userRegistered = False
                    }
  notice $ "Thanks, " ++ real ++ ". User details received."
  register
  
register = do
  u <- getUser
  tell$ show u
  case u of
   Just User{..} | not userRegistered
     -> when (all (not.null) [userUser,userNick]) $ do
             serverReply "001" [userNick,"Welcome."]
             modifyUser $ \u -> u { userRegistered = True }
   _ -> return ()

   
chanSendLeave :: String -> String -> String -> IRC ()
chanSendLeave typ chan msg = do
  chan <- chanByName chan
  case chan of
    Just Channel{..} -> channelMsg typ channelName [channelName,msg] False
    Nothing -> notice "No such channel."
    
handleQuit :: String -> IRC ()
handleQuit msg = do
  chans <- myChannels
  mapM_ (flip (chanSendLeave "QUIT") msg) $ map fst chans
  quitUser
  
quitUser :: IRC ()
quitUser = do
  nick <- maybe "" userNick <$> getUser
  ref <- getRef
  notice $ "Quitting " ++ show ref
  liftHulk $ modifyNicks $ M.delete nick
  let removeMe ch = ch { channelUsers = delete ref (channelUsers ch) }
  chans <- myChannels
  forM_ chans $ modifyChannels . M.adjust removeMe . fst

handleNick :: String -> IRC ()
handleNick nick = do
  nicks <- liftHulk (asks envNicks) >>= io . readMVar
  if M.member nick nicks
     then serverReply "433" ["*",nick,"Nickname is already in use."]
     else if validNick nick
             then registerNick nick
             else barf $ "Invalid nickname: " ++ nick
     
validNick :: String -> Bool
validNick = all ok where
  ok c = isDigit c || isLetter c || elem c "-_/\\;()[]{}?"

registerNick :: String -> IRC ()
registerNick nick = do
  u <- getUser
  let oldNick = maybe "" userNick $ u
  userReply "NICK" [nick]
  chans <- myChannels
  tell $ show chans
  myChannels >>= mapM_ (chanNickChange nick)
  if oldNick == "" && ((userUser <$> u) == Just "" || u==Nothing)
     then insertUser $ User { userUser = ""
                            , userName = ""
                            , userNick = nick 
                            , userRegistered = False }
     else modifyUser $ \u -> u { userNick = nick }
  ref <- getRef
  liftHulk $ modifyNicks $ M.insert nick ref . M.delete oldNick
  register

chanNickChange nick (name,_) = do
  channelMsg "NICK" name [nick] True

chanByName name = do
  channels <- liftHulk (asks envChannels) >>= io . readMVar
  return $ M.lookup name channels

myChannels = do
  channels <- liftHulk (asks envChannels) >>= io . readMVar
  ref <- getRef
  return $ filter ((elem ref).channelUsers.snd) $ M.toList $ channels

handleJoin :: String -> IRC ()
handleJoin name = do
  channels <- liftHulk (asks envChannels) >>= io . readMVar
  when (not $ M.member name channels) $ makeChannel name
  joinChannel name
  
sendChanNicks :: String -> IRC ()
sendChanNicks name = do
  nick <- maybe "" userNick <$> getUser
  chans <- getChans
  case M.lookup name chans of
    Nothing -> notice "Unknown channel."
    Just chan -> do 
      nicks <- liftHulk $ mapM getClientByRef (channelUsers chan)
      mapM_ (sendNicks nick) $ splitEvery 10 $ catMaybes $ nicks
      serverReply "366" [nick,name,"End of /NAMES list."]
  where sendNicks nick clients = do
                   nicks <- io $ mapM (readNick . clientUser) clients
                   serverReply "353" [nick,"@",name,unwords nicks]
            where readNick = fmap (fromMaybe "" . fmap userNick) . readMVar

joinChannel :: String -> IRC ()
joinChannel channel = do
  ref <- getRef
  let addMe ch = ch { channelUsers = ref : channelUsers ch }
  modifyChannels $ M.adjust addMe channel
  channelMsg "JOIN" channel [channel] False
  sendChanNicks channel

makeChannel :: String -> IRC ()
makeChannel name =
  modifyChannels $ M.insert name Channel { channelName = name
                                         , channelTopic = ""
                                         , channelUsers = []
                                         }

getChans = liftHulk (asks envChannels) >>= io . readMVar

getRef :: IRC Ref
getRef = asks clientHandle >>= io . readMVar

modifyChannels :: (Map String Channel -> Map String Channel) -> IRC ()
modifyChannels f = liftHulk $ do
  chans <- asks envChannels
  io $ modifyMVar_ chans $ return . f

getClientByRef :: Ref -> Hulk (Maybe Client)
getClientByRef ref = do
  clients <- getClients
  return $ M.lookup ref clients

getClientByNick :: String -> Hulk (Maybe Client)
getClientByNick nick = do
  nicks <- getNicks
  case M.lookup nick nicks of
    Just ref -> getClientByRef ref
    Nothing -> return Nothing

getNicks = asks envNicks >>= io . readMVar

getClients = asks envClients >>= io . readMVar

modifyNicks :: (Map String Ref -> Map String Ref) -> Hulk ()
modifyNicks f = do
  nicks <- asks envNicks
  io $ modifyMVar_ nicks $ return . f

getUser :: IRC (Maybe User)
getUser = asks clientUser >>= io . readMVar

modifyUser :: (User -> User) -> IRC ()
modifyUser f = do
  user <- asks clientUser
  io $ modifyMVar_ user $ return . fmap f

insertUser :: User -> IRC ()
insertUser u = do
  user <- asks clientUser
  io $ modifyMVar_ user $ return . const (Just u)

reply :: Message -> IRC ()
reply line = do
  ref <- asks clientHandle >>= io . readMVar
  reply' ref line
    
reply' :: Ref -> Message -> IRC ()
reply' ref line = do
  outgoing $ encode line
  io $ catch (hPutStrLn (unRef ref) $ encode line)
             (\e -> return ())

serverReply :: String -> [String] -> IRC ()
serverReply command msg = do
  hostname <- liftHulk $ config configHostname
  reply $ Message { msg_prefix  = Just $ Server hostname
                  , msg_command = command
                  , msg_params  = msg
                  }

userReply :: String -> [String] -> IRC ()
userReply command msg = do
  ref <- getRef 
  userReply' ref command msg

userReply' :: Ref -> String -> [String] -> IRC ()
userReply' ref command msg = do
  hostname <- liftHulk $ config configHostname
  host <- asks clientHostName
  user <- getUser
  reply' ref $ Message { 
    msg_prefix  = Just $ NickName (fromMaybe "" $ userNick <$> user)
                                  (userNick <$> user)
                                  (Just host)
  , msg_command = command
  , msg_params  = msg
  }

notice :: String -> IRC ()
notice = serverReply "NOTICE" . ("*":) . return
                  
unknownCommand :: String -> IRC ()
unknownCommand = serverReply "421" . ("*":) . return

barf :: String -> IRC ()
barf msg = do note msg; unknownCommand msg

inprogress :: String -> IRC ()
inprogress str = notice $ "*** " ++ str ++ " ..."
