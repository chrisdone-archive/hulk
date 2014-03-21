{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hulk.Server where

import           Hulk.Client                (handleCommand, makeCommand,
                                             outgoingWriter)
import           Hulk.Types

import           Control.Applicative
import           Control.Concurrent
{-import           Control.Concurrent.Delay-}
import           Control.Exception          (IOException, try)
import           Control.Monad
import           Control.Monad.Fix
import           Data.Aeson
import qualified Data.ByteString.Char8      as S8
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.CaseInsensitive
import           Data.Char

import           Data.Maybe
import           Data.Monoid
import           Data.Text                  (Text, pack, unpack)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import qualified Data.Text.IO               as T
import           Data.Time
import           Network
import           Network.FastIRC            hiding (UserName)
import qualified Network.FastIRC.IO         as IRC
import           System.Directory
import           System.FilePath
import           System.IO

-- | Start an IRC server with the given configuration.
start :: Config -> IO ()
start config = withSocketsDo $ do
  hSetBuffering stdout LineBuffering
  listenSock <- listenOn $ PortNumber (configListen config)
  statevar <- newMVar HulkState { stateClients  = mempty
                                , stateNicks    = mempty
                                , stateChannels = mempty }
  lvar <- newMVar ()
  forever $ do
    (handle,host,_port) <- accept listenSock
    hSetBuffering handle NoBuffering
    now <- getCurrentTime
    let conn = Conn { connRef = mkRef handle
                    , connHostname = pack host
                    , connServerName = configHostname config
                    , connTime = now
                    }
    auth <- getAuth config
    void $ forkIO $ handleClient lvar config handle statevar auth conn

-- | Handle a client connection.
handleClient :: MVar () -> Config -> Handle -> MVar HulkState -> (String,String) -> Conn -> IO ()
handleClient lvar config handle env auth conn = do
  messages <- newChan
  let writeMsg cmd =
        writeChan messages (Message Nothing (StringCmd cmd []))

  pinger <- forkIO $ forever $ do
    threadDelay (1000 * 1000 * 60 * 2)
    writeMsg "PINGPONG"

  void $ forkIO $ fix $ \loop -> do
    eline <- try (S8.hGetLine handle)
    case eline of
      Left (_::IOException) -> do killThread pinger
                                  writeMsg "DISCONNECT"
      Right line ->
        case readMessage line of
          Just msg -> do writeChan messages msg
                         loop
          Nothing -> loop

  fix $ \loop -> do
     msg <- readChan messages
     runClientHandler lvar config env handle conn auth msg
     case msg of
       Message  _  (StringCmd "DISCONNECT" _) -> return ()
       _ -> loop

-- | Handle a received message from the client.
runClientHandler :: MVar () -> Config -> MVar HulkState -> Handle -> Conn -> (String,String) -> Message -> IO ()
runClientHandler lvar config mstate handle conn auth msg = do
  now <- getCurrentTime
  instructions <- modifyMVar mstate $ \state -> return $
    let ((),newstate,instructions) = handleCommand config state now conn auth (msgCommand msg)
    in (newstate,instructions)
  forM_ instructions $ handleWriter lvar config handle

-- | Act on writer from the client.
handleWriter :: MVar () -> Config -> Handle -> HulkWriter -> IO ()
handleWriter lvar config@Config{..} handle writer = do
  case writer of
    SaveLog name rpl params -> saveToLog lvar config name rpl params
    MessageReply ref msg -> sendMessage ref msg
    LogReply line -> logLine line
    Close -> hClose handle
    Bump (Ref h) -> hClose h
    UpdateUserData udata -> do
      L.writeFile (configUserData </> normalizeUser (unpack (userText (userDataUser udata))))
                  (encode udata)
    SendEvents ref user -> do
      writers <- sendEvents config ref user
      mapM_ (handleWriter lvar config handle) (concat writers)

-- | Send a message to a client.
sendMessage :: Ref -> Message -> IO ()
sendMessage (Ref handle) msg =
  void $ (try $ IRC.hPutMessage handle msg :: IO (Either IOException ()))

-- | Add a line to the log file.
logLine :: Text -> IO ()
logLine = T.putStrLn

-- | Normalize the username for a filename.
normalizeUser :: [Char] -> [Char]
normalizeUser = filter (\c -> isDigit c || isLetter c)

-- | Send events that the user missed.
sendEvents :: Config -> Ref -> UserName -> IO [[HulkWriter]]
sendEvents config ref user = do
  events <- getLog config
  UserData{userDataLastSeen=lastSeen} <- getUser config (userText user)
  let filtered = flip filter events $ \(time,_from,_typ,_params) ->
                  time >. lastSeen
  forM filtered $ \msg -> do
    case msg of
      (time,from',rpl@RPL_PRIVMSG,[name,msg'])
        | name == userText user || "#" `T.isPrefixOf` name -> do
        let from = T.filter (\c -> isDigit c || isLetter c) from'
            user' = User (encodeUtf8 from)
                         (encodeUtf8 from)
                         "offline"
            message = Message (Just user')
                              (makeCommand rpl
                                           [name,"[" <> pack (show time) <> "] " <> msg'])
        return [MessageReply ref message
               ,outgoing message]
      _ -> return []

  where x >. y = x `diffUTCTime` y > 0
        outgoing msg = outgoingWriter ref msg

-- | Get the log.
getLog :: FromJSON b => Config -> IO [b]
getLog Config{..} = do
  contents <- L.readFile configLogFile
  return $ mapMaybe decode $ L8.lines contents

-- | Get the user data.
getUser :: Config -> Text -> IO UserData
getUser Config{..} name = do
  let fname = configUserData </> normalizeUser (unpack name)
  now <- getCurrentTime
  exists <- doesFileExist fname
  if exists
     then do contents <- L.readFile fname
             case decode contents of
               Just u -> return u
               Nothing -> error ("unable to parse user file: " ++ fname)
     else return $ UserData (UserName (mk name)) now

-- | Get authorization info.
getAuth :: Config -> IO (String,String)
getAuth Config{..} =
  (,) <$> readFile configPasswdKey
      <*> readFile configPasswd

-- | Save the message to the log.
saveToLog :: MVar () -> Config -> Text -> RPL -> [Text] -> IO ()
saveToLog lvar Config{..} name rpl params = do
  now <- getCurrentTime
  withMVar lvar $ const $
    L.appendFile configLogFile $
      encode ((now,name,rpl,params)) <> "\n"
