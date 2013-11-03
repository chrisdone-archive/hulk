{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hulk.Server (start) where

import           Hulk.Client
import           Hulk.Providers           ()
import           Hulk.Types

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Delay
import           Control.Exception
import           Control.Monad
import           Control.Monad.Fix
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy     as L
import           Data.CaseInsensitive
import           Data.Char
import qualified Data.Map                 as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text                (Text, pack, unpack)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO             as T
import           Data.Time
import           Network
import           Network.FastIRC hiding (UserName)
import qualified Network.FastIRC.IO       as IRC
import           Prelude                  hiding (catch)
import           System.Directory
import           System.FilePath
import           System.IO

-- | Start an IRC server with the given configuration.
start :: Config -> IO ()
start config = withSocketsDo $ do
  hSetBuffering stdout LineBuffering
  listenSock <- listenOn $ PortNumber (configListen config)
  statevar <- newMVar HulkState { stateClients  = M.empty
                                , stateNicks    = M.empty
                                , stateChannels = M.empty }
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
    _ <- forkIO $ handleClient config handle statevar auth conn
    return ()

-- | Handle a client connection.
handleClient :: Config -> Handle -> MVar HulkState -> (String,String) -> Conn -> IO ()
handleClient config handle env auth conn = do
  let run = runClientHandler config env handle conn auth
      fake cmd ps = run (Message Nothing (StringCmd cmd ps))

  pinger <- forkIO $ forever $ do
    delayMinutes 2
    fake "PINGPONG" []

  fix $ \loop -> do
    eline <- catch (Right <$> IRC.hGetIRCLine handle)
                   (\(e::IOException) -> do killThread pinger
                                            return $ Left e)
    case eline of
      Left{} -> fake "DISCONNECT" []
      Right line ->
        case readMessage line of
          Just msg -> do run msg
                         loop
          Nothing -> loop

  where newline c = c=='\n' || c=='\r'

-- | Handle a received message from the client.
runClientHandler :: Config -> MVar HulkState -> Handle -> Conn -> (String,String) -> Message -> IO ()
runClientHandler config state handle conn auth msg = do
  now <- getCurrentTime
  modifyMVar_ state $ \state -> do
    let ((),newstate,instructions) = handleCommand config state now conn auth (msgCommand msg)
    forM_ instructions $ handleWriter config handle
    return newstate

-- | Act on writer from the client.
handleWriter :: Config -> Handle -> HulkWriter -> IO ()
handleWriter config@Config{..} handle writer = do
  case writer of
    MessageReply ref msg -> sendMessage ref msg
    LogReply line -> logLine line
    Close -> hClose handle
    Bump (Ref handle) -> hClose handle
    UpdateUserData udata -> do
      L.writeFile (configUserData </> normalizeUser (unpack (userText (userDataUser udata))))
                  (encode udata)
    SendEvents ref user -> do
      writers <- sendEvents config ref user
      mapM_ (handleWriter config handle) (concat writers)
    SaveLog name rpl params -> saveToLog config name rpl params

-- | Send a message to a client.
sendMessage :: Ref -> Message -> IO ()
sendMessage (Ref handle) msg = do
  catch (IRC.hPutMessage handle msg)
        (\(_::IOException) -> hClose handle)

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
      (time,from',rpl@RPL_PRIVMSG,[name,msg])
        | name == userText user || "#" `T.isPrefixOf` name -> do
        let from = T.filter (\c -> isDigit c || isLetter c) from'
            user = User (encodeUtf8 from)
                        (encodeUtf8 from)
                        "offline"
            message = Message (Just user)
                              (makeCommand rpl
                                           [name,"[" <> pack (show time) <> "] " <> msg])
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
saveToLog :: Config -> Text -> RPL -> [Text] -> IO ()
saveToLog Config{..} name rpl params = do
  now <- getCurrentTime
  L.appendFile configLogFile $
    encode ((now,name,rpl,params)) <> "\n"
