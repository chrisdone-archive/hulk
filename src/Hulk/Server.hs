{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
module Hulk.Server (start) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Fix
import qualified Data.Map            as M
import           Network
import           Network.IRC
import           System.IO
import           System.IO.UTF8      as UTF8

import           Hulk.Types
import           Hulk.Client

-- | Start an IRC server with the given configuration.
start :: Config -> IO ()
start config = withSocketsDo $ do
  hSetBuffering stdout LineBuffering
  listenSock <- listenOn $ PortNumber (configListen config)
  envar <- newMVar Env { envClients = M.empty
                       , envNicks = M.empty
                       , envChannels = M.empty }
  forever $ do
    (handle,host,_port) <- accept listenSock
    hSetBuffering handle NoBuffering
    let conn = Conn { connRef = newRef handle
                    , connHostname = host
                    , connServerName = configHostname config
                    }
    _ <- forkIO $ handleClient handle envar conn
    return ()

-- | Handle a client connection.
handleClient :: Handle -> MVar Env -> Conn -> IO ()
handleClient handle env conn = do
  fix $ \loop -> do
    line <- catch (Right <$> UTF8.hGetLine handle) (return . Left)
    case line of
      Right line -> do runClientHandler env handle conn line; loop
      Left _err -> runClientHandler env handle conn $ 
                   makeLine DISCONNECT ["Connection lost."]

-- | Make an internal IRC event to give to the client handler.
makeLine :: Event -> [String] -> String
makeLine event params = (++"\r\n") $ encode $
  Message { msg_prefix = Nothing
          , msg_command = show event
          , msg_params = params }

-- | Handle a received line from the client.
runClientHandler :: MVar Env -> Handle -> Conn -> String -> IO ()
runClientHandler env handle conn line = do
  modifyMVar_ env $ \env -> do
    (replies,env) <- handleLine env conn line
    mapM_ (handleReplies handle) replies
    return env

-- | Act on replies from the client.
handleReplies :: Handle -> Reply -> IO ()
handleReplies handle reply = do
  case reply of
    MessageReply ref msg -> sendMessage ref msg
    LogReply line -> logLine line
    Close -> hClose handle

-- | Send a message to a client.
sendMessage :: Ref -> Message -> IO ()
sendMessage (Ref handle) msg = do
  catch (UTF8.hPutStrLn handle $ encode msg)
        (\_ -> hClose handle)

-- | Add a line to the log file.
logLine :: String -> IO ()
logLine = UTF8.putStrLn
