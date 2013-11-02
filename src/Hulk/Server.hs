{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
module Hulk.Server (start) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Delay
import           Control.Exception
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Reader
import qualified Data.Map                 as M
import           Data.Time
import           Network
import           Network.IRC
import           Prelude hiding (catch)
import           System.IO
import           System.IO.UTF8           as UTF8

import           Hulk.Client
import           Hulk.Providers           ()
import           Hulk.Types

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
    now <- getCurrentTime
    let conn = Conn { connRef = newRef handle
                    , connHostname = host
                    , connServerName = configHostname config
                    , connTime = now
                    }
    _ <- forkIO $ handleClient config handle envar conn
    return ()

-- | Handle a client connection.
handleClient :: Config -> Handle -> MVar Env -> Conn -> IO ()
handleClient config handle env conn = do
  let runHandle = runClientHandler config env handle conn
      runLine x y = runHandle $ makeLine x y
  pinger <- forkIO $ forever $ do delayMinutes 2; runLine PINGPONG []
  fix $ \loop -> do
    line <- catch (Right <$> UTF8.hGetLine handle)
                  (\(e::IOException) -> do killThread pinger
                                           return $ Left e)
    case filter (not.newline) <$> line of
      Right []   -> loop
      Right line -> do runHandle (line++"\r"); loop
      Left _err  -> runLine DISCONNECT ["Connection lost."]

  where newline c = c=='\n' || c=='\r'

-- | Make an internal IRC event to give to the client handler.
makeLine :: Event -> [String] -> String
makeLine event params = (++"\r") $ encode $
  Message { msg_prefix = Nothing
          , msg_command = show event
          , msg_params = params }

-- | Handle a received line from the client.
runClientHandler :: Config -> MVar Env -> Handle -> Conn -> String -> IO ()
runClientHandler config env handle conn line = do
  now <- getCurrentTime
  modifyMVar_ env $ \env -> do
    (replies,env) <- runReaderT (runHulkIO $ handleLine config env now conn line)
                                config
    mapM_ (handleReplies handle) replies
    return env

-- | Act on replies from the client.
handleReplies :: Handle -> Reply -> IO ()
handleReplies handle reply = do
  case reply of
    MessageReply ref msg -> sendMessage ref msg
    LogReply line -> logLine line
    Close -> hClose handle
    Bump (Ref handle) -> hClose handle

-- | Send a message to a client.
sendMessage :: Ref -> Message -> IO ()
sendMessage (Ref handle) msg = do
  catch (UTF8.hPutStrLn handle (encode msg ++ "\r"))
        (\(_::IOException) -> hClose handle)

-- | Add a line to the log file.
logLine :: String -> IO ()
logLine = UTF8.putStrLn
