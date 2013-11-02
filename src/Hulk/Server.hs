{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

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
import           Control.Monad.Reader
import qualified Data.Map                 as M
import           Data.Monoid
import           Data.Text (Text,pack,unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time
import           Network
import           Network.FastIRC
import qualified Network.FastIRC.IO as IRC
import           Network.FastIRC.Messages (Command(..))
import           Prelude hiding (catch)
import           System.IO
import           System.IO.UTF8           as UTF8

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
    let conn = Conn { connRef = mkRef handle
                    , connHostname = pack host
                    , connServerName = configHostname config
                    , connTime = now
                    }
    _ <- forkIO $ handleClient config handle envar conn
    return ()

-- | Handle a client connection.
handleClient :: Config -> Handle -> MVar Env -> Conn -> IO ()
handleClient config handle env conn = do
  let run = runClientHandler config env handle conn
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
runClientHandler :: Config -> MVar Env -> Handle -> Conn -> Message -> IO ()
runClientHandler config env handle conn msg = do
  now <- getCurrentTime
  modifyMVar_ env $ \env -> do
    (replies,env) <- runReaderT (runHulkIO $ handleCommand config env now conn (msgCommand msg))
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
  catch (IRC.hPutMessage handle msg)
        (\(_::IOException) -> hClose handle)

-- | Add a line to the log file.
logLine :: Text -> IO ()
logLine = T.putStrLn
