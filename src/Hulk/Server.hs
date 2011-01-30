{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
module Hulk.Server (start) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO
import           Control.Monad.Reader
import qualified Data.Map                     as M
import           Language.Haskell.Interpreter
import           Network
import           Network.IRC
import           System.IO
import           System.IO.UTF8               as UTF8
import           System.IO.Watch

import           Hulk.Client
import           Hulk.Providers               ()
import           Hulk.Types

-- | Start an IRC server with the given configuration.
start :: Config -> IO ()
start config = withSocketsDo $ do
  handler <- newMVar handleLine
  _ <- forkIO $ runHandlerReloader handler
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
    _ <- forkIO $ handleClient handler config handle envar conn
    return ()

-- | Handle a client connection.
handleClient :: MVar Handler -> Config -> Handle -> MVar Env -> Conn -> IO ()
handleClient handler config handle env conn = do
  let runHandle = runClientHandler handler config env handle conn
  runHandle $ makeLine CONNECT []
  fix $ \loop -> do
    line <- catch (Right <$> UTF8.hGetLine handle) (return . Left)
    case filter (not.newline) <$> line of
      Right []   -> loop
      Right line -> do runHandle (line++"\r"); loop
      Left _err  -> runHandle $ makeLine DISCONNECT ["Connection lost."]

  where newline c = c=='\n' || c=='\r'

-- | Make an internal IRC event to give to the client handler.
makeLine :: Event -> [String] -> String
makeLine event params = (++"\r") $ encode $
  Message { msg_prefix = Nothing
          , msg_command = show event
          , msg_params = params }

-- | Watch the Client module for changes. When it changes, reload it
-- and update the client handler MVar.
runHandlerReloader :: MVar Handler -> IO ()
runHandlerReloader handlerVar = do
  update <- newEmptyMVar
  _ <- forkIO $ onModify "Hulk/Client.hs" $ putMVar update ()
  _ <- runInterpreter $ do
    let c = ["Hulk.Providers","Hulk.Types","Hulk.Client"]
        reload = do loadModules c; setImports $ "Prelude" : c
        log = io . logLine
        reloader = do
          log "Watching for module changes ..."
          () <- io $ takeMVar update
          log "Module modified, reloading ..."
          reset
          reload
          log "Loaded. Interpreting function ..."
          f <- interpret "handleLine" (as :: Handler)
          log "Interpreted. Updating function ..."
          io $ modifyMVar_ handlerVar $ const $ return f
          log "Updated."
          reloader
    reload
    reloader
  return ()

hintTest :: IO ()
hintTest = do
  e <- runInterpreter $ do liftIO $ UTF8.putStrLn "Loading."
                           let c = ["Hulk.Providers",
                                    "Hulk.Types","Hulk.Client"]
                           loadModules c; setImports $ "Prelude" : c
                           liftIO $ UTF8.putStrLn "Interpreting"
                           f <- interpret "handleLine" (as :: Handler)
                           liftIO $ UTF8.putStrLn "Loaded."
  UTF8.putStrLn $ show e

-- | Handle a received line from the client.
runClientHandler :: MVar Handler -> Config -> MVar Env -> Handle -> Conn 
                 -> String
                 -> IO ()
runClientHandler handler config env handle conn line = do
  withMVar handler $ \handleLine ->
    modifyMVar_ env $ \env -> do
      logLine "About to use function ..."
      (replies,env) <- flip runReaderT config $
                         runHulkIO $ handleLine env conn line
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
