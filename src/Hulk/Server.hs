{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
module Hulk.Server (start) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Reader
import qualified Data.Map             as M
import           Network
import           Network.IRC
import           System.IO
import           System.IO.UTF8       as UTF8

import           Hulk.Providers ()
import           Hulk.Types


-- hs-plugins stuff
import System.Plugins
import StringProcAPI
import Control.Monad.Reader

import Hulk.Types
import qualified Data.Map as M
--

-- hs-plugins stuff
source = "StringProcPlugin.hs"
stub   = "Plugin.stub"
symbol = "resource"
--

firstBuild = do
  s <- makeWith source stub []
  case s of
    MakeSuccess _ obj -> do
      ls <- load obj ["."] [] symbol
      case ls of LoadSuccess m v -> return $ Right (m,v)
                 LoadFailure err -> return $ Left "load failed"
    MakeFailure e -> return $ Left $ show e
    
rebuild o@(m,plugin) = do
  s  <- makeWith source stub []   -- maybe recompile the source
  case s of
    MakeSuccess ReComp o -> do 
      ls <- reload m symbol
      case ls of LoadSuccess m' v' -> return $ Right (m',v')
                 LoadFailure err   -> return $ Left "reload failed"
    MakeSuccess NotReq _ -> return $ Right o
    MakeFailure e -> return $ Left $ show e

-- | Start an IRC server with the given configuration.
start :: Config -> IO ()
start config = withSocketsDo $ do
  o <- firstBuild
  o <- case o of Left e -> error e; Right x -> return x
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
    _ <- forkIO $ handleClient o config handle envar conn
    return ()

-- | Handle a client connection.
handleClient :: (Module,Handler) ->
                Config -> Handle -> MVar Env -> Conn -> IO ()
handleClient o config handle env conn = do
  let runHandle = runClientHandler o config env handle conn
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

-- | Handle a received line from the client.
runClientHandler :: (Module,Handler) ->
                    Config -> MVar Env -> Handle -> Conn -> String -> IO ()
runClientHandler o config env handle conn line = do
  modifyMVar_ env $ \env -> do
    to <- rebuild o
    case to of
      Left e -> do logLine $ "RELOAD ERROR: " ++ e
                   return env
      Right o@(_,handleLine) -> do
        (replies,env) <- runReaderT (runHulkIO $ handleLine env conn line) 
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

-- | Send a message to a client.
sendMessage :: Ref -> Message -> IO ()
sendMessage (Ref handle) msg = do
  catch (UTF8.hPutStrLn handle $ encode msg)
        (\_ -> hClose handle)

-- | Add a line to the log file.
logLine :: String -> IO ()
logLine = UTF8.putStrLn
