{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}
module Hulk.Server (start) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Reader
import qualified Data.ByteString      as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Map             as M
import           Network              hiding (accept)
import           Network.IRC          as IRC
import           Network.Socket       (accept)
import           Network.Socket.Util
import           OpenSSL.Session      (SSL)
import qualified OpenSSL.Session      as SSL
import           System.IO
import           System.IO.UTF8       as UTF8

import           Hulk.Client
import           Hulk.Providers       ()
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
    (socket,sockAddr) <- accept listenSock
    host <- sockAddrHost sockAddr
    _ <- forkOS $ do
      ctx <- initSSL config
      ssl <- SSL.connection ctx socket
      let conn = Conn { connRef = newRef ssl socket
                      , connHostname = host
                      , connServerName = configHostname config
                      }
      return ()
      SSL.accept ssl
      handleClient config ssl envar conn
    return ()

-- | Init SSL.
initSSL :: Config -> IO SSL.SSLContext
initSSL Config{..} = do
  ctx <- SSL.context
  SSL.contextSetPrivateKeyFile ctx configSSLKey
  SSL.contextSetCertificateFile ctx configCertificate
  ok <- SSL.contextCheckPrivateKey ctx
  unless ok $ error "Private key doesn't match certificate."
  return ctx

-- | Handle a client connection.
handleClient :: Config -> SSL -> MVar Env -> Conn -> IO ()
handleClient config ssl env conn = do
  let runHandle = runClientHandler config env ssl conn
  runHandle $ makeLine CONNECT []
  fix $ \loop -> do
    line <- catch (Right <$> sslGetUTF8Line ssl) (return . Left)
    case filter (not.newline) <$> line of
      Right []   -> loop
      Right line -> do runHandle (line++"\r"); loop
      Left _err  -> runHandle $ makeLine DISCONNECT ["Connection lost."]

  where newline c = c=='\n' || c=='\r'

-- | Get a line from the SSL connection and decode it to UTF8. Reads
-- one char at a time (should update this later.)
sslGetUTF8Line :: SSL -> IO String
sslGetUTF8Line ssl = go BS.empty where
  go acc = do
    c <- SSL.read ssl 1
    case UTF8.decode c of
      Just ('\n',_) -> return $ UTF8.toString acc
      Just{}        -> go $ BS.append acc c
      Nothing       -> go acc

-- | Make an internal IRC event to give to the client handler.
makeLine :: Event -> [String] -> String
makeLine event params = (++"\r") $ encode $
  Message { msg_prefix = Nothing
          , msg_command = show event
          , msg_params = params }

-- | Handle a received line from the client.
runClientHandler :: Config -> MVar Env -> SSL -> Conn -> String -> IO ()
runClientHandler config env ssl conn line = do
  modifyMVar_ env $ \env -> do
    (replies,env) <- runReaderT (runHulkIO $ handleLine env conn line) config
    mapM_ (handleReplies ssl) replies
    return env

-- | Act on replies from the client.
handleReplies :: SSL -> Reply -> IO ()
handleReplies ssl reply = do
  case reply of
    MessageReply ref msg -> sendMessage ref msg
    LogReply line -> logLine line
    Close -> SSL.shutdown ssl SSL.Unidirectional

-- | Send a message to a client.
sendMessage :: Ref -> Message -> IO ()
sendMessage (Ref ssl _) msg = do
  catch (SSL.write ssl $ UTF8.fromString $ IRC.encode msg)
        (\_ -> return ())

-- | Add a line to the log file.
logLine :: String -> IO ()
logLine = UTF8.putStrLn
