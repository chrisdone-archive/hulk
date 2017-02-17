{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:     Network.FastIRC.Session
-- Copyright:  (c) 2010 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
-- Stability:  alpha
--
-- This module implements a framework for IRC client software.
-- Essentially it consists of a dumb bot, which connects to and stays on
-- an IRC server waiting for commands.
--
-- Using the 'onEvent' function (or the convenience functions
-- 'onConnect', 'onDisconnect', etc.) you can attach event handlers to
-- certain events.  These event handlers are run in the 'Bot' monad,
-- which encapsulates the current state of the bot.
--
-- Please note that even though unlikely you should expect that parts of
-- this interface will be changed in future revisions.

module Network.FastIRC.Session
  ( -- * Types
    Bot,
    BotCommand(..),
    BotInfo(..),
    BotSession,
    Event(..),
    EventHandler,
    Params(..),

    -- * Functions
    ircSendCmd,
    ircSendMsg,
    ircSendString,
    onEvent,
    sendBotCmd,
    startBot,

    -- * Event utility functions
    onConnect,
    onDisconnect,
    onError,
    onLoggedIn,
    onMessage,
    onQuit,

    -- * Bot monad
    getBotInfo
  )
  where

import Control.Exception (catch)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Control.Applicative
import Control.Concurrent
import Data.Map (Map)
import Data.Unique
import MonadLib
import Network.Fancy
import Network.FastIRC.IO
import Network.FastIRC.Messages
import Network.FastIRC.ServerSet
import Network.FastIRC.Types
import System.IO


-- | Bot monad.

type Bot = ContT () (StateT Config (ReaderT Params IO))


-- | Commands to be sent to the bot.

data BotCommand
  -- | Add an event handler.
  = BotAddHandler (EventHandler -> IO ()) (Event -> Bot ())
  | BotDispatch Event          -- ^ Dispatch simulated event.
  | BotError String            -- ^ Simulate an error.
  | BotQuit (Maybe CommandArg) -- ^ Send a quit message.
  | BotRecv Message            -- ^ Simulate receiving of a message.
  | BotSendCmd Command         -- ^ Send a command to the IRC server.
  | BotSendMsg Message         -- ^ Send a message to the IRC server.
  | BotSendString MsgString    -- ^ Send a raw string to the IRC server.
  | BotTerminate               -- ^ Immediately kill the bot.


-- | Runtime bot information.

data BotInfo =
  BotInfo {
    botCurrentNick :: Maybe NickName
  }


-- | Bot session descriptor.

data BotSession =
  BotSession {
    botCmdChan :: Chan BotCommand  -- ^ Command channel.
  }


-- | Bot configuration at run-time.

data Config =
  Config {
    -- | Event handlers.
    botEventHandlers :: Map EventHandler (Event -> Bot ()),
    botEventChan     :: Chan Event, -- ^ Event channel.
    botHandle        :: Handle,     -- ^ Connection handle.
    botInfo          :: BotInfo,    -- ^ Current information.
    botIsQuitting    :: Bool,       -- ^ Quit command issued?
    botKillerThread  :: Maybe ThreadId, -- ^ Killer thread.
    botServers       :: ServerSet,  -- ^ Nicknames known to be servers.
    botSession       :: BotSession  -- ^ Session information.
  }


-- | A bot event.

data Event
  = ConnectedEvent       -- ^ Bot connected.
  | DisconnectedEvent    -- ^ Bot disconnected (either error or on demand).
  | ErrorEvent String    -- ^ Connection failed or disconnected on error.
  | LoggedInEvent        -- ^ Bot logged in (received numeric 001).
  | MessageEvent Message -- ^ Received message from server.
  | QuitEvent            -- ^ Bot disconnected on demand.
  deriving (Eq, Read, Show)


-- | Event handler identifier.

type EventHandler = Unique


-- | Parameters for an IRC client connection.

data Params =
  Params {
    botGetNick     :: IO NickName,      -- ^ IRC nick name generator.
    botGetUser     :: IO UserName,      -- ^ IRC user name generator.
    botGetRealName :: IO RealName,      -- ^ IRC real name generator.
    botPassword    :: Maybe CommandArg, -- ^ IRC server password.
    botServerAddr  :: Address           -- ^ IRC server address.
  }


-- | Core bot management thread.

botManager :: Params -> Config -> IO ()
botManager params cfg = do
  -- Initialize bot.
  let eventChan = botEventChan $ cfg
      cmdChan = botCmdChan . botSession $ cfg
      h = botHandle $ cfg

  writeChan eventChan ConnectedEvent

  dispatchThread <- forkIO $
    getChanContents eventChan >>= writeList2Chan cmdChan . map BotDispatch

  netThread <- forkIO $ networkHandler cmdChan (botHandle cfg)

  -- Main loop.
  runBot params cfg $ do
    sendLogin
    forever $ do
      bcmd <- inBase $ readChan cmdChan
      case bcmd of
        BotAddHandler reportId f -> do
          hid <- inBase newUnique
          handlers <- botEventHandlers <$> get
          sets_ (\cfg -> cfg { botEventHandlers = M.insert hid f handlers })
          inBase $ reportId hid

        BotDispatch ev -> do
          handlerList <- M.elems . botEventHandlers <$> get
          mapM_ ($ ev) handlerList

        BotError err -> do
          isQuitting <- botIsQuitting <$> get
          unless isQuitting . inBase . writeChan eventChan $ ErrorEvent err
          die

        BotQuit reason -> do
          inBase $ hPutCommand h (QuitCmd reason)
          ktid <- inBase . forkIO $
                    threadDelay 1000000 >>
                    writeChan cmdChan BotTerminate
          sets_ $ \cfg -> cfg { botIsQuitting = True,
                                botKillerThread = Just ktid }

        BotRecv msg ->
          inBase (writeChan eventChan $ MessageEvent msg) >>
          handleMsg msg

        BotSendCmd cmd    -> inBase $ hPutCommand h cmd
        BotSendMsg msg    -> inBase $ hPutMessage h msg
        BotSendString str -> inBase $ B.hPutStr h str
        BotTerminate      -> die

  -- Clean up.
  killThread dispatchThread
  killThread netThread

  where
    networkHandler :: Chan BotCommand -> Handle -> IO ()
    networkHandler cmdChan h = do
      res <- try $ hGetMessage h
      case res of
        Left err  -> writeChan cmdChan $ BotError (show err)
        Right msg ->
          writeChan cmdChan (BotRecv msg) >>
          networkHandler cmdChan h

    die :: Bot ()
    die = do
      isQuitting <- botIsQuitting <$> get
      ktidM <- botKillerThread <$> get
      handlerList <- M.elems . botEventHandlers <$> get
      when isQuitting $ mapM_ ($ QuitEvent) handlerList
      case ktidM of
        Just ktid -> inBase $ killThread ktid
        Nothing   -> return ()
      mapM_ ($ DisconnectedEvent) handlerList
      abort ()


-- | Default bot information.

defBotInfo :: BotInfo
defBotInfo =
  BotInfo { botCurrentNick = Nothing }


-- | Handle an incoming IRC message.

handleMsg :: Message -> Bot ()
handleMsg msg = do
  h <- botHandle <$> get
  let origin = msgOrigin msg
      cmd    = msgCommand msg
  eventChan <- botEventChan <$> get

  case cmd of
    NumericCmd 1 (myNick:_) -> do
      inBase $ writeChan eventChan LoggedInEvent
      sets_ $ \cfg -> let bi = (botInfo cfg) { botCurrentNick = Just myNick }
                      in cfg { botInfo = bi }

    PingCmd a b -> inBase $ hPutCommand h (PongCmd a b)

    _ -> return ()


-- | Send a command to the IRC server.

ircSendCmd :: BotSession -> Command -> IO ()
ircSendCmd bs = sendBotCmd bs . BotSendCmd


-- | Send a message (with origin) to the IRC server.  Note that IRC
-- servers ignore the origin prefix, so in general you would want to use
-- 'ircSendCmd' instead.

ircSendMsg :: BotSession -> Message -> IO ()
ircSendMsg bs = sendBotCmd bs . BotSendMsg


-- | Send a raw message string to the IRC server.  This is what most IRC
-- clients call /quote.

ircSendString :: BotSession -> MsgString -> IO ()
ircSendString bs = sendBotCmd bs . BotSendString


-- | Add an event handler.

onEvent :: BotSession -> (Event -> Bot ()) -> IO EventHandler
onEvent bs f = do
  let cmdChan = botCmdChan bs
  answerVar <- newEmptyMVar
  writeChan cmdChan $ BotAddHandler (putMVar answerVar) f
  takeMVar answerVar


-- | Run a 'Bot' monad computation.

runBot :: Params -> Config -> Bot () -> IO ()
runBot params cfg =
  fmap fst .
  runReaderT params .
  runStateT cfg .
  runContT return


-- | Send bot command to a bot.

sendBotCmd :: BotSession -> BotCommand -> IO ()
sendBotCmd bs cmd = writeChan (botCmdChan bs) cmd


-- | Send login commands.

sendLogin :: Bot ()
sendLogin = do
  h <- botHandle <$> get
  nick <- asks botGetNick >>= inBase
  user <- asks botGetUser >>= inBase
  real <- asks botGetRealName >>= inBase
  addr <- asks botServerAddr
  pass <- asks botPassword
  let (host, port) =
        case addr of
          IP h p   -> (B.pack h, B.pack $ show p)
          IPv4 h p -> (B.pack h, B.pack $ show p)
          IPv6 h p -> (B.pack h, B.pack $ show p)
          _        -> ("localhost", "6667")

  inBase $ do
    case pass of
      Just pwd -> hPutCommand h $ PassCmd pwd
      Nothing  -> return ()

    hPutCommand h $ NickCmd nick Nothing
    hPutCommand h $ UserCmd user host port real


-- | Launch an IRC bot.

startBot :: Params -> IO (Either IOError BotSession)
startBot params = do
  cmdChan <- newChan
  eventChan <- newChan
  errorVar <- newEmptyMVar
  let session = BotSession { botCmdChan = cmdChan }

  forkIO $
    let comp =
          withStream (botServerAddr params) $ \h ->
            let cfg =
                  Config {
                    botEventHandlers = M.empty,
                    botEventChan = eventChan,
                    botHandle = h,
                    botInfo = defBotInfo,
                    botIsQuitting = False,
                    botKillerThread = Nothing,
                    botServers = emptyServers,
                    botSession = session
                  }
            in do
              hSetBuffering h NoBuffering
              putMVar errorVar Nothing
              res <- try $ botManager params cfg
              case res of
                Left err -> do
                  hPutStrLn stderr "Warning (fastirc): unexpected exception:"
                  hPrint stderr err
                  hPutStrLn stderr "Please report this to the author."
                Right _ -> return ()
    in comp `catch` (putMVar errorVar . Just)

  error <- takeMVar errorVar
  case error of
    Nothing  -> return (Right session)
    Just err -> return (Left err)


-- | Action to run on connect.
onConnect :: BotSession -> Bot () -> IO EventHandler
onConnect bs c = onEvent bs $ \ev -> case ev of ConnectedEvent -> c; _ -> return ()

-- | Action to run on disconnect.
onDisconnect :: BotSession -> Bot () -> IO EventHandler
onDisconnect bs c = onEvent bs $ \ev -> case ev of DisconnectedEvent -> c; _ -> return ()

-- | Action to run on error (connection failed/aborted).
onError :: BotSession -> (String -> Bot ()) -> IO EventHandler
onError bs f = onEvent bs $ \ev -> case ev of ErrorEvent str -> f str; _ -> return ()

-- | Action to run after login (numeric 001 received).
onLoggedIn :: BotSession -> Bot () -> IO EventHandler
onLoggedIn bs c = onEvent bs $ \ev -> case ev of LoggedInEvent -> c; _ -> return ()

-- | Action to run when a message arrives.
onMessage :: BotSession -> (Message -> Bot ()) -> IO EventHandler
onMessage bs f = onEvent bs $ \ev -> case ev of MessageEvent msg -> f msg; _ -> return ()

-- | Action to run on quit.
onQuit :: BotSession -> Bot () -> IO EventHandler
onQuit bs c = onEvent bs $ \ev -> case ev of QuitEvent -> c; _ -> return ()


-- | Get current bot information.

getBotInfo :: Bot BotInfo
getBotInfo = botInfo <$> get
