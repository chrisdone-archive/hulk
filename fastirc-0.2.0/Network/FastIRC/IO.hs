{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:     Network.FastIRC.IO
-- Copyright:  (c) 2010 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
-- Stability:  alpha
--
-- This module helps you with doing input and output on IRC connections
-- or even log files.

module Network.FastIRC.IO
  ( hGetIRCLine,
    hGetMessage,
    hPutCommand,
    hPutMessage
  )
  where

import qualified Data.ByteString.Char8 as B
import Network.FastIRC.Messages
import Network.FastIRC.Types
import Network.FastIRC.Utils
import System.IO


-- | Read an IRC message string.

hGetIRCLine :: Handle -> IO MsgString
hGetIRCLine h = getl B.empty
  where
    getl :: MsgString -> IO MsgString
    getl buf = do
      c <- hGetChar h
      if isIRCEOLChar c
        then return buf
        else getl (B.snoc buf c)


-- | Read the next valid IRC message.

hGetMessage :: Handle -> IO Message
hGetMessage h = do
  line <- hGetIRCLine h
  if B.null line
    then hGetMessage h
    else
      case readMessage line of
        Just msg -> return msg
        Nothing  -> hGetMessage h


-- | Write an IRC command with no origin.

hPutCommand :: Handle -> Command -> IO ()
hPutCommand h cmd =
  B.hPutStr h $ B.append (showCommand cmd) "\r\n"


-- | Write an IRC message.

hPutMessage :: Handle -> Message -> IO ()
hPutMessage h msg =
  B.hPutStr h $ B.append (showMessage msg) "\r\n"
