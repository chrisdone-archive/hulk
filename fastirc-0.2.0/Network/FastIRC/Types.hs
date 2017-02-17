-- |
-- Module:     Network.FastIRC.Types
-- Copyright:  (c) 2010 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
-- Stability:  alpha
--
-- A number of convenient type aliases.

module Network.FastIRC.Types
  ( -- * Types
    ChannelKey,
    ChannelName,
    CommandArg,
    CommandName,
    HostName,
    MsgString,
    NickName,
    RealName,
    ServerName,
    TargetName,
    UserName
  )
  where

import qualified Data.ByteString.Char8 as B


type ChannelKey  = B.ByteString
type ChannelName = B.ByteString
type CommandArg  = B.ByteString
type CommandName = B.ByteString
type HostName    = B.ByteString
type MsgString   = B.ByteString
type NickName    = B.ByteString
type RealName    = B.ByteString
type ServerName  = B.ByteString
type TargetName  = B.ByteString
type UserName    = B.ByteString
