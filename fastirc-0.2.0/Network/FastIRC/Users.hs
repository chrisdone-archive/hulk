-- |
-- Module:     Network.FastIRC.Users
-- Copyright:  (c) 2010 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
-- Stability:  alpha
--
-- This module includes parsers for IRC users.

module Network.FastIRC.Users
  ( UserSpec(..),
    userIsServer,
    showUserSpec,
    userParser )
  where

import qualified Data.ByteString.Char8 as B
import Control.Applicative
import Data.Attoparsec.Char8 as P
import Network.FastIRC.ServerSet
import Network.FastIRC.Types
import Network.FastIRC.Utils


-- | IRC user or server.

data UserSpec
  -- | Nickname.
  = Nick NickName
  -- | Nickname, username and hostname.
  | User NickName UserName HostName
  deriving (Eq, Read, Show)


-- | Check whether a given nickname is a server.

userIsServer :: UserSpec -> ServerSet -> Bool
userIsServer (User _ _ _) _ = False
userIsServer (Nick nick) servers = isServer nick servers


-- | Turn a 'UserSpec' into a 'B.ByteString' in a format suitable to be
-- sent to the IRC server.

showUserSpec :: UserSpec -> MsgString
showUserSpec (Nick n) = n
showUserSpec (User n u h) = B.concat [ n, B.cons '!' u, B.cons '@' h ]


-- | A 'Parser' for IRC users and servers.

userParser :: Parser UserSpec
userParser =
  try full <|> nickOnly

  where
    full :: Parser UserSpec
    full =
      User <$> P.takeWhile1 isNickChar <* char '!'
           <*> P.takeWhile1 isUserChar <* char '@'
           <*> P.takeWhile1 isHostChar

    nickOnly :: Parser UserSpec
    nickOnly = Nick <$> P.takeWhile1 isNickChar
