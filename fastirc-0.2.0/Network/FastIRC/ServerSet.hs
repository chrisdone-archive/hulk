-- |
-- Module:     Network.FastIRC.ServerSet
-- Copyright:  (c) 2010 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
-- Stability:  alpha
--
-- Functions for dealing with sets of IRC servers.  Note that servers
-- are compared case-insensitively.

module Network.FastIRC.ServerSet
  ( -- * The server set type
    ServerSet,

    -- * Manipulation
    addServer,
    delServer,
    emptyServers,
    isServer,

    -- * Conversion
    serversFromList,
    serversToList
  )
  where

import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S
import Data.Char
import Network.FastIRC.Types


-- | A set of servers.  This data type uses 'S.Set' internally, but
-- the strings are handled case-insensitively.

newtype ServerSet = ServerSet (S.Set ServerName)


-- | Empty set of servers.

emptyServers :: ServerSet
emptyServers = ServerSet S.empty


-- | Add a server to a 'ServerSet'.

addServer :: ServerName -> ServerSet -> ServerSet
addServer s (ServerSet ss) = ServerSet $ S.insert (B.map toLower s) ss


-- | Remove a server from a 'ServerSet'.

delServer :: ServerName -> ServerSet -> ServerSet
delServer s (ServerSet ss) = ServerSet $ S.delete (B.map toLower s) ss


-- | Check whether specified server is in the set.

isServer :: ServerName -> ServerSet -> Bool
isServer s (ServerSet ss) = S.member (B.map toLower s) ss


-- | Build from list.

serversFromList :: [ServerName] -> ServerSet
serversFromList = ServerSet . S.fromList


-- | Convert to list.

serversToList :: ServerSet -> [ServerName]
serversToList (ServerSet ss) = S.toList ss
