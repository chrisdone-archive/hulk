module Network.Socket.Util where

import Network.Socket
import Network.Socket.Internal
import Network.BSD

sockAddrHost :: SockAddr -> IO String
sockAddrHost ~(SockAddrInet port haddr) = do
  flip catch (\e -> inet_ntoa haddr) $ do
    (HostEntry peer _ _ _) <- getHostByAddr AF_INET haddr
    return peer
