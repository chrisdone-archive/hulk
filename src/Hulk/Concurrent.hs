module Hulk.Concurrent where

import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.IO

import Hulk.Types

fork :: Hulk a -> Hulk ThreadId
fork m = do
  env <- ask
  io $ forkIO $ do _ <- runReaderT (runHulk m) env
                   return ()

withVar :: MVar a -> (a -> Hulk b) -> Hulk b
withVar var m = do
  env <- ask
  io $ withMVar var $ \var -> runReaderT (runHulk (m var)) env
