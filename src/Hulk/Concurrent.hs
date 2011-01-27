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
