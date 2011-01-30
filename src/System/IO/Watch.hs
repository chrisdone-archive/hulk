module System.IO.Watch where

import Control.Concurrent.Delay
import Control.Monad
import System.Directory

onModify :: FilePath -> IO () -> IO ()
onModify path m = do
  t <- getModificationTime path
  go t
  where go oldT = do
          delaySeconds 2
          newT <- getModificationTime path
          when (oldT < newT) m
          go newT
