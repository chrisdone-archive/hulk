module Hulk.Log 
    (doing
    ,tell
    ,performing
    ,incoming
    ,outgoing
    ,note)
    where

import Control.Monad.Reader
import Control.Concurrent
import System.IO
import Control.Monad.IO

import Hulk.Types
import Hulk.Config

instance Loggable Hulk where
  tell str = do
    h <- asks envTellHandle
    col <- (*2) `fmap` asks envLogColumn
    io $ withMVar h $ \h -> hPutStrLn h $ replicate col ' ' ++ str

instance Loggable IRC where
  tell str = liftHulk $ tell str

performing :: String -> Hulk a -> Hulk a
performing str m = do 
  doing str
  v <- indent  m
  tell "Done."
  return v

doing :: String -> Hulk ()
doing str = tell $ str ++ " ..."

indent :: Hulk a -> Hulk a
indent = local inc where
  inc env = env { envLogColumn = envLogColumn env + 1 }

incoming :: String -> IRC ()
incoming str = tell $ "<- " ++ str

outgoing :: String -> IRC ()
outgoing str = tell $ "-> " ++ str

note :: String -> IRC ()
note str = tell $ "Notice: " ++ str
