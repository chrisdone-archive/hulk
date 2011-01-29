{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-orphans #-}
module Hulk.Providers where

import Control.Applicative
import Control.Monad.Reader

import Hulk.Types

instance MonadProvider HulkIO where
  providePreface = maybeReadFile configPreface
  provideMotd = maybeReadFile configMotd
  provideKey = mustReadFile configPasswd
  providePasswords = mustReadFile configPasswdKey

maybeReadFile :: (Config -> Maybe FilePath) -> HulkIO (Maybe String)
maybeReadFile get = do 
  path <- asks get
  case path of
    Nothing -> return Nothing
    Just path -> Just <$> liftIO (readFile path)

mustReadFile :: (Config -> FilePath) -> HulkIO String
mustReadFile get = asks get >>= liftIO . readFile
