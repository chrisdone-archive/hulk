{-# LANGUAGE OverloadedStrings #-}

module Hulk.Providers where

import           Hulk.Types

import           Control.Applicative
import           Control.Monad.Reader
import           Data.Char
import           Data.Maybe
import           Data.Text (Text,pack,unpack)
import qualified Data.Text.IO as T
import           Data.Time
import           Data.Time.JSON
import           Prelude hiding (readFile)
import           System.Directory
import           System.FilePath
import           System.IO hiding (readFile)
import           System.IO.Strict (readFile)
import           Text.JSON as JSON

instance MonadProvider HulkIO where
  providePreface = maybeReadFile configPreface
  provideMotd = maybeReadFile configMotd
  provideKey = mustReadFile configPasswdKey
  providePasswords = mustReadFile configPasswd
  provideWriteUser udata = do
    path <- asks configUserData
    liftIO $ writeFile (path </> normalizeUser (unpack (userDataUser udata))) $ encode udata
  provideUser name = do
    path <- asks configUserData
    let fname = path </> normalizeUser (unpack name)
    now <- liftIO $ getCurrentTime
    exists <- liftIO $ doesFileExist fname
    if exists
       then do contents <- liftIO $ readFile fname
               case decode contents of
                 Ok u -> return u
                 JSON.Error e -> error e
       else return $ UserData name (DateTime now)
  provideLogger name rpl params = do
    path <- asks configLogFile
    now <- liftIO $ getCurrentTime
    liftIO $
      appendFile path $ encode ([showJSON $ DateTime now
                                ,showJSON name
                                ,showJSON rpl
                                ,showJSON params])
                        ++ "\n"
  provideLog = do
    path <- asks configLogFile
    contents <- liftIO $ readFile path
    return $ mapMaybe parse $ lines contents
      where parse line =
             case decode line of
               Ok event -> Just event
               _ -> Nothing

normalizeUser :: [Char] -> [Char]
normalizeUser = filter (\c -> isDigit c || isLetter c)

maybeReadFile :: (Config -> Maybe FilePath) -> HulkIO (Maybe Text)
maybeReadFile get = do
  path <- asks get
  case path of
    Nothing -> return Nothing
    Just path -> Just <$> liftIO (T.readFile path)

mustReadFile :: (Config -> FilePath) -> HulkIO Text
mustReadFile get = asks get >>= liftIO . T.readFile
