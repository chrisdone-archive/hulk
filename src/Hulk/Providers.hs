{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-orphans #-}
module Hulk.Providers where

import Control.Applicative
import Control.Monad.Reader
import Data.Char
import Data.Maybe
import Data.Time
import Data.Time.JSON
import System.FilePath
import System.Directory
import System.IO hiding (readFile)
import Prelude hiding (readFile)
import System.IO.Strict (readFile)
import Text.JSON as JSON

import Hulk.Types

instance MonadProvider HulkIO where
  providePreface = maybeReadFile configPreface
  provideMotd = maybeReadFile configMotd
  provideKey = mustReadFile configPasswdKey
  providePasswords = mustReadFile configPasswd
  provideWriteUser udata = do
    path <- asks configUserData
    liftIO $ writeFile (path </> normalizeUser (userDataUser udata)) $ encode udata
  provideUser name = do
    path <- asks configUserData
    let fname = path </> normalizeUser name
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
normalizeUser = takeWhile (\c -> isDigit c || isLetter c)

maybeReadFile :: (Config -> Maybe FilePath) -> HulkIO (Maybe String)
maybeReadFile get = do 
  path <- asks get
  case path of
    Nothing -> return Nothing
    Just path -> Just <$> liftIO (readFile path)

mustReadFile :: (Config -> FilePath) -> HulkIO String
mustReadFile get = asks get >>= liftIO . readFile
