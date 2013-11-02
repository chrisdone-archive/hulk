{-# LANGUAGE OverloadedStrings #-}

module Hulk.Providers where

import           Hulk.Types

import           Control.Applicative
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.CaseInsensitive       (mk)
import           Data.Char
import           Data.Maybe
import           Data.Monoid
import           Data.Text                  (Text, pack, unpack)
import qualified Data.Text.IO               as T
import           Data.Time
import           Prelude                    hiding (readFile)
import           System.Directory
import           System.FilePath
import           System.IO                  hiding (readFile)
import           System.IO.Strict           (readFile)

instance MonadProvider HulkIO where
  providePreface = maybeReadFile configPreface
  provideMotd = maybeReadFile configMotd
  provideKey = mustReadFile configPasswdKey
  providePasswords = mustReadFile configPasswd
  provideWriteUser udata = do
    path <- asks configUserData
    liftIO $ L.writeFile (path </> normalizeUser (unpack (userText (userDataUser udata)))) $ encode udata
  provideUser name = do
    path <- asks configUserData
    let fname = path </> normalizeUser (unpack name)
    now <- liftIO $ getCurrentTime
    exists <- liftIO $ doesFileExist fname
    if exists
       then do contents <- liftIO $ L.readFile fname
               case decode contents of
                 Just u -> return u
                 Nothing -> error ("unable to parse user file: " ++ fname)
       else return $ UserData (UserName (mk name)) now
  provideLogger name rpl params = do
    path <- asks configLogFile
    now <- liftIO $ getCurrentTime
    liftIO $
      L.appendFile path $ encode ((now
                                  ,name
                                  ,rpl
                                  ,params))
                          <> "\n"
  provideLog = do
    path <- asks configLogFile
    contents <- liftIO $ L.readFile path
    return $ mapMaybe decode $ L8.lines contents

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
