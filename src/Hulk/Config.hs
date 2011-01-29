{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing #-}
module Hulk.Config
    (Config(..)
    ,getConfig)
    where

import Control.Monad.Reader
import Data.Word
import Data.ConfigFile

import Hulk.Types

getConfig :: FilePath -> IO Config
getConfig conf = do
  contents <- readFile conf
  let config = do
        c <- readstring emptyCP contents
        hostname <- get c "LISTEN" "hostname"
        listen <- get c "LISTEN" "port"
        motd <- get c "STRINGS" "motd_file"
        preface <- get c "STRINGS" "preface_file"
        passwd <- get c "AUTH" "passwd_file"
        key <- get c "AUTH" "passwd_key"
        return Config { configListen = fromIntegral (listen::Word16)
                      , configMotd = Just motd 
                      , configHostname = hostname
                      , configPasswd = passwd
                      , configPasswdKey = key
                      , configPreface = Just preface
                      }
  case config of
    Left cperr -> error $ show cperr
    Right config -> return config
