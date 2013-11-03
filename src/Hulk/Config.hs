-- | Configuration parser for the server.

module Hulk.Config
    (Config(..)
    ,getConfig)
    where

import Hulk.Types

import Data.Word
import Data.ConfigFile
import Data.Text (pack)

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
        users <- get c "USERS" "data_dir"
        logs <- get c "LOGS" "event_log"
        chans <- fmap words $ get c "LOGS" "channels"
        return Config
          { configListen    = fromIntegral (listen::Word16)
          , configMotd      = Just motd
          , configHostname  = pack hostname
          , configPasswd    = passwd
          , configPasswdKey = key
          , configPreface   = Just preface
          , configUserData  = users
          , configLogFile   = logs
          , configLogChans  = map pack chans
          }
  case config of
    Left cperr   -> error $ show cperr
    Right config' -> return config'
