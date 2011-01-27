{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing #-}
module Hulk.Config
    (Config(..)
    ,getConfig
    ,config)
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
        return Config { configListen = fromIntegral (listen::Word16)
                      , configMotd = Just motd 
                      , configHostname = hostname
                      }
  case config of
    Left cperr -> error $ show cperr
    Right config -> return config

config :: (Config -> a) -> Hulk a
config f = asks $ f . envConfig
