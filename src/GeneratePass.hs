{-# LANGUAGE DeriveDataTypeable, RecordWildCards, ScopedTypeVariables #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing #-}
module Main where

import System.Console.CmdArgs
import Data.HMAC
import Codec.Binary.UTF8.String
import Numeric
import Data.String
import Control.Applicative

import Hulk.Types
import Hulk.Config

data Options = Options
  { conf      :: FilePath
  , user      :: String
  } deriving (Show,Data,Typeable)

options = Options
  { conf = def &= opt "hulk.conf" &= help "The config file."
  , user = "demo"
  }
  &= summary "Hulk IRC Daemon Password Generator (C) Chris Done 2011"
  &= help "Generates a password entry line for the Hulk passwd file."

optionsConf = conf

main = do
  options <- cmdArgs options
  config <- getConfig $ optionsConf options
  let keyFile = configPasswdKey config
  key <- takeWhile digilet <$> readFile keyFile
  pass <- filter (/='\n') <$> getLine
  putStrLn $ ((user options ++ " ") ++)
           $ concat $ map (\x -> showHex x "")
           $ hmac_sha1 (encode key) (encode pass)
