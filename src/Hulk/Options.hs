{-# LANGUAGE DeriveDataTypeable, RecordWildCards, ScopedTypeVariables #-} 
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Hulk.Options (Options
                    ,options
                    ,optionsConf) where

import System.Console.CmdArgs

data Options = Options
  { conf      :: FilePath
  } deriving (Show,Data,Typeable)

options = Options
  { conf = def &= opt "hulk.conf" &= help "The config file."
  }
  &= summary "Hulk IRC Daemon (C) Chris Done 2011"
  &= help "Runs an IRC server based on the provided configuration file."

optionsConf = conf