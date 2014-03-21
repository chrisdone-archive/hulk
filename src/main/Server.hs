{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Main where

import Network
import System.Console.CmdArgs
import System.Posix

import Hulk.Config  (getConfig)
import Hulk.Options (options,optionsConf)
import Hulk.Server  (start)
import Hulk.Types   ()

main :: IO ()
main = withSocketsDo $ do
  _ <- installHandler sigPIPE Ignore Nothing
  cmdArgs options >>= getConfig . optionsConf >>= start
