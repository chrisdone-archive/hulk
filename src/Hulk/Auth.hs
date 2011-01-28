module Hulk.Auth where

import Control.Arrow
import Data.HMAC
import Codec.Binary.UTF8.String
import Numeric
import Control.Monad.IO
import Data.String
import Control.Applicative

import Hulk.Types
import Hulk.Config

sha1 :: String -> String -> String
sha1 key str =
  concat $ map (\x -> showHex x "")
         $ hmac_sha1 (encode key) (encode str)

authenticate :: String -> String -> Hulk Bool
authenticate user pass = do
  keyFile <- config configPasswdKey
  key <- io $ takeWhile digilet <$> readFile keyFile
  passwdFile <- config configPasswd
  passwds <- fmap getPasswds $ io $ readFile passwdFile
  return $ any (== (user,sha1 key pass)) passwds
  where getPasswds = map readPair . lines
            where readPair = second (drop 1) . span (/=' ')
