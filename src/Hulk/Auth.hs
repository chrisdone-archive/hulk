{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
module Hulk.Auth where

import Codec.Binary.UTF8.String
import Control.Arrow
import Control.Monad
import Data.Char
import Data.HMAC
import Numeric

import Hulk.Types

sha1 :: String -> String -> String
sha1 key str =
  concat $ map (\x -> showHex x "")
         $ hmac_sha1 (encode key) (encode str)

authenticate :: MonadProvider m => String -> String -> m Bool
authenticate user pass = do
  key <- filter keyChars `liftM` provideKey
  passwds <- liftM getPasswds $ providePasswords
  return $ any (== (user,sha1 key pass)) passwds
  where getPasswds = map readPair . lines
            where readPair = second (drop 1) . span (/=' ')
        keyChars c = elem c ['a'..'z'] || isDigit c