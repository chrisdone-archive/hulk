{-# LANGUAGE OverloadedStrings #-}

module Hulk.Auth where

import Codec.Binary.UTF8.String
import Control.Arrow
import Control.Monad
import Data.Char
import Data.HMAC
import Data.Text (Text,pack,unpack)
import Numeric

import Hulk.Types

sha1 :: String -> String -> String
sha1 key str =
  concat $ map (\x -> showHex x "")
         $ hmac_sha1 (encode key) (encode str)

authenticate :: MonadProvider m => Text -> Text -> m Bool
authenticate user pass = do
  key <- filter keyChars `liftM` unpack `liftM` provideKey
  passwds <- liftM (getPasswds . unpack) $ providePasswords
  return $ any (== (unpack user,sha1 key (unpack pass))) passwds
  where getPasswds = map readPair . lines
            where readPair = second (drop 1) . span (/=' ')
        keyChars c = elem c ['a'..'z'] || isDigit c
