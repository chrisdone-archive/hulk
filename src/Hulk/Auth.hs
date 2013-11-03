{-# LANGUAGE OverloadedStrings #-}

module Hulk.Auth where

import Codec.Binary.UTF8.String
import Control.Arrow

import Data.Char
import Data.HMAC
import Data.Text (Text,pack,unpack)
import Numeric

sha1 :: String -> String -> String
sha1 key str =
  concat $ map (\x -> showHex x "")
         $ hmac_sha1 (encode key) (encode str)

authenticate :: String -> String -> Text -> Text -> Bool
authenticate keystr passwords user pass =
  any (== (unpack user,sha1 key (unpack pass)))
      passwds

  where key = filter keyChars keystr
        passwds = getPasswds passwords

        getPasswds = map readPair . lines
            where readPair = second (drop 1) . span (/=' ')
        keyChars c = elem c ['a'..'z'] || isDigit c
