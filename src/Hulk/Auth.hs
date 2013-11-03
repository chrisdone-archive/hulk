-- | Password authorization for the server.

module Hulk.Auth
  (authenticate
  ,sha1)
  where

import Codec.Binary.UTF8.String
import Control.Arrow

import Data.Char
import Data.HMAC
import Data.Text (Text,unpack)
import Numeric

-- | Authenticate the user.
authenticate :: String -- ^ Salt.
             -> String -- ^ Password file.
             -> Text   -- ^ User.
             -> Text   -- ^ Password.
             -> Bool   -- ^ Authenticated?
authenticate keystr passwords user pass =
  any (== (unpack user,sha1 key (unpack pass)))
      passwds

  where key = filter keyChars keystr
        passwds = getPasswds passwords

        getPasswds = map readPair . lines
            where readPair = second (drop 1) . span (/=' ')
        keyChars c = elem c ['a'..'z'] || isDigit c

-- | Make a sha1 string with the given salt.
sha1 :: String -- ^ Salt.
     -> String -- ^ String.
     -> String -- ^ Hashed string.
sha1 key str =
  concat $ map (\x -> showHex x "")
         $ hmac_sha1 (encode key) (encode str)
