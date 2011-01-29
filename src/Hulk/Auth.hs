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

authenticate :: (Functor m,MonadProvider m) => String -> String -> m Bool
authenticate user pass = do
  key <- provideKey
  passwds <- fmap getPasswds $ providePasswords
  return $ any (== (user,sha1 key pass)) passwds
  where getPasswds = map readPair . lines
            where readPair = second (drop 1) . span (/=' ')
