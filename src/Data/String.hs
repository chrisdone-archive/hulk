module Data.String where

import Data.List
import Data.Char

downcase = map toLower

digilet c = isDigit c || isLetter c