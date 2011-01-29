module Hulk.Event where

import Hulk.Types

readEventType :: String -> Event
readEventType str = 
  case reads str of
    [(a,"")] -> a
    _        -> NOTHING
