module Control.Concurrent.Delay where

import Control.Concurrent

delaySeconds :: Integer -> IO ()
delaySeconds 0 = return ()
delaySeconds n = do threadDelay (1000 * 1000); delaySeconds (n-1)

delayMinutes :: Integer -> IO ()
delayMinutes = delaySeconds . (*60)
