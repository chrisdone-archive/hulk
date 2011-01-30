module Control.Concurrent.Delay where

import Control.Monad.Trans
import Control.Concurrent

-- | Delay the thread for at least n seconds.
delaySeconds :: MonadIO m => Integer -> m ()
delaySeconds 0 = return ()
delaySeconds n = do liftIO $ threadDelay $ 1000 * 1000
                    delaySeconds $ n - 1
