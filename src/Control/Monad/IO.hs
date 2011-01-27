module Control.Monad.IO (io) where

import Control.Monad.Trans

io :: MonadIO m => IO a -> m a
io = liftIO
