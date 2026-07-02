module Bus.Database (MonadDatabase (..)) where

import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple (Connection)

class (MonadIO m) => MonadDatabase m where
    withConnection :: (Connection -> IO a) -> m a
