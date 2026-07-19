module Bus.Database.Class (MonadDatabase (..)) where

import Control.Monad.IO.Class (MonadIO)
import Database.Beam.Postgres (Pg)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Transaction (TransactionMode)

class (MonadIO m) => MonadDatabase m where
    withConnection :: (Connection -> m a) -> m a
    withTransactionMode :: TransactionMode -> (Connection -> m a) -> m a
    runBeam :: Connection -> Pg a -> m a
