module Bus.Database (MonadDatabase (..), BusTrackerDb (..), busTrackerDb) where

import Bus.Database.Table.User (UserT)
import Control.Monad.IO.Class (MonadIO)
import Database.Beam (Database, DatabaseSettings, TableEntity, defaultDbSettings)
import Database.Beam.Postgres (Pg)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Transaction (TransactionMode)
import GHC.Generics (Generic)

class (MonadIO m) => MonadDatabase m where
    withConnection :: (Connection -> m a) -> m a
    withTransactionMode :: TransactionMode -> (Connection -> m a) -> m a
    runBeam :: Connection -> Pg a -> m a

data BusTrackerDb f = BusTrackerDb
    { btUser :: f (TableEntity UserT)
    }
    deriving (Generic)

instance Database be BusTrackerDb

busTrackerDb :: DatabaseSettings be BusTrackerDb
busTrackerDb = defaultDbSettings
