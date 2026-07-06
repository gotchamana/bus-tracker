module Bus.Database.Repository.User (save) where

import Bus.Database (BusTrackerDb (btUser), MonadDatabase (runBeam, withTransactionMode), busTrackerDb)
import Bus.Database.Table.User (User)
import Database.Beam (insert, insertValues, runInsert)
import Database.PostgreSQL.Simple.Transaction (defaultTransactionMode)

save :: (MonadDatabase m) => User -> m ()
save user = withTransactionMode defaultTransactionMode $ \conn -> do
    runBeam conn $ runInsert (insert (btUser busTrackerDb) (insertValues [user]))
