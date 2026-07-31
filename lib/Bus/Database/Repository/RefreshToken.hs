module Bus.Database.Repository.RefreshToken (save) where

import Bus.Database.Class (MonadDatabase (runBeam, withTransactionMode))
import Bus.Database.Entity (BusTrackerDb (btRefreshToken), RefreshToken, busTrackerDb)
import Database.Beam (insert, insertValues, runInsert)
import Database.PostgreSQL.Simple.Transaction (defaultTransactionMode)

save :: (MonadDatabase m) => RefreshToken -> m ()
save token = withTransactionMode defaultTransactionMode $ \conn -> do
    runBeam conn $ runInsert (insert (btRefreshToken busTrackerDb) (insertValues [token]))
