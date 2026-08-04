module Bus.Database.Repository.RefreshToken (save, updateRevoked) where

import Bus.Database.Class (MonadDatabase (runBeam, withTransactionMode))
import Bus.Database.Entity (BusTrackerDb (btRefreshToken), RefreshToken, RefreshTokenT (rtkId, rtkRevoked, rtkUpdateTime), busTrackerDb)
import Data.Time (LocalTime)
import Data.UUID (UUID)
import Database.Beam (SqlValable (val_), insert, insertValues, runInsert, runUpdate, update, (&&.), (/=.), (<-.), (==.))
import Database.PostgreSQL.Simple.Transaction (defaultTransactionMode)

save :: (MonadDatabase m) => RefreshToken -> m ()
save token = withTransactionMode defaultTransactionMode $ \conn -> do
    runBeam conn $ runInsert (insert (btRefreshToken busTrackerDb) (insertValues [token]))

updateRevoked :: (MonadDatabase m) => UUID -> Bool -> LocalTime -> m ()
updateRevoked tokenId revoked updateTime = withTransactionMode defaultTransactionMode $ \conn -> do
    runBeam conn $
        runUpdate $
            update
                (btRefreshToken busTrackerDb)
                (\r -> (rtkRevoked r <-. val_ revoked) <> (rtkUpdateTime r <-. val_ updateTime))
                (\r -> rtkId r ==. val_ tokenId &&. rtkRevoked r /=. val_ revoked)
