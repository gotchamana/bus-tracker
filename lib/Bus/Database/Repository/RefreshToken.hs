module Bus.Database.Repository.RefreshToken (save, updateRevoked, existsByIdAndRevoked) where

import Bus.Database.Class (MonadDatabase (runBeam, withTransactionMode))
import Bus.Database.Entity (BusTrackerDb (btRefreshToken), RefreshToken, RefreshTokenT (rtkId, rtkRevoked, rtkUpdateTime), busTrackerDb)
import Data.Int (Int32)
import Data.Maybe (isJust)
import Data.Time (LocalTime)
import Data.UUID (UUID)
import Database.Beam (
    QGenExpr,
    SqlBool,
    SqlValable (val_),
    all_,
    as_,
    filter_,
    insert,
    insertValues,
    isFalse_,
    isTrue_,
    limit_,
    runInsert,
    runSelectReturningOne,
    runUpdate,
    select,
    sqlBool_,
    update,
    (&&.),
    (/=.),
    (<-.),
    (==.),
 )
import Database.Beam.Backend (BeamSqlBackend)
import Database.PostgreSQL.Simple.Transaction (defaultTransactionMode)

save :: (MonadDatabase m) => RefreshToken -> m ()
save token = withTransactionMode defaultTransactionMode $ \conn -> do
    runBeam conn $ runInsert (insert (btRefreshToken busTrackerDb) (insertValues [token]))

updateRevoked :: (MonadDatabase m) => UUID -> Bool -> LocalTime -> m ()
updateRevoked tokenId revoked updateTime = withTransactionMode defaultTransactionMode $ \conn -> do
    runBeam conn
        . runUpdate
        $ update
            (btRefreshToken busTrackerDb)
            (\r -> (rtkRevoked r <-. val_ revoked) <> (rtkUpdateTime r <-. val_ updateTime))
            (\r -> rtkId r ==. val_ tokenId &&. rtkRevoked r /=. val_ revoked)

existsByIdAndRevoked :: (MonadDatabase m) => UUID -> Bool -> m Bool
existsByIdAndRevoked tokenId revoked = withTransactionMode defaultTransactionMode $ \conn ->
    runBeam conn
        . fmap isJust
        . runSelectReturningOne
        . select
        . (as_ @Int32 (val_ 1) <$)
        . limit_ 1
        . filter_ (\token -> rtkId token ==. val_ tokenId &&. isBool_ revoked (sqlBool_ (rtkRevoked token)))
        . all_
        $ btRefreshToken busTrackerDb

isBool_ :: (BeamSqlBackend be) => Bool -> QGenExpr context be s SqlBool -> QGenExpr context be s Bool
isBool_ True = isTrue_
isBool_ False = isFalse_
