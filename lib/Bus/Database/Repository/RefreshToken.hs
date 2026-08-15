module Bus.Database.Repository.RefreshToken (save, updateRevoked, existsByIdAndRevoked) where

import Bus.Database.Entity (
    BusTrackerDb (btRefreshToken),
    RefreshToken,
    RefreshTokenT (rtkId, rtkRevoked, rtkUpdateTime),
    busTrackerDb,
 )
import Bus.Database.MonadDatabase (MonadDatabase (runBeam), withReadOnlyTransaction, withTransaction)
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
    isNotFalse_,
    isNotTrue_,
    isTrue_,
    limit_,
    runInsert,
    runSelectReturningOne,
    runUpdate,
    select,
    sqlBool_,
    update,
    (&&.),
    (<-.),
    (==.),
 )
import Database.Beam.Backend (BeamSqlBackend)

save :: (MonadDatabase m) => RefreshToken -> m ()
save token = withTransaction $ \conn -> do
    runBeam conn $ runInsert (insert (btRefreshToken busTrackerDb) (insertValues [token]))

updateRevoked :: (MonadDatabase m) => UUID -> Bool -> LocalTime -> m ()
updateRevoked tokenId revoked updateTime = withTransaction $ \conn -> do
    runBeam conn
        . runUpdate
        $ update
            (btRefreshToken busTrackerDb)
            (\r -> (rtkRevoked r <-. val_ revoked) <> (rtkUpdateTime r <-. val_ updateTime))
            (\r -> rtkId r ==. val_ tokenId &&. isNotBool_ revoked (sqlBool_ (rtkRevoked r)))

existsByIdAndRevoked :: (MonadDatabase m) => UUID -> Bool -> m Bool
existsByIdAndRevoked tokenId revoked = withReadOnlyTransaction $ \conn ->
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

isNotBool_ :: (BeamSqlBackend be) => Bool -> QGenExpr context be s SqlBool -> QGenExpr context be s Bool
isNotBool_ True = isNotTrue_
isNotBool_ False = isNotFalse_
