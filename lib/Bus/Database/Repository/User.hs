module Bus.Database.Repository.User (save, existsByAccount, findPasswordByAccount, findIdByAccount) where

import Bus.Database.Class (MonadDatabase (runBeam, withTransactionMode))
import Bus.Database.Entity (BusTrackerDb (btUser), User, UserT (usrAccount, usrId, usrPassword), busTrackerDb)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)
import Data.UUID (UUID)
import Database.Beam (
    SqlValable (val_),
    aggregate_,
    all_,
    as_,
    countAll_,
    guard_,
    insert,
    insertValues,
    runInsert,
    runSelectReturningOne,
    select,
    (==.),
 )
import Database.PostgreSQL.Simple.Transaction (defaultTransactionMode)

existsByAccount :: (MonadDatabase m, MonadFail m) => Text -> m Bool
existsByAccount account = withTransactionMode defaultTransactionMode $ \conn -> do
    Just count <- runBeam conn
        . runSelectReturningOne
        . select
        . aggregate_ (\_ -> as_ @Int32 countAll_)
        $ do
            user <- all_ (btUser busTrackerDb)
            guard_ (usrAccount user ==. val_ account)
            pure user

    pure (count == 1)

findIdByAccount :: (MonadDatabase m) => Text -> m (Maybe UUID)
findIdByAccount account = withTransactionMode defaultTransactionMode $ \conn -> do
    runBeam conn . runSelectReturningOne . select $ do
        user <- all_ (btUser busTrackerDb)
        guard_ (usrAccount user ==. val_ account)
        pure (usrId user)

findPasswordByAccount :: (MonadDatabase m) => Text -> m (Maybe ByteString)
findPasswordByAccount account = withTransactionMode defaultTransactionMode $ \conn -> do
    runBeam conn . runSelectReturningOne . select $ do
        user <- all_ (btUser busTrackerDb)
        guard_ (usrAccount user ==. val_ account)
        pure (usrPassword user)

save :: (MonadDatabase m) => User -> m ()
save user = withTransactionMode defaultTransactionMode $ \conn -> do
    runBeam conn $ runInsert (insert (btUser busTrackerDb) (insertValues [user]))
