module Bus.Database.Repository.User (save, existsByAccount, findPasswordByAccount) where

import Bus.Database (BusTrackerDb (btUser), MonadDatabase (runBeam, withTransactionMode), busTrackerDb)
import Bus.Database.Table.User (User, UserT (usrAccount, usrPassword))
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam (SqlValable (val_), aggregate_, all_, as_, countAll_, guard_, insert, insertValues, runInsert, runSelectReturningOne, select, (==.))
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

findPasswordByAccount :: (MonadDatabase m) => Text -> m (Maybe ByteString)
findPasswordByAccount account = withTransactionMode defaultTransactionMode $ \conn -> do
    runBeam conn . runSelectReturningOne . select $ do
        user <- all_ (btUser busTrackerDb)
        guard_ (usrAccount user ==. val_ account)
        pure (usrPassword user)

save :: (MonadDatabase m) => User -> m ()
save user = withTransactionMode defaultTransactionMode $ \conn -> do
    runBeam conn $ runInsert (insert (btUser busTrackerDb) (insertValues [user]))
