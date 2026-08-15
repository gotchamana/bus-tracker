module Bus.Database.Class (
    MonadDatabase (..),
    TransactionMode (..),
    Propagation (..),
    withTransaction,
    withReadOnlyTransaction,
    defaultTransactionMode,
    toPgTransactionMode,
) where

import Control.Monad.IO.Class (MonadIO)
import Database.Beam.Postgres (Pg)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Transaction (
    IsolationLevel (DefaultIsolationLevel),
    ReadWriteMode (DefaultReadWriteMode, ReadOnly),
 )

import Database.PostgreSQL.Simple.Transaction qualified as PostgreSQL

class (MonadIO m) => MonadDatabase m where
    withTransactionMode :: TransactionMode -> (Connection -> m a) -> m a
    runBeam :: Connection -> Pg a -> m a

data TransactionMode = TransactionMode
    { tmIsolationLevel :: IsolationLevel
    , tmReadWriteMode :: ReadWriteMode
    , tmPropagation :: Propagation
    }

data Propagation = Required | RequiredNew

withTransaction :: (MonadDatabase m) => (Connection -> m a) -> m a
withTransaction = withTransactionMode defaultTransactionMode

withReadOnlyTransaction :: (MonadDatabase m) => (Connection -> m a) -> m a
withReadOnlyTransaction = withTransactionMode defaultTransactionMode{tmReadWriteMode = ReadOnly}

defaultTransactionMode :: TransactionMode
defaultTransactionMode =
    TransactionMode
        { tmIsolationLevel = DefaultIsolationLevel
        , tmReadWriteMode = DefaultReadWriteMode
        , tmPropagation = Required
        }

toPgTransactionMode :: TransactionMode -> PostgreSQL.TransactionMode
toPgTransactionMode TransactionMode{tmIsolationLevel, tmReadWriteMode} =
    PostgreSQL.TransactionMode
        { isolationLevel = tmIsolationLevel
        , readWriteMode = tmReadWriteMode
        }
