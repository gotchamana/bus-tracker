{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Bus.Database.Entity (
    BusTrackerDb (..),
    busTrackerDb,
    RefreshTokenT (..),
    RefreshToken,
    RefreshTokenId,
    UserT (..),
    User,
    UserId,
    PrimaryKey (..),
) where

import Data.ByteString (ByteString)
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Data.Time (LocalTime)
import Data.UUID (UUID)
import Database.Beam (
    Beamable,
    Columnar,
    Database,
    DatabaseSettings,
    Table (PrimaryKey, primaryKey),
    TableEntity,
    dbModification,
    defaultDbSettings,
    modifyTableFields,
    tableModification,
    withDbModification,
 )
import GHC.Generics (Generic)

data BusTrackerDb f = BusTrackerDb
    { btUser :: f (TableEntity UserT)
    , btRefreshToken :: f (TableEntity RefreshTokenT)
    }
    deriving (Generic)

instance Database be BusTrackerDb

data RefreshTokenT f = RefreshTokenT
    { rtkId :: Columnar f UUID
    , rtkUserId :: PrimaryKey UserT f
    , rtkRevoked :: Columnar f Bool
    , rtkExpireTime :: Columnar f LocalTime
    , rtkCreateTime :: Columnar f LocalTime
    , rtkUpdateTime :: Columnar f LocalTime
    }
    deriving (Generic)

instance Beamable RefreshTokenT

instance Table RefreshTokenT where
    data PrimaryKey RefreshTokenT f = RefreshTokenId (Columnar f UUID) deriving (Generic)
    primaryKey = RefreshTokenId . rtkId

instance Beamable (PrimaryKey RefreshTokenT)

type RefreshToken = RefreshTokenT Identity

type RefreshTokenId = PrimaryKey RefreshTokenT Identity

data UserT f = User
    { usrId :: Columnar f UUID
    , usrAccount :: Columnar f Text
    , usrPassword :: Columnar f ByteString
    , usrCreateTime :: Columnar f LocalTime
    , usrUpdateTime :: Columnar f LocalTime
    }
    deriving (Generic)

instance Beamable UserT

instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f UUID) deriving (Generic)
    primaryKey = UserId . usrId

instance Beamable (PrimaryKey UserT)

type User = UserT Identity

type UserId = PrimaryKey UserT Identity

instance Show User where
    show User{..} =
        mconcat
            [ "User {usrId = "
            , show usrId
            , ", usrAccount = "
            , show usrAccount
            , ", usrPassword = <HIDDEN>, "
            , "usrCreateTime = "
            , show usrCreateTime
            , ", usrUpdateTime = "
            , show usrUpdateTime
            , "}"
            ]

busTrackerDb :: DatabaseSettings be BusTrackerDb
busTrackerDb =
    defaultDbSettings
        `withDbModification` dbModification
            { btRefreshToken =
                modifyTableFields
                    tableModification
                        { rtkUserId = UserId "user_id"
                        }
            }
