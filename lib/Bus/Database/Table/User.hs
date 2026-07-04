{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Bus.Database.Table.User (UserT (..), User, UserId) where

import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Data.Time (LocalTime)
import Data.UUID (UUID)
import Database.Beam (Beamable, Columnar, Table (PrimaryKey, primaryKey))
import GHC.Generics (Generic)

data UserT f = User
    { usrId :: Columnar f UUID
    , usrAccount :: Columnar f Text
    , usrPassword :: Columnar f Text
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
