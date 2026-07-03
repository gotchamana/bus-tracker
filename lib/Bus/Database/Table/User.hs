{-# LANGUAGE RecordWildCards #-}

module Bus.Database.Table.User (UserT (..)) where

import Bus.Database (MonadDatabase)
import Data.Text (Text)
import Data.Time (ZonedTime)
import Data.UUID (UUID)
import Database.Beam (Beamable, Columnar, Table)
import GHC.Generics (Generic)

data UserT f = User
    { usrId :: Columnar f UUID
    , usrAccount :: Columnar f Text
    , usrPassword :: Columnar f Text
    , usrCreateTime :: Columnar f ZonedTime
    , usrUpdateTime :: Columnar f ZonedTime
    }
    deriving (Generic)

instance Table UserT where

instance Beamable UserT

-- instance Show User where
--     show User{..} =
--         mconcat
--             [ "User {usrAccount = "
--             , show usrAccount
--             , ", usrPassword = <HIDDEN>, "
--             , "usrCreateTime = "
--             , show usrCreateTime
--             , ", usrUpdateTime = "
--             , show usrUpdateTime
--             , "}"
--             ]

-- createUser :: (MonadDatabase m) => User -> m ()
