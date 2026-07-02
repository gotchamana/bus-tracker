{-# LANGUAGE RecordWildCards #-}

module Bus.Web.User.Entity (User (..)) where

import Bus.Database (MonadDatabase)
import Data.Text (Text)
import Data.Time (ZonedTime)

data User = User
    { -- usrId :: UUID
      usrAccount :: Text
    , usrPassword :: Text
    , usrCreateTime :: ZonedTime
    , usrUpdateTime :: ZonedTime
    }

instance Show User where
    show User{..} =
        mconcat
            [ "User {usrAccount = "
            , show usrAccount
            , ", usrPassword = <HIDDEN>, "
            , "usrCreateTime = "
            , show usrCreateTime
            , ", usrUpdateTime = "
            , show usrUpdateTime
            , "}"
            ]

-- createUser :: (MonadDatabase m) => User -> m ()
