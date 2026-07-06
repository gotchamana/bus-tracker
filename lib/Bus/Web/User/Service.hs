{-# LANGUAGE OverloadedRecordDot #-}

module Bus.Web.User.Service (NewUser (..), save) where

import Bus.Database (MonadDatabase)
import Bus.Database.Table.User (UserT (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON (parseJSON), Options (fieldLabelModifier), defaultOptions, genericParseJSON)
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Text (Text)
import Data.Time (ZonedTime (zonedTimeToLocalTime), getZonedTime)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)

import Bus.Database.Repository.User qualified as UserRepo
import Data.UUID (UUID)

data NewUser = NewUser
    { usrAccount :: Text
    , usrPassword :: Text
    }
    deriving (Generic)

instance FromJSON NewUser where
    parseJSON = genericParseJSON (customOptions "usr")

save :: (MonadDatabase m) => NewUser -> m UUID
save user = do
    userId <- liftIO nextRandom
    now <- liftIO (zonedTimeToLocalTime <$> getZonedTime)

    UserRepo.save
        User
            { usrId = userId
            , usrAccount = user.usrAccount
            , usrPassword = user.usrPassword
            , usrCreateTime = now
            , usrUpdateTime = now
            }
    pure userId

customOptions :: String -> Options
customOptions fieldPrefix = defaultOptions{fieldLabelModifier = removePrefix}
  where
    removePrefix field = case stripPrefix fieldPrefix field of
        Just result -> case result of
            [] -> []
            (x : xs) -> toLower x : xs
        Nothing -> field
