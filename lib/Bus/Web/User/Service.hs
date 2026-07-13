{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Bus.Web.User.Service (NewUser (..), save, validateNewUser) where

import Bus.Database (MonadDatabase)
import Bus.Database.Table.User (UserT (..))
import Bus.Validation.Aeson (parseObject)
import Bus.Validation.Error (requestValidationException)
import Bus.Validation.Rerefined (NotEmpty, Trimmed, refineField)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (Object)
import Data.Aeson.BetterErrors (asText, key, throwCustomError)
import Data.Text (Text)
import Data.Time (ZonedTime (zonedTimeToLocalTime), getZonedTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import GHC.Stack (HasCallStack)
import Rerefined (Refined, unrefine)
import Rerefined.Predicates (And)
import Valida (Validation (..))

import Bus.Database.Repository.User qualified as UserRepo
import Data.Text qualified as Text

data NewUser = NewUser
    { usrAccount :: Refined (And Trimmed NotEmpty) Text
    , usrPassword :: Refined NotEmpty Text
    }
    deriving (Show)

validateNewUser :: (HasCallStack, MonadThrow m) => Object -> m NewUser
validateNewUser object =
    case parseObject parser object of
        Left err -> throwM (requestValidationException Nothing err)
        Right user -> pure user
  where
    parser = do
        account <- Text.strip <$> key "account" asText
        password <- key "password" asText

        let result =
                NewUser
                    <$> refineField "account" account
                    <*> refineField "password" password

        case result of
            Success user -> pure user
            Failure err -> throwCustomError err

save :: (MonadDatabase m) => NewUser -> m UUID
save user = do
    userId <- liftIO nextRandom
    now <- liftIO (zonedTimeToLocalTime <$> getZonedTime)

    UserRepo.save
        User
            { usrId = userId
            , usrAccount = unrefine user.usrAccount
            , usrPassword = unrefine user.usrPassword
            , usrCreateTime = now
            , usrUpdateTime = now
            }
    pure userId
