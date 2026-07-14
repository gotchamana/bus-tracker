{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Bus.Web.User.Service (NewUser (..), save, validateNewUser) where

import Bus.Database (MonadDatabase)
import Bus.Database.Table.User (UserT (..))
import Bus.Util.MessageCode (errorValidationDuplicateUserAccount)
import Bus.Validation.Aeson (parseObjectM)
import Bus.Validation.Error (ValidationError (..), requestValidationException)
import Bus.Validation.Rerefined (Length, NotEmpty, Trimmed, refineField)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans (lift)
import Crypto.KDF.BCrypt (hashPassword)
import Data.Aeson (Object)
import Data.Aeson.BetterErrors (asText, key, throwCustomError)
import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Time (ZonedTime (zonedTimeToLocalTime), getZonedTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import GHC.Stack (HasCallStack)
import Rerefined (Refined, unrefine)
import Rerefined.Predicates (And)
import Valida (Validation (..))

import Bus.Database.Repository.User qualified as UserRepo
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

data NewUser = NewUser
    { usrAccount :: Refined (And Trimmed (And NotEmpty (Length 5 20))) Text
    , usrPassword :: Refined (And NotEmpty (Length 12 20)) Text
    }
    deriving (Show)

validateNewUser :: (HasCallStack, MonadDatabase m, MonadThrow m, MonadFail m) => Object -> m NewUser
validateNewUser object = do
    result <- parseObjectM parser object

    case result of
        Left err -> throwM (requestValidationException Nothing err)
        Right user -> pure user
  where
    parser = do
        account <- Text.strip <$> key "account" asText
        password <- key "password" asText

        validationAccount <- lift (uniqueAccount account)

        let result =
                NewUser
                    <$> refineField "account" account
                    <*> refineField "password" password
                    <* validationAccount

        case result of
            Success user -> pure user
            Failure err -> throwCustomError err
    uniqueAccount account = do
        let err =
                ValidationError
                    { valField = Just "account"
                    , valMessage = "Duplicate account"
                    , valMessageCode = errorValidationDuplicateUserAccount
                    , valMessageArgs = HashMap.empty
                    }

        if Text.all isSpace account
            then pure (Success ())
            else do
                duplicate <- UserRepo.existsByAccount account

                if duplicate
                    then pure (Failure (err :| []))
                    else pure (Success ())

save :: (MonadDatabase m) => NewUser -> m UUID
save user = do
    userId <- liftIO nextRandom
    now <- liftIO (zonedTimeToLocalTime <$> getZonedTime)
    hashedPassword <- liftIO . hashPassword 15 . Text.encodeUtf8 . unrefine $ user.usrPassword

    UserRepo.save
        User
            { usrId = userId
            , usrAccount = unrefine user.usrAccount
            , usrPassword = hashedPassword
            , usrCreateTime = now
            , usrUpdateTime = now
            }
    pure userId
