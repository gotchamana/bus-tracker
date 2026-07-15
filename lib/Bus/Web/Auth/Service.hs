{-# LANGUAGE OverloadedStrings #-}

module Bus.Web.Auth.Service (validateLogin) where

import Bus.Auth (Token)
import Bus.Database (MonadDatabase)
import Bus.Validation.Aeson (parseObjectM)
import Bus.Validation.Error (ValidationError (..), requestValidationException)
import Bus.Validation.Rerefined (NotEmpty, Trimmed, refineField)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Trans (MonadTrans (lift))
import Crypto.KDF.BCrypt (validatePassword)
import Data.Aeson (Object)
import Data.Aeson.BetterErrors (asText, key, throwCustomError)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import Rerefined (Refined)
import Rerefined.Predicates (And)
import Valida (Validation (Failure, Success))

import Bus.Database.Repository.User qualified as UserRepo
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

data Login = Login
    { lgAccount :: Refined (And Trimmed NotEmpty) Text
    , lgPassword :: Refined NotEmpty Text
    }

validateLogin :: (HasCallStack, MonadDatabase m, MonadThrow m) => Object -> m Login
validateLogin object = do
    result <- parseObjectM parser object

    case result of
        Left err -> throwM (requestValidationException Nothing err)
        Right login -> pure login
  where
    parser = do
        account <- Text.strip <$> key "account" asText
        password <- key "password" asText

        v <- lift (checkPassword account password)

        let result =
                Login
                    <$> refineField "account" account
                    <*> refineField "password" password
                    <* v

        case result of
            Failure err -> throwCustomError err
            Success login -> pure login
    checkPassword account password = do
        let err =
                ValidationError
                    { valField = Nothing
                    , valMessage = "Invalid account or password"
                    , valMessageCode = "error.validation.invalid-user-credentials"
                    , valMessageArgs = HashMap.empty
                    }
            password' = Text.encodeUtf8 password

        maybeHash <- UserRepo.findPasswordByAccount account

        case maybeHash of
            Just hash ->
                if validatePassword password' hash
                    then pure (Success ())
                    else pure (Failure (err :| []))
            Nothing -> pure (Failure (err :| []))
