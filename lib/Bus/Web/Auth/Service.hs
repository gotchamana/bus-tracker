{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Bus.Web.Auth.Service (validateLogin) where

import Bus.Database.Class (MonadDatabase)
import Bus.Security.Jwt (TokenType (Access), signToken)
import Bus.Util.Either (maybeToEither)
import Bus.Util.MessageCode (errorValidationInvalidUserCredentials)
import Bus.Validation.Aeson (parseObject)
import Bus.Validation.Error (ValidationError (..), requestValidationException)
import Bus.Validation.Rerefined (NotEmpty, Trimmed, refineField)
import Control.Exception (throwIO)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Except (liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans (MonadTrans (lift))
import Crypto.JWT (encodeCompact)
import Crypto.KDF.BCrypt (validatePassword)
import Crypto.Store.PKCS8 (KeyPair)
import Data.Aeson (Object)
import Data.Aeson.BetterErrors (asText, key, throwCustomError)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import Rerefined (Refined, unrefine)
import Rerefined.Predicates (And)
import Valida (Validation (Failure, Success))

import Bus.Database.Repository.User qualified as UserRepo
import Data.ByteString qualified as ByteString
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

data Login = Login
    { lgAccount :: Refined (And Trimmed NotEmpty) Text
    , lgPassword :: Refined NotEmpty Text
    }

validateLogin :: (HasCallStack, MonadDatabase m, MonadThrow m) => Object -> KeyPair -> m Text
validateLogin object keyPair = do
    result <- runExceptT $ liftEither (parseObject parser object) >>= checkPassword

    case result of
        Left err -> throwM (requestValidationException Nothing err)
        Right login -> liftIO $ do
            signedJwt <- signToken keyPair (unrefine login.lgAccount) 600 Access

            case Text.decodeUtf8' (ByteString.toStrict (encodeCompact signedJwt)) of
                Left err -> throwIO err
                Right token -> pure token
  where
    parser = do
        account <- Text.strip <$> key "account" asText
        password <- key "password" asText

        let result =
                Login
                    <$> refineField "account" account
                    <*> refineField "password" password

        case result of
            Success login -> pure login
            Failure err -> throwCustomError err
    checkPassword login = do
        let err =
                ValidationError
                    { valField = Nothing
                    , valMessage = "Invalid account or password"
                    , valMessageCode = errorValidationInvalidUserCredentials
                    , valMessageArgs = HashMap.empty
                    }
            password' = Text.encodeUtf8 (unrefine login.lgPassword)

        maybeHash <- lift (UserRepo.findPasswordByAccount (unrefine login.lgAccount))
        hash <- liftEither (maybeToEither (err :| []) maybeHash)

        if validatePassword password' hash
            then pure login
            else liftEither (Left (err :| []))
