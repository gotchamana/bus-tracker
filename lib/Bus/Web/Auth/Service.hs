{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Bus.Web.Auth.Service (Login, Authentication (..), AuthToken (..), validateLogin, signAuthToken) where

import Bus.Database.Class (MonadDatabase)
import Bus.Security.Jwt (TokenType (Access, Refresh), signToken)
import Bus.Util.Either (maybeToEither)
import Bus.Util.MessageCode (errorValidationInvalidUserCredentials)
import Bus.Validation.Aeson (parseObject)
import Bus.Validation.Error (ValidationError (..), requestValidationException)
import Bus.Validation.Rerefined (NotEmpty, Trimmed, refineField)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Except (liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans (MonadTrans (lift))
import Crypto.JWT (encodeCompact)
import Crypto.KDF.BCrypt (validatePassword)
import Crypto.Store.PKCS8 (KeyPair)
import Data.Aeson (Object)
import Data.Aeson.BetterErrors (asText, key, throwCustomError)
import Data.ByteString (ByteString)
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

data Authentication = Authentication
    { auAccessToken :: AuthToken
    , auRefreshToken :: AuthToken
    }

data AuthToken = AuthToken
    { atTokenValue :: ByteString
    , atExpirationSec :: Int
    }

validateLogin :: (HasCallStack, MonadDatabase m, MonadThrow m) => Object -> m Login
validateLogin object = do
    result <- runExceptT $ liftEither (parseObject parser object) >>= checkPassword

    case result of
        Left err -> throwM (requestValidationException Nothing err)
        Right login -> pure login
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

signAuthToken :: (HasCallStack, MonadIO m) => KeyPair -> Login -> m Authentication
signAuthToken keyPair login = do
    let account = unrefine login.lgAccount
        accessExp = 10 * 60 -- 10 mins
        refreshExp = 24 * 60 * 60 -- 1 day
    (signedAccessJwt, signedRefreshJwt) <-
        liftIO $
            (,)
                <$> signToken keyPair account accessExp Access
                <*> signToken keyPair account refreshExp Refresh

    let access = ByteString.toStrict (encodeCompact signedAccessJwt)
        refresh = ByteString.toStrict (encodeCompact signedRefreshJwt)

    pure
        Authentication
            { auAccessToken =
                AuthToken
                    { atTokenValue = access
                    , atExpirationSec = accessExp
                    }
            , auRefreshToken =
                AuthToken
                    { atTokenValue = refresh
                    , atExpirationSec = refreshExp
                    }
            }
