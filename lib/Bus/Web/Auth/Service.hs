{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Bus.Web.Auth.Service (
    Login,
    Authentication (..),
    AuthToken (..),
    validateLogin,
    signAuthTokenByLogin,
    invalidateRefreshToken,
    signAuthTokenByRefreshToken,
) where

import Bus.Database.Entity (PrimaryKey (UserId), RefreshTokenT (..))
import Bus.Database.MonadDatabase (MonadDatabase, withTransaction)
import Bus.Exception (IllegalValueException (IllegalValueException), JwtException (JwtException), NoSuchValueException (NoSuchValueException))
import Bus.Security.Jwt (Token (Token, tokClaimsSet), TokenType (Access, Refresh), signToken)
import Bus.Util.Either (maybeToEither)
import Bus.Util.MessageCode (errorValidationInvalidUserCredentials, errorValidationRevokedRefreshToken)
import Bus.Validation.Aeson (parseObject)
import Bus.Validation.Error (ValidationError (..), requestValidationException)
import Bus.Validation.Rerefined (NotEmpty, Trimmed, refineField)
import Control.Lens ((^.), (^?))
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Except (liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans (MonadTrans (lift))
import Crypto.JWT (HasClaimsSet (claimExp, claimJti, claimSub), NumericDate (NumericDate), SignedJWT, encodeCompact, string, unsafeGetJWTPayload)
import Crypto.KDF.BCrypt (validatePassword)
import Crypto.Store.PKCS8 (KeyPair)
import Data.Aeson (Object)
import Data.Aeson.BetterErrors (asText, key, throwCustomError)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Time.LocalTime (LocalTime, ZonedTime (zonedTimeToLocalTime), getCurrentTimeZone, getZonedTime, utcToLocalTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import GHC.Stack (HasCallStack)
import Rerefined (Refined, unrefine)
import Rerefined.Predicates (And)
import Valida (Validation (Failure, Success))

import Bus.Database.Repository.RefreshToken qualified as RefreshTokenRepo
import Bus.Database.Repository.User qualified as UserRepo
import Data.ByteString qualified as ByteString
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.UUID qualified as UUID

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

signAuthTokenByLogin :: (HasCallStack, MonadDatabase m, MonadThrow m) => KeyPair -> Login -> m Authentication
signAuthTokenByLogin keyPair login = signAuthToken keyPair (unrefine login.lgAccount)

signAuthToken :: (HasCallStack, MonadDatabase m, MonadThrow m) => KeyPair -> Text -> m Authentication
signAuthToken keyPair account = do
    let accessExp = 10 * 60 -- 10 mins
        refreshExp = 24 * 60 * 60 -- 1 day
    (signedAccessJwt, signedRefreshJwt) <-
        liftIO $ do
            refreshTokenId <- UUID.toText <$> nextRandom
            (,)
                <$> signToken keyPair Nothing account accessExp Access
                <*> signToken keyPair (Just refreshTokenId) account refreshExp Refresh

    let access = ByteString.toStrict (encodeCompact signedAccessJwt)
        refresh = ByteString.toStrict (encodeCompact signedRefreshJwt)

    saveRefreshToken account signedRefreshJwt

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

saveRefreshToken :: (HasCallStack, MonadDatabase m, MonadThrow m) => Text -> SignedJWT -> m ()
saveRefreshToken account jwt = do
    (tokenId, expTime) <- case unsafeGetJWTPayload jwt of
        Left err -> throwM (JwtException err)
        Right Token{tokClaimsSet} -> do
            jti <- case tokClaimsSet ^. claimJti of
                Just jtiClaim -> pure jtiClaim
                Nothing -> throwM (NoSuchValueException "No jti claim in JWT")

            uuid <- case UUID.fromText jti of
                Just uuid -> pure uuid
                Nothing -> throwM (IllegalValueException ("Invalid UUID: " <> Text.unpack jti))

            expTime <- case tokClaimsSet ^. claimExp of
                Just (NumericDate time) -> pure time
                Nothing -> throwM (NoSuchValueException "No exp claim in JWT")

            pure (uuid, expTime)

    (now, timeZone) <- liftIO $ (,) <$> getLocalTime <*> getCurrentTimeZone
    userId <-
        UserRepo.findIdByAccount account >>= \case
            Just userId -> pure userId
            Nothing -> throwM (NoSuchValueException ("No such user: " <> Text.unpack account))

    RefreshTokenRepo.save
        RefreshTokenT
            { rtkId = tokenId
            , rtkRevoked = False
            , rtkUserId = UserId userId
            , rtkExpireTime = utcToLocalTime timeZone expTime
            , rtkCreateTime = now
            , rtkUpdateTime = now
            }

invalidateRefreshToken :: (HasCallStack, MonadDatabase m, MonadThrow m) => Token -> m ()
invalidateRefreshToken refreshToken = do
    tokenId <- getTokenId refreshToken
    time <- liftIO getLocalTime
    RefreshTokenRepo.updateRevoked tokenId True time

getTokenId :: (HasCallStack, MonadThrow m) => Token -> m UUID
getTokenId token =
    case token.tokClaimsSet ^. claimJti >>= UUID.fromText of
        Just tokenId -> pure tokenId
        Nothing -> throwM (NoSuchValueException "No jti found in token")

signAuthTokenByRefreshToken :: (HasCallStack, MonadDatabase m, MonadThrow m) => KeyPair -> Token -> m Authentication
signAuthTokenByRefreshToken keyPair refreshToken = do
    tokenId <- getTokenId refreshToken
    account <- case refreshToken.tokClaimsSet ^. claimSub >>= (^? string) of
        Just account -> pure account
        Nothing -> throwM (NoSuchValueException "No sub found in token")
    exists <- RefreshTokenRepo.existsByIdAndRevoked tokenId False

    if exists
        then do
            withTransaction $ \_ -> do
                invalidateRefreshToken refreshToken
                signAuthToken keyPair account
        else
            let revokedError =
                    ValidationError
                        { valField = Nothing
                        , valMessage = "Refresh token was revoked"
                        , valMessageCode = errorValidationRevokedRefreshToken
                        , valMessageArgs = HashMap.empty
                        }
             in throwM (requestValidationException Nothing (NonEmpty.singleton revokedError))

getLocalTime :: IO LocalTime
getLocalTime = zonedTimeToLocalTime <$> getZonedTime
