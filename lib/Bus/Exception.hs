{-# LANGUAGE OverloadedStrings #-}

module Bus.Exception (
    ApiException (..),
    CryptoStoreException (..),
    IllegalValueException (..),
    JwtException (..),
    NoSuchKeyException (..),
    NoSuchValueException (..),
    ErrorType,
    etyInvalidCredentials,
    etyInvalidRequestFormat,
    etyInvalidRequestParameters,
    etyMessage,
    etyMessageCode,
    etyType,
    etyUnknownError,
    isAsyncException,
    rethrowIO,
    etyMissingResource,
) where

import Bus.Util.MessageCode (
    errorApiInvalidCredentials,
    errorApiInvalidRequestFormat,
    errorApiInvalidRequestParameters,
    errorApiMissingResource,
    errorApiUnknownError,
 )
import Control.Exception (Exception (..), ExceptionWithContext, SomeAsyncException, throwIO)
import Crypto.JWT (JWTError)
import Crypto.Store.Error (StoreError)
import Data.Aeson (Object)
import Data.Text (Text)
import Data.Typeable (typeOf)
import Network.HTTP.Types (Status)

newtype NoBacktrace e = NoBacktrace e
    deriving (Show)

instance (Exception e) => Exception (NoBacktrace e) where
    fromException = fmap NoBacktrace . fromException
    toException (NoBacktrace e) = toException e
    backtraceDesired _ = False

newtype CryptoStoreException = CryptoStoreException StoreError deriving (Show)

instance Exception CryptoStoreException where
    displayException e@(CryptoStoreException err) = show (typeOf e) <> ": " <> show err

newtype JwtException = JwtException JWTError deriving (Show)

instance Exception JwtException where
    displayException e@(JwtException err) = show (typeOf e) <> ": " <> show err

newtype NoSuchKeyException = NoSuchKeyException String deriving (Show)

instance Exception NoSuchKeyException where
    displayException e@(NoSuchKeyException key) = show (typeOf e) <> ": " <> key

newtype NoSuchValueException = NoSuchValueException String deriving (Show)

instance Exception NoSuchValueException where
    displayException e@(NoSuchValueException msg) = show (typeOf e) <> ": " <> msg

newtype IllegalValueException = IllegalValueException String deriving (Show)

instance Exception IllegalValueException where
    displayException e@(IllegalValueException msg) = show (typeOf e) <> ": " <> msg

data ApiException = ApiException
    { apiHttpStatus :: Status
    , apiErrorType :: ErrorType
    , apiErrorDescription :: Maybe Text
    , apiErrorDetails :: Maybe Object
    }
    deriving (Show)

instance Exception ApiException

data ErrorType = ErrorType Text Text Text deriving (Show)

etyType :: ErrorType -> Text
etyType (ErrorType typ _ _) = typ

etyMessageCode :: ErrorType -> Text
etyMessageCode (ErrorType _ code _) = code

etyMessage :: ErrorType -> Text
etyMessage (ErrorType _ _ msg) = msg

etyInvalidCredentials :: ErrorType
etyInvalidCredentials = ErrorType "invalid-credentials" errorApiInvalidCredentials "Invalid credentials"

etyInvalidRequestParameters :: ErrorType
etyInvalidRequestParameters = ErrorType "invalid-request-parameters" errorApiInvalidRequestParameters "Invalid request parameters"

etyInvalidRequestFormat :: ErrorType
etyInvalidRequestFormat = ErrorType "invalid-request-format" errorApiInvalidRequestFormat "Invalid request format"

etyMissingResource :: ErrorType
etyMissingResource = ErrorType "missing-resource" errorApiMissingResource "Missing resource"

etyUnknownError :: ErrorType
etyUnknownError = ErrorType "unknown-error" errorApiUnknownError "Unknown error"

isAsyncException :: (Exception e) => e -> Bool
isAsyncException e =
    case fromException @SomeAsyncException (toException e) of
        Just _ -> True
        Nothing -> False

rethrowIO :: (Exception e) => ExceptionWithContext e -> IO a
rethrowIO e = throwIO (NoBacktrace e)
