{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bus.Exception (
    ApiException (..),
    CryptoStoreException (..),
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
) where

import Control.Exception (Exception (..), ExceptionWithContext, SomeAsyncException, throwIO)
import Crypto.Store.Error (StoreError)
import Data.Aeson (ToJSON)
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

data ApiException = forall a. (ToJSON a) => ApiException
    { apiHttpStatus :: Status
    , apiErrorType :: ErrorType
    , apiErrorDescription :: Maybe Text
    , apiErrorDetails :: Maybe a
    }

instance Show ApiException where
    show ApiException{..} =
        mconcat
            [ "ApiException {apiHttpStatus = "
            , show apiHttpStatus
            , ", apiErrorType = "
            , show apiErrorType
            , ", apiErrorDescription = "
            , show apiErrorDescription
            , ", apiErrorDetails = <HIDDEN>"
            , "}"
            ]

instance Exception ApiException

data ErrorType = ErrorType Text Text Text deriving (Show)

etyType :: ErrorType -> Text
etyType (ErrorType typ _ _) = typ

etyMessageCode :: ErrorType -> Text
etyMessageCode (ErrorType _ code _) = code

etyMessage :: ErrorType -> Text
etyMessage (ErrorType _ _ msg) = msg

etyInvalidCredentials :: ErrorType
etyInvalidCredentials = ErrorType "invalid-credentials" "error.invalid-credentials" "Invalid credentials"

etyInvalidRequestParameters :: ErrorType
etyInvalidRequestParameters = ErrorType "invalid-request-parameters" "error.invalid-request-parameters" "Invalid request parameters"

etyInvalidRequestFormat :: ErrorType
etyInvalidRequestFormat = ErrorType "invalid-request-format" "error.invalid-request-format" "Invalid request format"

etyUnknownError :: ErrorType
etyUnknownError = ErrorType "unknown-error" "error.unknown-error" "Unknown error"

isAsyncException :: (Exception e) => e -> Bool
isAsyncException e =
    case fromException @SomeAsyncException (toException e) of
        Just _ -> True
        Nothing -> False

rethrowIO :: (Exception e) => ExceptionWithContext e -> IO a
rethrowIO e = throwIO (NoBacktrace e)
