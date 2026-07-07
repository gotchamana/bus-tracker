module Bus.Exception (CryptoStoreException (..), isAsyncException, rethrowIO) where

import Control.Exception (Exception (..), ExceptionWithContext, SomeAsyncException, throwIO)
import Crypto.Store.Error (StoreError)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Typeable (typeOf)

newtype NoBacktrace e = NoBacktrace e
    deriving (Show)

instance (Exception e) => Exception (NoBacktrace e) where
    fromException = fmap NoBacktrace . fromException
    toException (NoBacktrace e) = toException e
    backtraceDesired _ = False

newtype CryptoStoreException = CryptoStoreException StoreError deriving (Show)

instance Exception CryptoStoreException where
    displayException e@(CryptoStoreException err) = show (typeOf e) <> ": " <> show err

data ApiException = ApiException
    { apiHttpStatus :: Int
    , apiErrorType :: ErrorType
    , apiErrorDescription :: Maybe Text
    , apiErrorDetails :: forall a. (ToJSON a) => a
    }

instance Show ApiException where
    show _ = ""

instance Exception ApiException

data ErrorType = ErrorType Text Text Text

etyType :: ErrorType -> Text
etyType (ErrorType typ _ _) = typ

etyMessageCode :: ErrorType -> Text
etyMessageCode (ErrorType _ code _) = code

etyMessage :: ErrorType -> Text
etyMessage (ErrorType _ _ msg) = msg

-- etyValidation

isAsyncException :: (Exception e) => e -> Bool
isAsyncException e =
    case fromException @SomeAsyncException (toException e) of
        Just _ -> True
        Nothing -> False

rethrowIO :: (Exception e) => ExceptionWithContext e -> IO a
rethrowIO e = throwIO (NoBacktrace e)
