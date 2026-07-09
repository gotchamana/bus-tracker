{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bus.Util (toHandler) where

import Bus.App (AppM (AppM), Env (envLoggingChan))
import Bus.Exception (ApiException (..), etyMessage, isAsyncException)
import Bus.Logging (logErrorEx, logWarnEx, runTChanLoggingT)
import Control.Exception (Exception (fromException), ExceptionWithContext (ExceptionWithContext), SomeException, try)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT))
import Data.Aeson (Options (fieldLabelModifier), ToJSON (toEncoding, toJSON), Value, defaultOptions, genericToEncoding, genericToJSON)
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Foldable (for_)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Network.HTTP.Types (hContentType)
import Network.HTTP.Types.Status (Status (statusCode, statusMessage))
import Network.URI (URIAuth (URIAuth, uriPort, uriRegName), nullURIAuth)
import Servant

import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

data ProblemDetails = ProblemDetails
    { pdType :: URI
    , pdTitle :: Text
    , pdDetail :: Text
    , pdErrors :: Maybe Value
    }
    deriving (Generic)

instance ToJSON ProblemDetails where
    toJSON = genericToJSON (customOptions "pd")
    toEncoding = genericToEncoding (customOptions "pd")

toHandler :: Env -> AppM a -> Handler a
toHandler env (AppM readerT) = do
    let loggingChan = envLoggingChan env
        action = runTChanLoggingT loggingChan (runReaderT readerT env)
        logging = liftIO . runTChanLoggingT loggingChan

    result <- liftIO (try @(ExceptionWithContext SomeException) action)

    case result of
        Left ewc@(ExceptionWithContext _ se) -> do
            when (isAsyncException se) (throwM se)

            for_ (fromException se) $ \ae -> do
                logging (logWarnEx ["API error"] ewc)

                toServerError ae >>= throwError

            logging (logErrorEx ["Unknown error"] ewc)

            throwError err500{errBody = "Some errors"}
        Right a -> pure a

toServerError :: (HasCallStack, MonadThrow m) => ApiException -> m ServerError
toServerError ApiException{..} = do
    reason <- byteStringToString (statusMessage apiHttpStatus)

    let uri =
            URI
                { uriScheme = "http"
                , uriAuthority =
                    Just
                        nullURIAuth
                            { uriRegName = ""
                            , uriPort = ""
                            }
                , uriPath = ""
                , uriQuery = ""
                , uriFragment = ""
                }
        pd =
            ProblemDetails
                { pdType = undefined
                , pdTitle = etyMessage apiErrorType
                , pdDetail = fromMaybe (etyMessage apiErrorType) apiErrorDescription
                , pdErrors = toJSON <$> apiErrorDetails
                }

    pure
        ServerError
            { errHTTPCode = statusCode apiHttpStatus
            , errReasonPhrase = reason
            , errHeaders = [(hContentType, "application/problem+json")]
            , errBody = ""
            }

byteStringToString :: (HasCallStack, MonadThrow m) => ByteString -> m String
byteStringToString bs =
    case Text.decodeUtf8' bs of
        Left err -> throwM err
        Right text -> pure (Text.unpack text)

customOptions :: String -> Options
customOptions fieldPrefix = defaultOptions{fieldLabelModifier = removePrefix}
  where
    removePrefix field = case stripPrefix fieldPrefix field of
        Just result -> case result of
            [] -> []
            (x : xs) -> toLower x : xs
        Nothing -> field
