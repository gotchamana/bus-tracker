{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bus.Util (toHandler) where

import Bus.App (AppM (AppM), Config (cfgServer), Env (envConfig, envLoggingChan), Server (svrPort))
import Bus.Exception (ApiException (..), ErrorType, etyMessage, etyMessageCode, etyType, etyUnknownError)
import Bus.Logging (logErrorEx, logWarnEx, runTChanLoggingT)
import Control.Exception (Exception (fromException), ExceptionWithContext (ExceptionWithContext), SomeAsyncException, SomeException, try)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT))
import Data.Aeson (Options (fieldLabelModifier), ToJSON (toEncoding, toJSON), Value, defaultOptions, encode, genericToEncoding, genericToJSON)
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
import Network.URI (URIAuth (uriPort, uriRegName), nullURIAuth)
import Rerefined (unrefine)
import Servant

import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

data ProblemDetails = ProblemDetails
    { pdType :: URI
    , pdTitle :: Text
    , pdDetail :: Text
    , pdMessageCode :: Text
    , pdErrors :: Maybe Value
    }
    deriving (Generic)

instance ToJSON ProblemDetails where
    toJSON = genericToJSON (customOptions "pd")
    toEncoding = genericToEncoding (customOptions "pd")

toHandler :: Env -> AppM a -> Handler a
toHandler env (AppM readerT) = do
    let loggingChan = envLoggingChan env
        port = unrefine env.envConfig.cfgServer.svrPort
        action = runTChanLoggingT loggingChan (runReaderT readerT env)
        logging = liftIO . runTChanLoggingT loggingChan

    result <- liftIO (try @(ExceptionWithContext SomeException) action)

    case result of
        Left ewc@(ExceptionWithContext _ se) -> do
            for_ (fromException @SomeAsyncException se) throwM

            for_ (fromException se) $ \ae -> do
                logging (logWarnEx ["API error"] ewc)
                toServerError port ae >>= throwError

            logging (logErrorEx ["Unknown error"] ewc)

            let details = problemDetails False port etyUnknownError Nothing Nothing

            throwError err500{errBody = encode details}
        Right a -> pure a

toServerError :: (HasCallStack, MonadThrow m) => Int -> ApiException -> m ServerError
toServerError port ApiException{..} = do
    reason <- byteStringToString (statusMessage apiHttpStatus)

    let details = problemDetails False port apiErrorType apiErrorDescription (toJSON <$> apiErrorDetails)

    pure
        ServerError
            { errHTTPCode = statusCode apiHttpStatus
            , errReasonPhrase = reason
            , errHeaders = [(hContentType, "application/problem+json")]
            , errBody = encode details
            }

problemDetails :: Bool -> Int -> ErrorType -> Maybe Text -> Maybe Value -> ProblemDetails
problemDetails secure port errorType errorDescription errorDetails =
    let scheme = if secure then "https:" else "http:"
        port' = case (secure, port) of
            (False, 80) -> ""
            (True, 443) -> ""
            _ -> ":" <> show port
        uri =
            URI
                { uriScheme = scheme
                , uriAuthority =
                    Just
                        nullURIAuth
                            { uriRegName = "localhost"
                            , uriPort = port'
                            }
                , uriPath = "/api/docs/problem-details"
                , uriQuery = ""
                , uriFragment = "#" <> Text.unpack (etyType errorType)
                }
        details =
            ProblemDetails
                { pdType = uri
                , pdTitle = etyMessage errorType
                , pdDetail = fromMaybe (etyMessage errorType) errorDescription
                , pdMessageCode = etyMessageCode errorType
                , pdErrors = errorDetails
                }
     in details

byteStringToString :: (HasCallStack, MonadThrow m) => ByteString -> m String
byteStringToString bs =
    case Text.decodeUtf8' bs of
        Left err -> throwM err
        Right text -> pure (Text.unpack text)

customOptions :: String -> Options
customOptions fieldPrefix =
    defaultOptions
        { fieldLabelModifier = removePrefix
        }
  where
    removePrefix field = case stripPrefix fieldPrefix field of
        Just result -> case result of
            [] -> []
            (x : xs) -> toLower x : xs
        Nothing -> field
