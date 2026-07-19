{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Bus.Web.App.Servant (waiApp) where

import Bus.App (AppM (AppM), Config (cfgServer), Env (envConfig, envLoggingChan), Server (svrPort))
import Bus.Exception (ApiException (..), ErrorType, etyInvalidRequestFormat, etyMessage, etyMessageCode, etyMissingResource, etyType, etyUnknownError)
import Bus.Logger (logErrorEx, logWarnEx, runTChanLoggingT)
import Bus.Util.Aeson (fieldPrefixRemovalOptions)
import Bus.Web.Auth.Api (AuthApi, authApi)
import Bus.Web.User.Api (UserApi, userApi)
import Control.Exception (Exception (fromException), ExceptionWithContext (ExceptionWithContext), SomeAsyncException, SomeException, try)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT))
import Data.Aeson (ToJSON (toEncoding, toJSON), Value, encode, genericToEncoding, genericToJSON)
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Network.HTTP.Types (Header, Status (statusCode, statusMessage), hContentType)
import Network.URI (URIAuth (uriPort, uriRegName), nullURIAuth)
import Network.Wai (Request)
import Rerefined (unrefine)
import Servant hiding (Header)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)

import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

type Api = AuthApi :<|> UserApi

type JwtAuth = AuthProtect "jwt"

type instance AuthServerData JwtAuth = Text

data ProblemDetails = ProblemDetails
    { pdType :: URI
    , pdTitle :: Text
    , pdDetail :: Text
    , pdMessageCode :: Text
    , pdErrors :: Maybe Value
    }
    deriving (Generic)

instance ToJSON ProblemDetails where
    toJSON = genericToJSON (fieldPrefixRemovalOptions "pd")
    toEncoding = genericToEncoding (fieldPrefixRemovalOptions "pd")

waiApp :: Env -> Application
waiApp env = serveWithContext apiProxy (errorFormatters env :. authHandler env :. EmptyContext) server'
  where
    server' = hoistServerWithContext apiProxy authContextProxy (toHandler env) server

apiProxy :: Proxy Api
apiProxy = Proxy

authContextProxy :: Proxy '[AuthHandler Request Text]
authContextProxy = Proxy

authHandler :: Env -> AuthHandler Request Text
authHandler env = mkAuthHandler f
  where
    f request = toHandler env $ do
        pure "user foo"

server :: ServerT Api AppM
server = authApi :<|> userApi

errorFormatters :: Env -> ErrorFormatters
errorFormatters env =
    let port = unrefine env.envConfig.cfgServer.svrPort
     in defaultErrorFormatters
            { bodyParserErrorFormatter = badRequestErrorFormatter port
            , urlParseErrorFormatter = badRequestErrorFormatter port
            , headerParseErrorFormatter = badRequestErrorFormatter port
            , notFoundErrorFormatter = missingResourceErrorFormatter port
            }

badRequestErrorFormatter :: Int -> ErrorFormatter
badRequestErrorFormatter port _ _ err =
    err400
        { errHeaders = [contentTypeProbleamJson]
        , errBody = encode (problemDetails False port etyInvalidRequestFormat (Just (Text.pack err)) Nothing)
        }

missingResourceErrorFormatter :: Int -> NotFoundErrorFormatter
missingResourceErrorFormatter port _ =
    err404
        { errHeaders = [contentTypeProbleamJson]
        , errBody = encode (problemDetails False port etyMissingResource Nothing Nothing)
        }

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

            throwError
                err500
                    { errHeaders = [contentTypeProbleamJson]
                    , errBody = encode (problemDetails False port etyUnknownError Nothing Nothing)
                    }
        Right a -> pure a

toServerError :: (HasCallStack, MonadThrow m) => Int -> ApiException -> m ServerError
toServerError port ApiException{..} = do
    reason <- byteStringToString (statusMessage apiHttpStatus)

    let details = problemDetails False port apiErrorType apiErrorDescription (toJSON <$> apiErrorDetails)

    pure
        ServerError
            { errHTTPCode = statusCode apiHttpStatus
            , errReasonPhrase = reason
            , errHeaders = [contentTypeProbleamJson]
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

contentTypeProbleamJson :: Header
contentTypeProbleamJson = (hContentType, "application/problem+json")

byteStringToString :: (HasCallStack, MonadThrow m) => ByteString -> m String
byteStringToString bs =
    case Text.decodeUtf8' bs of
        Left err -> throwM err
        Right text -> pure (Text.unpack text)
