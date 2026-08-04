{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bus.Web.App.Servant (waiApp) where

import Bus.Exception (
    ApiException (..),
    ErrorType,
    NoSuchKeyException (NoSuchKeyException),
    etyInvalidCredentials,
    etyInvalidRequestFormat,
    etyMessage,
    etyMessageCode,
    etyMissingResource,
    etyType,
    etyUnknownError,
 )
import Bus.Logger (logErrorEx, logWarnEx, runTChanLoggingT)
import Bus.Security.Jwt (Token (tokTokenType), TokenType (Access, Refresh), Tokens (Tokens, toksAccessToken, toksRefreshToken), verifyToken)
import Bus.Security.KeyStore (getKeyByFriendlyName)
import Bus.Util.Aeson (fieldPrefixRemovalOptions)
import Bus.Web.App.Endpoint (Api, server)
import Bus.Web.App.Type (
    AppM (AppM),
    Config (cfgSecurity, cfgServer),
    CookieNames (cknAccessToken, cknRefreshToken),
    Env (envConfig, envCookieNames, envKeyStore, envKeyStorePassword, envLoggingChan),
    Security (secJwtKeyFriendlyName),
    Server (svrPort),
 )
import Control.Exception (Exception (fromException), ExceptionWithContext (ExceptionWithContext), SomeAsyncException, SomeException, try)
import Control.Exception.Context (emptyExceptionContext)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Reader (MonadIO (liftIO), MonadReader, ReaderT (runReaderT), asks)
import Control.Monad.Time (MonadTime (currentTime, monotonicTime))
import Data.Aeson (ToJSON (toEncoding, toJSON), Value, encode, genericToEncoding, genericToJSON)
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Clock (getMonotonicTime)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Network.HTTP.Types (Header, Status (statusCode, statusMessage), hContentType, hCookie, status401)
import Network.URI (URIAuth (uriPort, uriRegName), nullURIAuth)
import Network.Wai (Request (requestHeaders))
import Rerefined (unrefine)
import Servant hiding (Header)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Web.Cookie (parseCookies)

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
    toJSON = genericToJSON (fieldPrefixRemovalOptions "pd")
    toEncoding = genericToEncoding (fieldPrefixRemovalOptions "pd")

newtype MockMonadTime a = MockMonadTime (ReaderT (UTCTime, Double) (Either SomeException) a)
    deriving (Functor, Applicative, Monad, MonadReader (UTCTime, Double), MonadThrow)

instance MonadTime MockMonadTime where
    currentTime = asks fst
    monotonicTime = asks snd

waiApp :: Env -> Application
waiApp env = serveWithContext apiProxy (errorFormatters env :. authHandler env :. EmptyContext) server'
  where
    server' = hoistServerWithContext apiProxy authContextProxy (toHandler env) server

apiProxy :: Proxy Api
apiProxy = Proxy

authContextProxy :: Proxy '[AuthHandler Request Tokens]
authContextProxy = Proxy

authHandler :: Env -> AuthHandler Request Tokens
authHandler = mkAuthHandler . authenticate

authenticate :: Env -> Request -> Handler Tokens
authenticate env request = toHandler env $ do
    (rawAccess, rawRefresh) <- case extractTokens of
        Just tokens -> pure tokens
        Nothing -> throwM defaultException{apiErrorDescription = Just "No token present"}
    keyPair <- getKeyPair
    result <-
        runMockMonadTime $
            (,)
                <$> verifyToken keyPair rawAccess
                <*> traverse (verifyToken keyPair) rawRefresh

    case result of
        Left err -> do
            logWarnEx ["JWT verification failed"] (ExceptionWithContext emptyExceptionContext err)
            throwM defaultException{apiErrorDescription = Just "Token verification failed"}
        Right (access, refresh) ->
            case (tokTokenType access, tokTokenType <$> refresh) of
                (Refresh, _) -> throwM defaultException{apiErrorDescription = Just "Wrong token type: refresh"}
                (_, Just Access) -> throwM defaultException{apiErrorDescription = Just "Wrong token type: access"}
                _ ->
                    pure
                        Tokens
                            { toksAccessToken = access
                            , toksRefreshToken = refresh
                            }
  where
    extractTokens = do
        cookies <- parseCookies <$> lookup hCookie (requestHeaders request)

        let cookieNames = env.envCookieNames
            access = lookup cookieNames.cknAccessToken cookies
            refresh = lookup cookieNames.cknRefreshToken cookies

        (,refresh) <$> access
    getKeyPair = do
        let jwtName = Text.unpack (unrefine env.envConfig.cfgSecurity.secJwtKeyFriendlyName)
            keyStore = env.envKeyStore
            password = env.envKeyStorePassword

        case getKeyByFriendlyName jwtName password keyStore of
            Just keyPair -> pure keyPair
            Nothing -> throwM (NoSuchKeyException jwtName)
    defaultException =
        ApiException
            { apiHttpStatus = status401
            , apiErrorType = etyInvalidCredentials
            , apiErrorDescription = Nothing
            , apiErrorDetails = Nothing
            }

runMockMonadTime :: (MonadIO m) => MockMonadTime a -> m (Either SomeException a)
runMockMonadTime (MockMonadTime readerT) = do
    time <- liftIO $ (,) <$> getCurrentTime <*> getMonotonicTime
    pure (runReaderT readerT time)

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
