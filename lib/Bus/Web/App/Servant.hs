{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

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
import Bus.Security.Jwt (
    Token (tokTokenType),
    TokenType (Access, Refresh),
    verifyToken,
 )
import Bus.Security.KeyStore (getKeyByFriendlyName)
import Bus.Util.Aeson (fieldPrefixRemovalOptions)
import Bus.Web.App.Type (
    AppM (AppM),
    Config (cfgSecurity, cfgServer),
    CookieNames (cknAccessToken, cknRefreshToken),
    Env (envConfig, envCookieNames, envKeyStore, envKeyStorePassword, envLoggingChan),
    Security (secJwtKeyFriendlyName),
    Server (svrPort),
 )
import Bus.Web.Auth.Api (login, logout)
import Bus.Web.User.Api (getUser, registerUser)
import Control.Exception (
    Exception (fromException),
    ExceptionWithContext (ExceptionWithContext),
    SomeAsyncException,
    SomeException,
    try,
 )
import Control.Exception.Context (emptyExceptionContext)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT (runReaderT), asks)
import Control.Monad.Time (MonadTime (currentTime, monotonicTime))
import Data.Aeson (Object, ToJSON (toEncoding, toJSON), Value, encode, genericToEncoding, genericToJSON)
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import GHC.Clock (getMonotonicTime)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Network.HTTP.Types (Status (statusCode, statusMessage), hContentType, hCookie, status401)
import Network.URI (URIAuth (uriPort, uriRegName), nullURIAuth)
import Network.Wai (Request (requestHeaders))
import Rerefined (unrefine)
import Servant
import Servant.Server.Internal.Delayed (addAuthCheck)
import Servant.Server.Internal.DelayedIO (DelayedIO, delayedFailFatal)
import Web.Cookie (SetCookie, parseCookies)

import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Types qualified as Http

type Api = AuthApi :<|> UserApi

type AuthApi =
    "auth"
        :> ( "login" :> ReqBody '[JSON] Object :> Verb 'POST 203 '[JSON] (Headers '[HSetCookie, HSetCookie] NoContent)
                :<|> "logout" :> WithAuth '[Access, Refresh] :> Verb 'POST 203 '[JSON] (Headers '[HSetCookie, HSetCookie] NoContent)
           )

type UserApi =
    "users"
        :> ( ReqBody '[JSON] Object :> PostCreated '[JSON] (HashMap Text UUID)
                :<|> WithAuth '[Access] :> Get '[JSON] Int
           )

type HSetCookie = Header "SetCookie" SetCookie

data WithAuth (a :: [TokenType])

instance (HasServer api ctx) => HasServer (WithAuth '[] :> api) ctx where
    type ServerT (WithAuth '[] :> api) m = ServerT api m

    hoistServerWithContext _ = hoistServerWithContext @api Proxy

    route _ = route @api Proxy

instance (HasServer (WithAuth xs :> api) ctx, HasContextEntry ctx Env) => HasServer (WithAuth (Access ': xs) :> api) ctx where
    type ServerT (WithAuth (Access ': xs) :> api) m = Token -> ServerT (WithAuth xs :> api) m

    hoistServerWithContext _ ctx nt s = hoistServerWithContext @(WithAuth xs :> api) Proxy ctx nt . s

    route _ ctx subserver = route @(WithAuth xs :> api) Proxy ctx (addAuthCheck subserver check)
      where
        check = authenticate Access (getContextEntry ctx)

instance (HasServer (WithAuth xs :> api) ctx, HasContextEntry ctx Env) => HasServer (WithAuth (Refresh ': xs) :> api) ctx where
    type ServerT (WithAuth (Refresh ': xs) :> api) m = Token -> ServerT (WithAuth xs :> api) m

    hoistServerWithContext _ ctx f s = hoistServerWithContext @(WithAuth xs :> api) Proxy ctx f . s

    route _ ctx subserver = route @(WithAuth xs :> api) Proxy ctx (addAuthCheck subserver check)
      where
        check = authenticate Refresh (getContextEntry ctx)

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
waiApp env = serveWithContext apiProxy (env :. errorFormatters env :. EmptyContext) server'
  where
    apiProxy = Proxy @Api
    contextProxy = Proxy @'[Env]
    server' = hoistServerWithContext apiProxy contextProxy (toHandler env) server

server :: ServerT Api AppM
server = authApi :<|> userApi

authApi :: ServerT AuthApi AppM
authApi = login :<|> logout

userApi :: ServerT UserApi AppM
userApi = registerUser :<|> getUser

authenticate :: (HasCallStack) => TokenType -> Env -> DelayedIO Token
authenticate tokenType env = do
    request <- ask
    result <- liftIO . runHandler . toHandler env $ authenticateApp tokenType request

    case result of
        Left err -> delayedFailFatal err
        Right token -> pure token

authenticateApp :: (HasCallStack) => TokenType -> Request -> AppM Token
authenticateApp tokenType request = do
    env <- ask

    rawToken <- case extractToken env.envCookieNames of
        Just token -> pure token
        Nothing ->
            let tokenType' = showTokenType tokenType
                description = Text.concat ["No ", tokenType', " token present"]
             in throwM defaultException{apiErrorDescription = Just description}
    keyPair <-
        getKeyPair
            (unrefine env.envConfig.cfgSecurity.secJwtKeyFriendlyName)
            env.envKeyStore
            env.envKeyStorePassword

    result <- runMockMonadTime (verifyToken keyPair rawToken)

    case result of
        Left err -> do
            logWarnEx ["JWT verification failed"] (ExceptionWithContext emptyExceptionContext err)
            throwM defaultException{apiErrorDescription = Just "Token verification failed"}
        Right token ->
            if tokTokenType token == tokenType
                then pure token
                else
                    let description =
                            Text.concat
                                [ "Wrong token type: expected "
                                , showTokenType tokenType
                                , ", but got "
                                , showTokenType (tokTokenType token)
                                ]
                     in throwM defaultException{apiErrorDescription = Just description}
  where
    extractToken cookieNames = do
        cookies <- parseCookies <$> lookup hCookie (requestHeaders request)

        let access = lookup cookieNames.cknAccessToken cookies
            refresh = lookup cookieNames.cknRefreshToken cookies

        case tokenType of
            Access -> access
            Refresh -> refresh
    getKeyPair alias keyStore password = do
        let jwtName = Text.unpack alias

        case getKeyByFriendlyName jwtName password keyStore of
            Just keyPair -> pure keyPair
            Nothing -> throwM (NoSuchKeyException jwtName)
    showTokenType = \case
        Access -> "access"
        Refresh -> "refresh"
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

contentTypeProbleamJson :: Http.Header
contentTypeProbleamJson = (hContentType, "application/problem+json")

byteStringToString :: (HasCallStack, MonadThrow m) => ByteString -> m String
byteStringToString bs =
    case Text.decodeUtf8' bs of
        Left err -> throwM err
        Right text -> pure (Text.unpack text)
