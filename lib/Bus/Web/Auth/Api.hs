{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bus.Web.Auth.Api (login, logout, refresh) where

import Bus.Exception (NoSuchKeyException (NoSuchKeyException))
import Bus.Security.Jwt (Token)
import Bus.Security.KeyStore (getKeyByFriendlyName)
import Bus.Web.App.Type (
    AppM,
    Config (cfgSecurity),
    CookieNames (cknAccessToken, cknRefreshToken),
    Env (envConfig, envCookieNames, envKeyStore, envKeyStorePassword),
    Security (secJwtKeyFriendlyName),
 )
import Bus.Web.Auth.Service (AuthToken (..), Authentication (Authentication))
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Reader (MonadReader (ask), asks)
import Data.Aeson (Object)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Rerefined (unrefine)
import Servant
import Web.Cookie (SetCookie (..), defaultSetCookie, sameSiteStrict)

import Bus.Web.Auth.Service qualified as AuthSvc
import Data.Text qualified as Text

type HSetCookie = Header "SetCookie" SetCookie

login :: Object -> AppM (Headers '[HSetCookie, HSetCookie] NoContent)
login object = do
    validLogin <- AuthSvc.validateLogin object

    env <- ask

    let jwtName = Text.unpack (unrefine env.envConfig.cfgSecurity.secJwtKeyFriendlyName)
        keyStore = env.envKeyStore
        password = env.envKeyStorePassword
        cookieNames = env.envCookieNames

    Authentication{auAccessToken, auRefreshToken} <-
        case getKeyByFriendlyName jwtName password keyStore of
            Just keyPair -> AuthSvc.signAuthTokenByLogin keyPair validLogin
            Nothing -> throwM (NoSuchKeyException jwtName)

    let accessCookie = tokenCookie cookieNames.cknAccessToken auAccessToken
        refreshCookie = tokenCookie cookieNames.cknRefreshToken auRefreshToken

    pure $
        NoContent
            & addHeader accessCookie
            & addHeader refreshCookie

logout :: Token -> Token -> AppM (Headers '[HSetCookie, HSetCookie] NoContent)
logout _accessToken refreshToken = do
    AuthSvc.invalidateRefreshToken refreshToken

    cookieNames <- asks envCookieNames

    let accessCookie =
            defaultSetCookie
                { setCookieName = cookieNames.cknAccessToken
                , setCookieValue = ""
                , setCookieHttpOnly = True
                , setCookieMaxAge = Just 0
                , setCookieSecure = True
                , setCookieSameSite = Just sameSiteStrict
                }
        refreshCookie =
            defaultSetCookie
                { setCookieName = cookieNames.cknRefreshToken
                , setCookieValue = ""
                , setCookieHttpOnly = True
                , setCookieMaxAge = Just 0
                , setCookieSecure = True
                , setCookieSameSite = Just sameSiteStrict
                }

    pure $
        NoContent
            & addHeader accessCookie
            & addHeader refreshCookie

refresh :: Token -> AppM (Headers '[HSetCookie, HSetCookie] NoContent)
refresh refreshToken = do
    env <- ask

    let jwtName = Text.unpack (unrefine env.envConfig.cfgSecurity.secJwtKeyFriendlyName)
        keyStore = env.envKeyStore
        password = env.envKeyStorePassword
        cookieNames = env.envCookieNames

    Authentication{auAccessToken, auRefreshToken} <-
        case getKeyByFriendlyName jwtName password keyStore of
            Just keyPair -> AuthSvc.signAuthTokenByRefreshToken keyPair refreshToken
            Nothing -> throwM (NoSuchKeyException jwtName)

    let accessCookie = tokenCookie cookieNames.cknAccessToken auAccessToken
        refreshCookie = tokenCookie cookieNames.cknRefreshToken auRefreshToken

    pure $
        NoContent
            & addHeader accessCookie
            & addHeader refreshCookie

tokenCookie :: ByteString -> AuthToken -> SetCookie
tokenCookie name AuthToken{..} =
    defaultSetCookie
        { setCookieName = name
        , setCookieValue = atTokenValue
        , setCookieHttpOnly = True
        , setCookieMaxAge = Just (fromIntegral atExpirationSec)
        , setCookieSecure = True
        , setCookieSameSite = Just sameSiteStrict
        }
