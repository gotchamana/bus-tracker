{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Bus.Web.Auth.Api (login, logout) where

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
import Bus.Web.Auth.Service (Authentication (Authentication))
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Reader (MonadReader (ask), asks)
import Data.Aeson (Object)
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

    let accessCookie =
            defaultSetCookie
                { setCookieName = cookieNames.cknAccessToken
                , setCookieValue = auAccessToken.atTokenValue
                , setCookieHttpOnly = True
                , setCookieMaxAge = Just (fromIntegral auAccessToken.atExpirationSec)
                , setCookieSecure = True
                , setCookieSameSite = Just sameSiteStrict
                }
        refreshCookie =
            defaultSetCookie
                { setCookieName = cookieNames.cknRefreshToken
                , setCookieValue = auRefreshToken.atTokenValue
                , setCookieHttpOnly = True
                , setCookieMaxAge = Just (fromIntegral auRefreshToken.atExpirationSec)
                , setCookieSecure = True
                , setCookieSameSite = Just sameSiteStrict
                }

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
