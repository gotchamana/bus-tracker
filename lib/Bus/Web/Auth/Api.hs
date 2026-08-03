{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Bus.Web.Auth.Api (login) where

import Bus.Exception (NoSuchKeyException (NoSuchKeyException))
import Bus.Security.KeyStore (getKeyByFriendlyName)
import Bus.Web.App.Type (AppM, Config (cfgSecurity), Env (envConfig, envKeyStore, envKeyStorePassword), Security (secJwtKeyFriendlyName))
import Bus.Web.Auth.Service (Authentication (Authentication))
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Reader (MonadReader (ask))
import Data.Aeson (Object)
import Data.Function ((&))
import Rerefined (unrefine)
import Servant
import Web.Cookie (
    SetCookie (..),
    defaultSetCookie,
    sameSiteStrict,
 )

import Bus.Web.Auth.Service qualified as AuthSvc
import Data.Text qualified as Text

login :: Object -> AppM (Headers '[Header "SetCookie" SetCookie, Header "SetCookie" SetCookie] NoContent)
login object = do
    validLogin <- AuthSvc.validateLogin object

    env <- ask

    let jwtName = Text.unpack (unrefine env.envConfig.cfgSecurity.secJwtKeyFriendlyName)
        keyStore = env.envKeyStore
        password = env.envKeyStorePassword

    Authentication{auAccessToken, auRefreshToken} <-
        case getKeyByFriendlyName jwtName password keyStore of
            Just keyPair -> AuthSvc.signAuthToken keyPair validLogin
            Nothing -> throwM (NoSuchKeyException jwtName)

    let accessCookie =
            defaultSetCookie
                { setCookieName = "accessToken"
                , setCookieValue = auAccessToken.atTokenValue
                , setCookieHttpOnly = True
                , setCookieMaxAge = Just (fromIntegral auAccessToken.atExpirationSec)
                , setCookieSecure = True
                , setCookieSameSite = Just sameSiteStrict
                }
        refreshCookie =
            defaultSetCookie
                { setCookieName = "refreshToken"
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
