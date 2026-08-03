{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Bus.Web.App.Endpoint (Api, server) where

import Bus.Security.Jwt (Token)
import Bus.Web.App.Type (AppM)
import Bus.Web.Auth.Api (login)
import Bus.Web.User.Api (getUser, registerUser)
import Data.Aeson (Object)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.UUID (UUID)
import Servant
import Servant.Server.Experimental.Auth (AuthServerData)
import Web.Cookie (SetCookie)

type Api = AuthApi :<|> UserApi

type AuthApi =
    "auth"
        :> ("login" :> ReqBody '[JSON] Object :> Verb 'POST 203 '[JSON] (Headers '[HSetCookie, HSetCookie] NoContent))

type UserApi =
    "users"
        :> ( ReqBody '[JSON] Object :> PostCreated '[JSON] (HashMap Text UUID)
                :<|> JwtAuth :> Get '[JSON] Int
           )

type HSetCookie = Header "SetCookie" SetCookie

type JwtAuth = AuthProtect "jwt"

type instance AuthServerData JwtAuth = Token

server :: ServerT Api AppM
server = authApi :<|> userApi

authApi :: ServerT AuthApi AppM
authApi = login

userApi :: ServerT UserApi AppM
userApi = registerUser :<|> getUser
