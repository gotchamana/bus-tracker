{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Bus.Servant.Auth (authContextProxy, authHandler, JwtAuth) where

import Bus.App (Env)
import Bus.Util.Servant (toHandler)
import Data.Text (Text)
import Network.Wai (Request)
import Servant
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)

type JwtAuth = AuthProtect "jwt"

type instance AuthServerData JwtAuth = Text

authContextProxy :: Proxy '[AuthHandler Request Text]
authContextProxy = Proxy

authHandler :: Env -> AuthHandler Request Text
authHandler env = mkAuthHandler f
  where
    f request = toHandler env $ do
        pure "user foo"
