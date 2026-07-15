module Bus.Web.Auth.Api (AuthApi, authApi) where

import Bus.App (AppM)
import Data.Aeson (Object)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Servant

import Bus.Web.Auth.Service qualified as AuthSvc
import Data.HashMap.Strict qualified as HashMap

type AuthApi = "auth" :> ("login" :> ReqBody '[JSON] Object :> Post '[JSON] (HashMap Text Text))

authApi :: ServerT AuthApi AppM
authApi = login

login :: Object -> AppM (HashMap Text Text)
login object = do
    AuthSvc.validateLogin object
    pure HashMap.empty
