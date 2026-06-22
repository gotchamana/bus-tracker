module Bus.Web.User.Api (UserApi, userApi) where

import Bus.App (AppM)
import Servant

type UserApi = "users" :> Get '[JSON] Int

userApi :: ServerT UserApi AppM
userApi = getUser

getUser :: AppM Int
getUser = pure 1
