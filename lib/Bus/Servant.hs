module Bus.Servant (waiApp) where

import Bus.App (AppM, Env)
import Bus.Servant.Auth
import Bus.Util (toHandler)
import Bus.Web.User.Api (UserApi, userApi)
import Servant

type Api = UserApi

apiProxy :: Proxy Api
apiProxy = Proxy

server :: ServerT Api AppM
server = userApi

waiApp :: Env -> Application
waiApp env = serveWithContext apiProxy (authContext env) server'
  where
    server' = hoistServerWithContext apiProxy authContextProxy (toHandler env) server
