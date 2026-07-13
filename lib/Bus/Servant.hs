{-# LANGUAGE OverloadedRecordDot #-}

module Bus.Servant (waiApp) where

import Bus.App (AppM, Config (cfgServer), Env (envConfig), Server (svrPort))
import Bus.Servant.Auth
import Bus.Util (badRequestErrorFormatter, missingResourceErrorFormatter, toHandler)
import Bus.Web.User.Api (UserApi, userApi)
import Rerefined (unrefine)
import Servant

type Api = UserApi

apiProxy :: Proxy Api
apiProxy = Proxy

server :: ServerT Api AppM
server = userApi

errorFormatters :: Env -> ErrorFormatters
errorFormatters env =
    let port = unrefine env.envConfig.cfgServer.svrPort
     in defaultErrorFormatters
            { bodyParserErrorFormatter = badRequestErrorFormatter port
            , urlParseErrorFormatter = badRequestErrorFormatter port
            , headerParseErrorFormatter = badRequestErrorFormatter port
            , notFoundErrorFormatter = missingResourceErrorFormatter port
            }

waiApp :: Env -> Application
waiApp env = serveWithContext apiProxy (errorFormatters env :. authHandler env :. EmptyContext) server'
  where
    server' = hoistServerWithContext apiProxy authContextProxy (toHandler env) server
