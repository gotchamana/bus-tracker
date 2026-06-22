module Bus.Servant (waiApp) where

import Bus.App (AppM (AppM), Env (envLoggingChan))
import Bus.Logging (runTChanLoggingT)
import Bus.Servant.Auth
import Bus.Web.User.Api (UserApi, userApi)
import Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT))
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

toHandler :: Env -> AppM a -> Handler a
toHandler env (AppM readerT) = liftIO $ runTChanLoggingT (envLoggingChan env) (runReaderT readerT env)
