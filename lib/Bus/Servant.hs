{-# LANGUAGE OverloadedStrings #-}

module Bus.Servant (waiApp) where

import Bus.App (AppM (AppM), Env (envLoggingChan))
import Bus.Exception (isAsyncException)
import Bus.Logging (logErrorEx, runTChanLoggingT)
import Bus.Servant.Auth
import Bus.Web.User.Api (UserApi, userApi)
import Control.Exception (ExceptionWithContext (ExceptionWithContext), SomeException, throwIO, try)
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
toHandler env (AppM readerT) = do
    let loggingChan = envLoggingChan env
        action = runTChanLoggingT loggingChan (runReaderT readerT env)

    result <- liftIO (try @(ExceptionWithContext SomeException) action)

    case result of
        Left e@(ExceptionWithContext _ se) -> do
            liftIO $
                if isAsyncException se
                    then throwIO se
                    else runTChanLoggingT loggingChan (logErrorEx ["Unknown error"] e)

            throwError err500 {errBody = "Some errors"}
        Right a -> pure a
