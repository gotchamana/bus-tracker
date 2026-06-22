{-# LANGUAGE OverloadedStrings #-}

module Bus.Servant (waiApp) where

import Bus.App (AppM (AppM), Env (envLoggingChan))
import Bus.Logging (runTChanLoggingT, logDebug)
import Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT))
import Servant

type Api = "users" :> Get '[JSON] Int

apiProxy :: Proxy Api
apiProxy = Proxy

server :: ServerT Api AppM
server = foo

foo :: AppM Int
foo = do
    logDebug "foo"
    pure 1

waiApp :: Env -> Application
waiApp env = serve apiProxy (toHandler env server)

toHandler :: Env -> AppM a -> Handler a
toHandler env (AppM readerT) = liftIO $ runTChanLoggingT (envLoggingChan env) (runReaderT readerT env)
