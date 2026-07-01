{-# LANGUAGE OverloadedStrings #-}

module Bus.Util (toHandler) where

import Bus.App (AppM (AppM), Env (envLoggingChan))
import Bus.Exception (isAsyncException)
import Bus.Logging (logErrorEx, runTChanLoggingT)
import Control.Exception (ExceptionWithContext (ExceptionWithContext), SomeException, throwIO, try)
import Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT))
import Servant

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

            throwError err500{errBody = "Some errors"}
        Right a -> pure a
