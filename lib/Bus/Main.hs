{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Bus.Main (defaultMain) where

import Bus.App (Config (..), Env (..), Security (Security, secKeyStoreFile, secKeyStorePasswordFile), Server (svrPort))
import Bus.Auth (KeyStore, readKeyStore)
import Bus.Exception (isAsyncException)
import Bus.Logging (logErrorEx, logInfo, logInfo', runTChanLoggingT, withAsyncLogging)
import Bus.Servant (waiApp)
import Control.Concurrent.STM (TChan, atomically)
import Control.Concurrent.STM.TChan (dupTChan, newBroadcastTChanIO)
import Control.Exception (ExceptionWithContext (ExceptionWithContext), someExceptionContext)
import Control.Monad (unless, void)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger.CallStack (LogLine)
import Data.Aeson (AesonException (AesonException), eitherDecodeStrict)
import Data.ByteString (ByteString)
import Data.Function ((&))
import GHC.Stack (HasCallStack)
import Network.Wai.Handler.Warp (
    Port,
    Settings,
    defaultSettings,
    getHost,
    runSettings,
    setBeforeMainLoop,
    setGracefulShutdownTimeout,
    setInstallShutdownHandler,
    setOnException,
    setPort,
    setServerName,
 )
import Rerefined.Refine (unrefine)
import System.File.OsPath (readFile')
import System.OsPath (OsPath, osp)
import System.Posix.Signals (Handler (CatchOnce), installHandler, sigINT, sigTERM)
import TextShow (TextShow (showt))

import Data.ByteString.Char8 qualified as BC
import Data.Text qualified as Text

defaultMain :: IO ()
defaultMain = do
    config <- loadConfig [osp|config.json|]
    (keyStore, keyStorePassword) <- loadKeyStore (cfgSecurity config)

    chan <- newBroadcastTChanIO
    duplicatedChan <- atomically (dupTChan chan)

    let env =
            Env
                { envConfig = config
                , envLoggingChan = chan
                , envKeyStore = keyStore
                , envKeyStorePassword = keyStorePassword
                }

    withAsyncLogging duplicatedChan $ \_ -> do
        runSettings (warpSettings chan (svrPort (cfgServer config))) (waiApp env)

loadConfig :: (HasCallStack) => OsPath -> IO Config
loadConfig path = do
    content <- readFile' path

    case eitherDecodeStrict content of
        Left err -> throwM (AesonException err)
        Right config -> pure config

loadKeyStore :: (HasCallStack) => Security -> IO (KeyStore, ByteString)
loadKeyStore Security{secKeyStoreFile, secKeyStorePasswordFile} = do
    password <- removeTrailingNewLine <$> readFile' (unrefine secKeyStorePasswordFile)

    (,password) <$> readKeyStore (unrefine secKeyStoreFile) password
  where
    removeTrailingNewLine bs =
        case BC.unsnoc bs of
            Just (bs', '\n') -> bs'
            _ -> bs

warpSettings :: TChan LogLine -> Port -> Settings
warpSettings chan port =
    defaultSettings
        & setPort port
        & setBeforeMainLoop (runTChanLoggingT chan logStartup)
        & setServerName ""
        & setOnException (\_ -> runTChanLoggingT chan . logException)
        & setInstallShutdownHandler shutdownHandler
        & setGracefulShutdownTimeout (Just 10)
  where
    logStartup =
        logInfo'
            [ "Warp is running, host="
            , Text.pack (show (getHost defaultSettings))
            , ", port="
            , showt port
            ]
    logException se = unless (isAsyncException se) $ logErrorEx [] (ExceptionWithContext (someExceptionContext se) se)
    shutdownHandler closeSocket =
        let action = do
                liftIO (runTChanLoggingT chan (logInfo "Warp is stopping"))
                closeSocket
         in do
                void $ installHandler sigINT (CatchOnce action) Nothing
                void $ installHandler sigTERM (CatchOnce action) Nothing
