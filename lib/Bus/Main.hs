{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Bus.Main (defaultMain) where

import Bus.App (Config (..), Env (..), Security (Security, secKeyStoreFile, secKeyStorePasswordFile), Server (svrPort))
import Bus.Auth (KeyStore, readKeyStore)
import Bus.Logging (logInfo', runTChanLoggingT, withAsyncLogging)
import Bus.Servant (waiApp)
import Control.Concurrent.STM (TChan, atomically)
import Control.Concurrent.STM.TChan (dupTChan, newBroadcastTChanIO)
import Control.Monad.Catch (MonadThrow (throwM))
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
        -- & setOnException
        -- & setInstallShutdownHandler
        -- & setGracefulShutdownTimeout
  where
    logStartup =
        logInfo'
            [ "Warp is running, host="
            , Text.pack (show (getHost defaultSettings))
            , ", port="
            , showt port
            ]
