{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Bus.Main (defaultMain) where

import Bus.App (Config (..), Env (..), Security (Security, secKeyStoreFile, secKeyStorePasswordFile))
import Bus.Auth (KeyStore, readKeyStore)
import Bus.Logging (withAsyncLogging)
import Bus.Servant (waiApp)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (dupTChan, newBroadcastTChanIO)
import Control.Monad.Catch (MonadThrow (throwM))
import Data.Aeson (AesonException (AesonException), eitherDecodeStrict)
import Data.ByteString (ByteString)
import GHC.Stack (HasCallStack)
import Network.Wai.Handler.Warp (run)
import Rerefined.Refine (unrefine)
import System.File.OsPath (readFile')
import System.OsPath (OsPath, osp)

import Data.ByteString.Char8 qualified as BC

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
        run 8080 (waiApp env)

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
