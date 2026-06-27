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
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow (throwM), try)
import Data.Aeson (AesonException (AesonException), eitherDecodeStrict)
import Data.ByteString (ByteString)
import Data.Text (unpack)
import GHC.Stack (HasCallStack)
import Network.Wai.Handler.Warp (run)
import System.File.OsPath (readFile')
import System.OsPath (OsPath, encodeUtf, osp)
import System.OsPath.Encoding (EncodingException)

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
    passwordFilePath <- try (encodeUtf (unpack secKeyStorePasswordFile)) >>= liftEitherEx @EncodingException
    password <- removeTrailingNewLine <$> readFile' passwordFilePath

    (,password) <$> readKeyStore (unpack secKeyStoreFile) password
  where
    liftEitherEx :: (HasCallStack, Exception e, MonadThrow m) => Either e a -> m a
    liftEitherEx = \case
        Left e -> throwM e
        Right a -> pure a
    removeTrailingNewLine bs =
        case BC.unsnoc bs of
            Just (bs', '\n') -> bs'
            _ -> bs
