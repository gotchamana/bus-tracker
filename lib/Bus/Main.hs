{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Bus.Main (defaultMain) where

import Bus.App (Config (..), Database (..), Env (..), Security (..), Server (..))
import Bus.Auth (KeyStore, readKeyStore)
import Bus.Exception (isAsyncException)
import Bus.Logging (logErrorEx, logInfo, logInfo', runTChanLoggingT, withAsyncLogging)
import Bus.Servant (waiApp)
import Control.Concurrent.STM (TChan, atomically)
import Control.Concurrent.STM.TChan (dupTChan, newBroadcastTChanIO)
import Control.Exception (ExceptionWithContext (ExceptionWithContext), bracket, someExceptionContext)
import Control.Monad (unless, void)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger.CallStack (LogLine)
import Data.Aeson (AesonException (AesonException), eitherDecodeStrict)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Pool (Pool, defaultPoolConfig, destroyAllResources, newPool)
import Data.Text (Text)
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection, close, connect)
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
import Data.Text.Encoding qualified as Text

defaultMain :: IO ()
defaultMain = do
    config <- loadConfig [osp|data/config.json|]
    (keyStore, keyStorePassword) <- loadKeyStore (cfgSecurity config)

    chan <- newBroadcastTChanIO
    duplicatedChan <- atomically (dupTChan chan)

    withAsyncLogging duplicatedChan $ \_ ->
        withDatabasePool config.cfgDatabase $ \pool -> do
            let env =
                    Env
                        { envConfig = config
                        , envLoggingChan = chan
                        , envKeyStore = keyStore
                        , envKeyStorePassword = keyStorePassword
                        , envDbPool = pool
                        }
                settings = warpSettings chan (unrefine config.cfgServer.svrPort)
                app = waiApp env

            runSettings settings app

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

withDatabasePool :: (HasCallStack) => Database -> (Pool Connection -> IO a) -> IO a
withDatabasePool Database{..} = bracket createDbPool destroyAllResources
  where
    createDbPool = do
        password <- case unrefine <$> dbPasswordFile of
            Just path -> readFile' path >>= (byteStringToText . removeTrailingNewLine)
            Nothing -> pure ""

        let connectInfo =
                ConnectInfo
                    { connectUser = Text.unpack dbUser
                    , connectPort = maybe 5432 (fromIntegral . unrefine) dbPort
                    , connectPassword = Text.unpack password
                    , connectHost = Text.unpack dbHost
                    , connectDatabase = Text.unpack dbDbName
                    }
            poolConfig = defaultPoolConfig (connect connectInfo) close (3 * 60) 10

        newPool poolConfig

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

removeTrailingNewLine :: ByteString -> ByteString
removeTrailingNewLine bs =
    case BC.unsnoc bs of
        Just (bs', '\n') -> bs'
        _ -> bs

byteStringToText :: (HasCallStack, MonadThrow m) => ByteString -> m Text
byteStringToText bs =
    case Text.decodeUtf8' bs of
        Left err -> throwM err
        Right text -> pure text
