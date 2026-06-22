{-# LANGUAGE OverloadedStrings #-}

module Bus.Main (defaultMain) where

import Bus.App (AppM (AppM), Config (..), Database (..), Env (Env, envConfig, envLoggingChan), Server (..))
import Bus.Logging (logDebug, runTChanLoggingT, withAsyncLogging)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (dupTChan, newBroadcastTChanIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Coerce (coerce)

defaultMain :: IO ()
defaultMain = do
    chan <- newBroadcastTChanIO
    duplicatedChan <- atomically (dupTChan chan)

    let server =
            Server
                { svrPort = 8080
                }
        database =
            Database
                { dbUrl = ""
                , dbUser = ""
                , dbPasswordFile = ""
                }
        env =
            Env
                { envConfig =
                    Config
                        { cfgServer = server
                        , cfgDatabase = database
                        }
                , envLoggingChan = chan
                }

    withAsyncLogging duplicatedChan $ \_ -> do
        runTChanLoggingT chan (runReaderT (coerce app) env)

app :: AppM ()
app = do
    logDebug "Hello World"
