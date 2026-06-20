{-# LANGUAGE OverloadedStrings #-}

module Bus.Main (defaultMain) where

import Bus.App (AppM (AppM), Config (..), Database (..), Env (Env, envConfig), Server (..))
import Bus.Logging (forkLoggingThread, logDebug)
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.Chan (Chan, newChan)
import Control.Monad.Logger.CallStack (LogLine, runChanLoggingT)
import Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT), MonadReader (ask))
import Data.Coerce (coerce)
import Control.Concurrent.STM.TChan (newBroadcastTChanIO)

defaultMain :: IO ()
defaultMain = do
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
                }

    -- chan <- newBroadcastTChanIO
    chan <- newChan
    forkLoggingThread chan

    runChanLoggingT chan (runReaderT (coerce (app chan)) env)

app :: Chan LogLine -> AppM ()
app chan = do
    env <- ask

    logDebug "foooooooooo"

    liftIO $ do
        withAsync
            (runChanLoggingT chan (runReaderT (logDebug "barrrrrrrrrrrrrr" >> pure ()) env))
            wait

        withAsync
            (runChanLoggingT chan (runReaderT (logDebug "quxxxxxxxxxxx" >> pure ()) env))
            wait
