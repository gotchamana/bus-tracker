{-# LANGUAGE OverloadedStrings #-}

module Bus.Main (defaultMain) where

import Bus.App (Config (..), Database (..), Env (Env, envConfig, envLoggingChan), Server (..))
import Bus.Auth (getKeyByFriendlyName, readKeyStore, signToken, verifyToken)
import Bus.Logging (withAsyncLogging)
import Bus.Servant (waiApp)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (dupTChan, newBroadcastTChanIO)
import Crypto.JOSE (runJOSE)
import Network.Wai.Handler.Warp (run)

defaultMain :: IO ()
defaultMain = do
    ff <- readKeyStore "server.pfx" "secret"

    case ff of
        Left err -> print err
        Right keyStore -> case getKeyByFriendlyName "jwt" "secret" keyStore of
            Just keyPair -> do
                a <- runJOSE (signToken keyPair "foo" 10 >>= verifyToken keyPair)
                print a
            Nothing -> pure ()

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
        run 8080 (waiApp env)
