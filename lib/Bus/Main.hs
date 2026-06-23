{-# LANGUAGE OverloadedStrings #-}

module Bus.Main (defaultMain) where

import Bus.App (Config (..), Database (..), Env (Env, envConfig, envLoggingChan), Server (..))
import Bus.Logging (withAsyncLogging)
import Bus.Servant (waiApp)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (dupTChan, newBroadcastTChanIO)
import Crypto.Store.PKCS12 (SafeBag, SafeContents (SafeContents), readP12File, recover, recoverAuthenticated, unPKCS12)
import Data.ASN1.Types (ASN1)
import Data.Coerce (coerce)
import Network.Wai.Handler.Warp (run)

getSecretKey :: String -> [SafeBag] -> Maybe ASN1
getSecretKey alias bags = Nothing

defaultMain :: IO ()
defaultMain = do
    e <- readP12File "server.pfx"

    let ff = do
            optAuthP12 <- e
            (password, pkcs12) <- recoverAuthenticated "secret" optAuthP12
            bags <- fmap concat . coerce @_ @(_ _ [[SafeBag]]) . recover password . unPKCS12 $ pkcs12

            pure ()

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
