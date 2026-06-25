{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bus.Main (defaultMain) where

import Bus.App (Config (..), Database (..), Env (Env, envConfig, envLoggingChan), Server (..))
import Bus.Logging (withAsyncLogging)
import Bus.Servant (waiApp)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (dupTChan, newBroadcastTChanIO)
import Control.Monad (guard)
import Control.Monad.Except (ExceptT (ExceptT), liftEither, runExceptT)
import Crypto.Store.Error (StoreError)
import Crypto.Store.PKCS12 (Attribute (..), Bag (Bag, bagAttributes, bagInfo), SafeBag, SafeContents (SafeContents), SafeInfo (SafeContentsBag, SecretBag), readP12File, recover, recoverAuthenticated, unPKCS12)
import Data.ASN1.Types (ASN1 (ASN1String, OctetString), asn1CharacterToString)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List (find)
import Data.Maybe (listToMaybe, mapMaybe)
import Network.Wai.Handler.Warp (run)
import qualified Data.ByteString as BS

getSecretKey :: String -> [SafeBag] -> Maybe ByteString
getSecretKey alias = findJust f
  where
    f (Bag{..}) =
        case bagInfo of
            SecretBag secret -> do
                alias' <- getAlias bagAttributes
                guard (alias == alias')
                findJust extract secret
            SafeContentsBag (SafeContents bags) -> getSecretKey alias bags
            _ -> Nothing
    getAlias attrs = do
        let oid = [1, 2, 840, 113549, 1, 9, 20]

        Attribute{attrValues = [ASN1String asn1Char]} <- find (\(Attribute{attrType}) -> attrType == oid) attrs
        asn1CharacterToString asn1Char
    extract = \case
        OctetString str -> Just str
        _ -> Nothing

findJust :: (a -> Maybe b) -> [a] -> Maybe b
findJust = (listToMaybe .) . mapMaybe

readKeyStore :: String -> ByteString -> IO (Either StoreError [SafeBag])
readKeyStore path password = runExceptT $ do
    optAuthP12 <- ExceptT (readP12File path)
    liftEither $ do
        (passwd, pkcs12) <- recoverAuthenticated password optAuthP12
        contents :: [[SafeBag]] <- coerce . recover passwd . unPKCS12 $ pkcs12
        pure (concat contents)

defaultMain :: IO ()
defaultMain = do
    ff <- readKeyStore "server.pfx" "secret"
    case ff of
        Left err -> print err
        Right bags -> print $ BS.length <$> getSecretKey "jwt" bags

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
