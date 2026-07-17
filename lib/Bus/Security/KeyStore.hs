{-# LANGUAGE RecordWildCards #-}

module Bus.Security.KeyStore (KeyStore, getKeyByFriendlyName, readKeyStore) where

import Bus.Exception (CryptoStoreException (CryptoStoreException))
import Bus.Util.Either (eitherToMaybe)
import Control.Exception (Exception)
import Control.Monad (guard)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Crypto.Store.PKCS12 (
    Bag (Bag, bagAttributes, bagInfo),
    SafeBag,
    SafeContents (SafeContents),
    SafeInfo (KeyBag, PKCS8ShroudedKeyBag, SafeContentsBag),
    getFriendlyName,
    getSafeKeys,
    readP12FileFromMemory,
    recover,
    recoverAuthenticated,
    toProtectionPassword,
    unPKCS12,
 )
import Crypto.Store.PKCS8 (KeyPair)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Maybe (listToMaybe, mapMaybe)
import GHC.Stack (HasCallStack)
import System.File.OsPath (readFile')
import System.OsPath (OsPath)

newtype KeyStore = KeyStore [SafeBag]

readKeyStore :: (HasCallStack, MonadThrow m, MonadIO m) => OsPath -> ByteString -> m KeyStore
readKeyStore path password = do
    p12 <- liftIO (readFile' path)

    liftEitherEx CryptoStoreException $ do
        optAuthP12 <- readP12FileFromMemory p12
        (passwd, pkcs12) <- recoverAuthenticated password optAuthP12
        contents :: [[SafeBag]] <- coerce . recover passwd . unPKCS12 $ pkcs12
        pure . KeyStore . concat $ contents

getKeyByFriendlyName :: String -> ByteString -> KeyStore -> Maybe KeyPair
getKeyByFriendlyName name password (KeyStore bags) = do
    bag <- getKeyBagByFriendlyName name bags

    let contents = SafeContents [bag]
        passwd = toProtectionPassword password
        keyPairs = traverse (recover passwd) (getSafeKeys contents)

    eitherToMaybe keyPairs >>= listToMaybe

getKeyBagByFriendlyName :: String -> [SafeBag] -> Maybe SafeBag
getKeyBagByFriendlyName name = findJust f
  where
    f bag@Bag{..} =
        let bag' = do
                name' <- getFriendlyName bagAttributes
                guard (name == name')
                Just bag
         in case bagInfo of
                SafeContentsBag (SafeContents bags) -> getKeyBagByFriendlyName name bags
                KeyBag _ -> bag'
                PKCS8ShroudedKeyBag _ -> bag'
                _ -> Nothing

liftEitherEx :: (HasCallStack, Exception e', MonadThrow m) => (e -> e') -> Either e a -> m a
liftEitherEx f = \case
    Left e -> throwM (f e)
    Right a -> pure a

findJust :: (a -> Maybe b) -> [a] -> Maybe b
findJust = (listToMaybe .) . mapMaybe
