{-# LANGUAGE RecordWildCards #-}

module Bus.Auth (readKeyStore, getKeyByFriendlyName, KeyStore) where

import Control.Monad (guard)
import Control.Monad.Except (ExceptT (ExceptT), liftEither, runExceptT)
import Crypto.Store.Error (StoreError)
import Crypto.Store.PKCS12 (
    Bag (Bag, bagAttributes, bagInfo),
    SafeBag,
    SafeContents (SafeContents),
    SafeInfo (KeyBag, PKCS8ShroudedKeyBag, SafeContentsBag),
    getFriendlyName,
    getSafeKeys,
    readP12File,
    recover,
    recoverAuthenticated,
    toProtectionPassword,
    unPKCS12,
 )
import Crypto.Store.PKCS8 (KeyPair)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Maybe (listToMaybe, mapMaybe)

newtype KeyStore = KeyStore [SafeBag]

readKeyStore :: String -> ByteString -> IO (Either StoreError KeyStore)
readKeyStore path password = runExceptT $ do
    optAuthP12 <- ExceptT (readP12File path)
    liftEither $ do
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

findJust :: (a -> Maybe b) -> [a] -> Maybe b
findJust = (listToMaybe .) . mapMaybe

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = \case
    Left _ -> Nothing
    Right r -> Just r
