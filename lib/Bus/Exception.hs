module Bus.Exception (CryptoStoreException (..)) where

import Control.Exception (Exception (displayException))
import Crypto.Store.Error (StoreError)
import Data.Typeable (typeOf)

newtype CryptoStoreException = CryptoStoreException StoreError deriving (Show)

instance Exception CryptoStoreException where
    displayException e@(CryptoStoreException err) = show (typeOf e) <> ": " <> show err
