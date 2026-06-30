module Bus.Exception (CryptoStoreException (..), isAsyncException) where

import Control.Exception (Exception (displayException, fromException, toException), SomeAsyncException)
import Crypto.Store.Error (StoreError)
import Data.Typeable (typeOf)

newtype CryptoStoreException = CryptoStoreException StoreError deriving (Show)

instance Exception CryptoStoreException where
    displayException e@(CryptoStoreException err) = show (typeOf e) <> ": " <> show err

isAsyncException :: (Exception e) => e -> Bool
isAsyncException e =
    case fromException @SomeAsyncException (toException e) of
        Just _ -> True
        Nothing -> False
