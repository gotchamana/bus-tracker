{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bus.Auth (
    KeyStore,
    Token (..),
    TokenType (..),
    getKeyByFriendlyName,
    readKeyStore,
    signToken,
    verifyToken,
) where

import Control.Applicative (Alternative (empty))
import Control.Lens ((&), (.~), (?~))
import Control.Monad (guard)
import Control.Monad.Except (ExceptT (ExceptT), MonadError, liftEither, runExceptT)
import Control.Monad.Time (MonadTime (currentTime))
import Crypto.JWT (
    ClaimsSet,
    HasClaimsSet (claimExp, claimIat, claimIss, claimNbf, claimSub, claimsSet),
    HasJWTValidationSettings (jwtValidationSettingsAllowedSkew, jwtValidationSettingsIssuerPredicate),
    JWTError,
    MonadRandom,
    NumericDate (NumericDate),
    SignedJWT,
    bestJWSAlg,
    defaultJWTValidationSettings,
    emptyClaimsSet,
    fromX509PrivKey,
    fromX509PubKey,
    newJWSHeaderProtected,
    signJWT,
    verifyJWT,
 )
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
import Crypto.Store.PKCS8 (KeyPair, keyPairToPrivKey, keyPairToPubKey)
import Data.Aeson (FromJSON, ToJSON (toJSON), Value (Object, String), parseJSON, withObject, withText, (.:))
import Data.Aeson.KeyMap (insert)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.String (IsString (fromString))
import Data.Text (Text, unpack)
import Data.Time (addUTCTime)
import Data.Typeable (Proxy (Proxy), typeRep)

newtype KeyStore = KeyStore [SafeBag]

data Token = Token
    { tokTokenType :: TokenType
    , tokClaimsSet :: ClaimsSet
    }
    deriving (Show)

instance HasClaimsSet Token where
    claimsSet f s@Token{tokClaimsSet} = fmap (\a -> s{tokClaimsSet = a}) (f tokClaimsSet)

instance FromJSON Token where
    parseJSON = withObject name $ \o -> do
        claims <- parseJSON (Object o)
        tokenType <- o .: "ttyp"

        pure
            Token
                { tokTokenType = tokenType
                , tokClaimsSet = claims
                }
      where
        name = show (typeRep @_ @Token Proxy)

instance ToJSON Token where
    toJSON Token{..} = ins "ttyp" tokTokenType (toJSON tokClaimsSet)
      where
        ins k v (Object o) = Object $ insert k (toJSON v) o
        ins _ _ a = a

data TokenType = Access | Refresh deriving (Show)

instance FromJSON TokenType where
    parseJSON = withText name $ \case
        "access" -> pure Access
        "refresh" -> pure Refresh
        _ -> empty
      where
        name = show (typeRep @_ @TokenType Proxy)

instance ToJSON TokenType where
    toJSON = \case
        Access -> String "access"
        Refresh -> String "refresh"

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

signToken :: (MonadRandom m, MonadTime m, MonadError JWTError m) => KeyPair -> Text -> Int -> TokenType -> m SignedJWT
signToken keyPair username expirationSec tokenType = do
    claims <- mkClaims (unpack username) expirationSec
    jwk <- fromX509PrivKey (keyPairToPrivKey keyPair)
    alg <- bestJWSAlg jwk

    let header = newJWSHeaderProtected alg
        token =
            Token
                { tokTokenType = tokenType
                , tokClaimsSet = claims
                }

    signJWT jwk header token

verifyToken :: (MonadTime m, MonadError JWTError m) => KeyPair -> SignedJWT -> m Token
verifyToken keyPair jwt = do
    let publicKey = keyPairToPubKey keyPair
        config =
            defaultJWTValidationSettings (const True)
                & jwtValidationSettingsIssuerPredicate .~ (== "bus-tracker")
                & jwtValidationSettingsAllowedSkew .~ 10

    jwk <- fromX509PubKey publicKey
    verifyJWT config jwk jwt

mkClaims :: (MonadTime m) => String -> Int -> m ClaimsSet
mkClaims subject expirationSec = do
    now <- currentTime

    let expiration = addUTCTime (fromIntegral expirationSec) now

    pure $
        emptyClaimsSet
            & claimIss ?~ "bus-tracker"
            & claimSub ?~ fromString subject
            & claimIat ?~ NumericDate now
            & claimNbf ?~ NumericDate now
            & claimExp ?~ NumericDate expiration

findJust :: (a -> Maybe b) -> [a] -> Maybe b
findJust = (listToMaybe .) . mapMaybe

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = \case
    Left _ -> Nothing
    Right r -> Just r
