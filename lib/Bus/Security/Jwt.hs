{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bus.Security.Jwt (
    Token (..),
    TokenType (..),
    signToken,
    verifyToken,
) where

import Bus.Exception (JwtException (JwtException))
import Control.Applicative (Alternative (empty))
import Control.Lens ((&), (.~), (?~))
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Time (MonadTime (currentTime))
import Crypto.JWT (
    ClaimsSet,
    HasClaimsSet (claimExp, claimIat, claimIss, claimJti, claimNbf, claimSub, claimsSet),
    HasJWTValidationSettings (jwtValidationSettingsAllowedSkew, jwtValidationSettingsIssuerPredicate),
    JWTError,
    MonadRandom,
    NumericDate (NumericDate),
    SignedJWT,
    bestJWSAlg,
    decodeCompact,
    defaultJWTValidationSettings,
    emptyClaimsSet,
    fromX509PrivKey,
    fromX509PubKey,
    newJWSHeaderProtected,
    runJOSE,
    signJWT,
    verifyJWT,
 )
import Crypto.Store.PKCS8 (KeyPair, keyPairToPrivKey, keyPairToPubKey)
import Data.Aeson (FromJSON, ToJSON (toJSON), Value (Object, String), parseJSON, withObject, withText, (.:))
import Data.Aeson.KeyMap (insert)
import Data.ByteString (ByteString)
import Data.String (IsString (fromString))
import Data.Text (Text, unpack)
import Data.Time (addUTCTime)
import Data.Typeable (Proxy (Proxy), typeRep)
import GHC.Stack (HasCallStack)

import Data.ByteString qualified as ByteString

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

data TokenType = Access | Refresh deriving (Eq, Show)

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

signToken :: (HasCallStack, MonadRandom m, MonadTime m, MonadThrow m) => KeyPair -> Maybe Text -> Text -> Int -> TokenType -> m SignedJWT
signToken keyPair tokenId username expirationSec tokenType = do
    claims <- mkClaims tokenId (unpack username) expirationSec
    jwk <- case fromX509PrivKey (keyPairToPrivKey keyPair) of
        Left err -> throwM (JwtException err)
        Right a -> pure a
    alg <- case bestJWSAlg jwk of
        Left err -> throwM (JwtException err)
        Right a -> pure a

    let header = newJWSHeaderProtected alg
        token =
            Token
                { tokTokenType = tokenType
                , tokClaimsSet = claims
                }

    result <- runJOSE (signJWT jwk header token)

    case result of
        Left err -> throwM (JwtException err)
        Right a -> pure a

verifyToken :: (HasCallStack, MonadTime m, MonadThrow m) => KeyPair -> ByteString -> m Token
verifyToken keyPair token = do
    let publicKey = keyPairToPubKey keyPair
        config =
            defaultJWTValidationSettings (const True)
                & jwtValidationSettingsIssuerPredicate .~ (== "bus-tracker")
                & jwtValidationSettingsAllowedSkew .~ 10

    result :: Either JWTError Token <- runJOSE $ do
        jwk <- fromX509PubKey publicKey
        jwt <- decodeCompact @SignedJWT (ByteString.fromStrict token)

        verifyJWT config jwk jwt

    case result of
        Left err -> throwM (JwtException err)
        Right t -> pure t

mkClaims :: (MonadTime m) => Maybe Text -> String -> Int -> m ClaimsSet
mkClaims tokenId subject expirationSec = do
    now <- currentTime

    let expiration = addUTCTime (fromIntegral expirationSec) now

    pure $
        emptyClaimsSet
            & claimJti .~ tokenId
            & claimIss ?~ "bus-tracker"
            & claimSub ?~ fromString subject
            & claimIat ?~ NumericDate now
            & claimNbf ?~ NumericDate now
            & claimExp ?~ NumericDate expiration
