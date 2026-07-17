{-# LANGUAGE OverloadedRecordDot #-}

module Bus.Web.Auth.Api (AuthApi, authApi) where

import Bus.App (AppM, Config (cfgSecurity), Env (envConfig, envKeyStore, envKeyStorePassword), Security (secJwtKeyFriendlyName))
import Bus.Auth (getKeyByFriendlyName)
import Bus.Exception (NoSuchKeyException (NoSuchKeyException))
import Bus.Util (fieldPrefixRemovalOptions)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Reader (MonadReader (ask))
import Data.Aeson (Object, ToJSON (toEncoding, toJSON), genericToEncoding, genericToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Rerefined (unrefine)
import Servant

import Bus.Web.Auth.Service qualified as AuthSvc
import Data.Text qualified as Text

type AuthApi = "auth" :> ("login" :> ReqBody '[JSON] Object :> Post '[JSON] TokenResponse)

newtype TokenResponse = TokenResponse
    { trAccess :: Text
    }
    deriving (Generic)

instance ToJSON TokenResponse where
    toJSON = genericToJSON (fieldPrefixRemovalOptions "tr")
    toEncoding = genericToEncoding (fieldPrefixRemovalOptions "tr")

authApi :: ServerT AuthApi AppM
authApi = login

login :: Object -> AppM TokenResponse
login object = do
    env <- ask

    let jwtName = Text.unpack (unrefine env.envConfig.cfgSecurity.secJwtKeyFriendlyName)
        keyStore = env.envKeyStore
        password = env.envKeyStorePassword

    jwt <- case getKeyByFriendlyName jwtName password keyStore of
        Just keyPair -> AuthSvc.validateLogin object keyPair
        Nothing -> throwM (NoSuchKeyException jwtName)

    pure (TokenResponse jwt)
