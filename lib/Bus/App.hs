module Bus.App (
    AppM (..),
    Env (..),
    Config (..),
    Server (..),
    Database (..),
    Security (..),
) where

import Bus.Auth (KeyStore)
import Control.Concurrent.STM.TChan (TChan)
import Control.Monad.Logger.CallStack (LogLine, LoggingT, MonadLogger, MonadLoggerIO)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Data.Aeson (FromJSON (parseJSON), Options (fieldLabelModifier), defaultOptions, genericParseJSON)
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype AppM a = AppM (ReaderT Env (LoggingT IO) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadLogger, MonadLoggerIO)

data Env = Env
    { envConfig :: Config
    , envLoggingChan :: TChan LogLine
    , envKeyStore :: KeyStore
    , envKeyStorePassword :: ByteString
    }

data Config = Config
    { cfgServer :: Server
    , cfgDatabase :: Database
    , cfgSecurity :: Security
    }
    deriving (Show, Generic)

instance FromJSON Config where
    parseJSON = genericParseJSON (customOptions "cfg")

newtype Server = Server
    { svrPort :: Int
    }
    deriving (Show, Generic)

instance FromJSON Server where
    parseJSON = genericParseJSON (customOptions "svr")

data Database = Database
    { dbUrl :: Text
    , dbUser :: Text
    , dbPasswordFile :: Text
    }
    deriving (Show, Generic)

instance FromJSON Database where
    parseJSON = genericParseJSON (customOptions "db")

data Security = Security
    { secKeyStoreFile :: Text
    , secKeyStorePasswordFile :: Text
    , secJwtKeyFriendlyName :: Text
    }
    deriving (Show, Generic)

instance FromJSON Security where
    parseJSON = genericParseJSON (customOptions "sec")

customOptions :: String -> Options
customOptions fieldPrefix = defaultOptions{fieldLabelModifier = removePrefix}
  where
    removePrefix field = case stripPrefix fieldPrefix field of
        Just result -> case result of
            [] -> []
            (x : xs) -> toLower x : xs
        Nothing -> field
