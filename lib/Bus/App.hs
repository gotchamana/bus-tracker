{-# LANGUAGE OverloadedStrings #-}

module Bus.App (
    AppM (..),
    Env (..),
    Config (..),
    Server (..),
    Database (..),
    Security (..),
) where

import Bus.Auth (KeyStore)
import Bus.Database (MonadDatabase (..))
import Bus.Exception (rethrowIO)
import Bus.Rerefined.Predicate (NetworkPort, NotEmpty, Trimmed, ValidPath)
import Control.Concurrent.STM.TChan (TChan)
import Control.Exception (Exception (displayException, toException), ExceptionWithContext (ExceptionWithContext), SomeException, try)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Logger.CallStack (LogLine, LoggingT, MonadLogger, MonadLoggerIO)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader, ReaderT, asks)
import Data.Aeson (FromJSON (parseJSON), Options (fieldLabelModifier), defaultOptions, genericParseJSON, withObject, withText, (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Coerce (coerce)
import Data.List (stripPrefix)
import Data.Pool (Pool, withResource)
import Data.Text (Text, unpack)
import Data.Typeable (Proxy (Proxy), typeRep)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import Rerefined.Predicate.Logical (And)
import Rerefined.Refine (Refined, prettyRefineFailure, refine)
import System.OsPath (OsPath, encodeUtf)

import Data.Text qualified as Text

newtype AppM a = AppM (ReaderT Env (LoggingT IO) a)
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader Env
        , MonadLogger
        , MonadLoggerIO
        , MonadThrow
        )

instance MonadDatabase AppM where
    withConnection action = do
        pool <- asks envDbPool
        result <- liftIO (withResource pool (try . action))

        case result of
            Left (ExceptionWithContext ctx (e :: SomeException)) -> liftIO . rethrowIO . ExceptionWithContext ctx . toException $ e
            Right a -> pure a

data Env = Env
    { envConfig :: Config
    , envLoggingChan :: TChan LogLine
    , envKeyStore :: KeyStore
    , envKeyStorePassword :: ByteString
    , envDbPool :: Pool Connection
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
    { svrPort :: Refined NetworkPort Int
    }
    deriving (Show, Generic)

instance FromJSON Server where
    parseJSON = withObject name $ \o -> do
        JsonNetworkPort port <- o .: "port"

        pure (Server{svrPort = port})
      where
        name = show (typeRep @_ @Server Proxy)

data Database = Database
    { dbHost :: Text
    , dbPort :: Maybe (Refined NetworkPort Int)
    , dbDbName :: Text
    , dbUser :: Text
    , dbPasswordFile :: Maybe (Refined ValidPath OsPath)
    }
    deriving (Show, Generic)

instance FromJSON Database where
    parseJSON = withObject name $ \o -> do
        host <- o .: "host"
        port <- coerce @(Parser (Maybe JsonNetworkPort)) (o .:? "port")
        dbName <- o .: "dbName"
        user <- o .: "user"
        passwordFile <- coerce @(Parser (Maybe JsonOsPath)) (o .:? "passwordFile")

        pure
            Database
                { dbHost = host
                , dbPort = port
                , dbDbName = dbName
                , dbUser = user
                , dbPasswordFile = passwordFile
                }
      where
        name = show (typeRep @_ @Database Proxy)

data Security = Security
    { secKeyStoreFile :: Refined ValidPath OsPath
    , secKeyStorePasswordFile :: Refined ValidPath OsPath
    , secJwtKeyFriendlyName :: Refined (And Trimmed NotEmpty) Text
    }
    deriving (Show)

instance FromJSON Security where
    parseJSON = withObject name $ \o -> do
        JsonOsPath storeFile <- o .: "keyStoreFile"
        JsonOsPath passwordFile <- o .: "keyStorePasswordFile"
        JsonTrimmedNonEmptyText friendlyName <- o .: "jwtKeyFriendlyName"

        pure
            Security
                { secKeyStoreFile = storeFile
                , secKeyStorePasswordFile = passwordFile
                , secJwtKeyFriendlyName = friendlyName
                }
      where
        name = show (typeRep @_ @Security Proxy)

newtype JsonOsPath = JsonOsPath (Refined ValidPath OsPath)

instance FromJSON JsonOsPath where
    parseJSON = withText name $ \text -> do
        case encodeUtf (Text.unpack text) of
            Left err -> fail (displayException err)
            Right path -> case refine path of
                Left err -> fail . unpack . prettyRefineFailure $ err
                Right r -> pure (JsonOsPath r)
      where
        name = show (typeRep @_ @OsPath Proxy)

newtype JsonTrimmedNonEmptyText = JsonTrimmedNonEmptyText (Refined (And Trimmed NotEmpty) Text)

instance FromJSON JsonTrimmedNonEmptyText where
    parseJSON = withText name $ \text -> do
        let trimmed = Text.strip text

        text' <- case refine trimmed of
            Left err -> fail . unpack . prettyRefineFailure $ err
            Right t -> pure t

        pure (JsonTrimmedNonEmptyText text')
      where
        name = show (typeRep @_ @Text Proxy)

newtype JsonNetworkPort = JsonNetworkPort (Refined NetworkPort Int)

instance FromJSON JsonNetworkPort where
    parseJSON val = do
        num <- parseJSON val

        port <- case refine num of
            Left err -> fail . unpack . prettyRefineFailure $ err
            Right p -> pure p

        pure (JsonNetworkPort port)

customOptions :: String -> Options
customOptions fieldPrefix = defaultOptions{fieldLabelModifier = removePrefix}
  where
    removePrefix field = case stripPrefix fieldPrefix field of
        Just result -> case result of
            [] -> []
            (x : xs) -> toLower x : xs
        Nothing -> field
