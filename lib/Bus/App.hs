module Bus.App (
    AppM (..),
    Env (..),
    Config (..),
    Server (..),
    Database (..),
) where

import Control.Monad.Logger.CallStack (LoggingT, MonadLogger, MonadLoggerIO)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Data.Text (Text)

newtype AppM a = AppM (ReaderT Env (LoggingT IO) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadLogger, MonadLoggerIO)

newtype Env = Env
    { envConfig :: Config
    }

data Config = Config
    { cfgServer :: Server
    , cfgDatabase :: Database
    }

newtype Server = Server
    { svrPort :: Int
    }

data Database = Database
    { dbUrl :: Text
    , dbUser :: Text
    , dbPasswordFile :: Text
    }
