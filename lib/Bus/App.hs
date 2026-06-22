module Bus.App (
    AppM (..),
    Env (..),
    Config (..),
    Server (..),
    Database (..),
) where

import Control.Concurrent.STM.TChan (TChan)
import Control.Monad.Logger.CallStack (LogLine, LoggingT, MonadLogger, MonadLoggerIO)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Data.Text (Text)

newtype AppM a = AppM (ReaderT Env (LoggingT IO) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadLogger, MonadLoggerIO)

data Env = Env
    { envConfig :: Config
    , envLoggingChan :: TChan LogLine
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
