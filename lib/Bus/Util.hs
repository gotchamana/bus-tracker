{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bus.Util (toHandler) where

import Bus.App (AppM (AppM), Env (envLoggingChan))
import Bus.Exception (ApiException (..), isAsyncException)
import Bus.Logging (logErrorEx, logWarnEx, runTChanLoggingT)
import Control.Exception (Exception (fromException), ExceptionWithContext (ExceptionWithContext), SomeException, try)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT))
import Data.Aeson (Options (fieldLabelModifier), ToJSON (toEncoding, toJSON), Value, defaultOptions, genericToEncoding, genericToJSON)
import Data.Char (toLower)
import Data.Foldable (for_)
import Data.List (stripPrefix)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant
import Network.HTTP.Types.Status (Status(statusCode, statusMessage))

data ProblemDetails = ProblemDetails
    { pdType :: URI
    , pdTitle :: Text
    , pdDetail :: Text
    , pdErrors :: Maybe Value
    }
    deriving (Generic)

instance ToJSON ProblemDetails where
    toJSON = genericToJSON (customOptions "pd")
    toEncoding = genericToEncoding (customOptions "pd")

toHandler :: Env -> AppM a -> Handler a
toHandler env (AppM readerT) = do
    let loggingChan = envLoggingChan env
        action = runTChanLoggingT loggingChan (runReaderT readerT env)
        logging = liftIO . runTChanLoggingT loggingChan

    result <- liftIO (try @(ExceptionWithContext SomeException) action)

    case result of
        Left ewc@(ExceptionWithContext _ se) -> do
            when (isAsyncException se) (throwM se)

            for_
                (fromException se)
                ( \ApiException{..} ->
                    logging (logWarnEx ["API error"] ewc)
                        >> throwError
                            ServerError
                                { errHTTPCode = statusCode apiHttpStatus
                                , errReasonPhrase = statusMessage apiHttpStatus
                                }
                )

            logging (logErrorEx ["Unknown error"] ewc)

            throwError err500{errBody = "Some errors"}
        Right a -> pure a

customOptions :: String -> Options
customOptions fieldPrefix = defaultOptions{fieldLabelModifier = removePrefix}
  where
    removePrefix field = case stripPrefix fieldPrefix field of
        Just result -> case result of
            [] -> []
            (x : xs) -> toLower x : xs
        Nothing -> field
