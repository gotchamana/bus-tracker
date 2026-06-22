{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Bus.Logging (
    logDebug,
    logError,
    logInfo,
    logWarn,
    runTChanLoggingT,
    withAsyncLogging,
) where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.Async (Async, withAsync)
import Control.Concurrent.STM (atomically, writeTChan)
import Control.Concurrent.STM.TChan (TChan, readTChan, tryReadTChan)
import Control.Exception (Exception (displayException, fromException, toException), SomeAsyncException, SomeException, catch)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger.CallStack (LogLevel (..), LogLine, LogStr, LoggingT (LoggingT), MonadLoggerIO (askLoggerIO), ToLogStr (toLogStr), defaultLoc, fromLogStr)
import Data.Foldable (for_)
import Data.List (isSuffixOf)
import Data.Text (Text)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import GHC.Stack (CallStack, HasCallStack, SrcLoc (srcLocModule), callStack, getCallStack)
import System.Process (Pid, getCurrentPid)
import Prelude hiding (log)

import Data.ByteString qualified as ByteString

runTChanLoggingT :: TChan LogLine -> LoggingT m a -> m a
runTChanLoggingT chan (LoggingT logging) = logging $ \loc source level msg -> atomically (writeTChan chan (loc, source, level, msg))

logDebug :: (HasCallStack, MonadLoggerIO m, MonadIO m) => Text -> m ()
logDebug = log callStack LevelDebug

logInfo :: (HasCallStack, MonadLoggerIO m, MonadIO m) => Text -> m ()
logInfo = log callStack LevelInfo

logWarn :: (HasCallStack, MonadLoggerIO m, MonadIO m) => Text -> m ()
logWarn = log callStack LevelWarn

logError :: (HasCallStack, MonadLoggerIO m, MonadIO m) => Text -> m ()
logError = log callStack LevelError

log :: (MonadLoggerIO m, MonadIO m) => CallStack -> LogLevel -> Text -> m ()
log cs level msg = do
    logger <- askLoggerIO
    currentTime <- liftIO getCurrentTime
    pid <- liftIO getCurrentPid
    threadId <- liftIO myThreadId

    let locModule = case getCallStack cs of
            [] -> ""
            (_, loc) : _ -> loc.srcLocModule
        formattedMsg = formatLog currentTime pid threadId locModule level (toLogStr msg)

    liftIO (logger defaultLoc "" level formattedMsg)

formatLog :: UTCTime -> Pid -> ThreadId -> String -> LogLevel -> LogStr -> LogStr
formatLog time pid threadId locModule level msg =
    mconcat
        [ toLogStr (formatTime defaultTimeLocale "%FT%T%3Q" time)
        , " "
        , formatLogLevel level
        , " "
        , toLogStr (show pid)
        , " --- ["
        , toLogStr (show threadId)
        , "] "
        , toLogStr (if null locModule then "<unknown>" else locModule)
        , " : "
        , msg
        , "\n"
        ]

formatLogLevel :: LogLevel -> LogStr
formatLogLevel = \case
    LevelDebug -> "DEBUG"
    LevelInfo -> "INFO "
    LevelWarn -> "WARN "
    LevelError -> "ERROR"
    LevelOther text -> toLogStr text

withAsyncLogging :: TChan LogLine -> (Async () -> IO ()) -> IO ()
withAsyncLogging chan = withAsync (catch @SomeException logging handleException)
  where
    logging = forever $ do
        (_, _, _, msg) <- atomically (readTChan chan)
        putLogStr msg
    handleException e =
        if isAsyncException e
            then do
                logs <- atomically $ unfoldrM (\_ -> fmap (,chan) <$> tryReadTChan chan) chan
                for_ logs $ \(_, _, _, msg) -> putLogStr msg
            else do
                let msg = "Logging failed: " <> displayException e

                if "\n" `isSuffixOf` msg
                    then putStr msg
                    else putStrLn msg
    putLogStr = ByteString.putStr . fromLogStr

unfoldrM :: (Monad m) => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrM f seed = do
    m <- f seed

    case m of
        Just (x, seed') -> (x :) <$> unfoldrM f seed'
        Nothing -> pure []

isAsyncException :: (Exception e) => e -> Bool
isAsyncException e =
    case fromException @SomeAsyncException (toException e) of
        Just _ -> True
        Nothing -> False
