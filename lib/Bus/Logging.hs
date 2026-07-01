{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Bus.Logging (
    logDebug',
    logDebug,
    logDebugEx,
    logError',
    logError,
    logErrorEx,
    logInfo',
    logInfo,
    logInfoEx,
    logWarn',
    logWarn,
    logWarnEx,
    runTChanLoggingT,
    withAsyncLogging,
) where

import Bus.Exception (isAsyncException)
import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.Async (Async, withAsync)
import Control.Concurrent.STM (atomically, writeTChan)
import Control.Concurrent.STM.TChan (TChan, readTChan, tryReadTChan)
import Control.Exception (Exception (displayException, fromException, toException), ExceptionWithContext (ExceptionWithContext), SomeAsyncException, SomeException, catch)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger.CallStack (LogLevel (..), LogLine, LogStr, LoggingT (LoggingT), MonadLoggerIO (askLoggerIO), ToLogStr (toLogStr), defaultLoc, fromLogStr)
import Data.Foldable (for_)
import Data.List (isSuffixOf)
import Data.Text (Text)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.Typeable (cast)
import GHC.Stack (CallStack, HasCallStack, SrcLoc (srcLocModule), callStack, getCallStack)
import System.Process (Pid, getCurrentPid)
import Prelude hiding (log)

import Data.ByteString qualified as ByteString
import Data.Text qualified as Text

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

logDebug' :: (HasCallStack, MonadLoggerIO m, MonadIO m) => [Text] -> m ()
logDebug' = log' callStack LevelDebug

logInfo' :: (HasCallStack, MonadLoggerIO m, MonadIO m) => [Text] -> m ()
logInfo' = log' callStack LevelInfo

logWarn' :: (HasCallStack, MonadLoggerIO m, MonadIO m) => [Text] -> m ()
logWarn' = log' callStack LevelWarn

logError' :: (HasCallStack, MonadLoggerIO m, MonadIO m) => [Text] -> m ()
logError' = log' callStack LevelError

logDebugEx :: (HasCallStack, MonadLoggerIO m, MonadIO m, Exception e) => [Text] -> ExceptionWithContext e -> m ()
logDebugEx = logEx callStack LevelDebug

logInfoEx :: (HasCallStack, MonadLoggerIO m, MonadIO m, Exception e) => [Text] -> ExceptionWithContext e -> m ()
logInfoEx = logEx callStack LevelInfo

logWarnEx :: (HasCallStack, MonadLoggerIO m, MonadIO m, Exception e) => [Text] -> ExceptionWithContext e -> m ()
logWarnEx = logEx callStack LevelWarn

logErrorEx :: (HasCallStack, MonadLoggerIO m, MonadIO m, Exception e) => [Text] -> ExceptionWithContext e -> m ()
logErrorEx = logEx callStack LevelError

log :: (MonadLoggerIO m, MonadIO m) => CallStack -> LogLevel -> Text -> m ()
log cs level msg = do
    logger <- askLoggerIO
    currentTime <- liftIO getCurrentTime
    pid <- liftIO getCurrentPid
    threadId <- liftIO myThreadId

    let locModule = case getCallStack cs of
            [] -> ""
            (_, loc) : _ -> loc.srcLocModule
        msg' = toLogStr (Text.stripEnd msg)
        formattedMsg = formatLog currentTime pid threadId locModule level msg'

    liftIO (logger defaultLoc "" level formattedMsg)

log' :: (MonadLoggerIO m, MonadIO m) => CallStack -> LogLevel -> [Text] -> m ()
log' cs level = \case
    [] -> pure ()
    msgs -> log cs level (Text.concat msgs)

logEx :: (MonadLoggerIO m, MonadIO m, Exception e) => CallStack -> LogLevel -> [Text] -> ExceptionWithContext e -> m ()
logEx cs level msgs ewc@(ExceptionWithContext _ e) =
    case msgs of
        [] -> log' cs level [exMsg]
        _ -> log' cs level (msgs <> ["\n", exMsg])
  where
    exMsg =
        -- Prevent log backtrace twice
        Text.pack $
            if isSomeException e
                then displayException e
                else displayException ewc

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

isSomeException :: (Exception e) => e -> Bool
isSomeException e =
    case cast @_ @SomeException e of
        Just _ -> True
        Nothing -> False
