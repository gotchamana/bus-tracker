{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bus.Logger (
    MonadLogger (..),
    LoggingT (..),
    withAsyncLogging,
    runTChanLoggingT,
    logTrace,
    logDebug,
    logInfo,
    logWarn,
    logError,
    logTrace',
    logDebug',
    logInfo',
    logWarn',
    logError',
    logTraceEx,
    logDebugEx,
    logInfoEx,
    logWarnEx,
    logErrorEx,
) where

import Bus.Exception (isAsyncException)
import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.Async (Async, withAsync)
import Control.Concurrent.STM (atomically, readTChan, tryReadTChan)
import Control.Concurrent.STM.TChan (TChan, writeTChan)
import Control.Exception (Exception (displayException), ExceptionWithContext (ExceptionWithContext), catch)
import Control.Monad (forever)
import Control.Monad.Catch (MonadThrow (throwM), SomeException)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (for_)
import Data.List (isSuffixOf)
import Data.Text (Text)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.Typeable (cast)
import GHC.Stack (CallStack, HasCallStack, SrcLoc (srcLocModule), callStack, getCallStack)
import System.Console.ANSI (Color (Black, Cyan, Green, Magenta, Red, Yellow), ColorIntensity (Dull, Vivid), ConsoleLayer (Foreground), SGR (Reset, SetColor), setSGRCode)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import System.Process (Pid, getCurrentPid)
import TextShow (TextShow (showt))
import Prelude hiding (log)

import Data.Text qualified as Text
import Data.Text.IO qualified as Text

class (Monad m) => MonadLogger m where
    logger :: LogEvent -> m ()

data LogEvent = LogEvent
    { logTimestamp :: UTCTime
    , logProcessId :: Pid
    , logThreadId :: ThreadId
    , logSourceLocation :: Maybe SrcLoc
    , logLevel :: LogLevel
    , logMessage :: Text
    }

data LogLevel = Trace | Debug | Info | Warn | Error

newtype LoggingT m a = LoggingT ((LogEvent -> IO ()) -> m a)

instance (Functor m) => Functor (LoggingT m) where
    fmap f (LoggingT g) = LoggingT $ fmap f . g

instance (Applicative m) => Applicative (LoggingT m) where
    pure a = LoggingT $ \_ -> pure a
    LoggingT f <*> LoggingT g = LoggingT $ \h -> f h <*> g h

instance (Monad m) => Monad (LoggingT m) where
    LoggingT f >>= g = LoggingT $ \h -> do
        a <- f h
        let LoggingT l = g a
        l h

instance (MonadIO m) => MonadIO (LoggingT m) where
    liftIO io = LoggingT $ \_ -> liftIO io

instance (MonadThrow m) => MonadThrow (LoggingT m) where
    throwM e = LoggingT $ \_ -> throwM e

instance (MonadFail m) => MonadFail (LoggingT m) where
    fail err = LoggingT $ \_ -> fail err

instance (MonadIO m) => MonadLogger (LoggingT m) where
    logger str = LoggingT $ \f -> liftIO (f str)

runTChanLoggingT :: TChan LogEvent -> LoggingT m a -> m a
runTChanLoggingT chan (LoggingT logging) = logging $ \event -> atomically (writeTChan chan event)

logTrace :: (HasCallStack, MonadLogger m, MonadIO m) => Text -> m ()
logTrace = log callStack Trace

logDebug :: (HasCallStack, MonadLogger m, MonadIO m) => Text -> m ()
logDebug = log callStack Debug

logInfo :: (HasCallStack, MonadLogger m, MonadIO m) => Text -> m ()
logInfo = log callStack Info

logWarn :: (HasCallStack, MonadLogger m, MonadIO m) => Text -> m ()
logWarn = log callStack Warn

logError :: (HasCallStack, MonadLogger m, MonadIO m) => Text -> m ()
logError = log callStack Error

logTrace' :: (HasCallStack, MonadLogger m, MonadIO m) => [Text] -> m ()
logTrace' = log' callStack Trace

logDebug' :: (HasCallStack, MonadLogger m, MonadIO m) => [Text] -> m ()
logDebug' = log' callStack Debug

logInfo' :: (HasCallStack, MonadLogger m, MonadIO m) => [Text] -> m ()
logInfo' = log' callStack Info

logWarn' :: (HasCallStack, MonadLogger m, MonadIO m) => [Text] -> m ()
logWarn' = log' callStack Warn

logError' :: (HasCallStack, MonadLogger m, MonadIO m) => [Text] -> m ()
logError' = log' callStack Error

logTraceEx :: (HasCallStack, MonadLogger m, MonadIO m, Exception e) => [Text] -> ExceptionWithContext e -> m ()
logTraceEx = logEx callStack Trace

logDebugEx :: (HasCallStack, MonadLogger m, MonadIO m, Exception e) => [Text] -> ExceptionWithContext e -> m ()
logDebugEx = logEx callStack Debug

logInfoEx :: (HasCallStack, MonadLogger m, MonadIO m, Exception e) => [Text] -> ExceptionWithContext e -> m ()
logInfoEx = logEx callStack Info

logWarnEx :: (HasCallStack, MonadLogger m, MonadIO m, Exception e) => [Text] -> ExceptionWithContext e -> m ()
logWarnEx = logEx callStack Warn

logErrorEx :: (HasCallStack, MonadLogger m, MonadIO m, Exception e) => [Text] -> ExceptionWithContext e -> m ()
logErrorEx = logEx callStack Error

log :: (MonadLogger m, MonadIO m) => CallStack -> LogLevel -> Text -> m ()
log cs level msg = do
    currentTime <- liftIO getCurrentTime
    pid <- liftIO getCurrentPid
    threadId <- liftIO myThreadId

    let source = case getCallStack cs of
            [] -> Nothing
            (_, loc) : _ -> Just loc
        logEvent =
            LogEvent
                { logTimestamp = currentTime
                , logProcessId = pid
                , logThreadId = threadId
                , logSourceLocation = source
                , logLevel = level
                , logMessage = msg
                }

    logger logEvent

log' :: (MonadLogger m, MonadIO m) => CallStack -> LogLevel -> [Text] -> m ()
log' cs level = \case
    [] -> pure ()
    msgs -> log cs level (Text.concat msgs)

logEx :: (MonadLogger m, MonadIO m, Exception e) => CallStack -> LogLevel -> [Text] -> ExceptionWithContext e -> m ()
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

withAsyncLogging :: TChan LogEvent -> (Async () -> IO ()) -> IO ()
withAsyncLogging chan = withAsync (catch @SomeException logging handleException)
  where
    logging = do
        hSetBuffering stdout LineBuffering

        forever (atomically (readTChan chan) >>= printLogEvent)
    handleException e =
        if isAsyncException e
            then do
                logs <- atomically $ unfoldrM (\_ -> fmap (,chan) <$> tryReadTChan chan) chan
                for_ logs printLogEvent
            else do
                let msg = "Logging failed: " <> displayException e

                if "\n" `isSuffixOf` msg
                    then putStr msg
                    else putStrLn msg

printLogEvent :: LogEvent -> IO ()
printLogEvent = Text.putStrLn . formatLog

formatLog :: LogEvent -> Text
formatLog LogEvent{..} =
    let locModule = maybe "<unknown>" srcLocModule logSourceLocation
     in Text.concat
            [ Text.pack (setSGRCode [SetColor Foreground Vivid Black])
            , Text.pack (formatTime defaultTimeLocale "%FT%T%3Q" logTimestamp)
            , Text.pack (setSGRCode [Reset])
            , " "
            , formatLogLevel True logLevel
            , " "
            , Text.pack (setSGRCode [SetColor Foreground Dull Magenta])
            , showt logProcessId
            , Text.pack (setSGRCode [Reset])
            , Text.pack (setSGRCode [SetColor Foreground Vivid Black])
            , " --- ["
            , showt logThreadId
            , "] "
            , Text.pack (setSGRCode [Reset])
            , Text.pack (setSGRCode [SetColor Foreground Dull Cyan])
            , Text.pack locModule
            , Text.pack (setSGRCode [Reset])
            , Text.pack (setSGRCode [SetColor Foreground Vivid Black])
            , " : "
            , Text.pack (setSGRCode [Reset])
            , Text.stripEnd logMessage
            ]

formatLogLevel :: Bool -> LogLevel -> Text
formatLogLevel colorized = \case
    Trace -> ansiColor colorized Vivid Green "TRACE"
    Debug -> ansiColor colorized Vivid Green "DEBUG"
    Info -> ansiColor colorized Vivid Green "INFO "
    Warn -> ansiColor colorized Vivid Yellow "WARN "
    Error -> ansiColor colorized Vivid Red "ERROR"

ansiColor :: Bool -> ColorIntensity -> Color -> Text -> Text
ansiColor True intensity color text =
    Text.concat
        [ Text.pack (setSGRCode [SetColor Foreground intensity color])
        , text
        , ansiReset
        ]
ansiColor False _ _ text = text

ansiReset :: Text
ansiReset = Text.pack (setSGRCode [Reset])

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
