{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bus.Logger (
    MonadLogger (..),
    LoggingT (..),
    LogEvent,
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
import Control.Exception (Exception (displayException), ExceptionWithContext (ExceptionWithContext), catch, throwIO)
import Control.Monad (forever)
import Control.Monad.Catch (MonadThrow (throwM), SomeException)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (ReaderT))
import Data.Char (isSpace)
import Data.Foldable (Foldable (toList), for_)
import Data.List (dropWhileEnd, isSuffixOf)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.Typeable (cast)
import GHC.Stack (CallStack, HasCallStack, SrcLoc (srcLocModule, srcLocStartCol, srcLocStartLine), callStack, getCallStack)
import System.Console.ANSI (Color (Black, Cyan, Green, Magenta, Red, Yellow), ColorIntensity (Dull, Vivid), ConsoleLayer (Foreground), SGR (Reset, SetColor), hSupportsANSIColor, setSGRCode)
import System.IO (BufferMode (LineBuffering), Handle, hPutStr, hPutStrLn, hSetBuffering, stdout)
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
    , logMessage :: NonEmpty Text
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

instance (MonadLogger m) => MonadLogger (ReaderT r m) where
    logger event = ReaderT $ \_ -> logger event

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
log cs level msg = log' cs level [msg]

log' :: (MonadLogger m, MonadIO m) => CallStack -> LogLevel -> [Text] -> m ()
log' cs level msgs = do
    case filter (not . Text.null) . mapLast Text.stripEnd . dropWhileEnd isBlank $ msgs of
        [] -> pure ()
        (m : ms) -> do
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
                        , logMessage = m :| ms
                        }

            logger logEvent
  where
    isBlank = Text.all isSpace

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
withAsyncLogging chan = withAsync $ do
    colorized <- hSupportsANSIColor stdout
    logging stdout colorized `catch` handleException stdout colorized
  where
    logging handle colorized = do
        hSetBuffering handle LineBuffering

        forever (atomically (readTChan chan) >>= hPrintLogEvent handle colorized)
    handleException :: Handle -> Bool -> SomeException -> IO ()
    handleException handle colorized e =
        if isAsyncException e
            then do
                logs <- atomically $ unfoldrM (\_ -> fmap (,chan) <$> tryReadTChan chan) chan
                for_ logs (hPrintLogEvent handle colorized)

                throwIO e
            else do
                let msg = "Logging failed: " <> displayException e

                if "\n" `isSuffixOf` msg
                    then hPutStr handle msg
                    else hPutStrLn handle msg

hPrintLogEvent :: Handle -> Bool -> LogEvent -> IO ()
hPrintLogEvent handle colorized event = Text.hPutStrLn handle (formatLog colorized event)

formatLog :: Bool -> LogEvent -> Text
formatLog colorized LogEvent{..} =
    let location =
            maybe
                ["<unknown>"]
                ( \srcLoc ->
                    [ Text.pack srcLoc.srcLocModule
                    , ":"
                    , showt srcLoc.srcLocStartLine
                    , ":"
                    , showt srcLoc.srcLocStartCol
                    ]
                )
                logSourceLocation
     in Text.concat . concat $
            [ ansiColor colorized Vivid Black (Text.pack (formatTime defaultTimeLocale "%FT%T%3Q" logTimestamp))
            , [" "]
            , formatLogLevel colorized logLevel
            , [" "]
            , ansiColor colorized Dull Magenta (showt logProcessId)
            , ansiColor' colorized Vivid Black [" --- [", showt logThreadId, "] "]
            , ansiColor' colorized Dull Cyan location
            , ansiColor colorized Vivid Black " : "
            , toList logMessage
            ]

formatLogLevel :: Bool -> LogLevel -> [Text]
formatLogLevel colorized = \case
    Trace -> ansiColor colorized Vivid Green "TRACE"
    Debug -> ansiColor colorized Vivid Green "DEBUG"
    Info -> ansiColor colorized Vivid Green "INFO "
    Warn -> ansiColor colorized Vivid Yellow "WARN "
    Error -> ansiColor colorized Vivid Red "ERROR"

ansiColor :: Bool -> ColorIntensity -> Color -> Text -> [Text]
ansiColor colorized intensity color text = ansiColor' colorized intensity color [text]

ansiColor' :: Bool -> ColorIntensity -> Color -> [Text] -> [Text]
ansiColor' True intensity color texts =
    case texts of
        [] -> []
        _ -> [Text.pack (setSGRCode [SetColor Foreground intensity color])] <> texts <> [ansiReset]
ansiColor' False _ _ texts = texts

ansiReset :: Text
ansiReset = Text.pack (setSGRCode [Reset])

unfoldrM :: (Monad m) => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrM f seed = do
    m <- f seed

    case m of
        Just (x, seed') -> (x :) <$> unfoldrM f seed'
        Nothing -> pure []

mapLast :: (a -> a) -> [a] -> [a]
mapLast _ [] = []
mapLast f [x] = [f x]
mapLast f (x : xs) = x : mapLast f xs

isSomeException :: (Exception e) => e -> Bool
isSomeException e =
    case cast @_ @SomeException e of
        Just _ -> True
        Nothing -> False
