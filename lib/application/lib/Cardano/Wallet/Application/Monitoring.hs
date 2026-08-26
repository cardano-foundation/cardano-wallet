{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Logging and monitoring setup for the wallet server, implemented directly
-- on top of @ekg-core@, @ekg-wai@ and @warp@. It replaces the former
-- @iohk-monitoring@ framework plumbing while preserving:
--
--   * the stdout/stderr/file text log format and severity routing,
--   * the EKG endpoint with its metric key names,
--   * the Prometheus endpoint with its metric name sanitization.
--
-- The metric endpoints are a process-wide resource: they are bound by the
-- first 'initTracer' that asks for them and released when the last holder
-- shuts down, so that nesting two 'initTracer' scopes does not bind the same
-- port twice or run two counter-capture threads.
module Cardano.Wallet.Application.Monitoring
    ( LogOutput (..)
    , initTracer

      -- * Text log format

      -- | Exposed for testing: the emitted layout is a compatibility
      -- contract with everything that greps wallet logs.
    , formatItem
    ) where

import Cardano.Wallet.Application.Metrics.Counters
    ( Counter (..)
    , CounterType (..)
    , nameCounter
    , readCounters
    )
import Cardano.BM.Data.LogItem
    ( LOContent (..)
    , LOMeta (..)
    , LogObject (..)
    , LoggerName
    , Measurable (..)
    , PrivacyAnnotation (..)
    , mkLOMeta
    )
import Cardano.BM.Data.Severity
    ( Severity (..)
    )
import Cardano.BM.Data.Tracer
    ( mkTracer
    )
import Cardano.BM.Trace
    ( Trace
    , appendName
    , traceNamedObject
    )
import Control.Concurrent
    ( killThread
    , threadDelay
    )
import Control.Monad
    ( forM
    , forM_
    , forever
    , unless
    , when
    )
import Control.Monad.IO.Class
    ( MonadIO
    , liftIO
    )
import Data.Aeson
    ( Value (..)
    , toJSON
    )
import Data.Maybe
    ( isJust
    , isNothing
    )
import Data.Scientific
    ( fromFloatDigits
    )
import Data.String
    ( fromString
    )
import Data.Text
    ( Text
    )
import Data.Time.Format
    ( defaultTimeLocale
    , formatTime
    )
import GHC.IO.Handle
    ( hDuplicate
    )
import System.Directory
    ( createDirectoryIfMissing
    )
import System.FilePath
    ( takeDirectory
    )
import System.IO
    ( BufferMode (..)
    , Handle
    , IOMode (..)
    , hClose
    , hSetBuffering
    , openFile
    , stderr
    , stdout
    )
import UnliftIO.Exception
    ( IOException
    , catch
    , finally
    )
import Prelude

import qualified Cardano.Wallet.Application.Version as V
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MV
import qualified Data.Aeson.Text as Aeson
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LB
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.IO.Unsafe as Unsafe
import qualified System.Metrics as Metrics
import qualified System.Metrics.Gauge as Gauge
import qualified System.Metrics.Label as Label
import qualified System.Remote.Monitoring.Wai as EKG

{-------------------------------------------------------------------------------
    Log outputs
-------------------------------------------------------------------------------}

-- | Defines the log outputs of the wallet.
data LogOutput
    = -- | Log to the console, with the given minimum severity.
      LogToStdStreams Severity
    | -- | Log to a file, with the given minimum severity.
      LogToFile FilePath Severity
    deriving (Show, Eq)

{-------------------------------------------------------------------------------
    Tracer setup
-------------------------------------------------------------------------------}

-- | Initialize the logging and monitoring infrastructure and obtain the
-- root 'Trace' for the given logger name and requested log outputs.
--
-- The optional endpoints are the EKG bind address and the Prometheus bind
-- address, as resolved from the environment by the caller.
--
-- Returns an action which shuts down all logging and monitoring resources,
-- and the root trace.
initTracer
    :: LoggerName
    -> [LogOutput]
    -> Maybe (String, Int)
    -> Maybe (String, Int)
    -> IO (IO (), Trace IO Text)
initTracer loggerName outputs mEkgUrl mPromUrl = do
    scribes <- concat <$> forM outputs mkScribe
    (mConsumer, isOwner) <- acquireMonitoring mEkgUrl mPromUrl
    let sink = mkTraceSink scribes mConsumer
        trace = appendName loggerName sink
    -- Counters are only captured when the EKG endpoint is enabled, like
    -- before the migration, and only by the scope that bound the endpoints.
    when (isOwner && isJust mEkgUrl) $ startCapturingMetrics trace
    let shutdown = releaseMonitoring `finally` mapM_ scribeFinalize scribes
    pure (shutdown, trace)

{-------------------------------------------------------------------------------
    Monitoring ownership

    A bound port is a process-wide resource, so the endpoint listeners and the
    counter-capture thread are owned by the process, not by an individual
    'initTracer' scope. Scopes take and drop references to them; the listeners
    start on the first reference and stop on the last. Without this, nesting
    two 'initTracer' scopes -- which the integration framework does -- would
    call 'EKG.forkServer' twice on the same port and run two capture threads.
-------------------------------------------------------------------------------}

-- | The process-wide monitoring resources, held while at least one
-- 'initTracer' scope is open.
data Monitoring = Monitoring
    { monRefCount :: !Int
    -- ^ How many live 'initTracer' scopes hold these resources.
    , monConsumer :: !(Maybe ((Text, Measurable) -> IO ()))
    -- ^ Shared sink mirroring 'LogValue's into the metric store. Shared, so
    -- that nested scopes do not each try to register the same metric names.
    , monCapture :: !(Maybe (Async.Async ()))
    -- ^ The counter-capture thread, started by the owning scope only.
    , monShutdown :: !(IO ())
    -- ^ Tears the endpoint listeners down.
    }

monitoringState :: MV.MVar (Maybe Monitoring)
monitoringState = Unsafe.unsafePerformIO (MV.newMVar Nothing)
{-# NOINLINE monitoringState #-}

-- | Take a reference to the monitoring endpoints, starting them if nobody
-- holds them yet. Returns the shared metrics consumer, and whether this call
-- is the one that started the endpoints.
--
-- The endpoints of a later reference are ignored: the bind addresses come
-- from the process environment at every call site, so nested scopes always
-- ask for the same ones.
acquireMonitoring
    :: Maybe (String, Int)
    -> Maybe (String, Int)
    -> IO (Maybe ((Text, Measurable) -> IO ()), Bool)
acquireMonitoring mEkgUrl mPromUrl =
    MV.modifyMVar monitoringState $ \case
        Just mon ->
            pure
                ( Just mon{monRefCount = monRefCount mon + 1}
                , (monConsumer mon, False)
                )
        Nothing -> do
            (mStore, shutdown) <- startMetrics mEkgUrl mPromUrl
            mConsumer <- traverse mkMetricsConsumer mStore
            let mon =
                    Monitoring
                        { monRefCount = 1
                        , monConsumer = mConsumer
                        , monCapture = Nothing
                        , monShutdown = shutdown
                        }
            pure (Just mon, (mConsumer, True))

-- | Drop a reference to the monitoring endpoints. The capture thread and the
-- listeners are torn down only when the last holder releases them.
releaseMonitoring :: IO ()
releaseMonitoring =
    MV.modifyMVar_ monitoringState $ \case
        Nothing -> pure Nothing
        Just mon
            | monRefCount mon > 1 ->
                pure (Just mon{monRefCount = monRefCount mon - 1})
            | otherwise -> do
                mapM_ Async.cancel (monCapture mon)
                monShutdown mon
                pure Nothing

-- | Hand the counter-capture thread over to the monitoring state, so that it
-- is cancelled at final release. A second capture thread can never be
-- registered: only the owning scope starts one, and it is cancelled here if
-- the state has moved on underneath it.
registerCapture :: Async.Async () -> IO ()
registerCapture a =
    MV.modifyMVar_ monitoringState $ \case
        Just mon
            | isNothing (monCapture mon) ->
                pure (Just mon{monCapture = Just a})
        other -> do
            Async.cancel a
            pure other

-- | Start the EKG and/or Prometheus servers according to the environment.
-- Returns the metric store (when any monitoring endpoint is enabled) and a
-- shutdown action.
startMetrics
    :: Maybe (String, Int)
    -> Maybe (String, Int)
    -> IO (Maybe Metrics.Store, IO ())
startMetrics mEkgUrl mPromUrl = case (mEkgUrl, mPromUrl) of
    (Just (host, port), _) -> do
        server <- EKG.forkServer (BS8.pack host) port
        let store = EKG.serverMetricStore server
        -- Wallet-owned equivalent of the former "iohk-monitoring version"
        -- label.
        versionLabel <- Metrics.createLabel "cardano-wallet version" store
        Label.set
            versionLabel
            (T.pack $ V.showFullVersion V.version V.gitRevision)
        shutdownProm <-
            maybe (pure (pure ())) (startPrometheus store) mPromUrl
        pure
            ( Just store
            , killThread (EKG.serverThreadId server) `finally` shutdownProm
            )
    (Nothing, Just promUrl) -> do
        store <- Metrics.newStore
        Metrics.registerGcMetrics store
        shutdownProm <- startPrometheus store promUrl
        pure (Just store, shutdownProm)
    (Nothing, Nothing) -> pure (Nothing, pure ())

{-------------------------------------------------------------------------------
    Trace sink: log messages to scribes, LogValue metrics to the EKG store
-------------------------------------------------------------------------------}

mkTraceSink
    :: [Scribe]
    -> Maybe ((Text, Measurable) -> IO ())
    -> Trace IO Text
mkTraceSink scribes mMetrics = mkTracer $ \(names, lo) -> do
    case loContent lo of
        LogValue iname value ->
            forM_ mMetrics $ \consume ->
                consume (names <> "." <> iname, value)
        content -> do
            let meta = loMeta lo
                sev = severity meta
                -- Confidential messages never reach public scribes.
                confidentialSkip =
                    privacy meta == Confidential && isMessage content
                msg = msgTextOf content
            unless confidentialSkip
                $ when (msg /= mempty)
                $ forM_ scribes
                $ \sc ->
                    when
                        (scribeMinSev sc <= sev && sev <= scribeMaxSev sc)
                        (scribeWrite sc (formatItem (scribeColorize sc) names meta msg))
  where
    isMessage LogMessage{} = True
    isMessage _ = False

    msgTextOf :: LOContent Text -> Text
    msgTextOf = \case
        LogMessage logItem -> case toJSON logItem of
            String m -> m
            m -> TL.toStrict $ Aeson.encodeToLazyText m
        LogError m -> m
        LogValue name value ->
            if name == ""
                then T.pack (showSI value)
                else name <> " = " <> T.pack (showSI value)

{-------------------------------------------------------------------------------
    Text log format
-------------------------------------------------------------------------------}

-- | Render one log line in the layout the wallet emitted before the
-- migration:
--
-- > <header> [<yyyy-mm-dd hh:mm:ss.SS UTC>] <message>
--
-- where @\<header\>@ is @[\<host:\>\<namespace\>:\<severity\>:\<threadid\>]@,
-- coloured as a whole by severity when the scribe is colourized, and the
-- @\<host:\>@ part is omitted when the host name is empty.
--
-- This is a port of @Cardano.BM.Backend.Log.formatItem@ from
-- @iohk-monitoring-0.2.1.2@, which the wallet no longer depends on. It is
-- deliberately not katip's @bracketFormat@: that emits the timestamp first,
-- drops the sub-second digits and the zone, splits the header into five
-- brackets and only colours the severity token.
formatItem :: Bool -> LoggerName -> LOMeta -> Text -> Text
formatItem withColor names meta msg =
    header <> " [" <> timestamp <> "] " <> msg
  where
    sev = severity meta
    header =
        colorBySeverity
            $ "["
                <> host
                <> names
                <> ":"
                <> renderSeverity sev
                <> ":"
                <> tid meta
                <> "]"
    host
        | hostname meta == mempty = mempty
        | otherwise = hostname meta <> ":"
    timestamp =
        T.pack $ formatTime defaultTimeLocale "%F %T%2Q %Z" (tstamp meta)
    colorBySeverity m = case sev of
        Emergency -> red m
        Alert -> red m
        Critical -> red m
        Error -> red m
        Notice -> magenta m
        Warning -> yellow m
        Info -> blue m
        Debug -> m
    red = colorize "31"
    yellow = colorize "33"
    magenta = colorize "35"
    blue = colorize "34"
    colorize c m
        | withColor = "\ESC[" <> c <> "m" <> m <> "\ESC[0m"
        | otherwise = m

-- | Severity as it was spelled in the log line, matching katip's
-- @renderSeverity@, which the former formatter used.
renderSeverity :: Severity -> Text
renderSeverity = \case
    Debug -> "Debug"
    Info -> "Info"
    Notice -> "Notice"
    Warning -> "Warning"
    Error -> "Error"
    Critical -> "Critical"
    Alert -> "Alert"
    Emergency -> "Emergency"

showSI :: Measurable -> String
showSI (Microseconds a) =
    show
        (fromFloatDigits (fromIntegral a / (1000 :: Float) / (1000 :: Float)))
        ++ showUnits (Seconds a)
showSI (Nanoseconds a) =
    show
        ( fromFloatDigits
            (fromIntegral a / (1000 :: Float) / (1000 :: Float) / (1000 :: Float))
        )
        ++ showUnits (Seconds a)
showSI v@(Seconds a) = show a ++ showUnits v
showSI v@(Bytes a) = show a ++ showUnits v
showSI v@(PureI a) = show a ++ showUnits v
showSI v@(PureD a) = show a ++ showUnits v
showSI v@(Severity a) = show a ++ showUnits v

showUnits :: Measurable -> String
showUnits (Microseconds _) = " µs"
showUnits (Nanoseconds _) = " ns"
showUnits (Seconds _) = " s"
showUnits (Bytes _) = " B"
showUnits (PureI _) = ""
showUnits (PureD _) = ""
showUnits (Severity _) = ""

{-------------------------------------------------------------------------------
    Scribes
-------------------------------------------------------------------------------}

data Scribe = Scribe
    { scribeMinSev :: !Severity
    , scribeMaxSev :: !Severity
    , scribeColorize :: !Bool
    , scribeWrite :: !(Text -> IO ())
    , scribeFinalize :: !(IO ())
    }

-- | Create scribes for a 'LogOutput' specification, keeping the legacy
-- severity splits between stdout, stderr and file outputs.
mkScribe :: LogOutput -> IO [Scribe]
mkScribe = \case
    LogToStdStreams sev -> do
        stderrScribe <- mkHandleScribe stderr True (max Warning sev) maxBound
        stdoutScribe <- mkHandleScribe stdout True sev (pred Warning)
        pure [stderrScribe, stdoutScribe]
    LogToFile fp sev -> do
        scribe <- mkFileScribe fp sev Critical
        pure [scribe]

mkHandleScribe :: Handle -> Bool -> Severity -> Severity -> IO Scribe
mkHandleScribe h colorize sev sevMax = do
    h' <- hDuplicate h -- will be closed on exit
    mkHandleScribeH h' colorize sev sevMax

mkHandleScribeH :: Handle -> Bool -> Severity -> Severity -> IO Scribe
mkHandleScribeH h colorize sev sevMax = do
    hSetBuffering h LineBuffering
    lock <- MV.newMVar ()
    let logger msg = MV.withMVar lock $ \_ -> TIO.hPutStrLn h msg
    pure
        $ Scribe
            { scribeMinSev = sev
            , scribeMaxSev = sevMax
            , scribeColorize = colorize
            , scribeWrite = logger
            , scribeFinalize = hClose h
            }

mkFileScribe :: FilePath -> Severity -> Severity -> IO Scribe
mkFileScribe fpath sev sevMax = do
    let prefixDir = takeDirectory fpath
    createDirectoryIfMissing True prefixDir
        `catch` prtoutException ("cannot create prefix directory: " ++ prefixDir)
    h <-
        catch
            (openFile fpath WriteMode)
            ( \e -> do
                prtoutException ("error while opening log: " ++ fpath) e
                -- fallback to standard output in case of exception
                return stdout
            )
    hSetBuffering h LineBuffering
    lock <- MV.newMVar ()
    let logger msg = MV.withMVar lock $ \_ -> TIO.hPutStrLn h msg
    pure
        $ Scribe
            { scribeMinSev = sev
            , scribeMaxSev = sevMax
            , scribeColorize = False
            , scribeWrite = logger
            , scribeFinalize = hClose h
            }

prtoutException :: String -> IOException -> IO ()
prtoutException desc e = do
    let msg = "ERROR: " ++ desc ++ ": " ++ show e
    TIO.hPutStrLn stderr (T.pack msg)

{-------------------------------------------------------------------------------
    Metrics consumer: LogValue -> ekg-core store
-------------------------------------------------------------------------------}

data MetricHandle
    = GaugeHandle !Gauge.Gauge
    | LabelHandle !Label.Label

-- | Consume 'LogValue' metrics and mirror them into the ekg-core store,
-- using exactly the key naming of the previous EKGView backend:
-- gauges get the suffix @.us .ns .s .B .int@, labels @.real@ and @.sev@.
mkMetricsConsumer :: Metrics.Store -> IO ((Text, Measurable) -> IO ())
mkMetricsConsumer store = do
    cache <- MV.newMVar HM.empty
    let
        getMetric :: Text -> (Text -> IO MetricHandle) -> IO MetricHandle
        getMetric name create = MV.modifyMVar cache $ \hm ->
            case HM.lookup name hm of
                Just h -> pure (hm, h)
                Nothing -> do
                    h <- create name
                    pure (HM.insert name h hm, h)

        setGauge logname ext v = do
            GaugeHandle g <-
                getMetric
                    (logname <> ext)
                    (fmap GaugeHandle . flip Metrics.createGauge store)
            Gauge.set g v

        setLabel logname ext v = do
            LabelHandle l <-
                getMetric
                    (logname <> ext)
                    (fmap LabelHandle . flip Metrics.createLabel store)
            Label.set l v

    pure $ \(logname, mvar) -> case mvar of
        Microseconds v -> setGauge logname ".us" (fromIntegral v)
        Nanoseconds v -> setGauge logname ".ns" (fromIntegral v)
        Seconds v -> setGauge logname ".s" (fromIntegral v)
        Bytes v -> setGauge logname ".B" (fromIntegral v)
        PureI v -> setGauge logname ".int" (fromIntegral v)
        PureD v -> setLabel logname ".real" (T.pack $ show v)
        Severity v -> setLabel logname ".sev" (T.pack $ show v)

{-------------------------------------------------------------------------------
    Counter capturing
-------------------------------------------------------------------------------}

-- | Capture runtime metrics counters every 30 seconds and trace them into
-- the metrics pipeline. The first sample is emitted immediately.
startCapturingMetrics :: Trace IO Text -> IO ()
startCapturingMetrics trace0 = do
    capture <- Async.async $ forever $ do
        cts <- readCounters counters
        traceCounters trace cts
        threadDelay 30000000 -- capture every 30 sec
    registerCapture capture
  where
    trace = appendName "metrics" trace0
    counters =
        [ MemoryCounter
        , StatInfo
        , NetCounter
        , IOCounter
        , RTSStats
        , SysInfo
        ]

traceCounters
    :: forall m a. MonadIO m => Trace m a -> [Counter] -> m ()
traceCounters tr = mapM_ $ \c@(Counter _ct cn cv) -> do
    mle <- liftIO $ mkLOMeta Notice Confidential
    traceNamedObject tr (mle, LogValue (nameCounter c <> "." <> cn) cv)

{-------------------------------------------------------------------------------
    Prometheus endpoint
-------------------------------------------------------------------------------}

-- | Serve the metric store in Prometheus text format.
startPrometheus :: Metrics.Store -> (String, Int) -> IO (IO ())
startPrometheus store (host, port) = do
    let app _request respond = do
            sample <- Metrics.sampleAll store
            respond
                $ Wai.responseLBS
                    Http.status200
                    []
                    (renderSimpleOutput sample)
        settings =
            Warp.setHost (fromString host)
                $ Warp.setPort port Warp.defaultSettings
    warpAsync <- Async.async $ Warp.runSettings settings app
    pure $ Async.cancel warpAsync

-- | Sanitize a metric name for Prometheus: dots, dashes and spaces become
-- underscores, then only letters and underscores are kept.
prepareName :: T.Text -> B.Builder
prepareName =
    B.byteString
        . T.encodeUtf8
        . T.filter (\c -> Char.isLetter c || c == '_')
        . T.map (\c -> if c `elem` (".- " :: [Char]) then '_' else c)

renderSimpleOutput :: Metrics.Sample -> LB.ByteString
renderSimpleOutput sample =
    B.toLazyByteString
        $ mconcat
        $ map (<> B.charUtf8 '\n')
        $ mconcat
        $ map renderSample
        $ HM.toList sample

renderSample :: (T.Text, Metrics.Value) -> [B.Builder]
renderSample (nm, Metrics.Counter v) =
    [prepareName nm <> " " <> B.int64Dec v]
renderSample (nm, Metrics.Gauge v) =
    [prepareName nm <> " " <> B.int64Dec v]
renderSample (nm, Metrics.Label v)
    | "{" `T.isPrefixOf` v =
        [prepareName nm <> " " <> B.byteString (T.encodeUtf8 v) <> " 1"]
    | isFloat v =
        [prepareName nm <> " " <> B.byteString (T.encodeUtf8 v)]
    | otherwise = []
renderSample (_, Metrics.Distribution _) = []

isFloat :: Text -> Bool
isFloat s = case (reads (T.unpack s) :: [(Double, String)]) of
    [(_, "")] -> True
    _ -> False
