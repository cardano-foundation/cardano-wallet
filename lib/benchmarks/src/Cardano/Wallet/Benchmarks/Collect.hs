{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}

{-# HLINT ignore "Functor law" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Cardano.Wallet.Benchmarks.Collect
    ( -- * Benchmark
      Benchmark (..)

      -- * Result
    , Result (..)
    , Unit (..)

      -- * Semantic
    , Semantic
    , mkSemantic
    , noSemantic

      -- * Collecting results
    , Reporter
    , addSemantic
    , readSemantic
    , report
    , newReporter
    , newReporterFromEnv
    , newReporterResourceT
    , newReporterResourceTFromEnv

      -- * Collecting results from criterion benchmarks
    , runCriterionBenchmark
    , convertUnit

    ) where

import Prelude

import Cardano.BM.Tracing
    ( HasSeverityAnnotation (..)
    , Severity (..)
    , Tracer
    , traceWith
    )
import Control.Comonad
    ( Comonad (..)
    )
import Control.Foldl
    ( Fold (..)
    , fold
    )
import Control.Monad
    ( void
    )
import Control.Monad.Cont
    ( ContT (..)
    )
import Control.Monad.Fix
    ( fix
    )
import Control.Monad.Trans.Resource
    ( ResourceT
    , register
    )
import Criterion.Measurement
    ( getTime
    , measure
    )
import Criterion.Measurement.Types
    ( Measured (..)
    )
import Data.Csv
    ( FromField (..)
    , FromNamedRecord (..)
    , Header
    , ToField (..)
    , ToNamedRecord (..)
    , header
    , namedRecord
    , (.:)
    , (.=)
    )
import Data.Csv.Incremental
    ( encodeByName
    , encodeNamedRecord
    )
import Data.Foldable
    ( toList
    )
import Data.Int
    ( Int64
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText
    )
import Data.Text.Class.Extended
    ( ToText (..)
    )
import System.Environment
    ( lookupEnv
    )
import UnliftIO
    ( MonadIO (..)
    , atomically
    , modifyTVar'
    , newTVarIO
    , readTVarIO
    )

import qualified Criterion.Measurement.Types as Cr
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

-- | A semantic for a benchmark.
newtype Semantic = Semantic [Text]
    deriving newtype (Show, Semigroup, Eq, Ord)

-- | An empty semantic.
noSemantic :: Semantic
noSemantic = Semantic []

instance FromField Semantic where
    parseField = fmap (mkSemantic . reverse . T.words) . parseField

instance ToField Semantic where
    toField = toField . T.unwords . reverse . unMkSemantic

-- | Create a new semantic from a list of words.
-- It will frop empty words.
-- It will replace spaces with dashes.
mkSemantic :: [Text] -> Semantic
mkSemantic = Semantic . filter (not . T.null) . fmap (T.replace " " "-")

unMkSemantic :: Semantic -> [Text]
unMkSemantic (Semantic xs) = toList xs

-- | Unit for a result.
data Unit
    = Seconds
    | Milliseconds
    | Microseconds
    | Nanoseconds
    | Bytes
    | KiloBytes
    | MegaBytes
    | GigaBytes
    | Count

instance Show Unit where
    show = showUnit

instance FromField Unit where
    parseField "s" = pure Seconds
    parseField "ms" = pure Milliseconds
    parseField "us" = pure Microseconds
    parseField "ns" = pure Nanoseconds
    parseField "B" = pure Bytes
    parseField "KB" = pure KiloBytes
    parseField "MB" = pure MegaBytes
    parseField "GB" = pure GigaBytes
    parseField "count" = pure Count
    parseField _ = fail "Invalid unit"

instance ToField Unit where
    toField = B8.pack . showUnit

showUnit :: Unit -> String
showUnit Seconds = "s"
showUnit Milliseconds = "ms"
showUnit Microseconds = "us"
showUnit Nanoseconds = "ns"
showUnit Bytes = "B"
showUnit KiloBytes = "KB"
showUnit MegaBytes = "MB"
showUnit GigaBytes = "GB"
showUnit Count = "count"

convertUnit :: Unit -> Unit -> Double -> Double
convertUnit from to = case (from, to) of
    (Seconds, Seconds) -> id
    (Seconds, Milliseconds) -> (* 1_000)
    (Seconds, Microseconds) -> (* 1_000_000)
    (Seconds, Nanoseconds) -> (* 1_000_000_000)
    (Milliseconds, Seconds) -> (/ 1_000)
    (Milliseconds, Milliseconds) -> id
    (Milliseconds, Microseconds) -> (* 1_000)
    (Milliseconds, Nanoseconds) -> (* 1_000_000)
    (Microseconds, Seconds) -> (/ 1_000_000)
    (Microseconds, Milliseconds) -> (/ 1_000)
    (Microseconds, Microseconds) -> id
    (Microseconds, Nanoseconds) -> (* 1_000)
    (Nanoseconds, Seconds) -> (/ 1_000_000_000)
    (Nanoseconds, Milliseconds) -> (/ 1_000_000)
    (Nanoseconds, Microseconds) -> (/ 1_000)
    (Nanoseconds, Nanoseconds) -> id
    (Bytes, Bytes) -> id
    (Bytes, KiloBytes) -> (/ 1_024)
    (Bytes, MegaBytes) -> (/ 1_024 ^ (2 :: Integer))
    (Bytes, GigaBytes) -> (/ 1_024 ^ (3 :: Integer))
    (KiloBytes, Bytes) -> (* 1_024)
    (KiloBytes, KiloBytes) -> id
    (KiloBytes, MegaBytes) -> (/ 1_024)
    (KiloBytes, GigaBytes) -> (/ 1_024 ^ (2 :: Integer))
    (MegaBytes, Bytes) -> (* 1_024 ^ (2 :: Integer))
    (MegaBytes, KiloBytes) -> (* 1_024)
    (MegaBytes, MegaBytes) -> id
    (MegaBytes, GigaBytes) -> (/ 1_024)
    (GigaBytes, Bytes) -> (* 1_024 ^ (3 :: Integer))
    (GigaBytes, KiloBytes) -> (* 1_024 ^ (2 :: Integer))
    (GigaBytes, MegaBytes) -> (* 1_024)
    (GigaBytes, GigaBytes) -> id
    (Count, Count) -> id
    _ -> error "Invalid unit"

-- | A result with a value and unit.
data Result = Result
    { resultValue :: Double
    , resultUnit :: Unit
    , resultIterations :: Int
    }

instance Show Result where
    show (Result value unit iterations) =
        show value
            ++ " "
            ++ show unit
            ++ " ("
            ++ show iterations
            ++ " iterations)"

-- | A benchmark result with a semantic and a value.
data Benchmark = Benchmark
    { benchmarkSemantic :: Semantic
    , benchmarkResult :: Result
    }
    deriving (Show)

instance HasSeverityAnnotation Benchmark where
    getSeverityAnnotation _ = Notice

instance ToText Benchmark where
    toText (Benchmark sem res) =
        T.unwords
            [ T.unwords $ reverse $ unMkSemantic sem
            , T.pack $ show res
            ]

instance ToNamedRecord Benchmark where
    toNamedRecord (Benchmark semantic (Result value unit iterations)) =
        namedRecord
            [ "semantic" .= toField semantic
            , "value" .= toField value
            , "units" .= toField unit
            , "iterations" .= toField iterations
            ]

addSemanticToBenchmark :: Semantic -> Benchmark -> Benchmark
addSemanticToBenchmark sem' (Benchmark sem res) = Benchmark (sem <> sem') res

instance FromNamedRecord Benchmark where
    parseNamedRecord v
        | length v == 4 =
            (\s r u i -> Benchmark s (Result r u i))
                <$> v .: "semantic"
                <*> v .: "value"
                <*> v .: "units"
                <*> v .: "iterations"
        | otherwise = fail "Expected 3 fields"

-- | An object to track results with an updatable Semantic
data Reporter m
    = Reporter
    { addSemantic :: Semantic -> Reporter m
    , readSemantic :: Semantic
    , report :: [Benchmark] -> m ()
    }

mkReport :: (Benchmark -> IO ()) -> Semantic -> Reporter IO
mkReport out sem =
    Reporter
        (\sem' -> mkReport out $ sem' <> sem)
        sem
        $ mapM_
        $ out . addSemanticToBenchmark sem

mkNullReport :: Applicative m => Reporter m
mkNullReport = Reporter (const mkNullReport) noSemantic (const $ pure ())

-- historical data use the 'units' (vs unit) column name
benchmarksHeader :: Header
benchmarksHeader = header ["semantic", "value", "units", "iterations"]

-- | Create a new reporter from a file path in a 'ContT' context.
newReporter
    :: FilePath
    -- ^ File path to write the results to
    -> Tracer IO Benchmark
    -- ^ Tracer for benchmark results
    -> Semantic
    -- ^ Root semantic for all benchmarks
    -> ContT r IO (Reporter IO)
newReporter fp tr sem0 = do
    outputVar <- newTVarIO mempty
    let update bench = do
            atomically
                $ modifyTVar' outputVar
                $ \bs -> bs <> encodeNamedRecord bench
            traceWith tr bench
    ContT $ \k -> do
        r <- k $ mkReport update sem0
        output <- readTVarIO outputVar
        BL.writeFile fp $ encodeByName benchmarksHeader output
        pure r

-- | Create a new reporter from the environment variable 'BENCHMARK_CSV_FILE'
-- in a 'ContT' context.
newReporterFromEnv
    :: Tracer IO Benchmark
    -- ^ Tracer for benchmark results
    -> Semantic
    -- ^ Root semantic for all benchmarks
    -> ContT r IO (Reporter IO)
newReporterFromEnv tr rootSem = do
    csvFile <- liftIO $ lookupEnv "BENCHMARK_CSV_FILE"
    case csvFile of
        Just fp -> newReporter fp tr rootSem
        Nothing -> pure mkNullReport

-- | Create a new reporter from a file path in a 'ResourceT' context.
newReporterResourceT
    :: MonadIO m
    => FilePath
    -- ^ File path to write the results to
    -> Tracer IO Benchmark
    -- ^ Tracer for benchmark results
    -> Semantic
    -- ^ Root semantic for all benchmarks
    -> ResourceT m (Reporter IO)
newReporterResourceT fp tr sem0 = do
    outputVar <- newTVarIO mempty
    let update bench = do
            atomically
                $ modifyTVar' outputVar
                $ \bs -> bs <> encodeNamedRecord bench
            traceWith tr bench
    void $ register $ do
        output <- readTVarIO outputVar
        BL.writeFile fp $ encodeByName benchmarksHeader output
    pure $ mkReport update sem0

-- | Create a new reporter from the environment variable 'BENCHMARK_CSV_FILE' in
-- a 'ResourceT' context.
newReporterResourceTFromEnv
    :: MonadIO m
    => Tracer IO Benchmark
    -- ^ Tracer for benchmark results
    -> Semantic
    -- ^ Root semantic for all benchmarks
    -> ResourceT m (Reporter IO)
newReporterResourceTFromEnv tr rootSem = do
    csvFile <- liftIO $ lookupEnv "BENCHMARK_CSV_FILE"
    case csvFile of
        Just fp -> newReporterResourceT fp tr rootSem
        Nothing -> pure mkNullReport

--------------------------------------------------------------------------------
--- Collecting results from criterion benchmarks -------------------------------
--------------------------------------------------------------------------------
data CountAndAppend a = CountAndAppend
    { cIterations :: !Int64
    , total :: !a
    }
    deriving stock (Show, Functor)

instance Monoid a => Monoid (CountAndAppend a) where
    mempty = CountAndAppend 0 mempty

instance Semigroup a => Semigroup (CountAndAppend a) where
    CountAndAppend n1 t1 <> CountAndAppend n2 t2 =
        CountAndAppend (n1 + n2) (t1 <> t2)

newtype Time = Time Double
    deriving newtype (Show, Num, Fractional, Ord, Eq, RealFrac, Real)

data MeasureAndTotal = MeasureAndTotal
    { measureTime :: !Time
    , measureTotalTime :: !Time
    }
    deriving (Show)

instance Semigroup MeasureAndTotal where
    MeasureAndTotal t1 tt1 <> MeasureAndTotal t2 tt2 =
        MeasureAndTotal (t1 + t2) (tt1 + tt2)

instance Monoid MeasureAndTotal where
    mempty = MeasureAndTotal 0 0

stateFromMeasured :: (Measured, Time) -> CountAndAppend MeasureAndTotal
stateFromMeasured (m, t) =
    CountAndAppend
        (measIters m)
        (MeasureAndTotal (Time $ measTime m) t)

-- | A special frontend for running criterion benchmarks with a given timeout.
-- It is designed to use 'Reporter IO' to collect the results and it is not
-- guaranteed that there is a statistical significance in the results.
-- In fact if the first iteration is already over the timeout, the result will
-- be the time of the first iteration.
runCriterionBenchmark
    :: Time
    -- ^ Timeout in seconds. At least 1 iteration will be run.
    -> Tracer IO Benchmark
    -- ^ Tracer for benchmark results
    -> Reporter IO
    -- ^ Reporter for benchmark results
    -> Cr.Benchmark
    -- ^ Criterion benchmark to run
    -> IO ()
runCriterionBenchmark timeout tr = go
  where
    go r (Cr.Benchmark s b) = do
        v <- runCriterion timeout (\n -> fst <$> measure b (fromIntegral n))
        let r' = addSemantic r $ mkSemantic [T.pack s]
            Time average = total v / fromIntegral (cIterations v)
            benchmark =
                Benchmark (readSemantic r')
                    $ Result average Seconds
                    $ fromIntegral
                    $ cIterations v
        traceWith tr benchmark
        report r' [benchmark]
    go r (Cr.BenchGroup s bs) = do
        let sem = mkSemantic [T.pack s]
        mapM_ (go $ addSemantic r sem) bs
    go r (Cr.Environment s c f) = do
        e <- s
        go r $ f e
        void $ c e

-- | Update a fold with a new value.
updateFold :: Fold input output -> input -> Fold input output
updateFold f x = fold (duplicate f) [x]

-- | A fold over a stream of (Measured, Time) where the time is the
-- real time of the full measurement.
-- It will output the total iterations, and the total measured time as from
-- the Measured values from criterion.
collectFold
    :: Time
    -- ^ Timeout in seconds. At least 1 iteration will be run.
    -> Fold (Measured, Time) (CountAndAppend Time, Int)
collectFold timeout = Fold add create value
  where
    add (state, _) v =
        let
            state'@(CountAndAppend n (MeasureAndTotal _ ut)) =
                state <> stateFromMeasured v
            timeLeft = max 0 $ timeout - ut
            estimate = ut / fromIntegral n
        in
            (state', floor $ timeLeft / estimate)
    create = (mempty, 1)
    value (state, j) = (fmap measureTime state, j)

withElapsedTime :: MonadIO m => m a -> m (a, Time)
withElapsedTime action = do
    start <- liftIO getTime
    result <- action
    end <- liftIO getTime
    pure (result, Time $ end - start)

runCriterion
    :: MonadIO m
    => Time
    -- ^ Timeout in seconds. At least 1 iteration will be run.
    -> (Int -> m Measured)
    -- ^ Action to run for each iteration
    -> m (CountAndAppend Time)
runCriterion t f = ($ (collectFold t)) $ fix $ \loop state -> do
    let (result, next) = extract state
    if next <= 0
        then pure result
        else do
            v <- withElapsedTime $ f next
            loop $ updateFold state v
