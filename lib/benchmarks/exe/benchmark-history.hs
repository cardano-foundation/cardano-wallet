{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import Prelude

import Buildkite.API
    ( Artifact (job_id, path)
    , Build (branch, jobs, number)
    , Job (finished_at)
    , Time (Time)
    , bailout
    , skip410
    )
import Buildkite.Artifacts.CSV
    ( fetchCSVArtifactContent
    )
import Buildkite.Client
    ( BuildAPI
    , BuildJobsMap
    , Query (..)
    , getArtifacts
    , getArtifactsContent
    , getBuildsOfBranch
    )
import Buildkite.Connection
    ( Connector
    , OrganizationName (..)
    , PipelineName (..)
    , newConnector
    )
import Buildkite.LimitsLock
    ( LimitsLockLog (..)
    )
import Cardano.Wallet.Benchmarks.Charting
    ( renderHarmonizedHistoryChartSVG
    )
import Cardano.Wallet.Benchmarks.Collect
    ( Benchmark (..)
    , Result (..)
    )
import Cardano.Wallet.Benchmarks.History
    ( History
    , IndexedSemantic (IndexedSemantic)
    , harmonizeHistory
    , parseHistory
    , pastDays
    , renderHarmonizedHistoryCsv
    )
import Control.Tracer
    ( Tracer
    , stdoutTracer
    , traceWith
    )
import Data.Csv
    ( decodeByName
    , encodeByName
    )
import Data.Foldable
    ( Foldable (..)
    )
import Data.Function
    ( (&)
    )
import Data.Functor.Contravariant
    ( (>$<)
    )
import Data.Semigroup
    ( First (..)
    )
import Data.Text
    ( Text
    , isSuffixOf
    )
import Data.Time
    ( Day
    , UTCTime (utctDay)
    )
import Options.Applicative
    ( Parser
    , ParserInfo
    , auto
    , execParser
    , fullDesc
    , header
    , help
    , info
    , long
    , metavar
    , option
    , progDesc
    , strOption
    )
import Streaming
    ( MonadIO (..)
    , Of (..)
    , Stream
    )
import System.FilePath
    ( (<.>)
    , (</>)
    )

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Map as Map
import qualified Data.Map.Monoidal.Strict as MMap
import qualified Data.Text as T
import qualified Streaming as S
import qualified Streaming.Prelude as S

cardanoFoundationName :: OrganizationName
cardanoFoundationName = OrganizationName "cardano-foundation"

mainPipeline :: PipelineName
mainPipeline = PipelineName "cardano-wallet"

bind
    :: Monad m
    => (a -> Stream (Of b) m x)
    -> Stream (Of a) m r
    -> Stream (Of b) m r
bind = flip S.for

data Progress
    = ProgressBuild Int Text
    | FoundCheckpoint Artifact

type Checkpoint = Maybe (BuildJobsMap, Artifact, BL8.ByteString)

exitOnCheckpoint
    :: Tracer IO Progress
    -> ((BuildJobsMap, Artifact) -> IO Checkpoint)
    -> Stream (Of (BuildJobsMap, Artifact)) IO ()
    -> Stream (Of (BuildJobsMap, Artifact)) IO Checkpoint
exitOnCheckpoint t f s = S.effect $ do
    m <- S.next s
    case m of
        Left () -> pure $ pure Nothing
        Right ((b, a), s') -> do
            let run = do
                    S.yield (b, a)
                    exitOnCheckpoint t f s'
            if path a == "benchmark-history.csv"
                then do
                    r <- f (b, a)
                    pure $ case r of
                        Nothing -> run
                        Just x -> do
                            liftIO $ traceWith t $ FoundCheckpoint a
                            pure $ Just x
                else pure run

getHistory
    :: Tracer IO Progress
    -> Connector
    -> Day
    -> IO (Of History Checkpoint)
getHistory progressTracer mkQuery d0 = do
    let queryPipeline = mkQuery cardanoFoundationName mainPipeline
        skip410Q = queryPipeline skip410
        bailoutQ = queryPipeline bailout
        getAnyCSVArtifact
            :: MonadIO m
            => (BuildJobsMap, Artifact)
            -> m (Maybe (BuildJobsMap, Artifact, BL8.ByteString))
        getAnyCSVArtifact =
            uncurry
                $ getArtifactsContent skip410Q fetchCSVArtifactContent
    S.foldMap Prelude.id
        $ getReleaseCandidateBuilds bailoutQ d0
        & bind (getArtifacts bailoutQ)
        & exitOnCheckpoint progressTracer getAnyCSVArtifact
        & S.filter (\(_, a) -> "bench-results.csv" `isSuffixOf` path a)
        & S.chain
            ( \(b, _) ->
                liftIO
                    $ traceWith progressTracer
                    $ ProgressBuild (number b) (branch b)
            )
        & S.mapM getAnyCSVArtifact
        & S.concat
        & bind historyPoints
        & bind
            ( \case
                Right h -> S.yield h
                Left e -> error e
            )

mkReleaseCandidateName :: Day -> String
mkReleaseCandidateName d = "release-candidate/v" ++ show d

getReleaseCandidateBuilds :: Query -> Day -> S.Stream (S.Of BuildAPI) IO ()
getReleaseCandidateBuilds q d = S.effect $ do
    ds <- pastDays d
    pure
        $ flip S.for (getBuildsOfBranch q . mkReleaseCandidateName)
        $ S.each ds

data Options = Options
    { _optSinceDate :: Day
    , _outputDir :: FilePath
    }

parseOptDate :: Parser Day
parseOptDate =
    option auto
        $ long "since"
            <> metavar "YYYY-MM-DD"
            <> help "The date to start collecting data from"

parseOptChartsDir :: Parser FilePath
parseOptChartsDir =
    strOption
        $ long "charts-dir"
            <> metavar "DIR"
            <> help "The directory to save the charts to"

optionsParser :: ParserInfo Options
optionsParser =
    info
        (Options <$> parseOptDate <*> parseOptChartsDir)
        ( fullDesc
            <> progDesc "Collect and process benchmark history data."
            <> header "benchmark-history - a tool for benchmark data analysis"
        )

data Logs = ProgressLogs Progress | ConnectorLogs LimitsLockLog

renderLogs :: Logs -> String
renderLogs = \case
    ProgressLogs (ProgressBuild n b) ->
        "Processing build " <> show n <> " on branch " <> T.unpack b
    ProgressLogs (FoundCheckpoint a) ->
        "Found chain point: " <> T.unpack (path a)
    ConnectorLogs (RateLimitReached s) ->
        "Rate limit reached. Waiting for " <> show s <> " seconds."

tracer :: Tracer IO Logs
tracer = renderLogs >$< stdoutTracer

main :: IO ()
main = do
    Options sinceDay outputDir <- execParser optionsParser
    connector <-
        newConnector "BUILDKITE_API_TOKEN" 10 $ ConnectorLogs >$< tracer
    result :> mCheckpoint <-
        getHistory (ProgressLogs >$< tracer) connector sinceDay
    old <- case mCheckpoint of
        Nothing -> pure mempty
        Just (_, _, b) -> case parseHistory b of
            Left e -> error e
            Right h -> pure h
    case harmonizeHistory $ result <> old of
        Left rs -> error $ "Failed to harmonize history: " ++ show rs
        Right harmonized -> do
            let csv =
                    uncurry encodeByName
                        $ renderHarmonizedHistoryCsv harmonized
            BL8.writeFile (outputDir </> "benchmark-history" <.> "csv") csv
            renderHarmonizedHistoryChartSVG outputDir harmonized

parseResults :: BL8.ByteString -> Either String ([(IndexedSemantic, Result)])
parseResults = fmap (fmap f . zip [0 ..] . toList . snd) . decodeByName
  where
    f (i, (Benchmark s r)) = (IndexedSemantic s i, r)

historyPoints
    :: Monad m
    => (BuildJobsMap, Artifact, BL8.ByteString)
    -> Stream (Of (Either String History)) m ()
historyPoints (b, a, r) =
    let
        rs = parseResults r
        t = finished_at =<< Map.lookup (job_id a) (jobs b)
    in
        case rs of
            Left e -> S.yield $ Left e
            Right rs' -> case t of
                Nothing -> pure ()
                Just (Time t') -> S.yield $ Right $ fold $ do
                    (i, r') <- rs'
                    pure
                        $ MMap.singleton i
                        $ MMap.singleton (utctDay t')
                        $ First r'
