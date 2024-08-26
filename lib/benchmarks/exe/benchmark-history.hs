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
    , bailout
    , newConnector
    , skip410
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
    , pastDays
    , renderHarmonizedHistoryCsv
    )
import Control.Tracer
    ( stdoutTracer
    )
import Data.Csv
    ( decodeByName
    , encodeByName
    )
import Data.Foldable
    ( Foldable (..)
    )
import Data.Functor.Contravariant
    ( (>$<)
    )
import Data.Semigroup
    ( First (..)
    )
import Data.Text
    ( isSuffixOf
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
    ( Of (..)
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

queryBuildkite
    :: Connector
    -> Day
    -> IO History
queryBuildkite q d0 = do
    let q' = q cardanoFoundationName mainPipeline
        skip410Q = q' skip410
        bailoutQ = q' bailout
    S.foldMap_ Prelude.id
        $ flip
            S.for
            ( \case
                Right h -> S.yield h
                Left e -> error e
            )
        $ flip S.for historyPoints
        $ flip
            S.for
            ( \(a, j) ->
                getArtifactsContent skip410Q fetchCSVArtifactContent j a
            )
        $ S.chain
            ( \(_, b) ->
                putStrLn
                    $ "Build number: "
                        <> show (number b)
                        <> ", branch: "
                        <> T.unpack (branch b)
            )
        $ S.filter (\(a, _) -> "bench-results.csv" `isSuffixOf` path a)
        $ S.map (\(b, a) -> (a, b))
        $ flip S.for (getArtifacts bailoutQ)
        $ getReleaseCandidateBuilds bailoutQ d0

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

main :: IO ()
main = do
    Options sinceDay outputDir <- execParser optionsParser
    connector <- newConnector "BUILDKITE_API_TOKEN" 10 $ show >$< stdoutTracer
    result <- queryBuildkite connector sinceDay
    let eHarmonized = harmonizeHistory result
    case eHarmonized of
        Left rs -> error $ "Failed to harmonize history: " ++ show rs
        Right harmonized -> do
            putStrLn $ "Harmonized history: " <> show harmonized
            let csv = uncurry encodeByName $ renderHarmonizedHistoryCsv harmonized
            BL8.writeFile (outputDir </> "benchmark_history" <.> "csv") csv
            renderHarmonizedHistoryChartSVG outputDir harmonized

parseResults :: BL8.ByteString -> Either String ([(IndexedSemantic, Result)])
parseResults = fmap (fmap f . zip [0 ..] . toList . snd) . decodeByName
  where
    f (i, (Benchmark s r)) = (IndexedSemantic s i, r)

historyPoints
    :: (BuildJobsMap, Artifact, BL8.ByteString)
    -> Stream (Of (Either String History)) IO ()
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
