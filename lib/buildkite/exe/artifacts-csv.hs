{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import Prelude

import Buildkite.API
    ( Artifact (..)
    , Build (..)
    )
import Buildkite.Artifacts
    ( artifactsHeader
    , mkArtifactRow
    )
import Buildkite.Client
    ( getArtifacts
    , getBuilds
    )
import Buildkite.Connection
    ( OrganizationName (..)
    , PipelineName (..)
    , bailout
    , newConnector
    )
import Buildkite.LimitsLock
    ( LimitsLockLog (..)
    )
import Control.Tracer
    ( Tracer (..)
    , traceWith
    )
import Data.ByteString.Builder
    ( toLazyByteString
    )
import Data.Csv.Builder
    ( encodeHeader
    , encodeNamedRecord
    )
import Data.Function
    ( (&)
    )
import Data.Functor.Contravariant
    ( (>$<)
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
    , optional
    , progDesc
    , strOption
    )

import qualified Data.ByteString.Lazy as BL
import qualified Streaming.Prelude as S

cardanoFoundation :: OrganizationName
cardanoFoundation = OrganizationName "cardano-foundation"

mainPipeline :: PipelineName
mainPipeline = PipelineName "cardano-wallet"

adminPipeline :: PipelineName
adminPipeline = PipelineName "cardano-wallet-buildkite-admin"

data Options = Options
    { _optUntilBuild :: Maybe Int
    , _optOutputPath :: FilePath
    }

parseOptUntilBuild :: Parser (Maybe Int)
parseOptUntilBuild =
    optional
        $ option auto
        $ long "until"
            <> metavar "BUILD_NUMBER"
            <> help "The build number until which to collect artifacts"

parseOptOutputPath :: Parser FilePath
parseOptOutputPath =
    strOption
        $ long "output"
            <> metavar "OUTPUT_PATH"
            <> help "The path to write the CSV output to"

optionsParser :: ParserInfo Options
optionsParser =
    info
        ( Options
            <$> parseOptUntilBuild
            <*> parseOptOutputPath
        )
        ( fullDesc
            <> progDesc "Collect and process benchmark history data."
            <> header "benchmark-history - a tool for benchmark data analysis"
        )

main :: IO ()
main = do
    Options mUntilBuild outputPath <- execParser optionsParser
    connector <-
        newConnector "BUILDKITE_API_TOKEN" 10
            $ LimitsLockLog >$< logsTracer
    let queryCardanoWallet = connector cardanoFoundation mainPipeline bailout
        queryAdmin = connector cardanoFoundation adminPipeline bailout
    _checkPoint <-
        S.head_
            $ getBuilds queryAdmin
            & S.chain (traceWith logsTracer . ProgressAdmin . number)
            & flip S.for (getArtifacts queryAdmin)
            & S.filter ((==) "finished" . state . snd)
            & S.filter ((==) "artifacts-history/artifacts.csv" . path . snd)
    {-     mHistoryUntil <- case checkPoint of
            Just (build, artifact) -> do
                history <-
                    getArtifactsContent queryAdmin fetchCSVArtifactContent build artifact
                pure $ Just ()
            Nothing -> pure mUntilBuild -}
    rows <-
        S.foldMap_ (encodeNamedRecord artifactsHeader)
            $ getBuilds queryCardanoWallet
            & S.takeWhile (\b -> maybe True (number b >) mUntilBuild)
            & S.chain (traceWith logsTracer . ProgressMain . number)
            & flip S.for (getArtifacts queryCardanoWallet)
            & S.filter ((==) "finished" . state . snd)
            & S.map (uncurry mkArtifactRow)
            & S.catMaybes

    BL.writeFile outputPath
        $ toLazyByteString
        $ encodeHeader artifactsHeader <> rows
    pure ()

renderLogs :: LimitsLockLog -> String
renderLogs (RateLimitReached secs) =
    "Rate limit reached. Waiting for " ++ show secs ++ " seconds"

data Logs
    = LimitsLockLog LimitsLockLog
    | ProgressMain Int
    | ProgressAdmin Int

logsTracer :: Tracer IO Logs
logsTracer = Tracer $ \case
    LimitsLockLog l -> putStrLn $ renderLogs l
    ProgressMain n -> putStrLn $ "Processing main build " ++ show n
    ProgressAdmin n -> putStrLn $ "Processing admin build " ++ show n
