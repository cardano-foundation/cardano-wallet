{-# LANGUAGE OverloadedStrings #-}

import Prelude

import Cardano.Wallet.Benchmarks.Charting
    ( renderHarmonizedHistoryChartSVG
    )
import Cardano.Wallet.Benchmarks.History
    ( History
    , harmonizeHistory
    , historyFromResults
    , parseHistory
    , parseResults
    , pastDays
    , renderHarmonizedHistoryCsv
    )
import Data.Csv
    ( encodeByName
    )
import Data.Foldable
    ( Foldable (..)
    )
import Data.Text
    ( isSuffixOf
    )
import Data.Time
    ( Day
    , defaultTimeLocale
    , parseTimeM
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
import System.Directory
    ( listDirectory
    )
import System.FilePath
    ( takeFileName
    , (<.>)
    , (</>)
    )

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T

-- | Collect history from a data directory.
-- The directory must contain subdirectories named
-- @YYYY-MM-DD@, each with @*-bench-results.csv@ files.
collectHistory :: FilePath -> Day -> IO History
collectHistory dataDir sinceDay = do
    dirs <- listDirectory dataDir
    validDays <- pastDays sinceDay
    let validDirs = filter (isValidDateDir validDays) dirs
    fold <$> mapM (collectDayDir dataDir) validDirs

isValidDateDir :: [Day] -> FilePath -> Bool
isValidDateDir validDays name =
    case parseDay name of
        Nothing -> False
        Just d -> d `elem` validDays

parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d"

collectDayDir :: FilePath -> FilePath -> IO History
collectDayDir dataDir dirName = do
    let dir = dataDir </> dirName
    case parseDay dirName of
        Nothing -> pure mempty
        Just day -> do
            files <- listDirectory dir
            let csvFiles =
                    filter isBenchCsv files
            fold <$> mapM (readCsvFile dir day) csvFiles

isBenchCsv :: FilePath -> Bool
isBenchCsv f =
    let name = T.pack (takeFileName f)
    in  "-bench-results.csv" `isSuffixOf` name
            || "bench-results.csv" == name

readCsvFile :: FilePath -> Day -> FilePath -> IO History
readCsvFile dir day file = do
    content <- BL8.readFile (dir </> file)
    case parseResults content of
        Left e -> do
            putStrLn
                $ "Warning: failed to parse "
                    <> file
                    <> ": "
                    <> e
            pure mempty
        Right rs -> pure $ historyFromResults day rs

data Options = Options
    { _optSinceDate :: Day
    , _outputDir :: FilePath
    , _dataDir :: FilePath
    , _checkpoint :: Maybe FilePath
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

parseOptDataDir :: Parser FilePath
parseOptDataDir =
    strOption
        $ long "data-dir"
            <> metavar "DIR"
            <> help
                "Directory with YYYY-MM-DD subdirs\
                \ containing CSV files"

parseOptCheckpoint :: Parser (Maybe FilePath)
parseOptCheckpoint =
    optional
        $ strOption
        $ long "checkpoint"
            <> metavar "FILE"
            <> help
                "Previous benchmark-history.csv\
                \ for incremental use"

optionsParser :: ParserInfo Options
optionsParser =
    info
        ( Options
            <$> parseOptDate
            <*> parseOptChartsDir
            <*> parseOptDataDir
            <*> parseOptCheckpoint
        )
        ( fullDesc
            <> progDesc
                "Collect and process benchmark\
                \ history data."
            <> header
                "benchmark-history\
                \ - a tool for benchmark data analysis"
        )

main :: IO ()
main = do
    Options sinceDay outputDir dataDir mCheckpoint <-
        execParser optionsParser
    new <- collectHistory dataDir sinceDay
    old <- case mCheckpoint of
        Nothing -> pure mempty
        Just cpFile -> do
            content <- BL8.readFile cpFile
            case parseHistory content of
                Left e ->
                    error
                        $ "Failed to parse checkpoint: "
                            <> e
                Right h -> pure h
    case harmonizeHistory $ new <> old of
        Left rs ->
            error
                $ "Failed to harmonize history: "
                    ++ show rs
        Right harmonized -> do
            let csv =
                    uncurry encodeByName
                        $ renderHarmonizedHistoryCsv
                            harmonized
            BL8.writeFile
                (outputDir </> "benchmark-history" <.> "csv")
                csv
            renderHarmonizedHistoryChartSVG
                outputDir
                harmonized
