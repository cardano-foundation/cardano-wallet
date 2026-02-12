module Main where

import Data.Aeson
    ( encode
    )
import Data.Text
    ( Text
    )
import FlakyTests.GHA
    ( FetchConfig (..)
    , defaultFetchConfig
    , fetchFlakyRuns
    )
import FlakyTests.Types
    ( FlakyEntry (..)
    , FlakySummary (..)
    , RunInfo (..)
    , TestFailure (..)
    )
import GitHub
    ( Auth (..)
    )
import Options.Applicative
    ( Parser
    , execParser
    , fullDesc
    , help
    , helper
    , info
    , long
    , metavar
    , optional
    , progDesc
    , short
    , strOption
    , (<**>)
    )
import System.Environment
    ( lookupEnv
    )
import System.Exit
    ( exitFailure
    )
import System.IO
    ( hPutStrLn
    , stderr
    )
import Prelude

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

newtype Options = Options
    { optBranch :: Maybe Text
    }

parseOptions :: Parser Options
parseOptions =
    Options
        <$> optional
            ( strOption
                ( long "branch"
                    <> short 'b'
                    <> metavar "BRANCH"
                    <> help "Filter by branch (default: master)"
                )
            )

main :: IO ()
main = do
    Options mBranch <-
        execParser
            $ info
                (parseOptions <**> helper)
                ( fullDesc
                    <> progDesc
                        "Collect flaky test data from GH Actions"
                )
    mToken <- lookupEnv "GITHUB_TOKEN"
    case mToken of
        Nothing -> do
            hPutStrLn stderr "GITHUB_TOKEN env var not set"
            exitFailure
        Just token -> do
            let auth = OAuth (T.encodeUtf8 (T.pack token))
                cfg =
                    defaultFetchConfig
                        { cfgBranch = mBranch
                        }
            result <- fetchFlakyRuns auth cfg
            case result of
                Left err -> do
                    hPutStrLn stderr $ "GitHub API error: " <> show err
                    exitFailure
                Right pairs -> do
                    let summary = aggregate pairs
                    BL.putStrLn (encode summary)

aggregate :: [(RunInfo, [TestFailure])] -> FlakySummary
aggregate pairs =
    FlakySummary
        $ Map.fromListWith
            merge
            [ (testPath tf, FlakyEntry 1 [ri])
            | (ri, tfs) <- pairs
            , tf <- tfs
            ]
  where
    merge (FlakyEntry c1 r1) (FlakyEntry c2 r2) =
        FlakyEntry (c1 + c2) (r1 <> r2)
