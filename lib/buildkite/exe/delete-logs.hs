{-# LANGUAGE OverloadedStrings #-}

import Prelude

import Buildkite.API
    ( Job (..)
    )
import Buildkite.Client
    ( deleteJobLogsMatching
    )
import Buildkite.Connection
    ( newConnector
    )
import Control.Tracer
    ( Contravariant (..)
    , stdoutTracer
    )
import Data.Text
    ( Text
    )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
    ( Parser
    , execParser
    , fullDesc
    , help
    , helper
    , info
    , long
    , metavar
    , progDesc
    , short
    , strOption
    , (<**>)
    )
import qualified Streaming.Prelude as S

data Options
    = Options
    { _branch :: String
    , _job :: Text
    }

parseOptBranch :: Parser String
parseOptBranch =
    strOption
        ( long "branch"
            <> short 'b'
            <> metavar "BRANCH"
            <> help "Branch to delete logs from"
        )

parseOptJob :: Parser Text
parseOptJob =
    strOption
        ( long "job"
            <> short 'j'
            <> metavar "JOB"
            <> help "Part of job name to delete logs from"
        )

parseOptions :: Parser Options
parseOptions =
    Options
        <$> parseOptBranch
        <*> parseOptJob

main :: IO ()
main = do
    Options branch job <-
        execParser
            $ info
                (parseOptions <**> helper)
                ( fullDesc
                    <> progDesc "Delete logs from specific jobs on a branch"
                )
    connector <-
        newConnector "BUILDKITE_API_TOKEN" 10 $ show `contramap` stdoutTracer
    let query =
            connector
                "cardano-foundation"
                "cardano-wallet"
    S.mapM_ renderLink $ deleteJobLogsMatching query branch job

renderLink :: (Int, Job) -> IO ()
renderLink (build, Job id' _ _ _ _) =
    T.putStrLn
        $ "https://buildkite.com/cardano-foundation/cardano-wallet/builds/"
            <> T.pack (show build)
            <> "#"
            <> id'
