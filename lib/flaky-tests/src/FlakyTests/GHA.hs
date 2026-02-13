-- |
-- Copyright: © 2025 Cardano Foundation
-- License: Apache-2.0
--
-- GH Actions API integration for fetching workflow run logs.
module FlakyTests.GHA
    ( fetchFlakyRuns
    , FetchConfig (..)
    , defaultFetchConfig
    )
where

import Codec.Archive.Zip
    ( Archive (..)
    , Entry (..)
    , fromEntry
    , toArchive
    )
import Data.ByteString.Lazy
    ( ByteString
    )
import Data.Maybe
    ( catMaybes
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Text
    ( Text
    )
import FlakyTests.HspecParser
    ( parseLog
    )
import FlakyTests.Types
    ( RunInfo (..)
    , TestFailure (..)
    )
import GitHub
    ( Auth (..)
    , Error
    , WithTotalCount (..)
    , executeRequest
    , untagId
    )
import GitHub.Data.Actions.WorkflowRuns
    ( WorkflowRun (..)
    )
import GitHub.Data.Name
    ( mkName
    )
import GitHub.Data.Options
    ( optionsWorkflowRunBranch
    , optionsWorkflowRunStatus
    )
import GitHub.Data.Request
    ( FetchCount (FetchAtLeast)
    )
import GitHub.Endpoints.Actions.WorkflowRuns
    ( workflowRunsR
    )
import Network.HTTP.Client
    ( Manager
    , Request (..)
    , httpLbs
    , parseRequest
    , responseBody
    , responseStatus
    )
import Network.HTTP.Client.TLS
    ( newTlsManager
    )
import Network.HTTP.Types.Status
    ( statusCode
    )
import System.IO
    ( hPutStrLn
    , stderr
    )
import Prelude

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

-- | Configuration for fetching flaky test data.
data FetchConfig = FetchConfig
    { cfgOwner :: !Text
    , cfgRepo :: !Text
    , cfgBranch :: !(Maybe Text)
    , cfgMaxRuns :: !Int
    , cfgMaxFailures :: !Int
    }
    deriving (Eq, Show)

-- | Default config for cardano-wallet Conway integration tests.
defaultFetchConfig :: FetchConfig
defaultFetchConfig =
    FetchConfig
        { cfgOwner = "cardano-foundation"
        , cfgRepo = "cardano-wallet"
        , cfgBranch = Just "master"
        , cfgMaxRuns = 50
        , cfgMaxFailures = 10
        }

-- | Fetch workflow runs and extract flaky test failures.
-- Returns a list of @(RunInfo, [TestFailure])@ pairs for
-- runs with 1–maxFailures failures.
fetchFlakyRuns
    :: Auth
    -> FetchConfig
    -> IO (Either Error [(RunInfo, [TestFailure])])
fetchFlakyRuns auth cfg = do
    manager <- newTlsManager
    let owner = mkName Proxy (cfgOwner cfg)
        repo = mkName Proxy (cfgRepo cfg)
        mods =
            optionsWorkflowRunStatus "failure"
                <> maybe mempty optionsWorkflowRunBranch (cfgBranch cfg)
        count = FetchAtLeast (fromIntegral (cfgMaxRuns cfg))
    result <- executeRequest auth $ workflowRunsR owner repo mods count
    case result of
        Left err -> pure (Left err)
        Right (WithTotalCount runs total) -> do
            let runList = V.toList runs
            hPutStrLn stderr
                $ "Found "
                    <> show (length runList)
                    <> " failed runs (total: "
                    <> show total
                    <> ")"
            pairs <- mapM (processRun auth manager cfg) runList
            pure (Right (catMaybes pairs))

processRun
    :: Auth
    -> Manager
    -> FetchConfig
    -> WorkflowRun
    -> IO (Maybe (RunInfo, [TestFailure]))
processRun auth manager cfg run = do
    let runIdVal = untagId (workflowRunWorkflowRunId run)
        runUrl' =
            "https://github.com/"
                <> cfgOwner cfg
                <> "/"
                <> cfgRepo cfg
                <> "/actions/runs/"
                <> T.pack (show runIdVal)
    hPutStrLn stderr $ "Processing run " <> show runIdVal <> "..."
    mLog <- downloadConwayLog auth manager cfg runIdVal
    case mLog of
        Nothing -> do
            hPutStrLn stderr "  No Conway log found"
            pure Nothing
        Just logContent -> do
            let (mSummary, failures) = parseLog logContent
            hPutStrLn stderr
                $ "  Summary: "
                    <> show mSummary
                    <> ", failures extracted: "
                    <> show (length failures)
            case mSummary of
                Nothing -> pure Nothing
                Just (totalEx, failCount)
                    | failCount >= 1
                    , failCount <= cfgMaxFailures cfg ->
                        let info =
                                RunInfo
                                    { runId = runIdVal
                                    , runDate = workflowRunCreatedAt run
                                    , runTotalExamples = totalEx
                                    , runFailureCount = failCount
                                    , runUrl = runUrl'
                                    }
                        in  pure (Just (info, failures))
                    | otherwise -> pure Nothing

-- | Download the log zip for a run and extract the Conway
-- integration test log file.
downloadConwayLog
    :: Auth
    -> Manager
    -> FetchConfig
    -> Int
    -> IO (Maybe Text)
downloadConwayLog auth manager cfg runIdVal = do
    -- The github package's downloadWorkflowRunLogsR returns a
    -- redirect URI. We handle it manually via http-client.
    let apiUrl =
            "https://api.github.com/repos/"
                <> T.unpack (cfgOwner cfg)
                <> "/"
                <> T.unpack (cfgRepo cfg)
                <> "/actions/runs/"
                <> show runIdVal
                <> "/logs"
    mZip <- downloadZip auth manager apiUrl
    case mZip of
        Nothing -> do
            hPutStrLn stderr "  Failed to download logs zip"
            pure Nothing
        Just zipBytes -> do
            let archive = toArchive zipBytes
                names = map eRelativePath (zEntries archive)
            hPutStrLn stderr
                $ "  Zip entries: " <> show names
            pure (extractConwayLog zipBytes)

downloadZip
    :: Auth
    -> Manager
    -> String
    -> IO (Maybe ByteString)
downloadZip auth manager url = do
    req <- parseRequest url
    let token = case auth of
            OAuth t -> t
            _ -> error "flaky-tests: only OAuth auth is supported"
        req' =
            req
                { requestHeaders =
                    [ ("Authorization", "Bearer " <> token)
                    , ("Accept", "application/vnd.github+json")
                    , ("User-Agent", "flaky-tests")
                    , ("X-GitHub-Api-Version", "2022-11-28")
                    ]
                }
    resp <- httpLbs req' manager
    if statusCode (responseStatus resp) == 200
        then pure (Just (responseBody resp))
        else pure Nothing

-- | Extract the Conway integration test log from a zip archive.
-- Looks for files matching @*Conway Integration Tests*@ or
-- @*conway*integration*@.
extractConwayLog :: ByteString -> Maybe Text
extractConwayLog zipBytes =
    let archive = toArchive zipBytes
        entries = zEntries archive
        conwayEntries =
            filter isConwayEntry entries
    in  case conwayEntries of
            [] -> Nothing
            (e : _) ->
                Just
                    $ T.decodeUtf8
                    $ BL.toStrict
                    $ fromEntry e
  where
    isConwayEntry e =
        let name = T.toLower (T.pack (eRelativePath e))
        in  T.isInfixOf "conway" name
                && T.isInfixOf "integration" name
