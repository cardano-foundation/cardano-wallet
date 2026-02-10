-- |
-- Copyright: © 2025 Cardano Foundation
-- License: Apache-2.0
--
-- Data types for flaky test collection.
module FlakyTests.Types
    ( TestFailure (..)
    , RunInfo (..)
    , FlakySummary (..)
    , FlakyEntry (..)
    )
where

import Prelude

import Data.Aeson
    ( ToJSON (..)
    , object
    , (.=)
    )
import Data.Map.Strict
    ( Map
    )
import Data.Text
    ( Text
    )
import Data.Time
    ( UTCTime
    )

-- | A single test failure extracted from a log.
data TestFailure = TestFailure
    { testPath :: !Text
    -- ^ Comma-separated describe hierarchy
    , errorSummary :: !Text
    -- ^ First line of the error message
    }
    deriving (Eq, Ord, Show)

instance ToJSON TestFailure where
    toJSON tf =
        object
            [ "test_path" .= testPath tf
            , "error_summary" .= errorSummary tf
            ]

-- | Metadata about a GH Actions workflow run.
data RunInfo = RunInfo
    { runId :: !Int
    , runDate :: !UTCTime
    , runTotalExamples :: !Int
    , runFailureCount :: !Int
    , runUrl :: !Text
    }
    deriving (Eq, Ord, Show)

instance ToJSON RunInfo where
    toJSON ri =
        object
            [ "run_id" .= runId ri
            , "date" .= runDate ri
            , "total_examples" .= runTotalExamples ri
            , "failure_count" .= runFailureCount ri
            , "url" .= runUrl ri
            ]

-- | A single entry in the flaky summary.
data FlakyEntry = FlakyEntry
    { flakyCount :: !Int
    , flakyRuns :: ![RunInfo]
    }
    deriving (Eq, Show)

instance ToJSON FlakyEntry where
    toJSON fe =
        object
            [ "count" .= flakyCount fe
            , "runs" .= flakyRuns fe
            ]

-- | Aggregated flaky test results: test name → frequency and runs.
newtype FlakySummary = FlakySummary
    { unFlakySummary :: Map Text FlakyEntry
    }
    deriving (Eq, Show)

instance ToJSON FlakySummary where
    toJSON = toJSON . unFlakySummary
