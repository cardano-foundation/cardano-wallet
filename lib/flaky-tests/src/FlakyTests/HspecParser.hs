-- |
-- Copyright: © 2025 Cardano Foundation
-- License: Apache-2.0
--
-- Parse hspec log output from GH Actions.
module FlakyTests.HspecParser
    ( parseLog
    , extractSummary
    , extractFailures
    , stripTimestamps
    , stripAnsi
    )
where

import Data.Char
    ( isDigit
    )
import Data.Text
    ( Text
    )
import FlakyTests.Types
    ( TestFailure (..)
    )
import Prelude

import qualified Data.Text as T

-- | Strip GHA timestamp prefixes of the form
-- @2026-02-10T14:40:35.4702189Z @
stripTimestamps :: Text -> Text
stripTimestamps = T.unlines . map stripLine . T.lines
  where
    stripLine line =
        -- Pattern: YYYY-MM-DDTHH:MM:SS.NNNNNNNZ<space>
        -- Find "Z " and check prefix looks like a timestamp
        case T.breakOn "Z " line of
            (prefix, rest)
                | not (T.null rest)
                , T.length prefix >= 20
                , T.length prefix <= 35
                , looksLikeTimestamp prefix ->
                    T.drop 2 rest -- drop "Z "
            _ -> line
    looksLikeTimestamp t =
        -- Check starts with digit (year) and contains 'T'
        case T.uncons t of
            Just (c, _) -> isDigit c && T.isInfixOf "T" t
            Nothing -> False

-- | Strip ANSI escape codes from text.
stripAnsi :: Text -> Text
stripAnsi = go
  where
    go t = case T.breakOn "\ESC[" t of
        (before, rest)
            | T.null rest -> before
            | otherwise ->
                let afterEsc = T.drop 2 rest -- drop "\ESC["
                    afterCode = T.dropWhile isParamChar afterEsc
                    -- drop the final command character (e.g. 'm')
                    remaining =
                        if T.null afterCode
                            then afterCode
                            else T.drop 1 afterCode
                in  before <> go remaining
    isParamChar c = isDigit c || c == ';'

-- | Extract the summary line @N examples, M failures@.
-- Returns @Nothing@ if not found.
extractSummary :: Text -> Maybe (Int, Int)
extractSummary input =
    let ls = T.lines (stripAnsi (stripTimestamps input))
    in  case filter isSummaryLine ls of
            [] -> Nothing
            xs -> parseSummary (last xs)
  where
    isSummaryLine l =
        T.isInfixOf "example" l && T.isInfixOf "failure" l
    parseSummary l =
        let ws = T.words (T.strip l)
        in  case ws of
                (exTxt : "examples," : fTxt : _) ->
                    case (readMaybe exTxt, readMaybe fTxt) of
                        (Just ex, Just f) -> Just (ex, f)
                        _ -> Nothing
                _ -> Nothing

-- | Extract failed test names from numbered failure lines.
-- Looks for lines like @  N) Describe, Context, test name@.
extractFailures :: Text -> [TestFailure]
extractFailures input =
    let cleaned = stripAnsi (stripTimestamps input)
        ls = T.lines cleaned
    in  mapMaybe parseFailureLine ls
  where
    parseFailureLine l =
        let trimmed = T.stripStart l
        in  case T.break (== ')') trimmed of
                (numPart, rest)
                    | not (T.null numPart)
                    , T.all isDigit numPart
                    , Just afterParen <-
                        T.stripPrefix ")" rest ->
                        let name = T.strip afterParen
                        in  if T.null name
                                then Nothing
                                else
                                    Just
                                        TestFailure
                                            { testPath = name
                                            , errorSummary = ""
                                            }
                _ -> Nothing

-- | Parse a full log: extract summary and failures.
parseLog
    :: Text
    -> (Maybe (Int, Int), [TestFailure])
parseLog input = (extractSummary input, extractFailures input)

-- Helpers

readMaybe :: (Read a) => Text -> Maybe a
readMaybe t = case reads (T.unpack t) of
    [(x, "")] -> Just x
    _ -> Nothing

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x : xs) = case f x of
    Just y -> y : mapMaybe f xs
    Nothing -> mapMaybe f xs
