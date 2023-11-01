{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.LatencyBenchShared
  ( -- * Measuring traces
    withLatencyLogging
  , measureApiLogs
  , LogCaptureFunc

    -- * Formatting results
  , fmtResult
  , fmtTitle
  ) where

import Prelude

import Cardano.BM.Backend.Switchboard
    ( effectuate
    )
import Cardano.BM.Configuration.Static
    ( defaultConfigStdout
    )
import Cardano.BM.Data.LogItem
    ( LOContent (..)
    , LOMeta (..)
    , LogObject (..)
    )
import Cardano.BM.Data.Severity
    ( Severity (..)
    )
import Cardano.BM.Setup
    ( setupTrace_
    , shutdown
    )
import Control.Monad
    ( replicateM_
    )
import Data.Maybe
    ( mapMaybe
    )
import Data.Time
    ( NominalDiffTime
    )
import Data.Time.Clock
    ( diffUTCTime
    )
import Fmt
    ( Builder
    , build
    , fixedF
    , fmt
    , fmtLn
    , indentF
    , padLeftF
    , (+|)
    , (|+)
    )
import Network.Wai.Middleware.Logging
    ( ApiLog (..)
    , HandlerLog (..)
    )
import UnliftIO.Exception
    ( bracket
    , onException
    )
import UnliftIO.STM
    ( TVar
    , atomically
    , newTVarIO
    , readTVarIO
    , writeTVar
    )

import qualified Cardano.BM.Configuration.Model as CM

meanAvg :: [NominalDiffTime] -> Double
meanAvg ts = sum (map realToFrac ts) * 1000 / fromIntegral (length ts)

buildResult :: [NominalDiffTime] -> Builder
buildResult [] = "ERR"
buildResult ts = build $ fixedF 1 $ meanAvg ts

fmtTitle :: Builder -> IO ()
fmtTitle title = fmt (indentF 4 title)

fmtResult :: String -> [NominalDiffTime] -> IO ()
fmtResult title ts =
    let titleExt = title|+" - " :: String
        titleF = padLeftF 30 ' ' titleExt
    in fmtLn (titleF+|buildResult ts|+" ms")

isLogRequestStart :: ApiLog -> Bool
isLogRequestStart = \case
    ApiLog _ LogRequestStart -> True
    _ -> False

isLogRequestFinish :: ApiLog -> Bool
isLogRequestFinish = \case
    ApiLog _ LogRequestFinish -> True
    _ -> False

measureApiLogs :: LogCaptureFunc ApiLog () -> IO a -> IO [NominalDiffTime]
measureApiLogs = measureLatency isLogRequestStart isLogRequestFinish

-- | Run tests for at least this long to get accurate timings.
sampleNTimes :: Int
sampleNTimes = 10

-- | Measure how long an action takes based on trace points and taking an
-- average of results over a short time period.
measureLatency
    :: (msg -> Bool) -- ^ Predicate for start message
    -> (msg -> Bool) -- ^ Predicate for end message
    -> LogCaptureFunc msg () -- ^ Log capture function.
    -> IO a -- ^ Action to run
    -> IO [NominalDiffTime]
measureLatency start finish capture action = do
    (logs, ()) <- capture $ replicateM_ sampleNTimes action
    pure $ extractTimings start finish logs

-- | Scan through iohk-monitoring logs and extract time differences between
-- start and end messages.
extractTimings
    :: (a -> Bool) -- ^ Predicate for start message
    -> (a -> Bool) -- ^ Predicate for end message
    -> [LogObject a] -- ^ Log messages
    -> [NominalDiffTime]
extractTimings isStart isFinish msgs = map2 mkDiff filtered
  where
    map2 _ [] = []
    map2 f (a:b:xs) = (f a b:map2 f xs)
    map2 _ _ = error "start trace without matching finish trace"

    mkDiff (False, start) (True, finish) = diffUTCTime finish start
    mkDiff (False, _) _ = error "missing finish trace"
    mkDiff (True, _) _ = error "missing start trace"

    filtered = mapMaybe filterMsg msgs
    filterMsg logObj = case loContent logObj of
        LogMessage msg | isStart msg -> Just (False, getTimestamp logObj)
        LogMessage msg | isFinish msg -> Just (True, getTimestamp logObj)
        _ -> Nothing
    getTimestamp = tstamp . loMeta

type LogCaptureFunc msg b = IO b -> IO ([LogObject msg], b)

withLatencyLogging
    :: (TVar [LogObject ApiLog] -> tracers)
    -> (tracers -> LogCaptureFunc ApiLog b -> IO a)
    -> IO a
withLatencyLogging setupTracers action = do
    tvar <- newTVarIO []
    cfg <- defaultConfigStdout
    CM.setMinSeverity cfg Debug
    bracket (setupTrace_ cfg "bench-latency") (shutdown . snd) $ \(_, sb) -> do
        action (setupTracers tvar) (logCaptureFunc tvar) `onException` do
            fmtLn "Action failed. Here are the captured logs:"
            readTVarIO tvar >>= mapM_ (effectuate sb) . reverse

logCaptureFunc :: TVar [LogObject ApiLog] -> LogCaptureFunc ApiLog b
logCaptureFunc tvar action = do
  atomically $ writeTVar tvar []
  res <- action
  logs <- readTVarIO tvar
  pure (reverse logs, res)
