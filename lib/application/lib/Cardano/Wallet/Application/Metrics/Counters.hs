{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Resource counters for the wallet process, read from the Linux @/proc@
-- filesystem. This is a faithful port of the counter readers previously
-- provided by @iohk-monitoring@, keeping the emitted metric names identical.
-- On non-Linux platforms no counters are read.
module Cardano.Wallet.Application.Metrics.Counters
    ( Counter (..)
    , CounterType (..)
    , nameCounter
    , readCounters
    ) where

import Cardano.BM.Data.LogItem
    ( Measurable (..)
    )
import Data.Aeson
    ( FromJSON
    , ToJSON
    )
import Data.Text
    ( Text
    )
import GHC.Generics
    ( Generic
    )
import Prelude

#ifdef linux_HOST_OS
import Control.Exception
    ( IOException
    , handle
    )
import Data.Foldable
    ( foldrM
    )
import Data.Maybe
    ( fromMaybe
    , mapMaybe
    )
import Data.Word
    ( Word64
    )
import GHC.Clock
    ( getMonotonicTimeNSec
    )

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified GHC.Stats as GhcStats
import qualified System.Posix.Files as F
import qualified System.Posix.Process as P
import qualified System.Posix.Types as PT
#endif

data Counter = Counter
    { cType :: !CounterType
    , cName :: !Text
    , cValue :: !Measurable
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CounterType
    = MonotonicClockTime
    | MemoryCounter
    | SysInfo
    | StatInfo
    | IOCounter
    | NetCounter
    | RTSStats
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

nameCounter :: Counter -> Text
nameCounter (Counter MonotonicClockTime _ _) = "Clock"
nameCounter (Counter MemoryCounter _ _) = "Mem"
nameCounter (Counter SysInfo _ _) = "Sys"
nameCounter (Counter StatInfo _ _) = "Stat"
nameCounter (Counter IOCounter _ _) = "IO"
nameCounter (Counter NetCounter _ _) = "Net"
nameCounter (Counter RTSStats _ _) = "RTS"

-- | Read the given counter families. The wallet uses memory, process, net,
-- io, ghc rts and sys stats.
readCounters :: [CounterType] -> IO [Counter]
#ifdef linux_HOST_OS
readCounters tts = do
    pid <- getProcessID
    takeMeasurements tts pid
#else
readCounters _ = pure []
#endif

#ifdef linux_HOST_OS

type ProcessID = PT.ProcessID

takeMeasurements :: [CounterType] -> ProcessID -> IO [Counter]
takeMeasurements tts pid =
    foldrM
        ( \(sel, fun) a ->
            if any (== sel) tts then (fun >>= \xs -> return $ a ++ xs) else return a
        )
        []
        selectors
  where
    selectors =
        [ (MonotonicClockTime, getMonoClock)
        , (MemoryCounter, readProcStatM pid)
        , (StatInfo, readProcStats pid)
        , (NetCounter, readProcNet pid)
        , (SysInfo, readSysStats pid)
        , (IOCounter, readProcIO pid)
        , (RTSStats, readRTSStats)
        ]

-- | Read monotonic clock.
getMonoClock :: IO [Counter]
getMonoClock = do
    t <- getMonotonicTimeNSec
    return [Counter MonotonicClockTime "monoclock" $ Microseconds (t `div` 1000)]

-- | Read GHC RTS statistics. Values are as per the last GC run, and are only
-- available if RTS statistics are enabled (@+RTS -T@).
readRTSStats :: IO [Counter]
readRTSStats = do
    iscollected <- GhcStats.getRTSStatsEnabled
    if iscollected
        then ghcstats
        else return []
  where
    ghcstats :: IO [Counter]
    ghcstats = do
        rts <- GhcStats.getRTSStats
        let getrts = ghcval rts
        return
            [ getrts (Bytes . fromIntegral . GhcStats.allocated_bytes, "bytesAllocated")
            , getrts (Bytes . fromIntegral . GhcStats.cumulative_live_bytes, "liveBytes")
            , getrts (Bytes . fromIntegral . GhcStats.max_live_bytes, "maxLiveBytes")
            , getrts (Bytes . fromIntegral . GhcStats.max_large_objects_bytes, "maxLargeBytes")
            , getrts (Bytes . fromIntegral . GhcStats.max_compact_bytes, "maxCompactBytes")
            , getrts (Bytes . fromIntegral . GhcStats.max_slop_bytes, "maxSlopBytes")
            , getrts (Bytes . fromIntegral . GhcStats.max_mem_in_use_bytes, "maxUsedMemBytes")
            , getrts (Bytes . fromIntegral . GhcStats.gcdetails_live_bytes . GhcStats.gc, "gcLiveBytes")
            , getrts (Bytes . fromIntegral . GhcStats.gcdetails_copied_bytes . GhcStats.gc, "gcCopiedBytes")
            , getrts (Nanoseconds . fromIntegral . GhcStats.gc_cpu_ns, "gcCpuNs")
            , getrts (Nanoseconds . fromIntegral . GhcStats.gc_elapsed_ns, "gcElapsedNs")
            , getrts (Nanoseconds . fromIntegral . GhcStats.cpu_ns, "cpuNs")
            , getrts (Nanoseconds . fromIntegral . GhcStats.elapsed_ns, "elapsedNs")
            , getrts (PureI . toInteger . GhcStats.gcs, "gcNum")
            , getrts (PureI . toInteger . GhcStats.major_gcs, "gcMajorNum")
            ]
    ghcval :: GhcStats.RTSStats -> ((GhcStats.RTSStats -> Measurable), Text) -> Counter
    ghcval s (f, n) = Counter RTSStats n (f s)

data Platform = UnknownPlatform | Linux | Darwin | Windows
    deriving (Show, Eq, Ord, Enum)

-- | Generic platform specific information.
readSysStats :: ProcessID -> IO [Counter]
readSysStats pid = do
    return
        [ Counter SysInfo "Pid" (PureI $ fromIntegral pid)
        , Counter SysInfo "Platform" (PureI $ fromIntegral $ fromEnum Linux)
        ]

getProcessID :: IO ProcessID
getProcessID = P.getProcessID

readProcList :: FilePath -> IO [Word64]
readProcList fname =
    handle (\(_ :: IOException) -> return []) $ do
        fs <- F.getFileStatus fname
        if readable fs
            then do
                s <- T.readFile fname
                let sl = T.words s
                return $ map (fromMaybe 0 . readMaybeText) sl
            else return []
  where
    -- Check if the file is readable by the user, not only by the owner.
    readable :: F.FileStatus -> Bool
    readable fs =
        F.intersectFileModes (F.fileMode fs) F.ownerReadMode == F.ownerReadMode

readMaybeText :: Integral a => Text -> Maybe a
readMaybeText t =
    case T.decimal t of
        Right (v, _) -> Just v
        _ -> Nothing

-- | Read process memory stats from @/proc/pid/statm@.
readProcStatM :: ProcessID -> IO [Counter]
readProcStatM pid = do
    ps0 <- readProcList (pathProcStatM pid)
    return
        $ map (\(cn, pv) -> Counter MemoryCounter cn (PureI $ toInteger pv))
        $ filter (\n -> fst n /= "unused")
        $ zip colnames ps0
  where
    -- NOTE: The placeholders are positional. @/proc/<pid>/statm@ holds
    -- @size resident shared text lib data dt@, so the names must be zipped
    -- against the /full/ column list before the placeholders are dropped.
    -- Filtering @colnames@ first would shift @data@ onto @lib@, which the
    -- kernel has hard-coded to 0 since Linux 2.6.
    colnames = ["size", "resident", "shared", "text", "unused", "data", "unused"]

-- | Read process stats from @/proc/pid/stat@.
readProcStats :: ProcessID -> IO [Counter]
readProcStats pid = do
    ps0 <- readProcList (pathProcStat pid)
    let
        -- The following indices in /proc/<pid>/stat correspond to:
        -- utime:  /proc/<pid>/stat[13]
        -- stime:  /proc/<pid>/stat[14]
        --
        -- NOTE: 'readProcList' returns [] when the file is unreadable, so the
        -- bounds guard is what keeps a failed read from throwing out of the
        -- unlinked capture thread and freezing every counter. Degrading to 0
        -- is the former behaviour.
        ticks =
            if length ps0 > 15
                then (ps0 !! 13) + (ps0 !! 14)
                else 0

        -- Insert CPU ticks @ 0 to get a more accurate picture in the beginning
        ps2 = [("cputicks", ticks)] <> filter (\n -> fst n /= "unused") (zip colnames ps0)

    return $
        map (\(cn, pv) -> Counter StatInfo cn (PureI $ toInteger pv)) $
            metricWanted [0, 20, 24, 42] ps2
  where
    colnames =
        [ "pid"
        , "unused"
        , "unused"
        , "ppid"
        , "pgrp"
        , "session"
        , "ttynr"
        , "tpgid"
        , "flags"
        , "minflt"
        , "cminflt"
        , "majflt"
        , "cmajflt"
        , "utime"
        , "stime"
        , "cutime"
        , "cstime"
        , "priority"
        , "nice"
        , "numthreads"
        , "itrealvalue"
        , "starttime"
        , "vsize"
        , "rss"
        , "rsslim"
        , "startcode"
        , "endcode"
        , "startstack"
        , "kstkesp"
        , "kstkeip"
        , "signal"
        , "blocked"
        , "sigignore"
        , "sigcatch"
        , "wchan"
        , "nswap"
        , "cnswap"
        , "exitsignal"
        , "processor"
        , "rtpriority"
        , "policy"
        , "blkio"
        , "guesttime"
        , "cguesttime"
        , "startdata"
        , "enddata"
        , "startbrk"
        , "argstart"
        , "argend"
        , "envstart"
        , "envend"
        , "exitcode"
        ]

-- | Read process IO stats from @/proc/pid/io@.
readProcIO :: ProcessID -> IO [Counter]
readProcIO pid = do
    ps0 <- readProcList (pathProcIO pid)
    return $
        map (\(cn, pv, u) -> Counter IOCounter cn (u pv)) $
            filter (\n -> fst3 n /= "ign") (zip3 colnames ps0 units)
  where
    fst3 (a, _, _) = a
    colnames =
        [ "ign"
        , "rchar"
        , "ign"
        , "wchar"
        , "ign"
        , "syscr"
        , "ign"
        , "syscw"
        , "ign"
        , "rbytes"
        , "ign"
        , "wbytes"
        , "ign"
        , "cxwbytes"
        ]
    units :: [Word64 -> Measurable]
    units =
        [ PureI . toInteger
        , Bytes
        , PureI . toInteger
        , Bytes
        , PureI . toInteger
        , PureI . toInteger
        , PureI . toInteger
        , PureI . toInteger
        , PureI . toInteger
        , Bytes
        , PureI . toInteger
        , Bytes
        , PureI . toInteger
        , Bytes
        ]

-- | Read net stats from @/proc/pid/net/netstat@.
readProcNet :: ProcessID -> IO [Counter]
readProcNet pid = do
    fields <- T.words . fourthLine . T.lines <$> T.readFile (pathProcNet pid)
    case
        fmap readMaybeText $ take 2 $ drop 7 fields of
        [Just netIn, Just netOut] ->
            return
                [ Counter NetCounter "IpExt:InOctets" (Bytes netIn)
                , Counter NetCounter "IpExt:OutOctets" (Bytes netOut)
                ]
        _ -> pure []
  where
    -- Assumption: 'IpExt:' values are on the fourth line of how the kernel
    -- displays the buffer.
    fourthLine ls = case drop 3 ls of
        l : _ -> l
        _ -> T.empty

metricWanted :: [Int] -> [(Text, Word64)] -> [(Text, Word64)]
metricWanted idxs ps =
    mapMaybe (\i -> if i < length ps then Just (ps !! i) else Nothing) idxs

pathProcStatM :: ProcessID -> FilePath
pathProcStatM pid = "/proc/" <> show pid <> "/statm"

pathProcStat :: ProcessID -> FilePath
pathProcStat pid = "/proc/" <> show pid <> "/stat"

pathProcIO :: ProcessID -> FilePath
pathProcIO pid = "/proc/" <> show pid <> "/io"

pathProcNet :: ProcessID -> FilePath
pathProcNet pid = "/proc/" <> show pid <> "/net/netstat"

#endif
