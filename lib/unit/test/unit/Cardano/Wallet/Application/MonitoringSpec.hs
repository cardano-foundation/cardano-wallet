{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2026 Cardano Foundation
-- License: Apache-2.0
--
-- Tests for the two compatibility contracts of the wallet-owned monitoring
-- setup that no type can check: the emitted text log layout, and the
-- process-wide ownership of the metric endpoints.
module Cardano.Wallet.Application.MonitoringSpec
    ( spec
    ) where

import Cardano.Wallet.Application.Monitoring
    ( formatItem
    , initTracer
    )
import Cardano.Wallet.Network.Ports
    ( getRandomPort
    , isPortOpen
    , simpleSockAddr
    )
import Cardano.Wallet.Tracing.Data.LogItem
    ( LOMeta (..)
    , PrivacyAnnotation (..)
    )
import Cardano.Wallet.Tracing.Data.Severity
    ( Severity (..)
    )
import Control.Concurrent
    ( threadDelay
    )
import Data.Text
    ( Text
    )
import Data.Time.Clock
    ( UTCTime
    )
import Network.Socket
    ( SockAddr
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldReturn
    )
import UnliftIO.Exception
    ( IOException
    , bracket
    , catch
    )
import Prelude

import qualified Data.Text as T

spec :: Spec
spec = do
    describe "text log format" $ do
        -- The layout pinned here is the one the wallet emitted before the
        -- iohk-monitoring removal, taken from a live capture:
        --
        -- > \ESC[34m[cardano-wallet.main:Info:4]\ESC[0m
        -- >   [2026-08-26 06:02:38.25 UTC] Running as v2026-08-21
        --
        -- Everything that greps wallet logs depends on it and nothing else
        -- checks it, so it is asserted rather than described in a comment.
        it "renders header, then bracketed timestamp, then message"
            $ formatItem False "cardano-wallet.main" (meta Info) "Running"
            `shouldBe` "[cardano-wallet.main:Info:4] \
                       \[2026-08-26 06:02:38.25 UTC] Running"

        it "keeps sub-second precision and the zone suffix"
            $ formatItem False "ns" (meta Info) "m"
            `shouldBe` "[ns:Info:4] [2026-08-26 06:02:38.25 UTC] m"

        it "prefixes the host name only when it is set" $ do
            formatItem False "ns" (meta Info){hostname = "myhost"} "m"
                `shouldBe` "[myhost:ns:Info:4] [2026-08-26 06:02:38.25 UTC] m"
            formatItem False "ns" (meta Info) "m"
                `shouldBe` "[ns:Info:4] [2026-08-26 06:02:38.25 UTC] m"

        it "colours the whole header, not just the severity token"
            $ formatItem True "ns" (meta Info) "m"
            `shouldBe` "\ESC[34m[ns:Info:4]\ESC[0m \
                       \[2026-08-26 06:02:38.25 UTC] m"

        it "colours Notice magenta, Warning yellow and Error red" $ do
            headerOf (formatItem True "ns" (meta Notice) "m")
                `shouldBe` "\ESC[35m[ns:Notice:4]\ESC[0m"
            headerOf (formatItem True "ns" (meta Warning) "m")
                `shouldBe` "\ESC[33m[ns:Warning:4]\ESC[0m"
            headerOf (formatItem True "ns" (meta Error) "m")
                `shouldBe` "\ESC[31m[ns:Error:4]\ESC[0m"

        it "leaves Debug uncoloured"
            $ headerOf (formatItem True "ns" (meta Debug) "m")
            `shouldBe` "[ns:Debug:4]"

    describe "monitoring endpoint ownership"
        -- Regression: 'initTracer' owns the EKG listener, and the integration
        -- framework nests two 'initTracer' scopes. Binding once per scope
        -- would call 'forkServer' on an already-bound port -- which ekg-wai
        -- rethrows into the caller -- and would start a second counter
        -- capture thread.
        $ it "nests: one listener, released only by the last holder"
        $ do
            port <- getRandomPort
            let ekg = Just ("127.0.0.1", fromIntegral port :: Int)
                addr = simpleSockAddr (127, 0, 0, 1) port
            bracket (initTracer "outer" [] ekg Nothing) fst $ \_ -> do
                eventuallyPort True addr
                (shutdownInner, _) <- initTracer "inner" [] ekg Nothing
                eventuallyPort True addr
                shutdownInner
                -- Closing the inner scope must not take the listener down.
                eventuallyPort True addr
            -- The last release must actually free the port.
            eventuallyPort False addr
  where
    meta :: Severity -> LOMeta
    meta sev =
        LOMeta
            { tstamp = timestamp
            , tid = "4"
            , hostname = ""
            , severity = sev
            , privacy = Public
            }

    timestamp :: UTCTime
    timestamp = read "2026-08-26 06:02:38.25 UTC"

    -- Everything up to the space that precedes the bracketed timestamp.
    headerOf :: Text -> Text
    headerOf = fst . T.breakOn " [2026-"

-- | Poll until the port reaches the wanted state. The listener starts and
-- stops asynchronously, so a bounded retry is required; the final attempt
-- asserts, so a port that never gets there fails the test.
--
-- 'isPortOpen' rethrows every connect error that is not @ECONNREFUSED@, and a
-- listener that is coming up or going down can answer @ECONNRESET@ instead.
-- Treat any such transient error as "not in the wanted state yet" rather than
-- letting it escape: it says nothing about ownership, which is what this is
-- measuring.
eventuallyPort :: Bool -> SockAddr -> IO ()
eventuallyPort wanted addr = go (100 :: Int)
  where
    probe :: IO (Maybe Bool)
    probe =
        (Just <$> isPortOpen addr)
            `catch` \(_ :: IOException) -> pure Nothing
    go 0 = probe `shouldReturn` Just wanted
    go n = do
        r <- probe
        if r == Just wanted
            then pure ()
            else threadDelay 50000 >> go (n - 1)
