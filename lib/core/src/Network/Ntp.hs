{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- This module provides the Ntp client related settings, types
-- and re-exports used in a number of places throught codebase.

module Network.Ntp
    ( withWalletNtpClient
    , getNtpStatus

    -- * re-exports from ntp-client
    , NtpTrace (..)
    , NtpClient (..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Wallet.Api.Types
    ( ApiNetworkClock (..), ApiNtpStatus (..), NtpSyncingStatus (..) )
import Control.Monad.STM
    ( atomically )
import Control.Tracer
    ( Tracer )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( ToText (..) )
import Network.NTP.Client
    ( IPVersion (..)
    , NtpClient (..)
    , NtpSettings (..)
    , NtpStatus (..)
    , NtpTrace (..)
    , withNtpClient
    )
import System.IOManager
    ( IOManager )

-- | Set up a 'NtpClient' and pass it to the given action. The 'NtpClient' is
-- terminated when the callback returns.
withWalletNtpClient
    :: IOManager
    -- ^ The global 'IOManager' instance, set up by the application main function.
    -> Tracer IO NtpTrace
    -- ^ Logging object
    -> (NtpClient -> IO a)
    -- ^ Action to run
    -> IO a
withWalletNtpClient ioManager tr = withNtpClient ioManager tr ntpSettings

-- | Hard-coded NTP servers for cardano-wallet.
ntpSettings :: NtpSettings
ntpSettings = NtpSettings
    { ntpServers = [ "0.de.pool.ntp.org", "0.europe.pool.ntp.org"
                   , "0.pool.ntp.org", "1.pool.ntp.org"
                   , "2.pool.ntp.org", "3.pool.ntp.org" ]
    , ntpRequiredNumberOfResults = 3
    , ntpResponseTimeout = 1_000_000
    , ntpPollDelay = 300_000_000
    }

instance ToText IPVersion where
    toText IPv4 = "IPv4"
    toText IPv6 = "IPv6"

instance ToText NtpTrace where
    toText msg = case msg of
        NtpTraceStartNtpClient ->
            "Starting ntp client"
        NtpTraceRestartDelay d ->
            "ntp client restart delay is " <> toText d
        NtpTraceRestartingClient ->
            "ntp client is restarting"
        NtpTraceIOError e ->
            "ntp client experienced io error " <> toText (show e)
        NtpTraceLookupsFails ->
            "ntp client failed to lookup the ntp servers"
        NtpTraceClientStartQuery ->
            "query to ntp client invoked"
        NtpTraceNoLocalAddr ->
            "no local address error when running ntp client"
        NtpTraceResult a ->
            "ntp client gives result of " <> toText (show a)
        NtpTraceRunProtocolResults a ->
            "ntp client run protocol results: " <> toText (show a)
        NtpTracePacketSent _ a ->
            "ntp client sent packet when running " <> toText (show a)
        NtpTracePacketSendError _ e ->
            "ntp client experienced error " <> toText (show e)
            <> " when sending packet"
        NtpTracePacketDecodeError _ e ->
            "ntp client experienced error " <> toText (show e)
            <> " when decoding packet"
        NtpTracePacketReceived _ a ->
            "ntp client received packet: " <> toText (show a)
        NtpTraceWaitingForRepliesTimeout v ->
            "ntp client experienced timeout using " <> toText v <> " protocol"

instance HasPrivacyAnnotation NtpTrace
instance HasSeverityAnnotation NtpTrace where
    getSeverityAnnotation ev = case ev of
        NtpTraceStartNtpClient -> Info
        NtpTraceRestartDelay _ -> Info
        NtpTraceRestartingClient -> Info
        NtpTraceIOError _ -> Notice
        NtpTraceLookupsFails -> Notice
        NtpTraceClientStartQuery -> Info
        NtpTraceNoLocalAddr -> Notice
        NtpTraceResult _ -> Info
        NtpTraceRunProtocolResults _ -> Info
        NtpTracePacketSent _ _ -> Debug
        NtpTracePacketSendError _ _ -> Notice
        NtpTracePacketDecodeError _ _ -> Notice
        NtpTracePacketReceived _ _ -> Debug
        NtpTraceWaitingForRepliesTimeout _ -> Notice

getNtpStatus :: NtpClient -> Bool -> IO ApiNetworkClock
getNtpStatus client forceCheck = (ApiNetworkClock . toStatus) <$>
    if forceCheck
    then ntpQueryBlocking client
    else atomically (ntpGetStatus client)
  where
    toStatus = \case
        NtpSyncPending ->
            ApiNtpStatus NtpSyncingStatusPending Nothing
        NtpSyncUnavailable ->
            ApiNtpStatus NtpSyncingStatusUnavailable Nothing
        NtpDrift ms ->
            ApiNtpStatus NtpSyncingStatusAvailable
            (Just $ Quantity (fromIntegral ms :: Integer))
