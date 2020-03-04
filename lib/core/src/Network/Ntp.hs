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
    ( ntpSettings
    , getNtpStatus

    -- * re-exports from ntp-client
    , withNtpClient
    , NtpTrace (..)
    , NtpClient (..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( DefinePrivacyAnnotation (..), DefineSeverity (..) )
import Cardano.Wallet.Api.Types
    ( ApiNetworkClock (..), ApiNtpStatus (..), NtpSyncingStatus (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( ToText (..) )
import Network.NTP.Client
    ( NtpClient (..), withNtpClient )
import Network.NTP.Packet
    ( Microsecond (..), NtpOffset (..) )
import Network.NTP.Query
    ( NtpSettings (..), NtpStatus (..) )
import Network.NTP.Trace
    ( IPVersion (..), NtpTrace (..) )

ntpSettings :: NtpSettings
ntpSettings = NtpSettings
    { ntpServers = [ "0.de.pool.ntp.org", "0.europe.pool.ntp.org"
                   , "0.pool.ntp.org", "1.pool.ntp.org"
                   , "2.pool.ntp.org", "3.pool.ntp.org" ]
    , ntpResponseTimeout = 1_000_000
    , ntpPollDelay       = 300_000_000
    }

instance ToText IPVersion where
    toText IPv4 = "IPv4"
    toText IPv6 = "IPv6"

instance ToText NtpTrace where
    toText msg = case msg of
        NtpTraceStartNtpClient ->
            "Starting ntp client"
        NtpTraceTriggerUpdate ->
            "ntp client is triggered"
        NtpTraceRestartDelay d ->
            "ntp client restart delay is "<>toText d
        NtpTraceRestartingClient ->
            "ntp client is restarted"
        NtpTraceClientSleeping ->
            "ntp client is going to sleep"
        NtpTraceIOError err ->
            "ntp client experienced io error " <> toText (show err)
        NtpTraceLookupServerFailed str ->
            "ntp client failed to lookup the ntp servers " <> toText str
        NtpTraceClientStartQuery ->
            "query to ntp client invoked"
        NtpTraceNoLocalAddr ->
            "no local address error when running ntp client"
        NtpTraceIPv4IPv6NoReplies ->
            "no replies from servers when running ntp client"
        NtpTraceReportPolicyQueryFailed ->
            "policy query error when running ntp client"
        NtpTraceQueryResult (Microsecond ms) ->
            "ntp client gives offset of "<> toText ms <> " microseconds"
        NtpTraceRunProtocolError ver err ->
            "ntp client experienced error "<>toText (show err)
            <>" when using "<>toText ver <> " protocol"
        NtpTraceRunProtocolNoResult ver ->
            "ntp client got no result when running "<> toText ver <> " protocol"
        NtpTraceRunProtocolSuccess ver ->
            "ntp client successfull running "<> toText ver <> " protocol"
        NtpTraceSocketOpen ver ->
            "ntp client opened socket when running "<> toText ver
        NtpTraceSocketClosed ver ->
            "ntp client closed socket when running "<> toText ver
        NtpTracePacketSent ver ->
            "ntp client sent packet when running "<> toText ver
        NtpTracePacketSentError ver err ->
            "ntp client experienced error "<> toText (show err)
            <>" when sending packet using "<> toText ver
        NtpTracePacketDecodeError ver err ->
            "ntp client experienced error "<> toText (show err)
            <>" when decoding packet using "<> toText ver
        NtpTracePacketReceived ver ->
            "ntp client received packet using "<> toText ver
        NtpTraceWaitingForRepliesTimeout ver ->
            "ntp client experienced timeout using "<> toText ver <> " protocol"

instance DefinePrivacyAnnotation NtpTrace
instance DefineSeverity NtpTrace where
    defineSeverity ev = case ev of
        NtpTraceStartNtpClient -> Info
        NtpTraceTriggerUpdate -> Info
        NtpTraceRestartDelay _ -> Info
        NtpTraceRestartingClient -> Info
        NtpTraceClientSleeping -> Info
        NtpTraceIOError _ -> Alert
        NtpTraceLookupServerFailed _ -> Alert
        NtpTraceClientStartQuery -> Info
        NtpTraceNoLocalAddr -> Alert
        NtpTraceIPv4IPv6NoReplies -> Info
        NtpTraceReportPolicyQueryFailed -> Alert
        NtpTraceQueryResult _ -> Info
        NtpTraceRunProtocolError _ _ -> Alert
        NtpTraceRunProtocolNoResult _ -> Info
        NtpTraceRunProtocolSuccess _ -> Debug
        NtpTraceSocketOpen _ -> Debug
        NtpTraceSocketClosed _ -> Debug
        NtpTracePacketSent _ -> Debug
        NtpTracePacketSentError _ _ -> Alert
        NtpTracePacketDecodeError _ _-> Alert
        NtpTracePacketReceived _ -> Debug
        NtpTraceWaitingForRepliesTimeout _ -> Alert

getNtpStatus :: NtpClient -> IO ApiNetworkClock
getNtpStatus client = ntpQueryBlocking client >>= \case
    NtpSyncPending ->
        pure $ ApiNetworkClock $
        ApiNtpStatus NtpSyncingStatusPending Nothing
    NtpSyncUnavailable ->
        pure $ ApiNetworkClock $
        ApiNtpStatus NtpSyncingStatusUnavailable Nothing
    (NtpDrift (NtpOffset (Microsecond ms))) ->
        pure $ ApiNetworkClock $
        ApiNtpStatus NtpSyncingStatusAvailable
        (Just $ Quantity $ fromIntegral ms)
