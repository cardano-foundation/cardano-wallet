{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- This module provides the Ntp client related settings, types
-- and re-exports used in a number of places throughout codebase.
module Network.Ntp
  ( withWalletNtpClient
  , getNtpStatus
  , NtpSyncingStatus (..)
  , NtpStatusWithOffset (..)
  , ForceCheck (..)

    -- * re-exports from ntp-client
  , NtpTrace (..)
  , NtpClient (..)
  )
where

import Cardano.BM.Data.Severity
  ( Severity (..)
  )
import Cardano.BM.Data.Tracer
  ( HasPrivacyAnnotation (..)
  , HasSeverityAnnotation (..)
  )
import Control.DeepSeq
  ( NFData
  )
import Control.Tracer
  ( Tracer
  )
import Data.Quantity
  ( Quantity (..)
  )
import Data.Text
  ( Text
  )
import Data.Text qualified as T
import Data.Text.Class
  ( ToText (..)
  )
import GHC.Generics
  ( Generic
  )
import Network.NTP.Client
  ( IPVersion (..)
  , NtpClient (..)
  , NtpSettings (..)
  , NtpStatus (..)
  , NtpTrace (..)
  , ResultOrFailure (..)
  , withNtpClient
  )
import System.IOManager
  ( IOManager
  )
import UnliftIO.STM
  ( atomically
  , checkSTM
  )
import Prelude

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
ntpSettings =
  NtpSettings
    { ntpServers =
        [ "0.de.pool.ntp.org"
        , "0.europe.pool.ntp.org"
        , "0.pool.ntp.org"
        , "1.pool.ntp.org"
        , "2.pool.ntp.org"
        , "3.pool.ntp.org"
        ]
    , ntpRequiredNumberOfResults = 3
    , ntpResponseTimeout = 1_000_000
    , ntpPollDelay = 300_000_000
    }

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data NtpSyncingStatus
  = NtpSyncingStatusUnavailable
  | NtpSyncingStatusPending
  | NtpSyncingStatusAvailable
  deriving (Eq, Generic, Show)
  deriving anyclass (NFData)

data NtpStatusWithOffset = NtpStatusWithOffset
  { status :: !NtpSyncingStatus
  , offset :: !(Maybe (Quantity "microsecond" Integer))
  }
  deriving (Eq, Generic, Show)
  deriving anyclass (NFData)

--------------------------------------------------------------------------------
-- Printing and Logging
--------------------------------------------------------------------------------

-- TODO: Move this upstream.
prettyNtpStatus :: NtpStatus -> Text
prettyNtpStatus = \case
  NtpDrift o -> "drifting by " <> prettyNtpOffset o
  NtpSyncPending -> "pending"
  NtpSyncUnavailable -> "unavailable"

-- Using 'Integral' here because 'NtpOffset' is not exposed :/
--
-- TODO: Move this upstream.
prettyNtpOffset :: Integral a => a -> Text
prettyNtpOffset n =
  T.pack (show $ fromIntegral @_ @Integer n) <> "μs"

-- TODO: Move this upstream
prettyResultOrFailure :: (a -> Text) -> ResultOrFailure a -> Text
prettyResultOrFailure prettyA = \case
  BothSucceeded a ->
    prettyA a
  SuccessAndFailure a ip e ->
    "succeeded and failed with " <> prettyA a <> ", " <> T.pack (show (ip, e))
  BothFailed e0 e1 ->
    "failed with " <> T.pack (show e0) <> ", " <> T.pack (show e1)

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
      "local clock is " <> prettyNtpStatus a
    NtpTraceRunProtocolResults a ->
      "ntp client run protocol results: "
        <> prettyResultOrFailure (T.intercalate ", " . map prettyNtpOffset) a
    NtpTracePacketSent _ a ->
      "ntp client sent packet when running " <> toText (show a)
    NtpTracePacketSendError _ e ->
      "ntp client experienced error "
        <> toText (show e)
        <> " when sending packet"
    NtpTracePacketDecodeError _ e ->
      "ntp client experienced error "
        <> toText (show e)
        <> " when decoding packet"
    NtpTracePacketReceived _ a ->
      "ntp client received packet: " <> toText (show a)
    NtpTraceWaitingForRepliesTimeout v ->
      "ntp client experienced timeout using " <> toText v <> " protocol"

instance HasPrivacyAnnotation NtpTrace

instance HasSeverityAnnotation NtpTrace where
  getSeverityAnnotation ev = case ev of
    NtpTraceStartNtpClient -> Debug
    NtpTraceRestartDelay _ -> Debug
    NtpTraceRestartingClient -> Debug
    NtpTraceIOError _ -> Notice
    NtpTraceLookupsFails -> Notice
    NtpTraceClientStartQuery -> Debug
    NtpTraceNoLocalAddr -> Notice
    NtpTraceResult (NtpDrift micro)
      | abs micro < (500 * ms) -> Debug -- Not sure what limits actually
      | abs micro < (1_000 * ms) -> Notice -- matter, but these seem
      | otherwise -> Warning -- reasonable.
    NtpTraceResult _ -> Debug
    NtpTraceRunProtocolResults _ -> Debug
    NtpTracePacketSent _ _ -> Debug
    NtpTracePacketSendError _ _ -> Notice
    NtpTracePacketDecodeError _ _ -> Notice
    NtpTracePacketReceived _ _ -> Debug
    NtpTraceWaitingForRepliesTimeout _ -> Notice
    where
      ms = 1_000

data ForceCheck = ForceBlockingRequest | CanUseCachedResults

getNtpStatus :: NtpClient -> ForceCheck -> IO NtpStatusWithOffset
getNtpStatus client forceCheck =
  toStatus <$> case forceCheck of
    ForceBlockingRequest ->
      -- Forces an NTP check / query on the central servers, use with care
      ntpQueryBlocking client
    CanUseCachedResults -> atomically $ do
      -- Reads a cached NTP status from an STM.TVar so we don't get
      -- blacklisted by the central NTP "authorities" for sending
      -- too many NTP requests.
      s <- ntpGetStatus client
      checkSTM (s /= NtpSyncPending)
      pure s
  where
    toStatus = \case
      NtpSyncPending ->
        NtpStatusWithOffset NtpSyncingStatusPending Nothing
      NtpSyncUnavailable ->
        NtpStatusWithOffset NtpSyncingStatusUnavailable Nothing
      NtpDrift ms ->
        NtpStatusWithOffset
          NtpSyncingStatusAvailable
          (Just $ Quantity (fromIntegral ms :: Integer))
