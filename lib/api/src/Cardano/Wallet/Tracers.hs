{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Tracing functionality for the Shelley wallet
--
module Cardano.Wallet.Tracers
    ( Tracers' (..)
    , Tracers
    , TracerSeverities
    , tracerLabels
    , tracerDescriptions
    , setupTracers
    , tracerSeverities
    , nullTracers
    ) where

import Prelude

import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation
    , HasSeverityAnnotation
    , filterSeverity
    )
import Cardano.BM.Extra
    ( trMessageText
    )
import Cardano.BM.Tracing
    ( Severity (..)
    , Trace
    , Tracer
    , appendName
    , nullTracer
    )
import Cardano.Pool.DB.Log
    ( PoolDbLog
    )
import Cardano.Wallet.Api.Http.Logging
    ( ApplicationLog
    )
import Cardano.Wallet.Api.Http.Shelley.Server
    ( WalletEngineLog
    )
import Cardano.Wallet.DB.Layer
    ( DBFactoryLog
    )
import Cardano.Wallet.Pools
    ( StakePoolLog
    )
import Cardano.Wallet.Shelley.Network
    ( NetworkLayerLog
    )
import Cardano.Wallet.TokenMetadata
    ( TokenMetadataLog
    )
import Control.Applicative
    ( Const (..)
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText
    )
import Network.NTP.Client
    ( NtpTrace
    )
import Network.Wai.Middleware.Logging
    ( ApiLog
    )

import qualified Data.Text as T

-- | The types of trace events produced by the Shelley API server.
data Tracers' f = Tracers
    { applicationTracer   :: f ApplicationLog
    , apiServerTracer     :: f ApiLog
    , tokenMetadataTracer :: f TokenMetadataLog
    , walletEngineTracer  :: f WalletEngineLog
    , walletDbTracer      :: f DBFactoryLog
    , poolsEngineTracer   :: f StakePoolLog
    , poolsDbTracer       :: f PoolDbLog
    , ntpClientTracer     :: f NtpTrace
    , networkTracer       :: f NetworkLayerLog
    }

-- | All of the Shelley 'Tracer's.
type Tracers m = Tracers' (Tracer m)

-- | The minimum severities for 'Tracers'. 'Nothing' indicates that tracing is
-- completely disabled.
type TracerSeverities = Tracers' (Const (Maybe Severity))

deriving instance Show TracerSeverities
deriving instance Eq TracerSeverities

-- | Construct a 'TracerSeverities' record with all tracers set to the given
-- severity.
tracerSeverities :: Maybe Severity -> TracerSeverities
tracerSeverities sev = Tracers
    { applicationTracer   = Const sev
    , apiServerTracer     = Const sev
    , tokenMetadataTracer = Const sev
    , walletDbTracer      = Const sev
    , walletEngineTracer  = Const sev
    , poolsEngineTracer   = Const sev
    , poolsDbTracer       = Const sev
    , ntpClientTracer     = Const sev
    , networkTracer       = Const sev
    }

-- | Set up tracing with textual log messages.
setupTracers ::
    TracerSeverities ->
    Trace IO Text ->
    Tracers IO
setupTracers sev tr =
    Tracers
        { applicationTracer =
            onoff applicationTracer $ mkTrace applicationTracer tr
        , apiServerTracer =
            onoff apiServerTracer $ mkTrace apiServerTracer tr
        , tokenMetadataTracer =
            onoff tokenMetadataTracer $ mkTrace tokenMetadataTracer tr
        , walletEngineTracer =
            onoff walletEngineTracer $ mkTrace walletEngineTracer tr
        , walletDbTracer =
            onoff walletDbTracer $ mkTrace walletDbTracer tr
        , poolsEngineTracer =
            onoff poolsEngineTracer $ mkTrace poolsEngineTracer tr
        , poolsDbTracer =
            onoff poolsDbTracer $ mkTrace poolsDbTracer tr
        , ntpClientTracer =
            onoff ntpClientTracer $ mkTrace ntpClientTracer tr
        , networkTracer =
            onoff networkTracer $ mkTrace networkTracer tr
        }
  where
    onoff
        :: forall m a b. (Monad m, HasSeverityAnnotation b)
        => (TracerSeverities -> Const (Maybe Severity) a)
        -> Tracer m b
        -> Tracer m b
    onoff f = case getConst (f sev) of
        Nothing -> const nullTracer
        Just s -> filterSeverity (const $ pure s)

    mkTrace
        :: (HasPrivacyAnnotation a, HasSeverityAnnotation a, ToText a)
        => (Tracers' (Const Text) -> Const Text a)
        -> Trace IO Text
        -> Tracer IO a
    mkTrace f =
        trMessageText . appendName (getConst $ f tracerLabels)

-- | Strings that the user can refer to tracers by.
tracerLabels :: Tracers' (Const Text)
tracerLabels = Tracers
    { applicationTracer   = Const "application"
    , apiServerTracer     = Const "api-server"
    , tokenMetadataTracer = Const "token-metadata"
    , walletEngineTracer  = Const "wallet-engine"
    , walletDbTracer      = Const "wallet-db"
    , poolsEngineTracer   = Const "pools-engine"
    , poolsDbTracer       = Const "pools-db"
    , ntpClientTracer     = Const "ntp-client"
    , networkTracer       = Const "network"
    }

-- | Names and descriptions of the tracers, for user documentation.
tracerDescriptions :: [(String, String)]
tracerDescriptions =
    [ ( lbl applicationTracer
      , "About start-up logic and the server's surroundings."
      )
    , ( lbl apiServerTracer
      , "About the HTTP API requests and responses."
      )
    , ( lbl walletEngineTracer
      , "About background wallet workers events and core wallet engine."
      )
    , ( lbl walletDbTracer
      , "About database operations of each wallet."
      )
    , ( lbl tokenMetadataTracer
      , "About the fetching of token metadata."
      )
    , ( lbl poolsEngineTracer
      , "About the background worker monitoring stake pools and stake pools engine."
      )
    , ( lbl poolsDbTracer
      , "About database operations on stake pools."
      )
    , ( lbl ntpClientTracer
      , "About ntp-client."
      )
    , ( lbl networkTracer
      , "About network communication with the node."
      )
    ]
  where
    lbl f = T.unpack . getConst . f $ tracerLabels

-- | Use a 'nullTracer' for each of the 'Tracer's in 'Tracers'
nullTracers :: Monad m => Tracers m
nullTracers = Tracers
    { applicationTracer   = nullTracer
    , apiServerTracer     = nullTracer
    , tokenMetadataTracer = nullTracer
    , walletEngineTracer  = nullTracer
    , walletDbTracer      = nullTracer
    , poolsEngineTracer   = nullTracer
    , poolsDbTracer       = nullTracer
    , ntpClientTracer     = nullTracer
    , networkTracer       = nullTracer
    }
