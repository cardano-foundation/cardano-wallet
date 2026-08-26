-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Common tracing interface: re-exports the core tracing surface kept by the
-- wallet after the removal of @iohk-monitoring@.
module Cardano.Wallet.Tracing.Tracing
    ( Tracer (..)
    , Trace
    , LogObject (..)
    , PrivacyAnnotation (..)
    , Severity (..)
    , ToObject (..)
    , TracingVerbosity (..)
    , HasPrivacyAnnotation (..)
    , HasSeverityAnnotation (..)
    , appendName
    , contramap
    , mkLOMeta
    , nullTracer
    , traceWith
    ) where

import Cardano.Wallet.Tracing.Data.LogItem
    ( LogObject (..)
    , PrivacyAnnotation (..)
    , mkLOMeta
    )
import Cardano.Wallet.Tracing.Data.Severity
    ( Severity (..)
    )
import Cardano.Wallet.Tracing.Data.Tracer
    ( HasPrivacyAnnotation (..)
    , HasSeverityAnnotation (..)
    , ToObject (..)
    , TracingVerbosity (..)
    )
import Cardano.Wallet.Tracing.Trace
    ( Trace
    , appendName
    )
import Control.Tracer
    ( Tracer (..)
    , contramap
    , nullTracer
    , traceWith
    )
