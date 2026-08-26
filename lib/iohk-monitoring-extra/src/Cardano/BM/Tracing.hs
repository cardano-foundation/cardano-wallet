-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Common tracing interface: re-exports the core tracing surface kept by the
-- wallet after the removal of @iohk-monitoring@.
module Cardano.BM.Tracing
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

import Cardano.BM.Data.LogItem
    ( LogObject (..)
    , PrivacyAnnotation (..)
    , mkLOMeta
    )
import Cardano.BM.Data.Severity
    ( Severity (..)
    )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..)
    , HasSeverityAnnotation (..)
    , ToObject (..)
    , TracingVerbosity (..)
    )
import Cardano.BM.Trace
    ( Trace
    , appendName
    )
import Control.Tracer
    ( Tracer (..)
    , contramap
    , nullTracer
    , traceWith
    )
