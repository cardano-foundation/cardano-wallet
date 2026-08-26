{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Tracers, tracer transformers, and severity/privacy annotations, implemented
-- directly on "contra-tracer". This module keeps the @Cardano.Wallet.Tracing.Data.Tracer@
-- interface used across the wallet after the removal of @iohk-monitoring@.
module Cardano.Wallet.Tracing.Data.Tracer
    ( -- * Tracers
      Tracer (..)
    , mkTracer
    , contramap
    , traceWith
    , nullTracer
    , natTracer
    , stdoutTracer
    , debugTracer
    , TracingVerbosity (..)
    , ToObject (..)

      -- * Annotations
    , HasSeverityAnnotation (..)
    , HasPrivacyAnnotation (..)

      -- * Filtering
    , filterSeverity
    ) where

import Cardano.Wallet.Tracing.Data.LogItem
    ( PrivacyAnnotation (..)
    )
import Cardano.Wallet.Tracing.Data.Severity
    ( Severity (..)
    )
import Control.Monad
    ( when
    )
import Control.Tracer
    ( Tracer (..)
    , contramap
    , debugTracer
    , natTracer
    , nullTracer
    , stdoutTracer
    , traceWith
    )
import Data.Aeson
    ( FromJSON (..)
    , Object
    , ToJSON (..)
    , Value (..)
    )
import Data.Text
    ( Text
    )
import Data.Word
    ( Word64
    )
import Prelude

#if MIN_VERSION_contra_tracer(0,2,0)
import qualified Control.Tracer.Arrow as TA
#endif
import Data.Aeson.Text
    ( encodeToLazyText
    )

import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text.Lazy as TL

-- | The single constructor helper for 'Tracer' values. Every tracer
-- construction site in the wallet must route through this function so that
-- both @contra-tracer-0.1@ and @>= 0.2@ are supported from one place.
--
-- The signature is CPP-guarded alongside the body because the two arms have
-- genuinely different types: @>= 0.2@ builds through
-- 'Control.Tracer.Arrow.emit', which requires @Applicative m@, while the
-- 0.1 constructor takes the function directly and does not. A single shared
-- signature cannot satisfy both -- the constraint is required on one arm and
-- reported as redundant (and thus fatal under -Werror) on the other.
#if MIN_VERSION_contra_tracer(0,2,0)
mkTracer :: Applicative m => (a -> m ()) -> Tracer m a
mkTracer f = Tracer (TA.emit f)
#else
mkTracer :: (a -> m ()) -> Tracer m a
mkTracer f = Tracer f
#endif

-- | Verbosity levels passed to 'ToObject' renderers.
data TracingVerbosity
    = MinimalVerbosity
    | NormalVerbosity
    | MaximalVerbosity
    deriving (Eq, Read, Ord, Show)

instance FromJSON TracingVerbosity where
    parseJSON (String str) = case str of
        "MinimalVerbosity" -> pure MinimalVerbosity
        "MaximalVerbosity" -> pure MaximalVerbosity
        "NormalVerbosity" -> pure NormalVerbosity
        err ->
            fail
                $ "Parsing of TracingVerbosity failed, "
                    <> show err
                    <> " is not a valid TracingVerbosity"
    parseJSON invalid =
        fail
            $ "Parsing of TracingVerbosity failed due to type mismatch. "
                <> "Encountered: "
                <> show invalid

-- | Transformation of a traced item to a JSON 'Object'.
class ToObject a where
    toObject :: TracingVerbosity -> a -> Object
    default toObject :: ToJSON a => TracingVerbosity -> a -> Object
    toObject _ v = case toJSON v of
        Object o -> o
        s@(String _) -> KeyMap.singleton "string" s
        _ -> mempty
    textTransformer :: a -> Object -> Text
    default textTransformer :: a -> Object -> Text
    textTransformer _ o = TL.toStrict $ encodeToLazyText o

instance ToObject () where
    toObject _ _ = mempty

instance ToObject String
instance ToObject Text
instance ToObject Value

-- | Extract a 'Cardano.Wallet.Tracing.Data.Severity.Severity' annotation from a traced
-- item. The default annotation is 'Debug'.
class HasSeverityAnnotation a where
    getSeverityAnnotation :: a -> Severity
    default getSeverityAnnotation :: a -> Severity
    getSeverityAnnotation _ = Debug

-- | Extract a 'Cardano.Wallet.Tracing.Data.LogItem.PrivacyAnnotation' from a traced item.
-- The default annotation is 'Public'.
class HasPrivacyAnnotation a where
    getPrivacyAnnotation :: a -> PrivacyAnnotation
    default getPrivacyAnnotation :: a -> PrivacyAnnotation
    getPrivacyAnnotation _ = Public

instance HasSeverityAnnotation Double
instance HasSeverityAnnotation Float
instance HasSeverityAnnotation Int
instance HasSeverityAnnotation Integer
instance HasSeverityAnnotation String
instance HasSeverityAnnotation Text
instance HasSeverityAnnotation Word64

instance HasPrivacyAnnotation Double
instance HasPrivacyAnnotation Float
instance HasPrivacyAnnotation Int
instance HasPrivacyAnnotation Integer
instance HasPrivacyAnnotation String
instance HasPrivacyAnnotation Text
instance HasPrivacyAnnotation Word64

-- | Filter traced items by severity: only items at or above the given limit
-- are traced.
filterSeverity
    :: forall m a
     . (Monad m, HasSeverityAnnotation a)
    => (a -> m Severity)
    -> Tracer m a
    -> Tracer m a
filterSeverity msevlimit tr = mkTracer $ \arg -> do
    sevlimit <- msevlimit arg
    when (getSeverityAnnotation arg >= sevlimit)
        $ traceWith tr arg
