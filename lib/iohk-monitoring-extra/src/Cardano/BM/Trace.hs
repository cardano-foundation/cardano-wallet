-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- A concrete 'Trace' with context naming and logging functions.
module Cardano.BM.Trace
    ( Trace
    , nullTracer
    , traceInTVar
    , traceInTVarIO

      -- * context naming
    , appendName

      -- * log functions
    , traceNamedObject
    , traceNamedItem
    , logAlert
    , logCritical
    , logDebug
    , logEmergency
    , logError
    , logInfo
    , logNotice
    , logWarning
    ) where

import Cardano.BM.Data.LogItem
    ( LOContent (LogMessage)
    , LOMeta
    , LogObject (..)
    , LoggerName
    , PrivacyAnnotation (..)
    , mkLOMeta
    )
import Cardano.BM.Data.Severity
    ( Severity (..)
    )
import Cardano.BM.Data.Tracer
    ( mkTracer
    )
import Control.Monad.IO.Class
    ( MonadIO
    , liftIO
    )
import Control.Tracer
    ( Tracer (..)
    , contramap
    , nullTracer
    , traceWith
    )
import Prelude

import qualified Control.Concurrent.STM as STM
import qualified Data.Text as T

-- | A 'Trace' carries a context name together with a 'LogObject'.
type Trace m a = Tracer m (LoggerName, LogObject a)

-- | Enter new named context. A new context name is prepended.
appendName :: LoggerName -> Trace m a -> Trace m a
appendName name tr = mkTracer $ \(names0, lo) ->
    let names = if names0 == T.empty then name else name <> "." <> names0
    in  traceWith tr (names, lo)

-- | Trace into a 'STM.TVar' (STM).
traceInTVar :: STM.TVar [a] -> Tracer STM.STM a
traceInTVar tvar = mkTracer $ \a -> STM.modifyTVar tvar (a :)

-- | Trace into a 'STM.TVar' from 'IO'.
traceInTVarIO :: STM.TVar [a] -> Tracer IO a
traceInTVarIO tvar = mkTracer $ \a ->
    STM.atomically $ STM.modifyTVar tvar (a :)

-- | Contramap a trace and produce the naming context.
named
    :: Tracer m (LoggerName, LogObject a) -> Tracer m (LOMeta, LOContent a)
named = contramap $ \(meta, loc) -> (mempty, LogObject mempty meta loc)

-- | Trace a ('LOMeta', 'LOContent') pair through the trace.
traceNamedObject
    :: Trace m a
    -> (LOMeta, LOContent a)
    -> m ()
traceNamedObject logTrace lo =
    traceWith (named logTrace) lo

-- | Create a 'LogObject' and enter it into the trace.
traceNamedItem
    :: MonadIO m
    => Trace m a
    -> PrivacyAnnotation
    -> Severity
    -> a
    -> m ()
traceNamedItem logTrace p s m =
    traceNamedObject logTrace
        =<< (,) <$> liftIO (mkLOMeta s p) <*> pure (LogMessage m)

logDebug
    , logInfo
    , logNotice
    , logWarning
    , logError
    , logCritical
    , logAlert
    , logEmergency
        :: MonadIO m => Trace m a -> a -> m ()
logDebug logTrace = traceNamedItem logTrace Public Debug
logInfo logTrace = traceNamedItem logTrace Public Info
logNotice logTrace = traceNamedItem logTrace Public Notice
logWarning logTrace = traceNamedItem logTrace Public Warning
logError logTrace = traceNamedItem logTrace Public Error
logCritical logTrace = traceNamedItem logTrace Public Critical
logAlert logTrace = traceNamedItem logTrace Public Alert
logEmergency logTrace = traceNamedItem logTrace Public Emergency
