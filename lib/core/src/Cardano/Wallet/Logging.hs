{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module contains utility functions for logging and mapping trace data.

module Cardano.Wallet.Logging
    ( transformTextTrace
    , logTrace
    , fromLogObject
    , trMessage
    , trMessageText
    , filterTraceSeverity
    , stdoutTextTracer
    ) where

import Prelude

import Cardano.BM.Data.LogItem
    ( LOContent (..), LOMeta (..), LogObject (..), mkLOMeta )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( DefinePrivacyAnnotation (..), DefineSeverity (..), Transformable (..) )
import Cardano.BM.Trace
    ( Trace, traceNamedItem )
import Control.Monad
    ( when )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Tracer
    ( Tracer (..), contramap, nullTracer, traceWith )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )

import qualified Data.Text.IO as T

-- | Converts a 'Text' trace into any other type of trace that has a 'ToText'
-- instance.
transformTextTrace :: ToText a => Trace IO Text -> Trace IO a
transformTextTrace = contramap (fmap toText) . filterNonEmpty

-- | Traces some data.
logTrace
    :: (MonadIO m, DefinePrivacyAnnotation a, DefineSeverity a)
    => Trace m a
    -> a
    -> m ()
logTrace tr msg = traceNamedItem tr priv sev msg
  where
    priv = definePrivacyAnnotation msg
    sev = defineSeverity msg

-- | Strips out the data that 'trMessage' (or
-- "Cardano.BM.Data.Tracer.toLogObject") adds.
fromLogObject :: Show a => Tracer IO a -> Tracer IO (LogObject a)
fromLogObject = contramap (getLogMessage . loContent)
  where
    getLogMessage (LogMessage a) = a
    getLogMessage msg = error $
        "internal error: unable to extract log message from content\n"
        <> show msg

-- | Tracer transformer which transforms traced items to their 'ToText'
-- representation and further traces them as a 'LogObject'. If the 'ToText'
-- representation is empty, then no tracing happens.
trMessageText
    :: (MonadIO m, ToText a, DefinePrivacyAnnotation a, DefineSeverity a)
    => Tracer m (LogObject Text)
    -> Tracer m a
trMessageText tr = Tracer $ \arg -> do
   let msg = toText arg
       tracer = if msg == mempty then nullTracer else tr
   lo <- LogObject
       <$> pure mempty
       <*> (mkLOMeta (defineSeverity arg) (definePrivacyAnnotation arg))
       <*> pure (LogMessage msg)
   traceWith tracer lo

-- | Tracer transformer which converts 'Trace m a' to 'Tracer m a' by wrapping
-- typed log messages into a 'LogObject'.
trMessage
    :: (MonadIO m, DefinePrivacyAnnotation a, DefineSeverity a)
    => Tracer m (LogObject a)
    -> Tracer m a
trMessage tr = Tracer $ \arg ->
   traceWith tr =<< LogObject
       <$> pure mempty
       <*> (mkLOMeta (defineSeverity arg) (definePrivacyAnnotation arg))
       <*> pure (LogMessage arg)

instance forall m a. (MonadIO m, ToText a, DefinePrivacyAnnotation a, DefineSeverity a) => Transformable Text m a where
    trTransformer _fmt _verb = Tracer . traceWith . trMessageText

-- | Tracer transformer which removes tracing below the qgiven severity limit.
filterTraceSeverity
    :: forall m a. (Monad m)
    => Severity
    -> Trace m a
    -> Trace m a
filterTraceSeverity sevlimit tr = Tracer $ \arg -> do
    when (severity (loMeta arg) >= sevlimit) $
        traceWith tr arg

-- | Trace transformer which removes empty traces.
filterNonEmpty
    :: forall m a. (Monad m, Monoid a, Eq a)
    => Trace m a
    -> Trace m a
filterNonEmpty tr = Tracer $ \arg -> do
    when (nonEmptyMessage (loContent arg)) $
        traceWith tr arg
  where
    nonEmptyMessage (LogMessage msg) = msg /= mempty
    nonEmptyMessage _ = True

-- | Creates a tracer that prints any 'ToText' log message. This is useful for
-- debugging functions in the REPL, when you need a 'Tracer' object.
stdoutTextTracer :: (MonadIO m, ToText a) => Tracer m a
stdoutTextTracer = Tracer $ liftIO . T.putStrLn . toText
