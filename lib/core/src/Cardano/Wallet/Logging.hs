{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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
    , stdoutTextTracer
    , BracketLog (..)
    , bracketTracer
    ) where

import Prelude

import Cardano.BM.Data.LogItem
    ( LOContent (..), LogObject (..), LoggerName, mkLOMeta )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..)
    , HasSeverityAnnotation (..)
    , Transformable (..)
    )
import Cardano.BM.Trace
    ( Trace, traceNamedItem )
import Control.Applicative
    ( (<*) )
import Control.Monad
    ( when )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Monad.IO.Unlift
    ( MonadUnliftIO )
import Control.Tracer
    ( Tracer (..), contramap, nullTracer, traceWith )
import Data.Aeson
    ( ToJSON )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import GHC.Generics
    ( Generic )
import UnliftIO.Exception
    ( SomeException (..), isSyncException, withException )

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as T

-- | Converts a 'Text' trace into any other type of trace that has a 'ToText'
-- instance.
transformTextTrace :: ToText a => Trace IO Text -> Trace IO a
transformTextTrace = contramap (fmap . fmap $ toText) . filterNonEmpty

-- | Traces some data.
logTrace
    :: (MonadIO m, HasPrivacyAnnotation a, HasSeverityAnnotation a)
    => Trace m a
    -> a
    -> m ()
logTrace tr msg = traceNamedItem tr priv sev msg
  where
    priv = getPrivacyAnnotation msg
    sev = getSeverityAnnotation msg

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
    :: (MonadIO m, ToText a, HasPrivacyAnnotation a, HasSeverityAnnotation a)
    => Tracer m (LoggerName, LogObject Text)
    -> Tracer m a
trMessageText tr = Tracer $ \arg -> do
   let msg = toText arg
       tracer = if msg == mempty then nullTracer else tr
   meta <- mkLOMeta (getSeverityAnnotation arg) (getPrivacyAnnotation arg)
   traceWith tracer (mempty, LogObject mempty meta (LogMessage msg))

-- | Tracer transformer which converts 'Trace m a' to 'Tracer m a' by wrapping
-- typed log messages into a 'LogObject'.
trMessage
    :: (MonadIO m, HasPrivacyAnnotation a, HasSeverityAnnotation a)
    => Tracer m (LoggerName, LogObject a)
    -> Tracer m a
trMessage tr = Tracer $ \arg -> do
   meta <- mkLOMeta (getSeverityAnnotation arg) (getPrivacyAnnotation arg)
   traceWith tr (mempty, LogObject mempty meta (LogMessage arg))

instance forall m a. (MonadIO m, ToText a, HasPrivacyAnnotation a, HasSeverityAnnotation a) => Transformable Text m a where
    trTransformer _verb = Tracer . traceWith . trMessageText

-- | Trace transformer which removes empty traces.
filterNonEmpty
    :: forall m a. (Monad m, Monoid a, Eq a)
    => Trace m a
    -> Trace m a
filterNonEmpty tr = Tracer $ \arg -> do
    when (nonEmptyMessage $ loContent $ snd arg) $
        traceWith tr arg
  where
    nonEmptyMessage (LogMessage msg) = msg /= mempty
    nonEmptyMessage _ = True

-- | Creates a tracer that prints any 'ToText' log message. This is useful for
-- debugging functions in the REPL, when you need a 'Tracer' object.
stdoutTextTracer :: (MonadIO m, ToText a) => Tracer m a
stdoutTextTracer = Tracer $ liftIO . B8.putStrLn . T.encodeUtf8 . toText

-- | Used for tracing around an action.
data BracketLog
    = BracketStart
    -- ^ Logged before the action starts.
    | BracketFinish
    -- ^ Logged after the action finishes.
    | BracketException
    -- ^ Logged when the action throws an exception.
    | BracketAsyncException
    -- ^ Logged when the action receives an async exception.
    deriving (Generic, Show, Eq, ToJSON)

instance ToText BracketLog where
    toText b = case b of
        BracketStart -> "start"
        BracketFinish -> "finish"
        BracketException -> "exception"
        BracketAsyncException -> "cancelled"

exceptionMsg :: SomeException -> BracketLog
exceptionMsg e = if isSyncException e
    then BracketException
    else BracketAsyncException

-- | Run a monadic action with 'BracketLog' traced around it.
bracketTracer :: MonadUnliftIO m => Tracer m BracketLog -> m a -> m a
bracketTracer tr action = do
    traceWith tr BracketStart
    withException
        (action <* traceWith tr BracketFinish)
        (traceWith tr . exceptionMsg)
