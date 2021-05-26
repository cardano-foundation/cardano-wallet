{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module contains utility functions for logging and mapping trace data.

module Cardano.Wallet.Logging
    ( -- * Conversions from BM framework
      logTrace
    , fromLogObject
    , trMessage
    , trMessageText

      -- * Formatting typed messages as plain text
    , transformTextTrace
    , stdoutTextTracer

      -- * Logging helpers
    , traceWithExceptT
    , unliftIOTracer

      -- * Logging and timing IO actions
    , BracketLog (..)
    , LoggedException (..)
    , bracketTracer
    , produceTimings

      -- * Combinators
    , flatContramapTracer
    ) where

import Prelude

import Cardano.BM.Data.LogItem
    ( LOContent (..), LogObject (..), LoggerName, mkLOMeta )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..)
    , HasSeverityAnnotation (..)
    , Transformable (..)
    )
import Cardano.BM.Trace
    ( Trace, traceNamedItem )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( when )
import Control.Monad.IO.Unlift
    ( MonadIO (..), MonadUnliftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Control.Tracer
    ( Tracer (..), contramap, natTracer, nullTracer, traceWith )
import Control.Tracer.Transformers.ObserveOutcome
    ( Outcome (..)
    , OutcomeFidelity (..)
    , OutcomeProgressionStatus (..)
    , mkOutcomeExtractor
    )
import Data.Aeson
    ( ToJSON (..), object, (.=) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Time.Clock
    ( DiffTime )
import Data.Time.Clock.System
    ( getSystemTime, systemToTAITime )
import Data.Time.Clock.TAI
    ( AbsoluteTime, diffAbsoluteTime )
import GHC.Generics
    ( Generic )
import UnliftIO.Exception
    ( Exception (..)
    , SomeException (..)
    , displayException
    , isSyncException
    , withException
    )

import Control.Monad.Catch
    ( MonadMask )
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
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

{-------------------------------------------------------------------------------
                                Logging helpers
-------------------------------------------------------------------------------}

-- | Run an 'ExceptT' action, then trace its result, all in one step.
traceWithExceptT :: Monad m => Tracer m (Either e a) -> ExceptT e m a -> ExceptT e m a
traceWithExceptT tr (ExceptT action) = ExceptT $ do
    res <- action
    traceWith tr res
    pure res

-- | Convert an IO tracer to a 'm' tracer.
unliftIOTracer :: MonadIO m => Tracer IO a -> Tracer m a
unliftIOTracer = natTracer liftIO

{-------------------------------------------------------------------------------
                                Bracketed logging
-------------------------------------------------------------------------------}

-- | Used for tracing around an action.
data BracketLog
    = BracketStart
    -- ^ Logged before the action starts.
    | BracketFinish
    -- ^ Logged after the action finishes.
    | BracketException (LoggedException SomeException)
    -- ^ Logged when the action throws an exception.
    | BracketAsyncException (LoggedException SomeException)
    -- ^ Logged when the action receives an async exception.
    deriving (Generic, Show, Eq, ToJSON)

instance ToText BracketLog where
    toText = \case
        BracketStart -> "start"
        BracketFinish -> "finish"
        BracketException e -> "exception: " <> toText e
        BracketAsyncException e -> "cancelled: " <> toText e

instance HasPrivacyAnnotation BracketLog
instance HasSeverityAnnotation BracketLog where
    -- | Default severities for 'BracketLog' - the enclosing log message may of
    -- course use different values.
    getSeverityAnnotation = \case
        BracketStart -> Debug
        BracketFinish -> Debug
        BracketException _ -> Error
        BracketAsyncException _ -> Debug

newtype LoggedException e = LoggedException e
    deriving (Generic, Show, Ord)

instance NFData e => NFData (LoggedException e)

instance NFData (LoggedException SomeException) where
    rnf (LoggedException e) = rnf (show e)

instance Exception e => ToText (LoggedException e) where
    toText (LoggedException e) = T.pack $ displayException e

instance Show e => Eq (LoggedException e) where
    a == b = show a == show b

instance Exception e => ToJSON (LoggedException e) where
    toJSON e = object ["exception" .= toText e]

exceptionMsg :: SomeException -> BracketLog
exceptionMsg e = if isSyncException e
    then BracketException $ LoggedException e
    else BracketAsyncException $ LoggedException e

-- | Run a monadic action with 'BracketLog' traced around it.
bracketTracer :: MonadUnliftIO m => Tracer m BracketLog -> m a -> m a
bracketTracer tr action = do
    traceWith tr BracketStart
    withException
        (action <* traceWith tr BracketFinish)
        (traceWith tr . exceptionMsg)

instance MonadIO m => Outcome m BracketLog where
  type IntermediateValue BracketLog = AbsoluteTime
  type OutcomeMetric BracketLog     = DiffTime

  classifyObservable = pure . \case
      BracketStart -> OutcomeStarts
      BracketFinish -> OutcomeEnds
      BracketException _ -> OutcomeEnds
      BracketAsyncException _ -> OutcomeEnds

  -- NOTE: The AbsoluteTime functions are required so that measurements are
  -- correct at times when leap seconds are applied. This is following the
  -- tracer-transformers example.
  captureObservableValue _   = systemToTAITime <$> liftIO getSystemTime
  computeOutcomeMetric _ x y = pure $ diffAbsoluteTime y x

-- Pair up bracketlogs with some context information
instance MonadIO m => Outcome m (ctx, BracketLog) where
  type IntermediateValue (ctx, BracketLog) = (ctx, IntermediateValue BracketLog)
  type OutcomeMetric (ctx, BracketLog) = (ctx, OutcomeMetric BracketLog)
  classifyObservable (_ctx, b) = classifyObservable b
  captureObservableValue (ctx, b) =
      (ctx,) <$> captureObservableValue b
  computeOutcomeMetric (ctx, b) (_, x) (_, y) =
      (ctx,) <$> computeOutcomeMetric b x y

-- | Get metric results from 'mkOutcomeExtractor' and throw away the rest.
fiddleOutcome
    :: Monad m
    => Tracer m (ctx, DiffTime)
    -> Tracer m (Either (ctx, BracketLog) (OutcomeFidelity (ctx, DiffTime)))
fiddleOutcome tr = Tracer $ \case
    Right (ProgressedNormally dt) -> runTracer tr dt
    _ -> pure ()

-- | Simplified wrapper for 'mkOutcomeExtractor'. This produces a timings
-- 'Tracer' from a 'Tracer' of messages @a@, and a function which can extract
-- the 'BracketLog' from @a@.
--
-- The extractor function can provide @ctx@, which could be the name of the
-- timed operation for example.
--
-- The produced tracer will make just one trace for each finished bracket.
-- It contains the @ctx@ from the extractor and the time difference.
produceTimings
    :: (MonadUnliftIO m, MonadMask m)
    => (a -> Maybe (ctx, BracketLog))
    -- ^ Function to extract BracketLog messages from @a@, paired with context.
    -> Tracer m (ctx, DiffTime)
    -- ^ The timings tracer, has time deltas for each finished bracket.
    -> m (Tracer m a)
produceTimings f trDiffTime = do
    extractor <- mkOutcomeExtractor
    let trOutcome = fiddleOutcome trDiffTime
        trBracket = extractor trOutcome
        tr = flatContramapTracer f trBracket
    pure tr

-- | Conditional mapping of a 'Tracer'.
flatContramapTracer
    :: Monad m
    => (a -> Maybe b)
    -> Tracer m b
    -> Tracer m a
flatContramapTracer p tr = Tracer $ \a -> case p a of
     Just b -> runTracer tr b
     Nothing -> pure ()
