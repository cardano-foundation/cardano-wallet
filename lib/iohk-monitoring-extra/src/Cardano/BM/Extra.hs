{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module contains utility functions for logging and mapping trace data.

module Cardano.BM.Extra
    ( -- * Conversions from BM framework
      trMessage
    , trMessageText

      -- * Formatting typed messages as plain text
    , transformTextTrace
    , stdoutTextTracer

      -- * Logging helpers
    , traceWithExceptT
    , traceResult
    , formatResultMsg
    , formatResultMsgWith
    , resultSeverity

      -- * Logging and timing IO actions
    , BracketLog
    , BracketLog' (..)
    , LoggedException (..)
    , bracketTracer
    , bracketTracer'
    , produceTimings

      -- * Tracer conversions
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
    ( Trace )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( when )
import Control.Monad.Catch
    ( MonadMask )
import Control.Monad.IO.Unlift
    ( MonadIO (..), MonadUnliftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT )
import Control.Tracer
    ( Tracer (..), contramap, nullTracer, traceWith )
import Control.Tracer.Transformers.ObserveOutcome
    ( Outcome (..)
    , OutcomeFidelity (..)
    , OutcomeProgressionStatus (..)
    , mkOutcomeExtractor
    )
import Data.Aeson
    ( ToJSON (..), Value (Null), object, (.=) )
import Data.Foldable
    ( forM_ )
import Data.Functor
    ( ($>) )
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
import Fmt
    ( Buildable (..), Builder, blockListF, blockMapF, nameF )
import GHC.Exts
    ( IsList (..) )
import GHC.Generics
    ( Generic )
import UnliftIO.Exception
    ( Exception (..)
    , SomeException (..)
    , displayException
    , isSyncException
    , withException
    )

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as T

-- | Converts a 'Text' trace into any other type of trace that has a 'ToText'
-- instance.
transformTextTrace :: ToText a => Trace IO Text -> Trace IO a
transformTextTrace = contramap (fmap . fmap $ toText) . filterNonEmpty

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
-- This is a more basic version of 'resultTracer'.
traceWithExceptT :: Monad m => Tracer m (Either e a) -> ExceptT e m a -> ExceptT e m a
traceWithExceptT tr (ExceptT action) = ExceptT $ do
    res <- action
    traceWith tr res
    pure res

-- | Log around an 'ExceptT' action. The result of the action is captured as an
-- 'Either' in the log message. Other unexpected exceptions are captured in the
-- 'BracketLog''.
traceResult
    :: MonadUnliftIO m
    => Tracer m (BracketLog' (Either e r))
    -> ExceptT e m r
    -> ExceptT e m r
traceResult tr = ExceptT . bracketTracer' id tr . runExceptT

-- | Format a tracer message from 'traceResult' as multiline text.
formatResultMsg
    :: (Show e, IsList t, Item t ~ (Text, v), Buildable v, Buildable r)
    => Text
    -- ^ Function name.
    -> t
    -- ^ Input parameters.
    -> BracketLog' (Either e r)
    -- ^ Logging around function.
    -> Builder
formatResultMsg = formatResultMsgWith (nameF "ERROR" . build . show) build

-- | Same as 'formatResultMsg', but accepts result formatters as parameters.
formatResultMsgWith
    :: (IsList t, Item t ~ (Text, v), Buildable v)
    => (e -> Builder)
    -- ^ Error message formatter
    -> (r -> Builder)
    -- ^ Result formatter
    -> Text
    -- ^ Function name.
    -> t
    -- ^ Input parameters.
    -> BracketLog' (Either e r)
    -- ^ Logging around function.
    -> Builder
formatResultMsgWith err fmt title params b = nameF (build title) $ blockListF
    [ nameF "inputs" (blockMapF params)
    , buildBracketLog (either err fmt) b
    ]

-- | A good default mapping of message severities for 'traceResult'.
resultSeverity :: Severity -> BracketLog' (Either e r) -> Severity
resultSeverity base = \case
    BracketStart -> base
    BracketFinish (Left _) -> Error
    BracketFinish (Right _) -> base
    BracketException _ -> Error
    BracketAsyncException _ -> base

{-------------------------------------------------------------------------------
                             Logging of Exceptions
-------------------------------------------------------------------------------}

-- | Exception wrapper with typeclass instances that exception types often don't
-- have.
newtype LoggedException e = LoggedException e
    deriving (Generic, Show, Ord)

instance NFData e => NFData (LoggedException e)

instance NFData (LoggedException SomeException) where
    rnf (LoggedException e) = rnf (show e)

instance Exception e => ToText (LoggedException e)

instance Exception e => Buildable (LoggedException e) where
    build (LoggedException e) = build $ displayException e

instance Show e => Eq (LoggedException e) where
    a == b = show a == show b

instance Exception e => ToJSON (LoggedException e) where
    toJSON e = object ["exception" .= toText e]

exceptionMsg :: SomeException -> (BracketLog' r)
exceptionMsg e = if isSyncException e
    then BracketException $ LoggedException e
    else BracketAsyncException $ LoggedException e

{-------------------------------------------------------------------------------
                                Bracketed logging
-------------------------------------------------------------------------------}

-- | Used for tracing around an action.
data BracketLog' r
    = BracketStart
    -- ^ Logged before the action starts.
    | BracketFinish r
    -- ^ Logged after the action finishes.
    | BracketException (LoggedException SomeException)
    -- ^ Logged when the action throws an exception.
    | BracketAsyncException (LoggedException SomeException)
    -- ^ Logged when the action receives an async exception.
    deriving (Generic, Show, Eq, ToJSON, Functor)

instance Buildable r => ToText (BracketLog' r)

instance Buildable r => Buildable (BracketLog' r) where
    build = buildBracketLog build

buildBracketLog :: (t -> Builder) -> BracketLog' t -> Builder
buildBracketLog toBuilder = \case
    BracketStart -> "start"
    BracketFinish (toBuilder -> r)
        | r == mempty -> "finish"
        | otherwise -> "finish: " <> r
    BracketException e -> "exception: " <> build e
    BracketAsyncException e -> "cancelled: " <> build e

instance HasPrivacyAnnotation (BracketLog' r)
instance HasSeverityAnnotation (BracketLog' r) where
    -- | Default severities for 'BracketLog' - the enclosing log message may of
    -- course use different values.
    getSeverityAnnotation = \case
        BracketStart -> Debug
        BracketFinish _ -> Debug
        BracketException _ -> Error
        BracketAsyncException _ -> Debug

-- | Placeholder for some unspecified result value in 'BracketLog' - it could be
-- @()@, or anything else.
data SomeResult = SomeResult deriving (Generic, Show, Eq)

instance Buildable SomeResult where
    build SomeResult = mempty

instance ToJSON SomeResult where
    toJSON SomeResult = Null

-- | Trace around an action, where the result doesn't matter.
type BracketLog = BracketLog' SomeResult

-- | Run a monadic action with 'BracketLog' traced around it.
bracketTracer :: MonadUnliftIO m => Tracer m BracketLog -> m a -> m a
bracketTracer = bracketTracer'' id (const SomeResult)

-- | Run a monadic action with 'BracketLog' traced around it.
bracketTracer'
    :: MonadUnliftIO m
    => (r -> a)
    -- ^ Transform value into log message.
    -> Tracer m (BracketLog' a)
    -- ^ Tracer.
    -> m r
    -- ^ Action.
    -> m r
bracketTracer' = bracketTracer'' id

-- | Run a monadic action with 'BracketLog' traced around it.
bracketTracer''
    :: MonadUnliftIO m
    => (b -> r)
    -- ^ Transform value into result.
    -> (b -> a)
    -- ^ Transform value into log message.
    -> Tracer m (BracketLog' a)
    -- ^ Tracer.
    -> m b
    -- ^ Action to produce value.
    -> m r
bracketTracer'' res msg tr action = do
    traceWith tr BracketStart
    withException
        (action >>= \val -> traceWith tr (BracketFinish (msg val)) $> res val)
        (traceWith tr . exceptionMsg)

instance MonadIO m => Outcome m (BracketLog' r) where
  type IntermediateValue (BracketLog' r) = AbsoluteTime
  type OutcomeMetric (BracketLog' r)     = DiffTime

  classifyObservable = pure . \case
      BracketStart -> OutcomeStarts
      BracketFinish _ -> OutcomeEnds
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

{-------------------------------------------------------------------------------
                               Tracer conversions
-------------------------------------------------------------------------------}
-- | Conditional mapping of a 'Tracer'.
flatContramapTracer
    :: Monad m
    => (a -> Maybe b)
    -> Tracer m b
    -> Tracer m a
flatContramapTracer p tr = Tracer $ \a -> forM_ (p a) (runTracer tr)
