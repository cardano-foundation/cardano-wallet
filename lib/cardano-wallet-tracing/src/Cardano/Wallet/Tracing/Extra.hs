{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module contains utility functions for logging and mapping trace data.
module Cardano.Wallet.Tracing.Extra
    ( -- * Formatting typed messages as plain text
      transformTextTrace
    , stdoutTextTracer

      -- * Wrapping typed messages as log objects
    , trMessage
    , trMessageText

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

import Cardano.Wallet.Tracing.Data.LogItem
    ( LOContent (..)
    , LogObject (..)
    , LoggerName
    , mkLOMeta
    )
import Cardano.Wallet.Tracing.Data.Severity
    ( Severity (..)
    )
import Cardano.Wallet.Tracing.Data.Tracer
    ( HasPrivacyAnnotation (..)
    , HasSeverityAnnotation (..)
    , mkTracer
    )
import Cardano.Wallet.Tracing.Trace
    ( Trace
    )
import Control.DeepSeq
    ( NFData (..)
    )
import Control.Monad
    ( when
    )
import Control.Monad.IO.Unlift
    ( MonadIO (..)
    , MonadUnliftIO
    )
import Control.Monad.Trans.Except
    ( ExceptT (..)
    , runExceptT
    )
import Control.Tracer
    ( Tracer (..)
    , contramap
    , nullTracer
    , traceWith
    )
import Data.Aeson
    ( ToJSON (..)
    , Value (Null)
    , object
    , (.=)
    )
import Data.Foldable
    ( forM_
    )
import Data.Functor
    ( ($>)
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText (..)
    )
import Data.Time.Clock
    ( DiffTime
    )
import Data.Time.Clock.System
    ( getSystemTime
    , systemToTAITime
    )
import Data.Time.Clock.TAI
    ( diffAbsoluteTime
    )
import Fmt
    ( Buildable (..)
    , Builder
    , blockListF
    , blockMapF
    , nameF
    )
import GHC.Exts
    ( IsList (..)
    )
import GHC.Generics
    ( Generic
    )
import UnliftIO
    ( atomically
    , newTVarIO
    , readTVar
    , writeTVar
    )
import UnliftIO.Exception
    ( Exception (..)
    , SomeException (..)
    , displayException
    , isSyncException
    , withException
    )
import Prelude

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
trMessageText tr = mkTracer $ \arg -> do
    let msg = toText arg
        tracer = if msg == mempty then nullTracer else tr
    meta <-
        mkLOMeta (getSeverityAnnotation arg) (getPrivacyAnnotation arg)
    traceWith tracer (mempty, LogObject mempty meta (LogMessage msg))

-- | Tracer transformer which converts 'Trace m a' to 'Tracer m a' by wrapping
-- typed log messages into a 'LogObject'.
trMessage
    :: (MonadIO m, HasPrivacyAnnotation a, HasSeverityAnnotation a)
    => Tracer m (LoggerName, LogObject a)
    -> Tracer m a
trMessage tr = mkTracer $ \arg -> do
    meta <-
        mkLOMeta (getSeverityAnnotation arg) (getPrivacyAnnotation arg)
    traceWith tr (mempty, LogObject mempty meta (LogMessage arg))

-- | Trace transformer which removes empty traces.
filterNonEmpty
    :: forall m a
     . (Monad m, Monoid a, Eq a)
    => Trace m a
    -> Trace m a
filterNonEmpty tr = mkTracer $ \arg -> do
    when
        (nonEmptyMessage $ loContent $ snd arg)
        $ traceWith tr arg
  where
    nonEmptyMessage (LogMessage msg) = msg /= mempty
    nonEmptyMessage _ = True

-- | Creates a tracer that prints any 'ToText' log message. This is useful for
-- debugging functions in the REPL, when you need a 'Tracer' object.
stdoutTextTracer :: (MonadIO m, ToText a) => Tracer m a
stdoutTextTracer = mkTracer $ liftIO . B8.putStrLn . T.encodeUtf8 . toText

{-------------------------------------------------------------------------------
                                Logging helpers
-------------------------------------------------------------------------------}

-- | Run an 'ExceptT' action, then trace its result, all in one step.
-- This is a more basic version of 'resultTracer'.
traceWithExceptT
    :: Monad m => Tracer m (Either e a) -> ExceptT e m a -> ExceptT e m a
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
formatResultMsgWith err fmt title params b =
    nameF (build title)
        $ blockListF
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
exceptionMsg e =
    if isSyncException e
        then BracketException $ LoggedException e
        else BracketAsyncException $ LoggedException e

{-------------------------------------------------------------------------------
                                Bracketed logging
-------------------------------------------------------------------------------}

-- | Used for tracing around an action.
data BracketLog' r
    = -- | Logged before the action starts.
      BracketStart
    | -- | Logged after the action finishes.
      BracketFinish r
    | -- | Logged when the action throws an exception.
      BracketException (LoggedException SomeException)
    | -- | Logged when the action receives an async exception.
      BracketAsyncException (LoggedException SomeException)
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

-- | Replacement for the former @mkOutcomeExtractor@-based timings. This
-- produces a timings 'Tracer' from a 'Tracer' of messages @a@, and a function
-- which can extract the 'BracketLog' from @a@.
--
-- The extractor function can provide @ctx@, which could be the name of the
-- timed operation for example.
--
-- The produced tracer will make just one trace for each finished bracket.
-- It contains the @ctx@ from the extractor and the time difference.
--
-- The timing uses TAI time so that measurements are correct at times when
-- leap seconds are applied, matching the previous @tracer-transformers@
-- implementation.
--
-- The pending start is held in a /single/ slot, which is what
-- @Control.Tracer.Transformers.ObserveOutcome.mkOutcomeExtractor@ did with its
-- @MVar (Maybe (IntermediateValue a))@. That gives the two edge cases their
-- former semantics:
--
--   * a second 'BracketStart' replaces the pending one (upstream emitted
--     @StartsBeforeEnds@, which @fiddleOutcome@ discarded), so the state stays
--     O(1) when starts outnumber ends;
--   * a finish with no pending start emits nothing (upstream emitted
--     @EndsBeforeStarted@, likewise discarded), rather than pairing the finish
--     with an unrelated older start.
produceTimings
    :: forall m ctx a
     . MonadUnliftIO m
    => (a -> Maybe (ctx, BracketLog))
    -- ^ Function to extract BracketLog messages from @a@, paired with context.
    -> Tracer m (ctx, DiffTime)
    -- ^ The timings tracer, has time deltas for each finished bracket.
    -> m (Tracer m a)
produceTimings f trDiffTime = do
    openTime <- liftIO $ newTVarIO Nothing
    pure $ mkTracer $ \arg ->
        forM_ (f arg) $ \(ctx, blog) -> do
            now <- liftIO $ systemToTAITime <$> getSystemTime
            case blog of
                BracketStart ->
                    atomically $ writeTVar openTime (Just now)
                _ -> do
                    mStart <- atomically $ do
                        mt <- readTVar openTime
                        writeTVar openTime Nothing
                        pure mt
                    forM_ mStart
                        $ \start ->
                            traceWith trDiffTime (ctx, diffAbsoluteTime now start)

{-------------------------------------------------------------------------------
                               Tracer conversions
-------------------------------------------------------------------------------}

-- | Conditional mapping of a 'Tracer'.
flatContramapTracer
    :: Monad m
    => (a -> Maybe b)
    -> Tracer m b
    -> Tracer m a
flatContramapTracer p tr = mkTracer $ \a -> forM_ (p a) (runTracer tr)
