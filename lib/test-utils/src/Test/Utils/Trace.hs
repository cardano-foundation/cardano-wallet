{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides functions for setting up and capturing logging so that expectations
-- about logging can be asserted in test scenarios.

module Test.Utils.Trace
     ( withLogging
     , captureLogging
     , traceSpec
     ) where

import Prelude

import Cardano.BM.Trace
    ( traceInTVarIO )
import Control.Tracer
    ( Tracer )
import Data.Text.Class
    ( ToText (..) )
import Say
    ( say )
import Test.Hspec
    ( HasCallStack, Spec, SpecWith, around )
import UnliftIO.Exception
    ( onException )
import UnliftIO.STM
    ( newTVarIO, readTVarIO )

-- | Run an action with a logging 'Trace' object, and a function to get all
-- messages that have been traced.
withLogging :: ((Tracer IO msg, IO [msg]) -> IO a) -> IO a
withLogging action = do
    tvar <- newTVarIO []
    let getMsgs = reverse <$> readTVarIO tvar
    action (traceInTVarIO tvar, getMsgs)

-- | Run an action with a 'Trace', returning captured log messages along with
-- the result of the action.
captureLogging :: (Tracer IO msg -> IO a) -> IO ([msg], a)
captureLogging action = withLogging $ \(tr, getMsgs) -> do
    res <- action tr
    msgs <- getMsgs
    pure (msgs, res)

-- | Provides a Tracer to the spec, which is silent, unless something goes
-- wrong. In that case, it dumps all the traces it has collected to stdout.
traceSpec :: (HasCallStack, ToText msg) => SpecWith (Tracer IO msg) -> Spec
traceSpec = around $ \spec -> withLogging $ \(tr, getMsgs) -> do
    let dumpLogs = getMsgs >>= mapM_ (say . toText)
        rule s = say ("--- Failed spec logs " <> s <> " ---")
        explain a = rule "BEGIN" *> a <* rule "END"
    spec tr `onException` explain dumpLogs
