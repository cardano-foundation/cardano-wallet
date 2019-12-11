-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Provides functions for setting up and capturing logging so that expectations
-- about logging can be asserted in test scenarios.

module Test.Utils.Trace
     ( withLogging
     , captureLogging
     ) where

import Prelude

import Cardano.BM.Data.LogItem
    ( LOContent (..), LogObject (..) )
import Cardano.BM.Trace
    ( Trace, traceInTVarIO )
import Control.Concurrent.STM.TVar
    ( newTVarIO, readTVarIO )
import Data.Maybe
    ( mapMaybe )

-- | Run an action with a logging 'Trace' object, and a function to get all
-- messages that have been traced.
withLogging :: ((Trace IO msg, IO [msg]) -> IO a) -> IO a
withLogging action = do
    tvar <- newTVarIO []
    let getMsgs = reverse . mapMaybe getLogMessage <$> readTVarIO tvar
    action (traceInTVarIO tvar, getMsgs)

-- | Run an action with a 'Trace', returning captured log messages along with
-- the result of the action.
captureLogging :: (Trace IO msg -> IO a) -> IO ([msg], a)
captureLogging action = withLogging $ \(tr, getMsgs) -> do
    res <- action tr
    msgs <- getMsgs
    pure (msgs, res)

getLogMessage :: LogObject msg -> Maybe msg
getLogMessage lo = case loContent lo of
    LogMessage msg -> Just msg
    _ -> Nothing
