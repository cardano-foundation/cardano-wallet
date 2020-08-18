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
     , countMsg
     ) where

import Prelude

import Cardano.BM.Trace
    ( traceInTVarIO )
import Control.Concurrent.STM.TVar
    ( newTVarIO, readTVarIO )
import Control.Tracer
    ( Tracer )
import Data.Generics.Internal.VL.Prism
    ( Prism', (^?) )
import Data.Maybe
    ( isJust )

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

-- | Count elements in the list matching the given Prism. Handy for counting log
-- messages which are typically constructed as sum types with many constructors.
--
-- A Prism look scary but can be obtained very easily if the target type is
-- deriving 'Generic'. From there, use
--
-- `Data.Generics.Sum.Constructor#_Ctor` from `generic-lens`.
--
-- __Example:__
--
-- >>> data MySumType = MyConstructor | MyOtherConstructor deriving Generic
--
-- >>> xs = [ MyConstructor, MyOtherConstructor, MyConstructor ]
--
-- >>> count (_Ctor @"MyConstructor") xs
-- 2
countMsg :: Prism' s a -> [s] -> Int
countMsg prism = length . filter (\x -> isJust (x ^? prism))
