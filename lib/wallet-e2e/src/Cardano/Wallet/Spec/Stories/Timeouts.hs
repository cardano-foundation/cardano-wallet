module Cardano.Wallet.Spec.Stories.Timeouts (timeoutsSpec) where

import Cardano.Wallet.Spec.Effect.Assert
    ( FilterOut (..), FxAssert, filterOutAssertions, runAssertError )
import Cardano.Wallet.Spec.Effect.Timeout
    ( FxTimeout, TimeoutAssertion (..), runTimeout, within )
import Cardano.Wallet.Spec.Effect.Trace
    ( FxTrace, runTracePure )
import Cardano.Wallet.Spec.Stories.Language
    ( FxStory )
import Control.Concurrent
    ( threadDelay )
import Data.Time.TimeSpan
    ( seconds )
import Effectful
    ( Eff, IOE, runEff )
import Effectful.State.Static.Local
    ( State, evalState, get, modify )
import Prelude hiding
    ( State, evalState, get, modify )
import Test.Syd
    ( Spec, describe, expectationFailure, it )

import qualified Effectful.Error.Static as E

-- | This story will not timeout as it takes less than 1 second.
noTimeout :: FxStory fxs '[FxTimeout, IOE] ()
noTimeout = do
    within (seconds 1) do
        liftIO $ threadDelay $ 1000000 - 10
        pure ()

-- | This story will timeout as it takes longer than 1 second.
timeoutFor10Milliseconds :: FxStory fxs '[FxTimeout, IOE] ()
timeoutFor10Milliseconds = do
    within (seconds 1) do
        liftIO $ threadDelay 1010000

-- | This story will timeout once, but then if retried will succeed.
timeoutFor10MillisecondsOnce :: FxStory fxs '[State Int, FxTimeout, IOE] ()
timeoutFor10MillisecondsOnce = do
    modify @Int (+ 10)
    delay <- get @Int
    within (seconds 1) do
        liftIO $ threadDelay $ 1000 * (1010 - delay)

type Story a =
    Eff
        [ FxTimeout
        , FxAssert
        , FxTrace
        , E.Error SomeException
        , IOE
        ]
        a

run :: [FilterOut] -> Story a -> IO (Either SomeException (a, Seq Text))
run fs x =
    x
        & runTimeout
        & filterOutAssertions fs
        & runAssertError
        & runTracePure
        & E.runErrorNoCallStack
        & runEff

timeoutsSpec :: Spec
timeoutsSpec = describe "Timeouts" do
    it "no timeout" do
        r <- run [] noTimeout
        case r of
            Left _ -> expectationFailure "should not timeout"
            Right _ -> pure ()
    it "timeout for 10 milliseconds" do
        r <- run [] timeoutFor10Milliseconds
        case r of
            Left e -> case fromException e of
                Just TimeoutAssertion -> pure ()
                _ -> expectationFailure "should timeout"
            Right _ -> expectationFailure "should timeout"
    it "timeout for 10 milliseconds, but retry" do
        r <-
            run [FilterOut (Proxy @TimeoutAssertion)]
                $ evalState (10 :: Int) timeoutFor10MillisecondsOnce
        case r of
            Left _ -> expectationFailure "should not timeout"
            Right _ -> pure ()
