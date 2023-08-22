module Cardano.Wallet.Spec.TimeoutSpec
    ( timeoutSpec
    ) where

import qualified Effectful.Error.Static as E

import Cardano.Wallet.Spec.Effect.Assert
    ( FxAssert, runAssertFailsFast )
import Cardano.Wallet.Spec.Effect.Timeout
    ( FxTimeout, runTimeout, within )
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

-- | This story will not timeout as it takes less than 1 second.
noTimeout :: FxStory fxs '[FxTimeout, IOE] ()
noTimeout = within (seconds 1) do
    liftIO $ threadDelay $ 1000000 - 10

-- | This story will timeout as it takes longer than 1 second.
timeoutIn10Milliseconds :: FxStory fxs '[FxTimeout, IOE] ()
timeoutIn10Milliseconds = within (seconds 1) do
    liftIO $ threadDelay 1010000

-- | This story will timeout once, but then if retried will succeed.
timeoutIn10MillisecondsOnce :: FxStory fxs '[State Int, FxTimeout, IOE] ()
timeoutIn10MillisecondsOnce = do
    modify @Int (+ 10)
    delay <- get @Int
    within (seconds 1) do liftIO $ threadDelay $ 1000 * (1010 - delay)

type Story a =
    Eff
        [ FxTimeout
        , FxAssert
        , FxTrace
        , E.Error SomeException
        , IOE
        ]
        a

run :: Story a -> IO (Either SomeException (a, Seq Text))
run =
    runTimeout
        >>> runAssertFailsFast
        >>> runTracePure
        >>> E.runErrorNoCallStack
        >>> runEff

timeoutSpec :: Spec
timeoutSpec = describe "Timeouts" do
    it "no timeout" do
        r <- run noTimeout
        case r of
            Left _ -> expectationFailure "should not timeout"
            Right _ -> pass
    it "timeout in 10 milliseconds" do
        run timeoutIn10Milliseconds >>= \case
            Left _ -> pass
            Right _ -> expectationFailure "should timeout"
    it "timeout in 10 milliseconds, but retry" do
        r <- run $ evalState (10 :: Int) timeoutIn10MillisecondsOnce
        case r of
            Left _ -> expectationFailure "should not timeout"
            Right _ -> pass
