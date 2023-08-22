module Cardano.Wallet.Spec.TimeoutSpec
    ( timeoutSpec
    ) where

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
    ( milliseconds, seconds, toMicroseconds )
import Effectful
    ( Eff, IOE, runEff )
import Effectful.Fail
    ( Fail, runFail )
import Prelude hiding
    ( State, evalState, get, modify )
import Test.Syd
    ( Spec, describe, expectationFailure, it )

-- | This story will not timeout as it takes less than 1 second.
noTimeout :: FxStory fxs '[FxTimeout, IOE] ()
noTimeout = within (seconds 1) do
    liftIO . threadDelay . round
        $ toMicroseconds (seconds 1) - toMicroseconds (milliseconds 100)

-- | This story will timeout as it takes longer than 1 second.
timeoutIn10Milliseconds :: FxStory fxs '[FxTimeout, IOE] ()
timeoutIn10Milliseconds = within (seconds 1) do
    liftIO $ threadDelay 1010000

type Story a = Eff [FxTimeout, FxAssert, FxTrace, Fail, IOE] a

run :: Story a -> IO (Either String (a, Seq Text))
run =
    runTimeout
        >>> runAssertFailsFast
        >>> runTracePure
        >>> runFail
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
