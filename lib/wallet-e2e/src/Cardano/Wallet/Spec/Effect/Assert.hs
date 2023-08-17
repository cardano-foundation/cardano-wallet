{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.Spec.Effect.Assert where

import qualified Effectful.Error.Static as Effect

import Cardano.Wallet.Spec.Effect.Trace
    ( FxTrace, trace )
import Control.Exception
    ( AssertionFailed (..) )
import Effectful
    ( (:>), Eff, Effect )
import Effectful.Dispatch.Dynamic
    ( interpret )
import Effectful.Error.Static
    ( throwError )
import Effectful.TH
    ( makeEffect )
import Prelude hiding
    ( trace )

data FxAssert :: Effect where
    Assert :: Text -> Bool -> FxAssert m ()

$(makeEffect ''FxAssert)

assertFail :: (FxAssert :> es) => Text -> Eff es ()
assertFail label = assert label False

runAssertError
    :: (Effect.Error SomeException :> es, FxTrace :> es)
    => Eff (FxAssert : es) a
    -> Eff es a
runAssertError = interpret \_ (Assert msg truth) -> do
    trace $ "Asserting that " <> msg
    unless truth $ throwError $ toException $ AssertionFailed $ toString msg
