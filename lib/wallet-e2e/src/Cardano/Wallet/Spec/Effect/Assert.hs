{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.Spec.Effect.Assert
    ( -- * Effect
      FxAssert
    , assert
    , assertEq
    , assertFail

      -- ** Handlers
    , runAssertFailsFast
    ) where

import Cardano.Wallet.Spec.Effect.Trace
    ( FxTrace
    , trace
    )
import Effectful
    ( Eff
    , Effect
    , (:>)
    )
import Effectful.Dispatch.Dynamic
    ( interpret
    )
import Effectful.Fail
    ( Fail
    )
import Effectful.TH
    ( makeEffect
    )
import Prelude hiding
    ( trace
    )

data FxAssert :: Effect where
    Assert :: Text -> Bool -> FxAssert m ()
    AssertEq :: (Show a, Eq a) => Text -> a -> a -> FxAssert m ()

$(makeEffect ''FxAssert)

assertFail :: (FxAssert :> es) => Text -> Eff es ()
assertFail label = assert label False

runAssertFailsFast
    :: (Fail :> es, FxTrace :> es)
    => Eff (FxAssert : es) a
    -> Eff es a
runAssertFailsFast = interpret \_ -> \case
    Assert msg truth -> do
        trace $ "Asserting that " <> msg <> ": " <> show truth
        unless truth $ fail $ toString msg
    AssertEq msg x y -> do
        let equality = x == y
        trace $ "Asserting that " <> msg <> ": " <> show equality
        unless equality . fail $ show x <> " == " <> show y
