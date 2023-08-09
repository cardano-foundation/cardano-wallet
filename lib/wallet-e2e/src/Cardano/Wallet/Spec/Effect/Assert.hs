{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.Spec.Effect.Assert where

import qualified Effectful.Error.Dynamic as Effect

import Cardano.Wallet.Spec.Effect.Trace
    ( FxTrace, trace )
import Effectful
    ( (:>), Eff, Effect )
import Effectful.Dispatch.Dynamic
    ( interpret )
import Effectful.Error.Dynamic
    ( throwError )
import Effectful.TH
    ( makeEffect )
import Prelude hiding
    ( trace )
import Text.Show
    ( show )

newtype Error = Error Text
    deriving newtype (Eq)

instance Show Error where
    show (Error msg) = "Assertion failed: " <> toString msg

data FxAssert :: Effect where
    Assert :: Text -> Bool -> FxAssert m ()

$(makeEffect ''FxAssert)

runAssertError ::
    (Effect.Error Error :> es, FxTrace :> es) =>
    Eff (FxAssert : es) a ->
    Eff es a
runAssertError = interpret \_ (Assert msg truth) -> do
    trace $ "Asserting that " <> msg
    unless truth $ throwError (Error msg)
