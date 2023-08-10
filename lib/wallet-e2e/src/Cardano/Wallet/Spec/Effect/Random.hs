{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.Spec.Effect.Random where

import Cardano.Wallet.Spec.Effect.Trace
    ( FxTrace
    , trace
    )
import Cardano.Wallet.Spec.Types
    ( Mnemonic
    )
import Effectful
    ( Eff
    , Effect
    , (:>)
    )
import Effectful.Dispatch.Dynamic
    ( interpret
    )
import Effectful.TH
    ( makeEffect
    )
import Prelude hiding
    ( trace
    )

data FxRandom :: Effect where
    RandomMnemonic :: FxRandom m Mnemonic

$(makeEffect ''FxRandom)

runRandomMock :: Mnemonic -> (FxTrace :> es) => Eff (FxRandom : es) a -> Eff es a
runRandomMock mnemonic = interpret \_ RandomMnemonic -> do
    trace "Generating a random mnemonic"
    pure mnemonic
