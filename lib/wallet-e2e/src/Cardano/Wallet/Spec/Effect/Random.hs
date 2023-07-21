{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.Spec.Effect.Random where

import Cardano.Wallet.Spec.Effect.Trace
    ( TRACE, trace )
import Cardano.Wallet.Spec.Types
    ( Mnemonic )
import Effectful
    ( (:>), Eff, Effect )
import Effectful.Dispatch.Dynamic
    ( interpret )
import Effectful.TH
    ( makeEffect )
import Prelude hiding
    ( trace )

data RANDOM :: Effect where
    RandomMnemonic :: RANDOM m Mnemonic

$(makeEffect ''RANDOM)

runRandomMock :: Mnemonic -> (TRACE :> es) => Eff (RANDOM : es) a -> Eff es a
runRandomMock mnemonic = interpret \_ RandomMnemonic -> do
    trace "Generating a random mnemonic"
    pure mnemonic
