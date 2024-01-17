module Cardano.Wallet.Test.Integration.PendingInEra
    ( pendingInConway
    , pendingInBabbage
    )
where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiEra (..)
    )
import Test.Hspec.Core.Spec
    ( Expectation
    , pendingWith
    )
import Test.Integration.Framework.Context
    ( Context (..)
    )

pendingInConway :: Context -> Expectation
pendingInConway ctx = case _mainEra ctx of
    ApiConway -> pendingWith "Pending in Conway era"
    _ -> pure ()

pendingInBabbage :: Context -> Expectation
pendingInBabbage ctx = case _mainEra ctx of
    ApiBabbage -> pendingWith "Pending in Babbage era"
    _ -> pure ()
