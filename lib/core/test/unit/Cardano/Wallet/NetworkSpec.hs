module Cardano.Wallet.NetworkSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Network
    ( ErrPostTx (..) )
import Test.Hspec
    ( Spec, describe, it )

spec :: Spec
spec = do
    describe "Pointless tests to cover 'Show' instances for errors" $ do
        testShow $ ErrPostTxBadRequest mempty
        testShow $ ErrPostTxProtocolFailure mempty

testShow :: Show a => a -> Spec
testShow a = it (show a) True
