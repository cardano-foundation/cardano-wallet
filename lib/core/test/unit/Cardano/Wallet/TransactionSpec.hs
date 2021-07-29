module Cardano.Wallet.TransactionSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Transaction
    ( ErrSignTx (..) )
import Test.Hspec
    ( Spec, describe, it )

spec :: Spec
spec =
    describe "Pointless tests to cover 'Show' instances for errors" $ do
        testShow $ ErrSignTxKeyNotFound $ Address mempty

testShow :: Show a => a -> Spec
testShow a = it (show a) True
