-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
module Cardano.Wallet.Primitive.Types.TxSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Tx
    ( MockSealedTx (..), SealedTx (..), mockSealedTx, sealedTxFromBytes )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ()
import Data.ByteString
    ( ByteString, pack )
import Data.Either
    ( isLeft )
import Test.Hspec
    ( Spec, describe )
import Test.Hspec.Extra
    ( parallel )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..), Property, (.&&.), (===) )

spec :: Spec
spec = parallel $ describe "SealedTx" $ do
    prop "sealedTxFromBytes - won't accept gibberish" prop_sealedTxGibberish
    prop "mockSealedTx - passes through mock values" prop_mockSealedTx

{-------------------------------------------------------------------------------
                         Evaluation of SealedTx fields
-------------------------------------------------------------------------------}

prop_sealedTxGibberish :: Gibberish -> Property
prop_sealedTxGibberish (Gibberish bs) =
    isLeft (serialisedTx <$> sealedTxFromBytes bs) .&&.
    isLeft (cardanoTx <$> sealedTxFromBytes bs)

prop_mockSealedTx :: Gibberish -> Property
prop_mockSealedTx (Gibberish bs) =
    serialisedTx (unMockSealedTx (mockSealedTx bs)) === bs

newtype Gibberish = Gibberish ByteString deriving (Show, Read, Eq)

instance Arbitrary Gibberish where
    arbitrary = Gibberish . pack <$> arbitrary
