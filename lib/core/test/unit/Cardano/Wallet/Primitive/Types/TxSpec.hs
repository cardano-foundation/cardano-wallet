{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
module Cardano.Wallet.Primitive.Types.TxSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (..), TxBurn, TxMint, mockSealedTx, sealedTxFromBytes )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxBurn, genTxMint, shrinkTxBurn, shrinkTxMint )
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
import Test.QuickCheck.Classes
    ( ordLaws )
import Test.Utils.Laws
    ( testLaws )

spec :: Spec
spec = do

    parallel $ describe "SealedTx" $ do
        prop "sealedTxFromBytes - won't accept gibberish"
            prop_sealedTxGibberish
        prop "mockSealedTx - passes through mock values"
            prop_mockSealedTx

    parallel $ describe "Minting and burning" $ do
        testLaws @TxMint ordLaws
        testLaws @TxBurn ordLaws

{-------------------------------------------------------------------------------
                         Evaluation of SealedTx fields
-------------------------------------------------------------------------------}

prop_sealedTxGibberish :: Gibberish -> Property
prop_sealedTxGibberish (Gibberish bs) =
    isLeft (serialisedTx <$> sealedTxFromBytes bs) .&&.
    isLeft (cardanoTx <$> sealedTxFromBytes bs)

prop_mockSealedTx :: Gibberish -> Property
prop_mockSealedTx (Gibberish bs) =
    serialisedTx (mockSealedTx bs) === bs

newtype Gibberish = Gibberish ByteString deriving (Show, Read, Eq)

instance Arbitrary Gibberish where
    arbitrary = Gibberish . pack <$> arbitrary

--------------------------------------------------------------------------------
-- Minting and burning
--------------------------------------------------------------------------------

instance Arbitrary TxMint where
    arbitrary = genTxMint
    shrink = shrinkTxMint

instance Arbitrary TxBurn where
    arbitrary = genTxBurn
    shrink = shrinkTxBurn
