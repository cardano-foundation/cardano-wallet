{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Shelley.Compatibility.LedgerSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Script
    ( KeyHash (..), KeyRole (..), Script )
import Cardano.Wallet.Gen
    ( genScript )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRange, shrinkTokenBundleSmallRange )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName, TokenPolicyId )
import Cardano.Wallet.Primitive.Types.TokenPolicy.Gen
    ( genTokenNameLargeRange, genTokenPolicyIdLargeRange )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
    ( genTokenQuantityFullRange, shrinkTokenQuantityFullRange )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxIn.Gen
    ( genTxIn, shrinkTxIn )
import Cardano.Wallet.Primitive.Types.Tx.TxOut.Gen
    ( genTxOutCoin, shrinkTxOutCoin )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( Convert (..), toLedgerTimelockScript, toWalletScript )
import Data.Proxy
    ( Proxy (..) )
import Data.Typeable
    ( Typeable, typeRep )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardBabbage )
import Test.Cardano.Ledger.Allegra.Arbitrary
    ()
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess )
import Test.QuickCheck
    ( Arbitrary (..), elements, property, vectorOf, (===) )

import qualified Data.ByteString as BS

spec :: Spec
spec = describe "Cardano.Wallet.Shelley.Compatibility.LedgerSpec" $

    modifyMaxSuccess (const 1000) $ do

    describe "Roundtrip conversions" $ do

        ledgerRoundtrip $ Proxy @Coin
        ledgerRoundtrip $ Proxy @TokenBundle
        ledgerRoundtrip $ Proxy @TokenName
        ledgerRoundtrip $ Proxy @TokenPolicyId
        ledgerRoundtrip $ Proxy @TokenQuantity
        ledgerRoundtrip $ Proxy @TxIn

    describe "Timelock roundtrips (toLedgerTimelockScript, toWalletScript)" $ do
        let ledger = toLedgerTimelockScript @StandardBabbage
        let wallet = toWalletScript (const Unknown)

        it "ledger . wallet . ledger == ledger" $ property $ \s -> do
            -- Ignore key role by doing one extra conversion
            ledger (wallet $ ledger s) === ledger s

        it "ledger . wallet == id" $ property $ \s -> do
            ledger (wallet s) === s

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

ledgerRoundtrip
    :: forall w l. (Arbitrary w, Eq w, Show w, Typeable w, Convert w l)
    => Proxy w
    -> Spec
ledgerRoundtrip proxy = it title $
    property $ \a -> toWallet (toLedger @w a) === a
  where
    title = mconcat
        [ "Can perform roundtrip conversion for values of type '"
        , show (typeRep proxy)
        , "'"
        ]

--------------------------------------------------------------------------------
-- Arbitraries
--------------------------------------------------------------------------------

instance Arbitrary Coin where
    -- This instance is used to test roundtrip conversions, so it's important
    -- that we generate coins across the full range available.
    arbitrary = genTxOutCoin
    shrink = shrinkTxOutCoin

instance Arbitrary TokenBundle where
    arbitrary = genTokenBundleSmallRange
    shrink = shrinkTokenBundleSmallRange

instance Arbitrary TokenName where
    arbitrary = genTokenNameLargeRange
    -- No shrinking

instance Arbitrary TokenPolicyId where
    arbitrary = genTokenPolicyIdLargeRange
    -- No shrinking

instance Arbitrary TokenQuantity where
    arbitrary = genTokenQuantityFullRange
    shrink = shrinkTokenQuantityFullRange

instance Arbitrary TxIn where
    arbitrary = genTxIn
    shrink = shrinkTxIn

instance Arbitrary (Script KeyHash) where
    arbitrary = do
        keyHashes <- vectorOf 10 arbitrary
        genScript keyHashes

instance Arbitrary KeyHash where
    arbitrary = do
        cred <- elements [Payment, Delegation, Policy, Unknown]
        KeyHash cred . BS.pack <$> vectorOf 28 arbitrary
