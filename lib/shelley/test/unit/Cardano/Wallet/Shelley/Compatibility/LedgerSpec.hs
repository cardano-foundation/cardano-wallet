{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Shelley.Compatibility.LedgerSpec
    ( spec
    ) where

import Prelude

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
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxOutCoin, shrinkTxOutCoin )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( Convert (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Typeable
    ( Typeable, typeRep )
import Test.Hspec
    ( Spec, describe, it, parallel )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess )
import Test.QuickCheck
    ( Arbitrary (..), property, (===) )

spec :: Spec
spec = describe "Cardano.Wallet.Shelley.Compatibility.LedgerSpec" $

    modifyMaxSuccess (const 1000) $ do

    parallel $ describe "Roundtrip conversions" $ do

        ledgerRoundtrip $ Proxy @Coin
        ledgerRoundtrip $ Proxy @TokenBundle
        ledgerRoundtrip $ Proxy @TokenName
        ledgerRoundtrip $ Proxy @TokenPolicyId
        ledgerRoundtrip $ Proxy @TokenQuantity

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
