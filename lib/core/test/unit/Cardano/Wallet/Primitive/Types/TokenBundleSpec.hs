{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.Types.TokenBundleSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle.TokenMapSpec
    ()
import Test.Hspec
    ( Spec, describe )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess )
import Test.QuickCheck
    ( Arbitrary (..), Gen, choose )
import Test.QuickCheck.Classes
    ( eqLaws, semigroupLaws )
import Test.Utils.Laws
    ( testLawsMany )

spec :: Spec
spec =
    describe "Token bundle properties" $
    modifyMaxSuccess (const 1000) $ do

    describe "Class instances obey laws" $ do
        testLawsMany @TokenBundle
            [ eqLaws
            , semigroupLaws
            ]

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

genSmallCoin :: Gen Coin
genSmallCoin = Coin <$> choose (0, 63)

instance Arbitrary TokenBundle where
    arbitrary = TokenBundle <$> genSmallCoin <*> arbitrary
