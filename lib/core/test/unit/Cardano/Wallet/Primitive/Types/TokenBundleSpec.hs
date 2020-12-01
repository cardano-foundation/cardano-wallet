{-# LANGUAGE NamedFieldPuns #-}
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
import Control.Monad
    ( join )
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

genStrictlyPositiveCoin :: Gen Coin
genStrictlyPositiveCoin = Coin <$> choose (1, 64)

shrinkStrictlyPositiveCoin :: Coin -> [Coin]
shrinkStrictlyPositiveCoin (Coin c) = Coin <$> filter (> 0) (shrink c)

instance Arbitrary TokenBundle where
    arbitrary = TokenBundle <$> genStrictlyPositiveCoin <*> arbitrary
    shrink TokenBundle {coin, tokens} = join
        [ [ TokenBundle coin' tokens
          | coin' <- shrinkStrictlyPositiveCoin coin
          ]
        , [ TokenBundle coin tokens'
          | tokens' <- shrink tokens
          ]
        ]
