{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.CoinSelection.CollateralSpec where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection.Collateral
    ( pureAdaValue )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (TokenBundle) )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRangePositive, shrinkTokenBundleSmallRangePositive )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , NonNegative (..)
    , Property
    , checkCoverage
    , choose
    , conjoin
    , counterexample
    , cover
    , coverTable
    , forAll
    , property
    , scale
    , shuffle
    , sized
    , tabulate
    , vector
    , (===)
    )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.Binary as B
import qualified Data.Binary.BitPut as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Test.Cardano.Chain.Common.Gen as Byron

spec :: Spec
spec = do
    parallel $ describe "collateral suitability" $ do
        it "pureAdaValue only returns when token bundle has only ADA" $
            property prop_pureAdaValue

prop_pureAdaValue :: TokenBundle -> Property
prop_pureAdaValue bundle =
    let
        result = pureAdaValue bundle
    in
        checkCoverage $
        cover 30 (not (TokenBundle.isCoin bundle))
            "Token bundle has at least 1 non-ada asset" $
        cover 30 (TokenBundle.isCoin bundle)
            "Token bundle has no non-ada assets" $
        if TokenBundle.isCoin bundle
        then result === Just (TokenBundle.coin bundle)
        else result === Nothing

-- prop_bootstrapAddresses :: Address -> Property
-- prop_bootstrapAddresses addr =
--     case classifyCollateralAddress addr of
        
instance Arbitrary TokenBundle where
    arbitrary = genTokenBundleSmallRangePositive
    shrink = shrinkTokenBundleSmallRangePositive

instance Arbitrary Address where
    arbitrary = genBootstrapAddress

genBootstrapAddress :: Gen Address
genBootstrapAddress = do
    let header = BSL.toStrict $ B.runBitPut $ B.putBit True
    payload <- BS.pack <$> vector 42

    pure $ Address $ header <> payload

-- test :: IO [Either AddrNotSuitableForCollateral Address]
-- test = do
--     addrs <- sample genBootstrapAddress
--     pure $
--         fmap classifyCollateralAddress 
