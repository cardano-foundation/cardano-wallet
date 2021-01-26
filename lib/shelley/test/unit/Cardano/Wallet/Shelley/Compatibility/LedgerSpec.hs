{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Shelley.Compatibility.LedgerSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoinAny, shrinkCoinAny )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( Flat (..), TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRange, shrinkTokenBundleSmallRange )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetIdLargeRange )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName, TokenPolicyId )
import Cardano.Wallet.Primitive.Types.TokenPolicy.Gen
    ( genTokenNameLargeRange, genTokenPolicyIdLargeRange )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
    ( genTokenQuantityMixed, shrinkTokenQuantityMixed )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( Convert (..), computeMinimumAdaQuantity )
import Control.Monad
    ( replicateM )
import Data.Bifunctor
    ( bimap )
import Data.Proxy
    ( Proxy (..) )
import Data.Typeable
    ( Typeable, typeRep )
import Data.Word
    ( Word64 )
import Fmt
    ( pretty )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it, parallel )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess )
import Test.QuickCheck
    ( Arbitrary (..)
    , Blind (..)
    , Gen
    , Positive (..)
    , Property
    , checkCoverage
    , choose
    , counterexample
    , cover
    , property
    , withMaxSuccess
    , (===)
    )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.Set as Set

spec :: Spec
spec = describe "Cardano.Wallet.Shelley.Compatibility.LedgerSpec" $

    modifyMaxSuccess (const 1000) $ do

    parallel $ describe "Roundtrip conversions" $ do

        ledgerRoundtrip $ Proxy @Coin
        ledgerRoundtrip $ Proxy @TokenBundle
        ledgerRoundtrip $ Proxy @TokenName
        ledgerRoundtrip $ Proxy @TokenPolicyId
        ledgerRoundtrip $ Proxy @TokenQuantity

    parallel $ describe "Properties for computeMinimumAdaQuantity" $ do

        it "prop_computeMinimumAdaQuantity_forCoin" $
            property prop_computeMinimumAdaQuantity_forCoin
        it "prop_computeMinimumAdaQuantity_agnosticToAdaQuantity" $
            property prop_computeMinimumAdaQuantity_agnosticToAdaQuantity
        it "prop_computeMinimumAdaQuantity_agnosticToAssetQuantities" $
            property prop_computeMinimumAdaQuantity_agnosticToAssetQuantities

    parallel $ describe "Unit tests for computeMinimumAdaQuantity" $ do

        it "unit_computeMinimumAdaQuantity_emptyBundle" $
            property unit_computeMinimumAdaQuantity_emptyBundle
        it "unit_computeMinimumAdaQuantity_fixedSizeBundle_8" $
            property unit_computeMinimumAdaQuantity_fixedSizeBundle_8
        it "unit_computeMinimumAdaQuantity_fixedSizeBundle_64" $
            property unit_computeMinimumAdaQuantity_fixedSizeBundle_64
        it "unit_computeMinimumAdaQuantity_fixedSizeBundle_256" $
            property unit_computeMinimumAdaQuantity_fixedSizeBundle_256

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

prop_computeMinimumAdaQuantity_forCoin
    :: ProtocolMinimum Coin
    -> Coin
    -> Property
prop_computeMinimumAdaQuantity_forCoin
    (ProtocolMinimum protocolMinimum) coin =
        computeMinimumAdaQuantity protocolMinimum (TokenBundle.fromCoin coin)
            === protocolMinimum

prop_computeMinimumAdaQuantity_agnosticToAdaQuantity
    :: Blind TokenBundle
    -> ProtocolMinimum Coin
    -> Coin
    -> Property
prop_computeMinimumAdaQuantity_agnosticToAdaQuantity
    (Blind bundle) (ProtocolMinimum protocolMinimum) newCoin =
        checkCoverage $
        cover 90 (newCoin /= TokenBundle.getCoin bundle)
            "Coin value is different" $
        counterexample counterexampleText $
        compute bundle === compute (TokenBundle.setCoin bundle newCoin)
  where
    compute = computeMinimumAdaQuantity protocolMinimum
    counterexampleText = unlines
        [ "bundle:"
        , pretty (Flat bundle)
        ]

prop_computeMinimumAdaQuantity_agnosticToAssetQuantities
    :: Blind TokenBundle
    -> ProtocolMinimum Coin
    -> Positive Int
    -> Property
prop_computeMinimumAdaQuantity_agnosticToAssetQuantities
    (Blind bundle) (ProtocolMinimum protocolMinimum) (Positive scalingFactor) =
        checkCoverage $
        cover 50 (scalingFactor > 1)
            "Scaling factor is greater than 1" $
        cover 40 (assetCount >= 1)
            "Token bundle has at least 1 non-ada asset" $
        cover 20 (assetCount >= 2)
            "Token bundle has at least 2 non-ada assets" $
        cover 10 (assetCount >= 4)
            "Token bundle has at least 4 non-ada assets" $
        counterexample counterexampleText $
        compute bundle === compute (scaleAllQuantities bundle)
  where
    assetCount = Set.size $ TokenBundle.getAssets bundle
    compute = computeMinimumAdaQuantity protocolMinimum
    scaleAllQuantities = adjustAllQuantities (* fromIntegral scalingFactor)
    counterexampleText = unlines
        [ "bundle:"
        , pretty (Flat bundle)
        ]

--------------------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------------------

-- | Creates a test to compute the minimum ada quantity for a token bundle with
--   a fixed number of assets, where the expected result is a constant.
--
-- Policy identifiers, asset names, token quantities are all allowed to vary.
--
unit_computeMinimumAdaQuantity_fixedSizeBundle
    :: TokenBundle
    -- ^ Fixed size bundle
    -> Coin
    -- ^ Expected minimum ada quantity
    -> Property
unit_computeMinimumAdaQuantity_fixedSizeBundle bundle expectation =
    withMaxSuccess 100 $
    computeMinimumAdaQuantity protocolMinimum bundle === expectation
  where
    protocolMinimum = Coin 1_000_000

-- | Generates a token bundle with a fixed number of assets.
--
-- Policy identifiers, asset names, token quantities are all allowed to vary.
--
genFixedSizeTokenBundle :: Int -> Gen TokenBundle
genFixedSizeTokenBundle fixedAssetCount
    = TokenBundle.fromFlatList
        <$> genCoin
        <*> replicateM fixedAssetCount genAssetQuantity
  where
    genAssetQuantity = (,)
        <$> genAssetIdLargeRange
        <*> genTokenQuantity
    genCoin = Coin
        <$> choose (1, unCoin maxBound)
    genTokenQuantity = TokenQuantity . fromIntegral
        -- Although the ledger specification allows token quantities of
        -- unlimited sizes, in practice we'll only see transaction outputs
        -- where the token quantities are bounded by the size of a 'Word64'.
        <$> choose (1, maxBound :: Word64)

unit_computeMinimumAdaQuantity_emptyBundle :: Property
unit_computeMinimumAdaQuantity_emptyBundle =
    unit_computeMinimumAdaQuantity_fixedSizeBundle TokenBundle.empty $
        Coin 1000000

unit_computeMinimumAdaQuantity_fixedSizeBundle_8
    :: Blind (FixedSize8 TokenBundle) -> Property
unit_computeMinimumAdaQuantity_fixedSizeBundle_8 (Blind (FixedSize8 b)) =
    unit_computeMinimumAdaQuantity_fixedSizeBundle b $
        Coin 3888885

unit_computeMinimumAdaQuantity_fixedSizeBundle_64
    :: Blind (FixedSize64 TokenBundle) -> Property
unit_computeMinimumAdaQuantity_fixedSizeBundle_64 (Blind (FixedSize64 b)) =
    unit_computeMinimumAdaQuantity_fixedSizeBundle b $
        Coin 22555533

unit_computeMinimumAdaQuantity_fixedSizeBundle_256
    :: Blind (FixedSize256 TokenBundle) -> Property
unit_computeMinimumAdaQuantity_fixedSizeBundle_256 (Blind (FixedSize256 b)) =
    unit_computeMinimumAdaQuantity_fixedSizeBundle b $
        Coin 86555469

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

adjustAllQuantities :: (Natural -> Natural) -> TokenBundle -> TokenBundle
adjustAllQuantities f b = uncurry TokenBundle.fromFlatList $ bimap
    (adjustCoin)
    (fmap (fmap adjustTokenQuantity))
    (TokenBundle.toFlatList b)
  where
    adjustCoin :: Coin -> Coin
    adjustCoin = Coin . fromIntegral . f . fromIntegral . unCoin

    adjustTokenQuantity :: TokenQuantity -> TokenQuantity
    adjustTokenQuantity = TokenQuantity . f . unTokenQuantity

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
-- Adaptors
--------------------------------------------------------------------------------

newtype ProtocolMinimum a = ProtocolMinimum { unProtocolMinimum :: a }
    deriving (Eq, Show)

newtype FixedSize8 a = FixedSize8 { unFixedSize8 :: a }
    deriving (Eq, Show)

newtype FixedSize64 a = FixedSize64 { unFixedSize64 :: a }
    deriving (Eq, Show)

newtype FixedSize256 a = FixedSize256 { unFixedSize256 :: a }
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Arbitraries
--------------------------------------------------------------------------------

instance Arbitrary Coin where
    arbitrary = genCoinAny
    shrink = shrinkCoinAny

instance Arbitrary (ProtocolMinimum Coin) where
    arbitrary
        = ProtocolMinimum
        . Coin
        . (* 1_000_000)
        . fromIntegral @Int @Word64
        . getPositive <$> arbitrary
    shrink
        = fmap (ProtocolMinimum . Coin)
        . shrink
        . unCoin
        . unProtocolMinimum

instance Arbitrary TokenBundle where
    arbitrary = genTokenBundleSmallRange
    shrink = shrinkTokenBundleSmallRange

instance Arbitrary (FixedSize8 TokenBundle) where
    arbitrary = FixedSize8 <$> genFixedSizeTokenBundle 8
    -- No shrinking

instance Arbitrary (FixedSize64 TokenBundle) where
    arbitrary = FixedSize64 <$> genFixedSizeTokenBundle 64
    -- No shrinking

instance Arbitrary (FixedSize256 TokenBundle) where
    arbitrary = FixedSize256 <$> genFixedSizeTokenBundle 256
    -- No shrinking

instance Arbitrary TokenName where
    arbitrary = genTokenNameLargeRange
    -- No shrinking

instance Arbitrary TokenPolicyId where
    arbitrary = genTokenPolicyIdLargeRange
    -- No shrinking

instance Arbitrary TokenQuantity where
    arbitrary = genTokenQuantityMixed
    shrink = shrinkTokenQuantityMixed
