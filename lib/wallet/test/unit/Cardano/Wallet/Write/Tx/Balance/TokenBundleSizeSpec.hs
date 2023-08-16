{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cardano.Wallet.Write.Tx.Balance.TokenBundleSizeSpec where

import Prelude

import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundle, genTokenBundleSmallRange, shrinkTokenBundleSmallRange )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TokenBundleSizeAssessment (..), TxSize (..), txOutMaxCoin, txOutMinCoin )
import Cardano.Wallet.Primitive.Types.Tx.TxOut.Gen
    ( genTxOutTokenBundle )
import Cardano.Wallet.Write.Tx.Balance.TokenBundleSize
    ( TokenBundleMaxSize (TokenBundleMaxSize)
    , assessTokenBundleSize
    , computeTokenBundleSerializedLengthBytes
    , tokenBundleSizeAssessor
    )
import Cardano.Wallet.Write.Tx.Balance.TokenBundleSize.Gen
    ( genTokenBundleMaxSize, shrinkTokenBundleMaxSize )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Blind (..)
    , Property
    , conjoin
    , counterexample
    , property
    , resize
    , withMaxSuccess
    , (===)
    , (==>)
    )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle

spec :: Spec
spec = describe "Assessing the sizes of token bundles" $ do
    it "prop_assessTokenBundleSize_enlarge" $
        property prop_assessTokenBundleSize_enlarge
    it "prop_assessTokenBundleSize_shrink" $
        property prop_assessTokenBundleSize_shrink
    it "unit_assessTokenBundleSize_fixedSizeBundle_32" $
        property unit_assessTokenBundleSize_fixedSizeBundle_32
    it "unit_assessTokenBundleSize_fixedSizeBundle_48" $
        property unit_assessTokenBundleSize_fixedSizeBundle_48
    it "unit_assessTokenBundleSize_fixedSizeBundle_64" $
        property unit_assessTokenBundleSize_fixedSizeBundle_64
    it "unit_assessTokenBundleSize_fixedSizeBundle_128" $
        property unit_assessTokenBundleSize_fixedSizeBundle_128

--------------------------------------------------------------------------------
-- Assessing the sizes of token bundles
--------------------------------------------------------------------------------

-- Enlarging a token bundle that is over the size limit should yield a token
-- bundle that is still over the size limit.
--
prop_assessTokenBundleSize_enlarge
    :: Blind (VariableSize1024 TokenBundle)
    -> Blind (VariableSize16 TokenBundle)
    -> Property
prop_assessTokenBundleSize_enlarge b1' b2' =
    assess b1 == TokenBundleSizeExceedsLimit ==> conjoin
        [ assess (b1 `TokenBundle.add` b2)
            === TokenBundleSizeExceedsLimit
        , assess (b1 `TokenBundle.setCoin` txOutMaxCoin)
            === TokenBundleSizeExceedsLimit
        ]
  where
    assess = assessTokenBundleSize
        $ tokenBundleSizeAssessor maryTokenBundleMaxSize
    b1 = unVariableSize1024 $ getBlind b1'
    b2 = unVariableSize16 $ getBlind b2'

-- Shrinking a token bundle that is within the size limit should yield a token
-- bundle that is still within the size limit.
--
prop_assessTokenBundleSize_shrink
    :: Blind (VariableSize1024 TokenBundle)
    -> Blind (VariableSize16 TokenBundle)
    -> TokenBundleMaxSize
    -> Property
prop_assessTokenBundleSize_shrink b1' b2' maxSize =
    assess b1 == TokenBundleSizeWithinLimit ==> conjoin
        [ assess (b1 `TokenBundle.difference` b2)
            === TokenBundleSizeWithinLimit
        , assess (b1 `TokenBundle.setCoin` txOutMinCoin)
            === TokenBundleSizeWithinLimit
        ]
  where
    assess = assessTokenBundleSize (tokenBundleSizeAssessor maxSize)
    b1 = unVariableSize1024 $ getBlind b1'
    b2 = unVariableSize16 $ getBlind b2'

-- | Creates a test to assess the size of a token bundle with a fixed number of
--   assets, where the expected result is a constant.
--
-- Policy identifiers, asset names, token quantities are all allowed to vary.
--
unit_assessTokenBundleSize_fixedSizeBundle
    :: TokenBundle
    -- ^ Fixed size bundle
    -> TokenBundleSizeAssessment
    -- ^ Expected size assessment
    -> TokenBundleMaxSize
    -- ^ TokenBundle assessor function
    -> TxSize
    -- ^ Expected min length (bytes)
    -> TxSize
    -- ^ Expected max length (bytes)
    -> Property
unit_assessTokenBundleSize_fixedSizeBundle
    bundle
    expectedAssessment
    maxSize
    expectedMinLengthBytes
    expectedMaxLengthBytes =
        withMaxSuccess 100 $
        counterexample counterexampleText $
        conjoin . fmap property $
            [ actualAssessment  == expectedAssessment
            , actualLengthBytes >= expectedMinLengthBytes
            , actualLengthBytes <= expectedMaxLengthBytes
            ]
  where
    actualAssessment = assessTokenBundleSize
        (tokenBundleSizeAssessor maxSize)
        bundle
    actualLengthBytes = computeTokenBundleSerializedLengthBytes bundle
    counterexampleText = unlines
        [ "Expected min length bytes:"
        , show expectedMinLengthBytes
        , "Expected max length bytes:"
        , show expectedMaxLengthBytes
        , "Actual length bytes:"
        , show actualLengthBytes
        , "Expected assessment:"
        , show expectedAssessment
        , "Actual assessment:"
        , show actualAssessment
        ]

unit_assessTokenBundleSize_fixedSizeBundle_32
    :: Blind (FixedSize32 TokenBundle) -> Property
unit_assessTokenBundleSize_fixedSizeBundle_32 (Blind (FixedSize32 b)) =
    unit_assessTokenBundleSize_fixedSizeBundle b
        TokenBundleSizeWithinLimit
        maryTokenBundleMaxSize
        (TxSize 2116) (TxSize 2380)

unit_assessTokenBundleSize_fixedSizeBundle_48
    :: Blind (FixedSize48 TokenBundle) -> Property
unit_assessTokenBundleSize_fixedSizeBundle_48 (Blind (FixedSize48 b)) =
    unit_assessTokenBundleSize_fixedSizeBundle b
        TokenBundleSizeWithinLimit
        maryTokenBundleMaxSize
        (TxSize 3172) (TxSize 3564)

unit_assessTokenBundleSize_fixedSizeBundle_64
    :: Blind (FixedSize64 TokenBundle) -> Property
unit_assessTokenBundleSize_fixedSizeBundle_64 (Blind (FixedSize64 b)) =
    unit_assessTokenBundleSize_fixedSizeBundle b
        TokenBundleSizeExceedsLimit
        maryTokenBundleMaxSize
        (TxSize 4228) (TxSize 4748)

unit_assessTokenBundleSize_fixedSizeBundle_128
    :: Blind (FixedSize128 TokenBundle) -> Property
unit_assessTokenBundleSize_fixedSizeBundle_128 (Blind (FixedSize128 b)) =
    unit_assessTokenBundleSize_fixedSizeBundle b
        TokenBundleSizeExceedsLimit
        maryTokenBundleMaxSize
        (TxSize 8452) (TxSize 9484)

instance Arbitrary TokenBundle where
    arbitrary = genTokenBundleSmallRange
    shrink = shrinkTokenBundleSmallRange

instance Arbitrary TokenBundleMaxSize where
    arbitrary = genTokenBundleMaxSize
    shrink = shrinkTokenBundleMaxSize

newtype FixedSize32 a = FixedSize32 { unFixedSize32 :: a }
    deriving (Eq, Show)

newtype FixedSize48 a = FixedSize48 { unFixedSize48 :: a }
    deriving (Eq, Show)

newtype FixedSize64 a = FixedSize64 { unFixedSize64 :: a }
    deriving (Eq, Show)

newtype FixedSize128 a = FixedSize128 { unFixedSize128 :: a }
    deriving (Eq, Show)

newtype VariableSize16 a = VariableSize16 { unVariableSize16 :: a}
    deriving (Eq, Show)

newtype VariableSize1024 a = VariableSize1024 { unVariableSize1024 :: a}
    deriving (Eq, Show)

instance Arbitrary (FixedSize32 TokenBundle) where
    arbitrary = FixedSize32 <$> genTxOutTokenBundle 32
    -- No shrinking

instance Arbitrary (FixedSize48 TokenBundle) where
    arbitrary = FixedSize48 <$> genTxOutTokenBundle 48
    -- No shrinking

instance Arbitrary (FixedSize64 TokenBundle) where
    arbitrary = FixedSize64 <$> genTxOutTokenBundle 64
    -- No shrinking

instance Arbitrary (FixedSize128 TokenBundle) where
    arbitrary = FixedSize128 <$> genTxOutTokenBundle 128
    -- No shrinking

instance Arbitrary (VariableSize16 TokenBundle) where
    arbitrary = VariableSize16 <$> resize 16 genTokenBundle
    -- No shrinking

instance Arbitrary (VariableSize1024 TokenBundle) where
    arbitrary = VariableSize1024 <$> resize 1024 genTokenBundle
    -- No shrinking

maryTokenBundleMaxSize :: TokenBundleMaxSize
maryTokenBundleMaxSize = TokenBundleMaxSize 4000
