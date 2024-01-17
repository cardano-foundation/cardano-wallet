{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Internal.Cardano.Write.Tx.Balance.TokenBundleSizeSpec where

import Prelude

import Cardano.CoinSelection.Size
    ( TokenBundleSizeAssessment (..)
    )
import Cardano.Ledger.Api.Era
    ( eraProtVerLow
    )
import Cardano.Ledger.Api.PParams
    ( PParams
    , ppMaxValSizeL
    , ppProtocolVersionL
    )
import Control.Lens
    ( (&)
    , (.~)
    )
import Data.Default
    ( def
    )
import Data.Monoid.Monus
    ( Monus ((<\>))
    )
import Data.Word
    ( Word64
    )
import Internal.Cardano.Write.Tx
    ( IsRecentEra (..)
    , ProtVer (..)
    , RecentEra (..)
    , StandardBabbage
    , StandardConway
    , Version
    )
import Internal.Cardano.Write.Tx.Balance.TokenBundleSize
    ( TokenBundleSizeAssessor
    , assessTokenBundleSize
    , computeTokenBundleSerializedLengthBytes
    , mkTokenBundleSizeAssessor
    )
import Numeric.Natural
    ( Natural
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Blind (..)
    , Gen
    , Property
    , arbitraryBoundedEnum
    , conjoin
    , counterexample
    , oneof
    , property
    , resize
    , withMaxSuccess
    , (===)
    , (==>)
    )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as W.TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as W
    ( TokenBundle
    )
import qualified Cardano.Wallet.Primitive.Types.TokenBundle.Gen as W
import qualified Cardano.Wallet.Primitive.Types.Tx.Constraints as W
    ( TxSize (..)
    , txOutMaxCoin
    , txOutMinCoin
    )
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut.Gen as W

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
    :: Blind (VariableSize1024 W.TokenBundle)
    -> Blind (VariableSize16 W.TokenBundle)
    -> PParamsInRecentEra
    -> Property
prop_assessTokenBundleSize_enlarge b1' b2' pp =
    assess b1 == TokenBundleSizeExceedsLimit ==> conjoin
        [ assess (b1 <> b2)
            === TokenBundleSizeExceedsLimit
        , assess (b1 `W.TokenBundle.setCoin` W.txOutMaxCoin)
            === TokenBundleSizeExceedsLimit
        ]
  where
    assess = assessTokenBundleSize $ mkAssessorFromPParamsInRecentEra pp
    b1 = unVariableSize1024 $ getBlind b1'
    b2 = unVariableSize16 $ getBlind b2'

-- Shrinking a token bundle that is within the size limit should yield a token
-- bundle that is still within the size limit.
--
prop_assessTokenBundleSize_shrink
    :: Blind (VariableSize1024 W.TokenBundle)
    -> Blind (VariableSize16 W.TokenBundle)
    -> PParamsInRecentEra
    -> Property
prop_assessTokenBundleSize_shrink b1' b2' pp =
    assess b1 == TokenBundleSizeWithinLimit ==> conjoin
        [ assess (b1 <\> b2)
            === TokenBundleSizeWithinLimit
        , assess (b1 `W.TokenBundle.setCoin` W.txOutMinCoin)
            === TokenBundleSizeWithinLimit
        ]
  where
    assess = assessTokenBundleSize $ mkAssessorFromPParamsInRecentEra pp
    b1 = unVariableSize1024 $ getBlind b1'
    b2 = unVariableSize16 $ getBlind b2'

-- | Creates a test to assess the size of a token bundle with a fixed number of
--   assets, where the expected result is a constant.
--
-- Policy identifiers, asset names, token quantities are all allowed to vary.
--
unit_assessTokenBundleSize_fixedSizeBundle
    :: W.TokenBundle
    -- ^ Fixed size bundle
    -> TokenBundleSizeAssessment
    -- ^ Expected size assessment
    -> TokenBundleSizeAssessor
    -- ^ W.TokenBundle assessor function
    -> W.TxSize
    -- ^ Expected min length (bytes)
    -> W.TxSize
    -- ^ Expected max length (bytes)
    -> Property
unit_assessTokenBundleSize_fixedSizeBundle
    bundle
    expectedAssessment
    assessor
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
    actualAssessment = assessTokenBundleSize assessor bundle
    v = eraProtVerLow @StandardBabbage
    actualLengthBytes = computeTokenBundleSerializedLengthBytes bundle v
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
    :: Blind (FixedSize32 W.TokenBundle) -> Property
unit_assessTokenBundleSize_fixedSizeBundle_32 (Blind (FixedSize32 b)) =
    unit_assessTokenBundleSize_fixedSizeBundle b
        TokenBundleSizeWithinLimit
        babbageTokenBundleSizeAssessor
        (W.TxSize 2116) (W.TxSize 2380)

unit_assessTokenBundleSize_fixedSizeBundle_48
    :: Blind (FixedSize48 W.TokenBundle) -> Property
unit_assessTokenBundleSize_fixedSizeBundle_48 (Blind (FixedSize48 b)) =
    unit_assessTokenBundleSize_fixedSizeBundle b
        TokenBundleSizeWithinLimit
        babbageTokenBundleSizeAssessor
        (W.TxSize 3172) (W.TxSize 3564)

unit_assessTokenBundleSize_fixedSizeBundle_64
    :: Blind (FixedSize64 W.TokenBundle) -> Property
unit_assessTokenBundleSize_fixedSizeBundle_64 (Blind (FixedSize64 b)) =
    unit_assessTokenBundleSize_fixedSizeBundle b
        TokenBundleSizeExceedsLimit
        babbageTokenBundleSizeAssessor
        (W.TxSize 4228) (W.TxSize 4748)

unit_assessTokenBundleSize_fixedSizeBundle_128
    :: Blind (FixedSize128 W.TokenBundle) -> Property
unit_assessTokenBundleSize_fixedSizeBundle_128 (Blind (FixedSize128 b)) =
    unit_assessTokenBundleSize_fixedSizeBundle b
        TokenBundleSizeExceedsLimit
        babbageTokenBundleSizeAssessor
        (W.TxSize 8452) (W.TxSize 9484)

instance Arbitrary W.TokenBundle where
    arbitrary = W.genTokenBundleSmallRange
    shrink = W.shrinkTokenBundleSmallRange

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

instance Arbitrary (FixedSize32 W.TokenBundle) where
    arbitrary = FixedSize32 <$> W.genTxOutTokenBundle 32
    -- No shrinking

instance Arbitrary (FixedSize48 W.TokenBundle) where
    arbitrary = FixedSize48 <$> W.genTxOutTokenBundle 48
    -- No shrinking

instance Arbitrary (FixedSize64 W.TokenBundle) where
    arbitrary = FixedSize64 <$> W.genTxOutTokenBundle 64
    -- No shrinking

instance Arbitrary (FixedSize128 W.TokenBundle) where
    arbitrary = FixedSize128 <$> W.genTxOutTokenBundle 128
    -- No shrinking

instance Arbitrary (VariableSize16 W.TokenBundle) where
    arbitrary = VariableSize16 <$> resize 16 W.genTokenBundle
    -- No shrinking

instance Arbitrary (VariableSize1024 W.TokenBundle) where
    arbitrary = VariableSize1024 <$> resize 1024 W.genTokenBundle
    -- No shrinking

instance Arbitrary Version where
    arbitrary = arbitraryBoundedEnum

data PParamsInRecentEra
    = PParamsInBabbage (PParams StandardBabbage)
    | PParamsInConway (PParams StandardConway)
    deriving (Show, Eq)

instance Arbitrary PParamsInRecentEra where
    arbitrary = error "TODO conway: Arbitrary PParamsInRecentEra"
    -- arbitrary = oneof
    --     [ PParamsInBabbage <$> genPParams RecentEraBabbage
    --     , PParamsInConway <$> genPParams RecentEraConway
    --     ]

    --   where
    --     genPParams
    --         :: IsRecentEra era
    --         => RecentEra era
    --         -> Gen (PParams era)
    --     genPParams _era = do
    --         ver <- arbitrary
    --         maxSize <- genMaxSizeBytes
    --         return $ def
    --             & ppProtocolVersionL .~ (ProtVer ver 0)
    --                 -- minor version doesn't matter
    --             & ppMaxValSizeL .~ maxSize
    --       where
    --         genMaxSizeBytes :: Gen Natural
    --         genMaxSizeBytes =
    --             oneof
    --             -- Generate values close to the mainnet value of 4000 bytes
    --             -- (and guard against underflow)
    --             [ fromIntegral . max 0 . (4000 +) <$> arbitrary @Int

    --             -- Generate more extreme values (both small and large)
    --             , fromIntegral <$> arbitrary @Word64
    --             ]

babbageTokenBundleSizeAssessor :: TokenBundleSizeAssessor
babbageTokenBundleSizeAssessor = mkTokenBundleSizeAssessor
    $ (def :: PParams StandardBabbage)
        & ppProtocolVersionL .~ (ProtVer (eraProtVerLow @StandardBabbage) 0)
        & ppMaxValSizeL .~ maryTokenBundleMaxSizeBytes
  where
    maryTokenBundleMaxSizeBytes = 4000

mkAssessorFromPParamsInRecentEra
    :: PParamsInRecentEra
    -> TokenBundleSizeAssessor
mkAssessorFromPParamsInRecentEra (PParamsInBabbage pp) =
    mkTokenBundleSizeAssessor pp
mkAssessorFromPParamsInRecentEra (PParamsInConway pp) =
    mkTokenBundleSizeAssessor pp
