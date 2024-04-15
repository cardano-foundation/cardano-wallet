{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoNumericUnderscores #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NamedFieldPuns #-}

{- HLINT ignore "Use null" -}

module Internal.Cardano.Write.Tx.Balance.Size.CoinSpec where

import Prelude

import Cardano.Numeric.Util
    ( power
    )
import Data.Either
    ( isLeft
    , isRight
    )
import Data.Function
    ( (&)
    )
import Data.Generics.Internal.VL.Lens
    ( view
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Monoid.Monus
    ( Monus ((<\>))
    )
import Internal.Cardano.Write.Tx
    ( FeePerByte (..)
    )
import Internal.Cardano.Write.Tx.Balance.Size.Coin
    ( ErrMoreSurplusNeeded (..)
    , TxFeeAndChange (..)
    , costOfIncreasingCoin
    , distributeSurplus
    , distributeSurplusDelta
    , maximumCostOfIncreasingCoin
    , sizeOfCoin
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , Testable
    , arbitrarySizedNatural
    , checkCoverage
    , conjoin
    , counterexample
    , cover
    , forAll
    , frequency
    , liftShrink2
    , listOf
    , property
    , scale
    , shrinkList
    , shrinkMapBy
    , withMaxSuccess
    , (===)
    )
import Test.QuickCheck.Extra
    ( report
    , shrinkNatural
    )

import qualified Cardano.Api as CardanoApi
import qualified Cardano.Api.Gen as CardanoApi
import qualified Cardano.Api.Shelley as CardanoApi
import qualified Cardano.Wallet.Primitive.Ledger.Convert as Convert
import qualified Cardano.Wallet.Primitive.Types.Coin as W
    ( Coin (..)
    )
import qualified Cardano.Wallet.Primitive.Types.Coin as W.Coin
import qualified Cardano.Wallet.Primitive.Types.Coin.Gen as W
import qualified Cardano.Wallet.Primitive.Types.Tx.Constraints as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W.TxOut
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut.Gen as W
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS
import qualified Data.Foldable as F

spec :: Spec
spec = do
    describe "sizeOfCoin" $ do
        let coinToWord64Clamped = fromMaybe maxBound . W.Coin.toWord64Maybe
        let cborSizeOfCoin =
                W.TxSize
                . fromIntegral
                . BS.length
                . CBOR.toStrictByteString
                . CBOR.encodeWord64 . coinToWord64Clamped

        let isBoundary c =
                sizeOfCoin c /= sizeOfCoin (c <\> W.Coin 1)
                || sizeOfCoin c /= sizeOfCoin (c <> W.Coin 1)

        it "matches the size of the Word64 CBOR encoding" $
            property $ checkCoverage $
                forAll CardanoApi.genEncodingBoundaryLovelace $ \l -> do
                    let c = cardanoToWalletCoin l
                    let expected = cborSizeOfCoin c

                    -- Use a low coverage requirement of 0.01% just to
                    -- ensure we see /some/ amount of every size.
                    let coverSize s = cover 0.01 (s == expected) (show s)
                    sizeOfCoin c === expected
                        & coverSize (W.TxSize 1)
                        & coverSize (W.TxSize 2)
                        & coverSize (W.TxSize 3)
                        & coverSize (W.TxSize 5)
                        & coverSize (W.TxSize 9)
                        & cover 0.5 (isBoundary c) "boundary case"

        describe "boundary case goldens" $ do
            it "1 byte to 2 byte boundary" $ do
                sizeOfCoin (W.Coin 23) `shouldBe` W.TxSize 1
                sizeOfCoin (W.Coin 24) `shouldBe` W.TxSize 2
            it "2 byte to 3 byte boundary" $ do
                sizeOfCoin (W.Coin $ 2 `power` 8 - 1) `shouldBe` W.TxSize 2
                sizeOfCoin (W.Coin $ 2 `power` 8    ) `shouldBe` W.TxSize 3
            it "3 byte to 5 byte boundary" $ do
                sizeOfCoin (W.Coin $ 2 `power` 16 - 1) `shouldBe` W.TxSize 3
                sizeOfCoin (W.Coin $ 2 `power` 16    ) `shouldBe` W.TxSize 5
            it "5 byte to 9 byte boundary" $ do
                sizeOfCoin (W.Coin $ 2 `power` 32 - 1) `shouldBe` W.TxSize 5
                sizeOfCoin (W.Coin $ 2 `power` 32    ) `shouldBe` W.TxSize 9

    describe "costOfIncreasingCoin" $ do
        it "costs 176 lovelace to increase 4294.967295 ada (2^32 - 1 lovelace) \
           \by 1 lovelace on mainnet" $ do

            let expectedCostIncrease = W.Coin 176
            let mainnet = mainnetFeePerByte
            costOfIncreasingCoin mainnet (W.Coin $ 2 `power` 32 - 1) (W.Coin 1)
                `shouldBe` expectedCostIncrease

        it "produces results in the range [0, 8 * feePerByte]" $
            property $ \c increase -> do
                let res = costOfIncreasingCoin (FeePerByte 1) c increase
                counterexample (show res <> "out of bounds") $
                    res >= W.Coin 0 && res <= W.Coin 8

    describe "distributeSurplus" $ do

        it "prop_distributeSurplus_onSuccess_conservesSurplus" $
            prop_distributeSurplus_onSuccess_conservesSurplus
                & property
        it "prop_distributeSurplus_onSuccess_coversCostIncrease" $
            prop_distributeSurplus_onSuccess_coversCostIncrease
                & property
        it "prop_distributeSurplus_onSuccess_doesNotReduceChangeCoinValues" $
            prop_distributeSurplus_onSuccess_doesNotReduceChangeCoinValues
                & property
        it "prop_distributeSurplus_onSuccess_doesNotReduceFeeValue" $
            prop_distributeSurplus_onSuccess_doesNotReduceFeeValue
                & property
        it "prop_distributeSurplus_onSuccess_preservesChangeLength" $
            prop_distributeSurplus_onSuccess_preservesChangeLength
                & property
        it "prop_distributeSurplus_onSuccess_preservesChangeAddresses" $
            prop_distributeSurplus_onSuccess_preservesChangeAddresses
                & property
        it "prop_distributeSurplus_onSuccess_preservesChangeNonAdaAssets" $
            prop_distributeSurplus_onSuccess_preservesChangeNonAdaAssets
                & property
        it "prop_distributeSurplus_onSuccess_onlyAdjustsFirstChangeValue" $
            prop_distributeSurplus_onSuccess_onlyAdjustsFirstChangeValue
                & property
        it "prop_distributeSurplus_onSuccess_increasesValuesByDelta" $
            prop_distributeSurplus_onSuccess_increasesValuesByDelta
                & property

    describe "distributeSurplusDelta" $ do

        -- NOTE: The test values below make use of 255 being encoded as 2 bytes,
        -- and 256 as 3 bytes.

        describe "when increasing change increases fee" $
            it "will increase fee (99 lovelace for change, 1 for fee)" $
                distributeSurplusDelta
                    (FeePerByte 1)
                    (W.Coin 100)
                    (TxFeeAndChange (W.Coin 200) [W.Coin 200])
                    `shouldBe`
                    Right (TxFeeAndChange (W.Coin 1) [W.Coin 99])

        describe "when increasing fee increases fee" $
            it "will increase fee (98 lovelace for change, 2 for fee)" $ do
                distributeSurplusDelta
                    (FeePerByte 1)
                    (W.Coin 100)
                    (TxFeeAndChange (W.Coin 255) [W.Coin 200])
                    `shouldBe`
                    Right (TxFeeAndChange (W.Coin 2) [W.Coin 98])

        describe
            (unwords
                [ "when increasing the change costs more in fees than the"
                , "increase itself"
                ]) $ do
            it "will try burning the surplus as fees" $ do
                distributeSurplusDelta
                    mainnetFeePerByte
                    (W.Coin 10)
                    (TxFeeAndChange (W.Coin 200) [W.Coin 255])
                    `shouldBe`
                    Right (TxFeeAndChange (W.Coin 10) [W.Coin 0])

            it "will fail if neither the fee can be increased" $ do
                distributeSurplusDelta
                    mainnetFeePerByte
                    (W.Coin 10)
                    (TxFeeAndChange (W.Coin 255) [W.Coin 255])
                    `shouldBe`
                    Left (ErrMoreSurplusNeeded $ W.Coin 34)

        describe "when no change output is present" $ do
            it "will burn surplus as excess fees" $
                property $ \surplus fee0 -> do
                    distributeSurplusDelta
                        (FeePerByte 1)
                        surplus
                        (TxFeeAndChange fee0 [])
                        `shouldBe`
                        Right (TxFeeAndChange surplus [])

        it "prop_distributeSurplusDelta_coversCostIncreaseAndConservesSurplus" $
            prop_distributeSurplusDelta_coversCostIncreaseAndConservesSurplus
                & withMaxSuccess 10_000
                & property

-- A helper function to generate properties for 'distributeSurplus' on
-- success.
--
prop_distributeSurplus_onSuccess
    :: Testable prop
    => (FeePerByte
        -> W.Coin
        -> TxFeeAndChange [W.TxOut]
        -> TxFeeAndChange [W.TxOut]
        -> prop)
    -> FeePerByte
    -> TxBalanceSurplus W.Coin
    -> TxFeeAndChange [W.TxOut]
    -> Property
prop_distributeSurplus_onSuccess propertyToTest policy txSurplus fc =
    checkCoverage $
    cover 50
        (isRight mResult)
        "isRight mResult" $
    cover 10
        (length changeOriginal == 0)
        "length changeOriginal == 0" $
    cover 10
        (length changeOriginal == 1)
        "length changeOriginal == 1" $
    cover 50
        (length changeOriginal >= 2)
        "length changeOriginal >= 2" $
    cover 2
        (feeOriginal == W.Coin 0)
        "feeOriginal == W.Coin 0" $
    cover 2
        (feeOriginal == W.Coin 1)
        "feeOriginal == W.Coin 1" $
    cover 50
        (feeOriginal >= W.Coin 2)
        "feeOriginal >= W.Coin 2" $
    cover 1
        (surplus == W.Coin 0)
        "surplus == W.Coin 0" $
    cover 1
        (surplus == W.Coin 1)
        "surplus == W.Coin 1" $
    cover 50
        (surplus >= W.Coin 2)
        "surplus >= W.Coin 2" $
    either
        (const $ property True)
        (property . propertyToTest policy surplus fc)
        mResult
  where
    TxBalanceSurplus surplus = txSurplus
    TxFeeAndChange feeOriginal changeOriginal = fc

    mResult :: Either ErrMoreSurplusNeeded (TxFeeAndChange [W.TxOut])
    mResult = distributeSurplus policy surplus fc

-- Verifies that the 'distributeSurplus' function conserves the surplus: the
-- total increase in the fee and change ada quantities should be exactly equal
-- to the given surplus.
--
prop_distributeSurplus_onSuccess_conservesSurplus
    :: FeePerByte
    -> TxBalanceSurplus W.Coin
    -> TxFeeAndChange [W.TxOut]
    -> Property
prop_distributeSurplus_onSuccess_conservesSurplus =
    prop_distributeSurplus_onSuccess $ \_policy surplus
        (TxFeeAndChange feeOriginal changeOriginal)
        (TxFeeAndChange feeModified changeModified) ->
        surplus ===
            (feeModified <> F.foldMap W.TxOut.coin changeModified)
            <\>
            (feeOriginal <> F.foldMap W.TxOut.coin changeOriginal)

-- The 'distributeSurplus' function should cover the cost of any increases in
-- 'Coin' values.
--
-- If the total cost of encoding ada quantities has increased by 𝛿c, then the
-- fee value should have increased by at least 𝛿c.
--
prop_distributeSurplus_onSuccess_coversCostIncrease
    :: FeePerByte
    -> TxBalanceSurplus W.Coin
    -> TxFeeAndChange [W.TxOut]
    -> Property
prop_distributeSurplus_onSuccess_coversCostIncrease =
    prop_distributeSurplus_onSuccess $ \policy _surplus
        (TxFeeAndChange feeOriginal changeOriginal)
        (TxFeeAndChange feeModified changeModified) -> do
        let coinsOriginal = feeOriginal : (W.TxOut.coin <$> changeOriginal)
        let coinsModified = feeModified : (W.TxOut.coin <$> changeModified)
        let coinDeltas = zipWith (<\>) coinsModified coinsOriginal
        let costIncrease = F.foldMap
                (uncurry $ costOfIncreasingCoin policy)
                (coinsOriginal `zip` coinDeltas)
        feeModified <\> feeOriginal >= costIncrease
            & report feeModified "feeModified"
            & report feeOriginal "feeOriginal"
            & report costIncrease "costIncrease"

-- Since the 'distributeSurplus' function is not aware of the minimum ada
-- quantity or how to calculate it, it should never allow change ada values to
-- decrease.
--
prop_distributeSurplus_onSuccess_doesNotReduceChangeCoinValues
    :: FeePerByte
    -> TxBalanceSurplus W.Coin
    -> TxFeeAndChange [W.TxOut]
    -> Property
prop_distributeSurplus_onSuccess_doesNotReduceChangeCoinValues =
    prop_distributeSurplus_onSuccess $ \_policy _surplus
        (TxFeeAndChange _feeOriginal changeOriginal)
        (TxFeeAndChange _feeModified changeModified) ->
            all (uncurry (<=)) $ zip
                (W.TxOut.coin <$> changeOriginal)
                (W.TxOut.coin <$> changeModified)

-- The 'distributeSurplus' function should never return a 'fee' value that is
-- less than the original value.
--
prop_distributeSurplus_onSuccess_doesNotReduceFeeValue
    :: FeePerByte
    -> TxBalanceSurplus W.Coin
    -> TxFeeAndChange [W.TxOut]
    -> Property
prop_distributeSurplus_onSuccess_doesNotReduceFeeValue =
    prop_distributeSurplus_onSuccess $ \_policy _surplus
        (TxFeeAndChange feeOriginal _changeOriginal)
        (TxFeeAndChange feeModified _changeModified) ->
            feeOriginal <= feeModified

-- The 'distributeSurplus' function should increase values by the exact amounts
-- indicated in 'distributeSurplusDelta'.
--
-- This is actually an implementation detail of 'distributeSurplus'.
--
-- However, it's useful to verify that this is true by subtracting the delta
-- values from the result of 'distributeSurplus', which should produce the
-- original fee and change values.
--
prop_distributeSurplus_onSuccess_increasesValuesByDelta
    :: FeePerByte
    -> TxBalanceSurplus W.Coin
    -> TxFeeAndChange [W.TxOut]
    -> Property
prop_distributeSurplus_onSuccess_increasesValuesByDelta =
    prop_distributeSurplus_onSuccess $ \policy surplus
        (TxFeeAndChange feeOriginal changeOriginal)
        (TxFeeAndChange feeModified changeModified) ->
            let (TxFeeAndChange feeDelta changeDeltas) =
                    either (error . show) id
                    $ distributeSurplusDelta policy surplus
                    $ TxFeeAndChange
                        (feeOriginal)
                        (W.TxOut.coin <$> changeOriginal)
            in
            (TxFeeAndChange
                (feeModified <\> feeDelta)
                (zipWith W.TxOut.subtractCoin changeDeltas changeModified)
            )
            ===
            TxFeeAndChange feeOriginal changeOriginal

-- The 'distributeSurplus' function should only adjust the very first change
-- value.  All other change values should be left untouched.
--
-- This is actually an implementation detail of 'distributeSurplus'.
--
-- In principle, 'distributeSurplus' could allow itself to adjust any of the
-- change values in order to find a (marginally) more optimal solution.
-- However, for reasons of simplicity, we only adjust the first change value.
--
-- Here we verify that the implementation indeed only adjusts the first change
-- value, as expected.
--
prop_distributeSurplus_onSuccess_onlyAdjustsFirstChangeValue
    :: FeePerByte
    -> TxBalanceSurplus W.Coin
    -> TxFeeAndChange [W.TxOut]
    -> Property
prop_distributeSurplus_onSuccess_onlyAdjustsFirstChangeValue =
    prop_distributeSurplus_onSuccess $ \_policy _surplus
        (TxFeeAndChange _feeOriginal changeOriginal)
        (TxFeeAndChange _feeModified changeModified) ->
            (drop 1 changeOriginal) ===
            (drop 1 changeModified)

-- The 'distributeSurplus' function should never adjust addresses of change
-- outputs.
--
prop_distributeSurplus_onSuccess_preservesChangeAddresses
    :: FeePerByte
    -> TxBalanceSurplus W.Coin
    -> TxFeeAndChange [W.TxOut]
    -> Property
prop_distributeSurplus_onSuccess_preservesChangeAddresses =
    prop_distributeSurplus_onSuccess $ \_policy _surplus
        (TxFeeAndChange _feeOriginal changeOriginal)
        (TxFeeAndChange _feeModified changeModified) ->
            (view #address <$> changeOriginal) ===
            (view #address <$> changeModified)

-- The 'distributeSurplus' function should always return exactly the same
-- number of change outputs that it was given. It should never create or
-- destroy change outputs.
--
prop_distributeSurplus_onSuccess_preservesChangeLength
    :: FeePerByte
    -> TxBalanceSurplus W.Coin
    -> TxFeeAndChange [W.TxOut]
    -> Property
prop_distributeSurplus_onSuccess_preservesChangeLength =
    prop_distributeSurplus_onSuccess $ \_policy _surplus
        (TxFeeAndChange _feeOriginal changeOriginal)
        (TxFeeAndChange _feeModified changeModified) ->
            length changeOriginal === length changeModified

-- The 'distributeSurplus' function should never adjust the values of non-ada
-- assets.
--
prop_distributeSurplus_onSuccess_preservesChangeNonAdaAssets
    :: FeePerByte
    -> TxBalanceSurplus W.Coin
    -> TxFeeAndChange [W.TxOut]
    -> Property
prop_distributeSurplus_onSuccess_preservesChangeNonAdaAssets =
    prop_distributeSurplus_onSuccess $ \_policy _surplus
        (TxFeeAndChange _feeOriginal changeOriginal)
        (TxFeeAndChange _feeModified changeModified) ->
            (view (#tokens . #tokens) <$> changeOriginal) ===
            (view (#tokens . #tokens) <$> changeModified)

-- Verify that 'distributeSurplusDelta':
--
--    - covers the increase to the fee requirement incurred as a result of
--      increasing the fee value and change values.
--
--    - conserves the surplus:
--        - feeDelta + sum changeDeltas == surplus
--
prop_distributeSurplusDelta_coversCostIncreaseAndConservesSurplus
    :: FeePerByte -> W.Coin -> W.Coin -> [W.Coin] -> Property
prop_distributeSurplusDelta_coversCostIncreaseAndConservesSurplus
    feePolicy surplus fee0 change0 =
    checkCoverage $
    cover 2  (isLeft  mres) "Failure" $
    cover 50 (isRight mres) "Success" $
    report mres "Result" $
    counterexample (show mres) $ case mres of
        Left (ErrMoreSurplusNeeded shortfall) ->
            conjoin
                [ property $ surplus < maxCoinCostIncrease
                , property $ shortfall > W.Coin 0
                , costOfIncreasingCoin feePolicy fee0 surplus
                    === surplus <> shortfall
                ]
        Right (TxFeeAndChange feeDelta changeDeltas) -> do
            let feeRequirementIncrease = mconcat
                    [ costOfIncreasingCoin feePolicy fee0 feeDelta
                    , F.fold $ zipWith (costOfIncreasingCoin feePolicy)
                        change0
                        changeDeltas
                    ]
            conjoin
                [ property $ feeDelta >= feeRequirementIncrease
                    & counterexample ("fee requirement increased by "
                        <> show feeRequirementIncrease
                        <> " but the fee delta was just "
                        <> show feeDelta
                        )
                , F.fold changeDeltas <> feeDelta
                    === surplus
                ]
  where
    mres = distributeSurplusDelta
        feePolicy surplus (TxFeeAndChange fee0 change0)
    maxCoinCostIncrease = maximumCostOfIncreasingCoin feePolicy

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

cardanoToWalletCoin :: CardanoApi.Lovelace -> W.Coin
cardanoToWalletCoin = Convert.toWallet . CardanoApi.toShelleyLovelace

mainnetFeePerByte :: FeePerByte
mainnetFeePerByte = FeePerByte 44

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

newtype TxBalanceSurplus a = TxBalanceSurplus {unTxBalanceSurplus :: a}
    deriving (Eq, Show)

instance Arbitrary (TxBalanceSurplus W.Coin) where
    -- We want to test cases where the surplus is zero. So it's important that
    -- we do not restrict ourselves to positive coins here.
    arbitrary = TxBalanceSurplus <$> frequency
        [ (8, W.genCoin)
        , (4, W.genCoin & scale (* (2 `power`  4)))
        , (2, W.genCoin & scale (* (2 `power`  8)))
        , (1, W.genCoin & scale (* (2 `power` 16)))
        ]
    shrink = shrinkMapBy TxBalanceSurplus unTxBalanceSurplus W.shrinkCoin


instance Arbitrary (TxFeeAndChange [W.TxOut]) where
    arbitrary = do
        fee <- W.genCoin
        change <- frequency
            [ (1, pure [])
            , (1, (: []) <$> W.genTxOut)
            , (6, listOf W.genTxOut)
            ]
        pure $ TxFeeAndChange{fee, change}
    shrink (TxFeeAndChange fee change) =
        uncurry TxFeeAndChange <$> liftShrink2
            (W.shrinkCoin)
            (shrinkList W.shrinkTxOut)
            (fee, change)

instance Arbitrary W.Coin where
    arbitrary = W.genCoinPositive
    shrink = W.shrinkCoinPositive

instance Arbitrary FeePerByte where
    arbitrary = frequency
        [ (1, pure mainnetFeePerByte)
        , (7, FeePerByte <$> arbitrarySizedNatural)
        ]
    shrink (FeePerByte x) =
        FeePerByte <$> shrinkNatural x

