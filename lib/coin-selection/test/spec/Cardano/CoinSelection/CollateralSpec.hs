{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Hoist not" -}

{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
module Cardano.CoinSelection.CollateralSpec where

import Prelude hiding
    ( sequence )

import Cardano.CoinSelection.Collateral
    ( PerformSelection
    , SearchSpaceLimit (..)
    , SearchSpaceRequirement (..)
    , SelectionCollateralError (..)
    , SelectionConstraints (..)
    , SelectionParams (..)
    , SelectionResult (..)
    , firstRight
    , guardSearchSpaceSize
    , numberOfSubsequencesOfSize
    , performSelection
    , searchSpaceLimitDefault
    , selectCollateralLargest
    , selectCollateralSmallest
    , submaps
    , subsequencesOfSize
    , takeUntil
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoinPositive, shrinkCoinPositive )
import Control.Monad
    ( forM_ )
import Data.Either
    ( isLeft, isRight )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.IntCast
    ( intCast )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Set
    ( Set )
import Data.Word
    ( Word64 )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , NonNegative (..)
    , Property
    , arbitraryBoundedEnum
    , checkCoverage
    , choose
    , conjoin
    , counterexample
    , cover
    , coverTable
    , forAll
    , frequency
    , genericShrink
    , property
    , scale
    , shuffle
    , sized
    , tabulate
    , (===)
    )
import Test.QuickCheck.Quid
    ( Hexadecimal (..), Quid )
import Text.Pretty.Simple
    ( pShow )

import qualified Data.Bits as Bits
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy as TL

spec :: Spec
spec = do

    describe "performSelection" $ do

        it "prop_performSelection_general" $
            property prop_performSelection_general

    describe "selectCollateralSmallest" $ do

        it "prop_selectCollateralSmallest_general" $
            property prop_selectCollateralSmallest_general
        it "prop_selectCollateralSmallest_optimal" $
            property prop_selectCollateralSmallest_optimal
        it "prop_selectCollateralSmallest_constrainedSelectionCount" $
            property prop_selectCollateralSmallest_constrainedSelectionCount

        unitTests_selectCollateralSmallest_optimal
        unitTests_selectCollateralSmallest_constrainedSelectionCount
        unitTests_selectCollateralSmallest_constrainedSearchSpace

    describe "selectCollateralLargest" $ do

        it "prop_selectCollateralLargest_general" $
            property prop_selectCollateralLargest_general
        it "prop_selectCollateralLargest_optimal" $
            property prop_selectCollateralLargest_optimal

        unitTests_selectCollateralLargest_optimal
        unitTests_selectCollateralLargest_insufficient

    describe "SingleBitCoinMap" $ do

        it "prop_genSingleBitCoinMap" $
            property prop_genSingleBitCoinMap
        it "prop_shrinkSingleBitCoinMap" $
            property prop_shrinkSingleBitCoinMap

    describe "shrinkListToPrefixes" $ do

        it "prop_shrinkListToPrefixes" $
            property prop_shrinkListToPrefixes

    describe "submaps" $ do

        it "prop_submaps_isSubmapOf" $
            property prop_submaps_isSubmapOf
        it "prop_submaps_size" $
            property prop_submaps_size
        it "prop_submaps_unions" $
            property prop_submaps_unions

        unitTests_submaps

    describe "subsequencesOfSize" $ do

        it "prop_subsequencesOfSize" $
            property prop_subsequencesOfSize

        unitTests_subsequencesOfSize
        unitTests_numberOfSubsequencesOfSize_withinBounds
        unitTests_numberOfSubsequencesOfSize_bounds
        unitTests_numberOfSubsequencesOfSize_outOfBounds

    describe "firstRight" $ do

        it "prop_firstRight" $
            property prop_firstRight

    describe "takeUntil" $ do

        it "prop_takeUntil_identity" $
            property prop_takeUntil_identity
        it "prop_takeUntil_head" $
            property prop_takeUntil_head
        it "prop_takeUntil_takeWhile" $
            property prop_takeUntil_takeWhile

        unitTests_takeUntil

    describe "guardSearchSpaceSize" $ do

        unitTests_guardSearchSpaceSize

--------------------------------------------------------------------------------
-- Properties that are general to all collateral selection strategies
--------------------------------------------------------------------------------

-- Using the given collateral selection function, tests that general properties
-- hold for a wide variety of:
--
--  - available coin maps
--  - minimum selection amounts
--  - maximum selection sizes
--
prop_performSelection_general_withFunction
    :: PerformSelection LongInputId -> Property
prop_performSelection_general_withFunction performSelectionFn =
    checkCoverage $
    forAll (arbitrary @(Map LongInputId Coin))
        $ \coinsAvailable ->
    forAll (scale (* 4) genMinimumSelectionAmount)
        $ \(MinimumSelectionAmount minimumSelectionAmount) ->
    forAll (choose (1, 4))
        $ \maximumSelectionSize ->
    let constraints = SelectionConstraints
            { maximumSelectionSize
            , searchSpaceLimit = searchSpaceLimitDefault
            } in
    let params = SelectionParams
            { coinsAvailable
            , minimumSelectionAmount
            } in
    prop_performSelection_general_withResult constraints params $
        performSelectionFn constraints params

prop_performSelection_general_withResult
    :: (Ord inputId, Show inputId)
    => SelectionConstraints
    -> SelectionParams inputId
    -> Either (SelectionCollateralError inputId) (SelectionResult inputId)
    -> Property
prop_performSelection_general_withResult constraints params eitherErrorResult =
    cover 20.0 (isLeft  eitherErrorResult) "Failure" $
    cover 20.0 (isRight eitherErrorResult) "Success" $
    counterexample ("Params: " <> show (Pretty params)) $
    either
        (prop_performSelection_onFailure constraints params)
        (prop_performSelection_onSuccess constraints params)
        (eitherErrorResult)

prop_performSelection_onFailure
    :: (Ord inputId, Show inputId)
    => SelectionConstraints
    -> SelectionParams inputId
    -> SelectionCollateralError inputId
    -> Property
prop_performSelection_onFailure constraints params err =
    counterexample ("Error: " <> show (Pretty err)) $
    conjoin
        [ F.fold (largestCombinationAvailable err)
            < view #minimumSelectionAmount params
        , F.length (largestCombinationAvailable err)
            <= maximumSelectionSize constraints
        , largestCombinationAvailable err
            `Map.isSubmapOf` coinsAvailable params
        ]

prop_performSelection_onSuccess
    :: (Ord inputId, Show inputId)
    => SelectionConstraints
    -> SelectionParams inputId
    -> SelectionResult inputId
    -> Property
prop_performSelection_onSuccess constraints params result =
    counterexample ("Result: " <> show (Pretty result)) $
    conjoin
        [ F.fold (coinsAvailable params)
            >= view #minimumSelectionAmount params
        , F.fold (coinsSelected result)
            >= view #minimumSelectionAmount params
        , F.length (coinsSelected result)
            <= maximumSelectionSize constraints
        , coinsSelected result
            `Map.isSubmapOf` coinsAvailable params
        ]

--------------------------------------------------------------------------------
-- Selecting collateral (top-level function)
--------------------------------------------------------------------------------

-- Tests that general properties hold for 'performSelection'.
--
prop_performSelection_general :: Property
prop_performSelection_general =
    prop_performSelection_general_withFunction performSelection

--------------------------------------------------------------------------------
-- Selecting collateral by giving priority to smallest values first
--------------------------------------------------------------------------------

-- Tests that general properties hold for 'selectCollateralSmallest'.
--
prop_selectCollateralSmallest_general :: Property
prop_selectCollateralSmallest_general =
    prop_performSelection_general_withFunction selectCollateralSmallest

-- In this test, we only consider sets of available coins that when sorted into
-- ascending order are prefixes of the following sequence, consisting of the
-- powers of two sorted into ascending order:
--
--     [2^0, 2^1, 2^2, 2^3, ...] = [1, 2, 4, 8, ...]
--
-- When expressed in binary, the powers of two have the following form:
--
--     [1, 10, 100, 1000, ...]
--
-- When considering a given minimum selection amount 'm', we can determine
-- which binary bits within 'm' are set to '1', and how many.
--
-- In the case where we request an optimal selection (one where the total
-- selected value is minimized), the above information allows us to predict:
--
--    - the expected number of selected coins;
--    - the expected total value of the selected coins;
--    - the expected identities of the selected coins.
--
-- This property provides us with a way to test that selections are optimal
-- without having to reproduce parts of the selection algorithm in the test.
--
-- Example:
--
-- Consider a minimum selection amount of 42. This is equivalent to the binary
-- value 101010.
--
-- From this information alone, we can expect that the optimal selection:
--
--    - has exactly three coins;
--    - has a total value of 42;
--    - consists of the coins with binary values {10, 1000, 100000}.
--
prop_selectCollateralSmallest_optimal
    :: SingleBitCoinMap
    -> MinimumSelectionAmount
    -> Property
prop_selectCollateralSmallest_optimal
    (SingleBitCoinMap coinsAvailable)
    (MinimumSelectionAmount minimumSelectionAmount) =
    checkCoverage $
    conjoin
        [ prop_performSelection_general_withResult
            constraints params eitherErrorResult
        , prop_extra
        ]
  where
    prop_extra :: Property
    prop_extra =
        coverOptimalCoinCount $
        case eitherErrorResult of
            Left _ -> conjoin
                [ F.fold coinsAvailable < minimumSelectionAmount
                ]
            Right r -> conjoin
                [ F.length (coinsSelected r) == optimalCoinCount
                , F.fold   (coinsSelected r) == minimumSelectionAmount
                ]

    constraints = SelectionConstraints
        { maximumSelectionSize
        , searchSpaceLimit = UnsafeNoSearchSpaceLimit
        }
    params = SelectionParams
        { coinsAvailable
        , minimumSelectionAmount
        }

    eitherErrorResult = selectCollateralSmallest constraints params

    -- Specify a maximum number of collateral entries that makes it possible to
    -- make a selection that's exactly equal to the minimum collateral amount:
    maximumSelectionSize :: Int
    maximumSelectionSize = optimalCoinCount

    optimalCoinCount :: Int
    optimalCoinCount = Bits.popCount (unCoin minimumSelectionAmount)

    coverOptimalCoinCount
        = coverTable title ((, 8.0) . show @Int <$> [1 .. 4])
        . tabulate title [show optimalCoinCount]
      where
        title = "Optimal coin count"

-- This test is similar to 'prop_selectCollateralSmallest_optimal', except that
-- we deliberately constrain the maximum selection count so that it is not
-- possible to produce an optimal selection (one where the total selected value
-- is minimized).
--
-- With this constraint in place, we can expect that the returned selection has
-- a total value that is greater than the minimum selection amount.
--
prop_selectCollateralSmallest_constrainedSelectionCount
    :: SingleBitCoinMap
    -> MinimumSelectionAmount
    -> Property
prop_selectCollateralSmallest_constrainedSelectionCount
    (SingleBitCoinMap coinsAvailable)
    (MinimumSelectionAmount minimumSelectionAmount) =
    checkCoverage $
    conjoin
        [ prop_performSelection_general_withResult
            constraints params eitherErrorResult
        , prop_extra
        ]
  where
    prop_extra :: Property
    prop_extra =
        coverOptimalCoinCount $
        case eitherErrorResult of
            Left _ ->
                -- No extra conditions in addition to standard conditions.
                property True
            Right r ->
                -- Check that we cover a range of selected coin counts:
                let numberOfCoinsSelected = F.length (coinsSelected r) in
                cover 10.0 (numberOfCoinsSelected == 1)
                    "Number of coins selected = 1" $
                cover 10.0 (numberOfCoinsSelected == 2)
                    "Number of coins selected = 2" $
                cover 10.0 (numberOfCoinsSelected == maximumSelectionSize)
                    "Number of coins selected = maximum allowed" $
                conjoin
                    [ Map.size (coinsSelected r)
                        < optimalCoinCount
                    , F.fold (coinsSelected r)
                        > minimumSelectionAmount
                    , F.fold (coinsSelected r)
                        < minimumSelectionAmount `scaleCoin` 2
                    ]

    constraints = SelectionConstraints
        { maximumSelectionSize
        , searchSpaceLimit = UnsafeNoSearchSpaceLimit
        }
    params = SelectionParams
        { coinsAvailable
        , minimumSelectionAmount
        }

    eitherErrorResult = selectCollateralSmallest constraints params

    -- Deliberately constrain the maximum number of collateral entries so that
    -- it's impossible to make a selection that's exactly equal to the minimum
    -- collateral amount:
    maximumSelectionSize :: Int
    maximumSelectionSize = optimalCoinCount - 1

    optimalCoinCount :: Int
    optimalCoinCount = Bits.popCount (unCoin minimumSelectionAmount)

    coverOptimalCoinCount
        = coverTable title ((, 8.0) . show @Int <$> [1 .. 4])
        . tabulate title [show optimalCoinCount]
      where
        title = "Optimal coin count"

unitTests_selectCollateralSmallest_optimal :: Spec
unitTests_selectCollateralSmallest_optimal = unitTests
    "unitTests_selectCollateralSmallest_optimal"
    (uncurry selectCollateralSmallest)
    (mkTest <$> tests)
  where
    coinsAvailable =
        [A ▶ 1, B ▶ 2, C ▶ 4, D ▶ 8, E ▶ 16, F ▶ 32, G ▶ 64, H ▶ 128]
    mkTest (minimumSelectionAmount, coinsSelected) = UnitTestData
        { params =
            ( SelectionConstraints
                { maximumSelectionSize = Map.size coinsAvailable
                , searchSpaceLimit = UnsafeNoSearchSpaceLimit
                }
            , SelectionParams
                { coinsAvailable = Coin <$> coinsAvailable
                , minimumSelectionAmount = Coin minimumSelectionAmount
                }
            )
        , result = Right $ SelectionResult $ Coin <$> coinsSelected
        }
    tests =
        [ (1, [A ▶ 1                    ])
        , (2, [       B ▶ 2             ])
        , (3, [A ▶ 1, B ▶ 2             ])
        , (4, [              C ▶ 4      ])
        , (5, [A ▶ 1,        C ▶ 4      ])
        , (6, [       B ▶ 2, C ▶ 4      ])
        , (7, [A ▶ 1, B ▶ 2, C ▶ 4      ])
        , (8, [                    D ▶ 8])
        ]

unitTests_selectCollateralSmallest_constrainedSelectionCount :: Spec
unitTests_selectCollateralSmallest_constrainedSelectionCount = unitTests
    "unitTests_selectCollateralSmallest_constrainedSelectionCount"
    (uncurry selectCollateralSmallest)
    (mkTest <$> tests)
  where
    coinsAvailable =
        [A ▶ 1, B ▶ 2, C ▶ 4, D ▶ 8, E ▶ 16, F ▶ 32, G ▶ 64, H ▶ 128]
    mkTest (minimumSelectionAmount, coinsSelected) = UnitTestData
        { params =
            ( SelectionConstraints
                { maximumSelectionSize = 1
                , searchSpaceLimit = UnsafeNoSearchSpaceLimit
                }
            , SelectionParams
                { coinsAvailable = Coin <$> coinsAvailable
                , minimumSelectionAmount = Coin minimumSelectionAmount
                }
            )
        , result = Right $ SelectionResult $ Coin <$> coinsSelected
        }
    tests =
        [ (1, [A ▶ 1])
        , (2, [B ▶ 2])
        , (3, [C ▶ 4])
        , (4, [C ▶ 4])
        , (5, [D ▶ 8])
        , (6, [D ▶ 8])
        , (7, [D ▶ 8])
        , (8, [D ▶ 8])
        ]

unitTests_selectCollateralSmallest_constrainedSearchSpace :: Spec
unitTests_selectCollateralSmallest_constrainedSearchSpace = unitTests
    "unitTests_selectCollateralSmallest_constrainedSearchSpace"
    (uncurry selectCollateralSmallest)
    (mkTest <$> tests)
  where
    coinsAvailable =
        [A ▶ 1, B ▶ 2, C ▶ 4, D ▶ 8, E ▶ 16, F ▶ 32, G ▶ 64, H ▶ 128]
    maximumSelectionSize =
        Map.size coinsAvailable
    Just searchSpaceLimit = SearchSpaceLimit <$>
        maximumSelectionSize `numberOfSubsequencesOfSize` 2
    mkTest (minimumSelectionAmount, coinsSelected) = UnitTestData
        { params =
            ( SelectionConstraints
                { maximumSelectionSize
                , searchSpaceLimit
                }
            , SelectionParams
                { coinsAvailable = Coin <$> coinsAvailable
                , minimumSelectionAmount = Coin minimumSelectionAmount
                }
            )
        , result = Right $ SelectionResult $ Coin <$> coinsSelected
        }
    tests =
        [ (129, [A ▶ 1, H ▶ 128])
        , (130, [B ▶ 2, H ▶ 128])
        , (131, [C ▶ 4, H ▶ 128])
        , (132, [C ▶ 4, H ▶ 128])
        , (133, [D ▶ 8, H ▶ 128])
        , (134, [D ▶ 8, H ▶ 128])
        , (135, [D ▶ 8, H ▶ 128])
        , (136, [D ▶ 8, H ▶ 128])
        ]

--------------------------------------------------------------------------------
-- Selecting collateral by giving priority to largest values first
--------------------------------------------------------------------------------

-- Tests that general properties hold for 'selectCollateralLargest'.
--
prop_selectCollateralLargest_general :: Property
prop_selectCollateralLargest_general =
    prop_performSelection_general_withFunction selectCollateralLargest

-- In this test, we test that 'selectCollateralLargest' only fails if:
--
--    sum of available coins < minimum selection amount
--
-- (This assertion is not part of the general properties that apply to all
-- selection strategies, so we must test it separately.)
--
prop_selectCollateralLargest_optimal
    :: SingleBitCoinMap
    -> MinimumSelectionAmount
    -> Property
prop_selectCollateralLargest_optimal
    (SingleBitCoinMap coinsAvailable)
    (MinimumSelectionAmount minimumSelectionAmount) =
    checkCoverage $
    conjoin
        [ prop_performSelection_general_withResult
            constraints params eitherErrorResult
        , prop_extra
        ]
  where
    prop_extra :: Property
    prop_extra =
        case eitherErrorResult of
            Left _ ->
                property $ F.fold coinsAvailable < minimumSelectionAmount
            Right r ->
                -- Check that we cover a range of selected coin counts:
                let numberOfCoinsSelected = F.length (coinsSelected r) in
                cover 10.0 (numberOfCoinsSelected == 1)
                    "Number of coins selected = 1" $
                cover 10.0 (numberOfCoinsSelected == 2)
                    "Number of coins selected = 2" $
                cover 10.0 (numberOfCoinsSelected == maximumSelectionSize)
                    "Number of coins selected = maximum allowed" $
                property $ F.fold coinsAvailable >= minimumSelectionAmount

    eitherErrorResult = selectCollateralLargest constraints params

    constraints = SelectionConstraints
        { maximumSelectionSize
        , searchSpaceLimit = UnsafeNoSearchSpaceLimit
        }
    params = SelectionParams
        { coinsAvailable
        , minimumSelectionAmount
        }

    maximumSelectionSize :: Int
    maximumSelectionSize = optimalCoinCount

    optimalCoinCount :: Int
    optimalCoinCount = Bits.popCount (unCoin minimumSelectionAmount)

unitTests_selectCollateralLargest_optimal :: Spec
unitTests_selectCollateralLargest_optimal = unitTests
    "unitTests_selectCollateralLargest_optimal"
    (uncurry selectCollateralLargest)
    (mkTest <$> tests)
  where
    coinsAvailable =
        [A ▶ 1, B ▶ 2, C ▶ 4, D ▶ 8, E ▶ 16, F ▶ 32, G ▶ 64, H ▶ 128]
    mkTest (minimumSelectionAmount, coinsSelected) = UnitTestData
        { params =
            ( SelectionConstraints
                { maximumSelectionSize = 3
                , searchSpaceLimit = UnsafeNoSearchSpaceLimit
                }
            , SelectionParams
                { coinsAvailable = Coin <$> coinsAvailable
                , minimumSelectionAmount = Coin minimumSelectionAmount
                }
            )
        , result = Right $ SelectionResult $ Coin <$> coinsSelected
        }
    tests =
        [ (224, [F ▶ 32, G ▶ 64, H ▶ 128])
        , (192, [        G ▶ 64, H ▶ 128])
        , (160, [F ▶ 32,         H ▶ 128])
        , (128, [                H ▶ 128])
        , ( 96, [F ▶ 32, G ▶ 64         ])
        , ( 64, [        G ▶ 64         ])
        , ( 32, [F ▶ 32                 ])
        , ( 16, [F ▶ 32                 ])
        , (  8, [F ▶ 32                 ])
        , (  4, [F ▶ 32                 ])
        , (  2, [F ▶ 32                 ])
        , (  1, [F ▶ 32                 ])
        ]

unitTests_selectCollateralLargest_insufficient :: Spec
unitTests_selectCollateralLargest_insufficient = unitTests
    "unitTests_selectCollateralLargest_insufficient"
    (uncurry selectCollateralLargest)
    (mkTest <$> tests)
  where
    coinsAvailable =
        [A ▶ 1, B ▶ 2, C ▶ 4, D ▶ 8, E ▶ 16, F ▶ 32, G ▶ 64, H ▶ 128]
    mkTest (minimumSelectionAmount, largestCombinationAvailable) = UnitTestData
        { params =
            ( SelectionConstraints
                { maximumSelectionSize = 3
                , searchSpaceLimit = UnsafeNoSearchSpaceLimit
                }
            , SelectionParams
                { coinsAvailable = Coin <$> coinsAvailable
                , minimumSelectionAmount = Coin minimumSelectionAmount
                }
            )
        , result = Left SelectionCollateralError
            { largestCombinationAvailable =
                Coin <$> largestCombinationAvailable
            , minimumSelectionAmount =
                Coin minimumSelectionAmount
            }
        }
    tests =
        [ ( 225, [F ▶ 32, G ▶ 64, H ▶ 128])
        , ( 256, [F ▶ 32, G ▶ 64, H ▶ 128])
        , ( 512, [F ▶ 32, G ▶ 64, H ▶ 128])
        , (1_024, [F ▶ 32, G ▶ 64, H ▶ 128])
        ]

--------------------------------------------------------------------------------
-- Maps with single-bit coins (coins that are powers of two)
--------------------------------------------------------------------------------

-- | Represents a map of coins whose values are unique powers of two.
--
-- Maps of this type have coin values that when sorted into ascending order are
-- prefixes of the following sequence, consisting of the powers of two sorted
-- into ascending order:
--
--     [2^0, 2^1, 2^2, 2^3, ...] = [1, 2, 4, 8, ...]
--
-- Input identifiers are assigned randomly.
--
newtype SingleBitCoinMap = SingleBitCoinMap
    { unSingleBitCoinMap :: Map ShortInputId Coin
    }
    deriving (Eq, Show)

instance Arbitrary SingleBitCoinMap where
    arbitrary = genSingleBitCoinMap
    shrink = shrinkSingleBitCoinMap

allSingleBitCoins :: [Coin]
allSingleBitCoins = Coin . ((2 :: Natural) ^) <$> [0 :: Natural .. ]

genSingleBitCoinMap :: Gen SingleBitCoinMap
genSingleBitCoinMap = sized $ \size -> do
    maxCoin <- Coin . fromIntegral <$> choose (0, size)
    let singleBitCoins = L.takeWhile (<= maxCoin) allSingleBitCoins
    inputIds <- shuffle (take (length singleBitCoins) [A .. ])
    pure $ SingleBitCoinMap $ Map.fromList $ zip inputIds singleBitCoins

shrinkSingleBitCoinMap :: SingleBitCoinMap -> [SingleBitCoinMap]
shrinkSingleBitCoinMap (SingleBitCoinMap m) =
    SingleBitCoinMap . Map.fromList <$>
        shrinkListToPrefixes (L.sortOn snd (Map.toList m))

prop_genSingleBitCoinMap :: SingleBitCoinMap -> Property
prop_genSingleBitCoinMap (SingleBitCoinMap m) = conjoin
    [ F.all ((== 1) . Bits.popCount . unCoin) m
    , Bits.popCount (unCoin (F.fold m) + 1) == 1
    ]

prop_shrinkSingleBitCoinMap :: SingleBitCoinMap -> Property
prop_shrinkSingleBitCoinMap m =
    conjoin (prop_shrinkSingleBitCoinMap <$> shrinkSingleBitCoinMap m)

--------------------------------------------------------------------------------
-- Minimum selection amounts
--------------------------------------------------------------------------------

-- | Represents a minimum selection amount.
--
-- Minimum selection amounts are always strictly positive (non-zero).
--
newtype MinimumSelectionAmount = MinimumSelectionAmount
    { unMinimumSelectionAmount :: Coin }
    deriving (Eq, Ord, Show)

instance Arbitrary MinimumSelectionAmount where
    arbitrary = genMinimumSelectionAmount
    shrink = shrinkMinimumSelectionAmount

genMinimumSelectionAmount :: Gen MinimumSelectionAmount
genMinimumSelectionAmount = MinimumSelectionAmount <$> genCoinPositive

shrinkMinimumSelectionAmount
    :: MinimumSelectionAmount -> [MinimumSelectionAmount]
shrinkMinimumSelectionAmount (MinimumSelectionAmount c) =
    MinimumSelectionAmount <$> shrinkCoinPositive c

--------------------------------------------------------------------------------
-- Shrinking lists to prefixes
--------------------------------------------------------------------------------

-- | Shrinks a list to a sequence of shorter prefixes.
--
shrinkListToPrefixes :: [a] -> [[a]]
shrinkListToPrefixes xs
    | n <= 1 =
        []
    | otherwise =
        flip take xs <$> lengths
  where
    n = length xs
    lengths = L.nub
        [ 0
        , 1
        , n `div` 8
        , n `div` 4
        , n `div` 2
        , n - 1
        ]

prop_shrinkListToPrefixes :: [Int] -> Property
prop_shrinkListToPrefixes xs =
    property $ all (`L.isPrefixOf` xs) (shrinkListToPrefixes xs)

--------------------------------------------------------------------------------
-- Submaps
--------------------------------------------------------------------------------

prop_submaps_isSubmapOf :: Property
prop_submaps_isSubmapOf =
    forAll (scale (`mod` 8) (arbitrary @(Map Int Int))) $ \m ->
    property $ all (`Map.isSubmapOf` m) (submaps m)

prop_submaps_size :: Property
prop_submaps_size =
    forAll (scale (`mod` 8) (arbitrary @(Map Int Int))) $ \m ->
    property $ Set.size (submaps m) == 2 ^ (Map.size m)

prop_submaps_unions :: Property
prop_submaps_unions =
    forAll (scale (`mod` 8) (arbitrary @(Map Int Int))) $ \m ->
    property $ Map.unions (Set.toList (submaps m)) == m

unitTests_submaps :: Spec
unitTests_submaps = unitTests
    "unitTests_submaps"
    (submaps @ShortInputId @Int)
    (mkTest <$> tests)
  where
    mkTest (params, result) = UnitTestData {params, result}
    tests =
        [ ( [    ]
          , [ [] ]
          )
        , (   [A ▶ 1, B ▶ 2]
          , [ [            ]
            , [       B ▶ 2]
            , [A ▶ 1       ]
            , [A ▶ 1, B ▶ 2]
            ]
          )
        , (   [A ▶ 1, B ▶ 2, C ▶ 3]
          , [ [                   ]
            , [              C ▶ 3]
            , [       B ▶ 2       ]
            , [       B ▶ 2, C ▶ 3]
            , [A ▶ 1              ]
            , [A ▶ 1,        C ▶ 3]
            , [A ▶ 1, B ▶ 2       ]
            , [A ▶ 1, B ▶ 2, C ▶ 3]
            ]
          )
        ]

--------------------------------------------------------------------------------
-- Subsequences
--------------------------------------------------------------------------------

prop_subsequencesOfSize :: Property
prop_subsequencesOfSize =
    forAll (scale (`div` 4) (arbitrary @(Set Int))) $
        \xs ->
    forAll (scale (`mod` 4) (arbitrary @(NonNegative Int))) $
        \(NonNegative k) ->
    prop xs k
  where
    prop xs k =
        checkCoverage $

        -- Values of n and k relative to one another:
        cover 20.0 (n > 0 && k > 0 && n > k)
            "n > 0 && k > 0 && n >= k" $
        cover 1.00 (n > 0 && k > 0 && n == k)
            "n > 0 && k > 0 && n == k" $
        cover 0.10 (n > 0 && k > 0 && n < k)
            "n > 0 && k > 0 && n < k" $

        -- Values of n:
        coverTable "n" ((, 2.0) . show @Int <$> [0 .. 9]) $
        tabulate "n" [show n] $

        -- Values of k:
        coverTable "k" ((, 5.0) . show @Int <$> [0 .. 3]) $
        tabulate "k" [show k] $

        case (n, k) of
            (0, _) ->
                subsequences === []
            (_, 0) ->
                subsequences === []
            (_, _) | n < k ->
                subsequences === []
            (_, _) ->
                conjoin
                    [ length subsequences
                        == expectedNumberOfSubsequences
                    , length subsequences
                        == Set.size subsets
                    , Set.unions (F.toList subsets)
                        == xs
                    , all ((== k) . length) subsequences
                    ]
      where
        n = Set.size xs
        subsequences = Set.toList xs `subsequencesOfSize` k
        subsets = Set.fromList (Set.fromList <$> subsequences)
        Just expectedNumberOfSubsequences = n `numberOfSubsequencesOfSize` k

unitTests_subsequencesOfSize :: Spec
unitTests_subsequencesOfSize = unitTests
    "unitTests_subsequencesOfSize"
    (uncurry (subsequencesOfSize @Int))
    (mkTest <$> tests)
  where
    mkTest (sequence, size, output) =
        UnitTestData {params = (sequence, size), result = output}
    tests =
        [ ( [1, 2, 3, 4]
          , 0
          , []
          )
        , ( [1, 2, 3, 4]
          , 1
          , [[1], [2], [3], [4]]
          )
        , ( [1, 2, 3, 4]
          , 2
          , [[1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]]
          )
        , ( [1, 2, 3, 4]
          , 3
          , [[1, 2, 3], [1, 2, 4], [1, 3, 4], [2, 3, 4]]
          )
        , ( [1, 2, 3, 4]
          , 4
          , [[1, 2, 3, 4]]
          )
        , ( [1, 2, 3, 4]
          , 5
          , []
          )
        ]

-- This test allows us to demonstrate that `numberOfSubsequencesOfSize` exits
-- quickly in the event that the computed result is large, but within bounds.
--
unitTests_numberOfSubsequencesOfSize_withinBounds :: Spec
unitTests_numberOfSubsequencesOfSize_withinBounds = unitTests
    "unitTests_numberOfSubsequencesOfSize_withinBounds"
    (uncurry numberOfSubsequencesOfSize)
    (mkTest <$> tests)
  where
    mkTest (n, k, output) =
        UnitTestData {params = (n, k), result = Just output}
    tests =
        [ (100,  1,                       100)
        , (100,  2,                     4_950)
        , (100,  4,                 3_921_225)
        , (100,  8,           186_087_894_300)
        , (100, 16, 1_345_860_629_046_814_650)
        ]

-- This test allows us to demonstrate that `numberOfSubsequencesOfSize` gives
-- correct answers when inputs are close to or at boundary values.
--
unitTests_numberOfSubsequencesOfSize_bounds :: Spec
unitTests_numberOfSubsequencesOfSize_bounds = unitTests
    "unitTests_numberOfSubsequencesOfSize_withinBounds"
    (uncurry numberOfSubsequencesOfSize)
    (mkTest <$> tests)
  where
    mkTest (n, k, output) =
        UnitTestData {params = (n, k), result = Just output}
    tests =
        [ (           0,            0,            1)
        , (           0,            1,            0)
        , (           0, maxBound - 1,            0)
        , (           0, maxBound    ,            0)
        , (           1,            0,            1)
        , (           1,            1,            1)
        , (           1, maxBound - 1,            0)
        , (           1, maxBound    ,            0)
        , (maxBound - 1,            0,            1)
        , (maxBound - 1,            1, maxBound - 1)
        , (maxBound - 1, maxBound - 1,            1)
        , (maxBound - 1, maxBound    ,            0)
        , (maxBound    ,            0,            1)
        , (maxBound    ,            1, maxBound    )
        , (maxBound    , maxBound - 1, maxBound    )
        , (maxBound    , maxBound    ,            1)
        ]

-- This test allows us to demonstrate that `numberOfSubsequencesOfSize` exits
-- quickly in the event that the computed result is out of bounds.
--
unitTests_numberOfSubsequencesOfSize_outOfBounds :: Spec
unitTests_numberOfSubsequencesOfSize_outOfBounds = unitTests
    "unitTests_numberOfSubsequencesOfSize_outOfBounds"
    (uncurry numberOfSubsequencesOfSize)
    (mkTest <$> tests)
  where
    mkTest (n, k) =
        UnitTestData {params = (n, k), result = Nothing}
    tests =
        [ (1_000_000,    10)
        , (1_000_000,   100)
        , (1_000_000, 1_000)
        ]

--------------------------------------------------------------------------------
-- Stopping at the first successful result
--------------------------------------------------------------------------------

data SuccessOrFailure a
    = Success a
    | Failure a
    deriving (Eq, Functor, Generic, Show)

instance Arbitrary (SuccessOrFailure ()) where
    arbitrary = frequency
        [ (1, pure $ Success ())
        , (8, pure $ Failure ())
        ]
    shrink = genericShrink

prop_firstRight :: NonEmpty (SuccessOrFailure ()) -> Property
prop_firstRight sofs =
    checkCoverage $
    -- Check that we cover both success and failure:
    cover 10.0 (isLeft  result) "Failure" $
    cover 10.0 (isRight result) "Success" $
    counterexample (show sofs) $
    positionExpected === positionActual
  where
    result :: Either Int Int
    result = firstRight
        (simulateSuccessOrFailure <$> NE.zipWith (fmap . const) [1 ..] sofs)
        -- We never actually process the input, so we can assert that the input
        -- is never evaluated:
        (error "💥 BANG!")

    simulateSuccessOrFailure :: SuccessOrFailure Int -> a -> Either Int Int
    simulateSuccessOrFailure sof = const $ case sof of
        Success i -> Right i
        Failure i -> Left  i

    positionActual = case result of
        Left  i -> i
        Right i -> i
    positionExpected = case result of
        Left  _ -> length sofs
        Right _ -> length (NE.takeWhile (== Failure ()) sofs) + 1

--------------------------------------------------------------------------------
-- Taking items from a list until a predicate becomes true
--------------------------------------------------------------------------------

prop_takeUntil_identity :: [Int] -> Property
prop_takeUntil_identity xs =
    takeUntil (const False) xs === xs

prop_takeUntil_head :: [Int] -> Property
prop_takeUntil_head xs =
    case xs of
        []    -> result === [ ]
        x : _ -> result === [x]
  where
    result = takeUntil (const True) xs

prop_takeUntil_takeWhile :: [Int] -> Property
prop_takeUntil_takeWhile xs =
    checkCoverage $
    cover 80.0
        (takeWhileLength < takeUntilLength && takeUntilLength < length xs)
        "takeWhileLength < takeUntilLength && takeUntilLength < length xs" $
    cover 2.0
        (takeWhileLength < takeUntilLength && takeUntilLength == length xs)
        "takeWhileLength < takeUntilLength && takeUntilLength == length xs" $
    cover 2.0
        (takeWhileLength == takeUntilLength && takeUntilLength == length xs)
        "takeWhileLength == takeUntilLength && takeUntilLength == length xs" $
    conjoin
        [ takeWhileResult `L.isPrefixOf` xs
        , takeUntilResult `L.isPrefixOf` xs
        , all (not . condition) takeWhileResult
        , all (not . condition) (take takeWhileLength takeUntilResult)
        , all (      condition) (drop takeWhileLength takeUntilResult)
        , (drop takeWhileLength takeUntilResult) == take 1
          (drop takeWhileLength xs)
        ]
  where
    condition = ((== 0) . (`mod` 4))
    takeUntilResult = takeUntil (      condition) xs
    takeWhileResult = takeWhile (not . condition) xs
    takeUntilLength = length takeUntilResult
    takeWhileLength = length takeWhileResult

unitTests_takeUntil :: Spec
unitTests_takeUntil = unitTests
    "unitTests_takeUntil"
    (uncurry (takeUntil @Int))
    (mkTest <$> tests)
  where
    mkTest (condition, input, output) =
        UnitTestData {params = (condition, input), result = output}
    tests =
        [ ( (const False)
          , []
          , []
          )
        , ( (const True)
          , []
          , []
          )
        , ( (const False)
          , [0, 1, 2, 4, 8, 16, 32, 64, 128, 256]
          , [0, 1, 2, 4, 8, 16, 32, 64, 128, 256]
          )
        , ( (const True)
          , [0, 1, 2, 4, 8, 16, 32, 64, 128, 256]
          , [0]
          )
        , ( (>= 32)
          , [0, 1, 2, 4, 8, 16, 32, 64, 128, 256]
          , [0, 1, 2, 4, 8, 16, 32]
          )
        , ( (> 32)
          , [0, 1, 2, 4, 8, 16, 32, 64, 128, 256]
          , [0, 1, 2, 4, 8, 16, 32, 64]
          )
        ]

--------------------------------------------------------------------------------
-- Guarding search space size
--------------------------------------------------------------------------------

-- Tests that 'guardSearchSpaceSize' really does avoid performing a computation
-- if the required search space is greater than the search space limit.
--
unitTests_guardSearchSpaceSize :: Spec
unitTests_guardSearchSpaceSize =
  unitTests
    "unitTests_guardSearchSpaceSize"
    (\(r, l, c) -> guardSearchSpaceSize @String r l c)
    (mkTest <$> tests)
  where
    mkTest (requirement, limit, computation, result) =
        UnitTestData
            { params = (requirement, limit, computation)
            , result
            }
    tests =
        [ ( SearchSpaceRequirementUnknown
          , SearchSpaceLimit 1
          , shouldNotBeEvaluated
          , Nothing
          )
        , ( SearchSpaceRequirementUnknown
          , UnsafeNoSearchSpaceLimit
          , Just "apple"
          , Just "apple"
          )
        , ( SearchSpaceRequirement 100
          , SearchSpaceLimit 99
          , shouldNotBeEvaluated
          , Nothing
          )
        , ( SearchSpaceRequirement 100
          , SearchSpaceLimit 100
          , Just "banana"
          , Just "banana"
          )
        , ( SearchSpaceRequirement 100
          , SearchSpaceLimit 101
          , Just "cherry"
          , Just "cherry"
          )
        , ( SearchSpaceRequirement 100
          , UnsafeNoSearchSpaceLimit
          , Just "dragonfruit"
          , Just "dragonfruit"
          )
        ]
    shouldNotBeEvaluated = error "💥 BANG!"

--------------------------------------------------------------------------------
-- Unit test support
--------------------------------------------------------------------------------

data UnitTestData params result = UnitTestData
    { params :: params
    , result :: result
    }
    deriving (Eq, Generic, Show)

unitTests
    :: (Eq result, Show result)
    => String
    -> (params -> result)
    -> [UnitTestData params result]
    -> Spec
unitTests title f unitTestData =
    describe title $
    forM_ (zip testNumbers unitTestData) $
        \(testNumber :: Int, test) -> do
            let subtitle = "Unit test #" <> show testNumber
            it subtitle $
                let resultExpected = view #result test in
                let resultActual = f (view #params test) in
                property $ Pretty resultExpected === Pretty resultActual
  where
    testNumbers :: [Int]
    testNumbers = [1 ..]

-- A convenient shorthand for expressing a key-value pair, which makes it
-- possible to express maps with the following concise syntax:
--
--    >>> [A ▶ 10, B ▶ 20, C ▶ 30] :: Map ShortInputId Coin
--
--    fromList
--        [ ( A, Coin 10 )
--        , ( B, Coin 20 )
--        , ( C, Coin 30 )
--        ]
--
(▶) :: a -> b -> (a, b)
(▶) = (,)

--------------------------------------------------------------------------------
-- Input identification numbers: short
--------------------------------------------------------------------------------

data ShortInputId
    = A | B | C | D | E | F | G | H | I | J | K | L | M
    | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    deriving (Bounded, Enum, Eq, Ord, Show)

instance Arbitrary ShortInputId where
    arbitrary = genShortInputId

genShortInputId :: Gen ShortInputId
genShortInputId = arbitraryBoundedEnum

--------------------------------------------------------------------------------
-- Input identification numbers: long
--------------------------------------------------------------------------------

newtype LongInputId = LongInputId (Hexadecimal Quid)
    deriving stock (Eq, Ord, Read, Show)
    deriving Arbitrary via Quid

--------------------------------------------------------------------------------
-- Miscellaneous
--------------------------------------------------------------------------------

scaleCoin :: Coin -> Word64 -> Coin
scaleCoin (Coin c) w = Coin (c * intCast w)

instance Arbitrary Coin where
    arbitrary = genCoinPositive
    shrink = shrinkCoinPositive

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
    shrink = genericShrink

newtype Pretty a = Pretty { unPretty :: a }
    deriving Eq

instance Show a => Show (Pretty a) where
    show (Pretty a) = TL.unpack ("\n" <> pShow a <> "\n")
