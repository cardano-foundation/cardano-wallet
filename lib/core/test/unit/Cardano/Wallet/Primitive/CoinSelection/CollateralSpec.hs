{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Numerals #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.CoinSelection.CollateralSpec where

import Prelude hiding
    ( sequence )

import Cardano.Wallet.Primitive.CoinSelection.Collateral
    ( SearchSpaceLimit (..)
    , SelectCollateralError (..)
    , SelectCollateralParams (..)
    , SelectCollateralResult (..)
    , firstRight
    , numberOfSubsequencesOfSize
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
    , frequency
    , genericShrink
    , property
    , scale
    , shuffle
    , sized
    , tabulate
    , (===)
    )
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

    parallel $ describe "selectCollateralSmallest" $ do

        it "prop_selectCollateralSmallest_optimal" $
            property prop_selectCollateralSmallest_optimal
        it "prop_selectCollateralSmallest_constrainedSelectionCount" $
            property prop_selectCollateralSmallest_constrainedSelectionCount

        unitTests_selectCollateralSmallest_optimal
        unitTests_selectCollateralSmallest_constrainedSelectionCount
        unitTests_selectCollateralSmallest_constrainedSearchSpace

    parallel $ describe "selectCollateralLargest" $ do

        it "prop_selectCollateralLargest" $
            property prop_selectCollateralLargest

        unitTests_selectCollateralLargest_optimal
        unitTests_selectCollateralLargest_insufficient

    parallel $ describe "SingleBitCoinMap" $ do

        it "prop_genSingleBitCoinMap" $
            property prop_genSingleBitCoinMap
        it "prop_shrinkSingleBitCoinMap" $
            property prop_shrinkSingleBitCoinMap

    parallel $ describe "shrinkListToPrefixes" $ do

        it "prop_shrinkListToPrefixes" $
            property prop_shrinkListToPrefixes

    parallel $ describe "submaps" $ do

        it "prop_submaps_isSubmapOf" $
            property prop_submaps_isSubmapOf
        it "prop_submaps_size" $
            property prop_submaps_size
        it "prop_submaps_unions" $
            property prop_submaps_unions

        unitTests_submaps

    parallel $ describe "subsequencesOfSize" $ do

        it "prop_subsequencesOfSize" $
            property prop_subsequencesOfSize

        unitTests_subsequencesOfSize
        unitTests_numberOfSubsequencesOfSize_withinBounds
        unitTests_numberOfSubsequencesOfSize_outOfBounds

    parallel $ describe "firstRight" $ do

        it "prop_firstRight" $
            property prop_firstRight

    parallel $ describe "takeUntil" $ do

        it "prop_takeUntil_identity" $
            property prop_takeUntil_identity
        it "prop_takeUntil_head" $
            property prop_takeUntil_identity
        it "prop_takeUntil_takeWhile" $
            property prop_takeUntil_identity

        unitTests_takeUntil

--------------------------------------------------------------------------------
-- Properties common to all collateral selection strategies
--------------------------------------------------------------------------------

prop_selectCollateral_common
    :: (Ord inputId, Show inputId)
    => SelectCollateralParams inputId
    -> Either (SelectCollateralError inputId) (SelectCollateralResult inputId)
    -> Property
prop_selectCollateral_common params eitherErrorResult =
    cover 20.0 (isLeft  eitherErrorResult) "Failure" $
    cover 20.0 (isRight eitherErrorResult) "Success" $
    counterexample ("Params: " <> show (Pretty params)) $
    either
        (prop_selectCollateral_error  params)
        (prop_selectCollateral_result params)
        (eitherErrorResult)

prop_selectCollateral_error
    :: (Ord inputId, Show inputId)
    => SelectCollateralParams inputId
    -> SelectCollateralError inputId
    -> Property
prop_selectCollateral_error params err =
    counterexample ("Error: " <> show (Pretty err)) $
    conjoin
        [ F.fold (largestCombinationAvailable err)
            < minimumSelectionAmount params
        , F.length (largestCombinationAvailable err)
            <= maximumSelectionSize params
        , largestCombinationAvailable err
            `Map.isSubmapOf` coinsAvailable params
        ]

prop_selectCollateral_result
    :: (Ord inputId, Show inputId)
    => SelectCollateralParams inputId
    -> SelectCollateralResult inputId
    -> Property
prop_selectCollateral_result params result =
    counterexample ("Result: " <> show (Pretty result)) $
    conjoin
        [ F.fold (coinsAvailable params)
            >= minimumSelectionAmount params
        , F.fold (coinsSelected result)
            >= minimumSelectionAmount params
        , F.length (coinsSelected result)
            <= maximumSelectionSize params
        , coinsSelected result
            `Map.isSubmapOf` coinsAvailable params
        ]

--------------------------------------------------------------------------------
-- Selecting collateral by giving priority to smallest values first
--------------------------------------------------------------------------------

prop_selectCollateralSmallest_optimal
    :: SingleBitCoinMap
    -> MinimumSelectionAmount
    -> Property
prop_selectCollateralSmallest_optimal
    (SingleBitCoinMap coinsAvailable)
    (MinimumSelectionAmount minimumSelectionAmount) =
    checkCoverage $
    conjoin
        [ prop_selectCollateral_common params eitherErrorResult
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

    params = SelectCollateralParams
        { coinsAvailable
        , maximumSelectionSize
        , minimumSelectionAmount
        , searchSpaceLimit = UnsafeNoSearchSpaceLimit
        }

    eitherErrorResult = selectCollateralSmallest params

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

prop_selectCollateralSmallest_constrainedSelectionCount
    :: SingleBitCoinMap
    -> MinimumSelectionAmount
    -> Property
prop_selectCollateralSmallest_constrainedSelectionCount
    (SingleBitCoinMap coinsAvailable)
    (MinimumSelectionAmount minimumSelectionAmount) =
    checkCoverage $
    conjoin
        [ prop_selectCollateral_common params eitherErrorResult
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

    params = SelectCollateralParams
        { coinsAvailable
        , maximumSelectionSize
        , minimumSelectionAmount
        , searchSpaceLimit = UnsafeNoSearchSpaceLimit
        }

    eitherErrorResult = selectCollateralSmallest params

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
    selectCollateralSmallest
    (mkTest <$> tests)
  where
    coinsAvailable =
        [A ▶ 1, B ▶ 2, C ▶ 4, D ▶ 8, E ▶ 16, F ▶ 32, G ▶ 64, H ▶ 128]
    mkTest (minimumSelectionAmount, coinsSelected) = UnitTestData
        { params = SelectCollateralParams
            { coinsAvailable
            , maximumSelectionSize = Map.size coinsAvailable
            , minimumSelectionAmount
            , searchSpaceLimit = UnsafeNoSearchSpaceLimit
            }
        , result = Right SelectCollateralResult {coinsSelected}
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
    selectCollateralSmallest
    (mkTest <$> tests)
  where
    coinsAvailable =
        [A ▶ 1, B ▶ 2, C ▶ 4, D ▶ 8, E ▶ 16, F ▶ 32, G ▶ 64, H ▶ 128]
    mkTest (minimumSelectionAmount, coinsSelected) = UnitTestData
        { params = SelectCollateralParams
            { coinsAvailable
            , maximumSelectionSize = 1
            , minimumSelectionAmount
            , searchSpaceLimit = UnsafeNoSearchSpaceLimit
            }
        , result = Right SelectCollateralResult { coinsSelected }
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
    selectCollateralSmallest
    (mkTest <$> tests)
  where
    coinsAvailable =
        [A ▶ 1, B ▶ 2, C ▶ 4, D ▶ 8, E ▶ 16, F ▶ 32, G ▶ 64, H ▶ 128]
    maximumSelectionSize =
        Map.size coinsAvailable
    Just searchSpaceLimit = SearchSpaceLimit <$>
        maximumSelectionSize `numberOfSubsequencesOfSize` 2
    mkTest (minimumSelectionAmount, coinsSelected) = UnitTestData
        { params = SelectCollateralParams
            { coinsAvailable
            , maximumSelectionSize
            , minimumSelectionAmount
            , searchSpaceLimit
            }
        , result = Right SelectCollateralResult {coinsSelected}
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

prop_selectCollateralLargest
    :: SingleBitCoinMap
    -> MinimumSelectionAmount
    -> Property
prop_selectCollateralLargest
    (SingleBitCoinMap coinsAvailable)
    (MinimumSelectionAmount minimumSelectionAmount) =
    checkCoverage $
    conjoin
        [ prop_selectCollateral_common params eitherErrorResult
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
                    "Number of coins selected = maximum allowed"
                True

    eitherErrorResult = selectCollateralLargest params

    params = SelectCollateralParams
        { coinsAvailable
        , maximumSelectionSize
        , minimumSelectionAmount
        , searchSpaceLimit = UnsafeNoSearchSpaceLimit
        }

    maximumSelectionSize :: Int
    maximumSelectionSize = optimalCoinCount

    optimalCoinCount :: Int
    optimalCoinCount = Bits.popCount (unCoin minimumSelectionAmount)

unitTests_selectCollateralLargest_optimal :: Spec
unitTests_selectCollateralLargest_optimal = unitTests
    "unitTests_selectCollateralLargest_optimal"
    selectCollateralLargest
    (mkTest <$> tests)
  where
    coinsAvailable =
        [A ▶ 1, B ▶ 2, C ▶ 4, D ▶ 8, E ▶ 16, F ▶ 32, G ▶ 64, H ▶ 128]
    mkTest (minimumSelectionAmount, coinsSelected) = UnitTestData
        { params = SelectCollateralParams
            { coinsAvailable
            , maximumSelectionSize = 3
            , minimumSelectionAmount
            , searchSpaceLimit = UnsafeNoSearchSpaceLimit
            }
        , result = Right SelectCollateralResult { coinsSelected }
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
    selectCollateralLargest
    (mkTest <$> tests)
  where
    coinsAvailable =
        [A ▶ 1, B ▶ 2, C ▶ 4, D ▶ 8, E ▶ 16, F ▶ 32, G ▶ 64, H ▶ 128]
    mkTest (minimumSelectionAmount, largestCombinationAvailable) =
        UnitTestData
        { params = SelectCollateralParams
            { coinsAvailable
            , maximumSelectionSize = 3
            , minimumSelectionAmount
            , searchSpaceLimit = UnsafeNoSearchSpaceLimit
            }
        , result = Left SelectCollateralError {largestCombinationAvailable}
        }
    tests =
        [ ( 225, [F ▶ 32, G ▶ 64, H ▶ 128])
        , ( 256, [F ▶ 32, G ▶ 64, H ▶ 128])
        , ( 512, [F ▶ 32, G ▶ 64, H ▶ 128])
        , (1024, [F ▶ 32, G ▶ 64, H ▶ 128])
        ]

--------------------------------------------------------------------------------
-- Maps with single-bit coins (coins that are powers of two)
--------------------------------------------------------------------------------

newtype SingleBitCoinMap = SingleBitCoinMap
    { unSingleBitCoinMap :: Map TestInputId Coin
    }
    deriving (Eq, Show)

instance Arbitrary SingleBitCoinMap where
    arbitrary = genSingleBitCoinMap
    shrink = shrinkSingleBitCoinMap

allSingleBitCoins :: [Coin]
allSingleBitCoins = Coin . ((2 :: Word64) ^) <$> [0 :: Word64 .. ]

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
    (submaps @TestInputId @Int)
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
                    , Set.unions subsets
                        == xs
                    , all (== k) (length <$> subsequences)
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

unitTests_numberOfSubsequencesOfSize_withinBounds :: Spec
unitTests_numberOfSubsequencesOfSize_withinBounds = unitTests
    "unitTests_numberOfSubsequencesOfSize_withinBounds"
    (uncurry numberOfSubsequencesOfSize)
    (mkTest <$> tests)
  where
    mkTest (n, k, output) =
        UnitTestData {params = (n, k), result = Just output}
    tests =
        [ (100, 1,          100)
        , (100, 2,         4950)
        , (100, 3,       161700)
        , (100, 4,      3921225)
        , (100, 5,     75287520)
        , (100, 6,   1192052400)
        , (100, 7,  16007560800)
        , (100, 8, 186087894300)
        ]

unitTests_numberOfSubsequencesOfSize_outOfBounds :: Spec
unitTests_numberOfSubsequencesOfSize_outOfBounds = unitTests
    "unitTests_numberOfSubsequencesOfSize_outOfBounds"
    (uncurry numberOfSubsequencesOfSize)
    (mkTest <$> tests)
  where
    mkTest (n, k) =
        UnitTestData {params = (n, k), result = Nothing}
    tests =
        [ (1_000_000,   10)
        , (1_000_000,  100)
        , (1_000_000, 1000)
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

(▶) :: a -> b -> (a, b)
(▶) = (,)

data TestInputId
    = A | B | C | D | E | F | G | H | I | J | K | L | M
    | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    deriving (Enum, Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Miscellaneous
--------------------------------------------------------------------------------

scaleCoin :: Coin -> Word64 -> Coin
scaleCoin (Coin c) w = Coin (c * w)

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
    shrink = genericShrink

newtype Pretty a = Pretty { unPretty :: a }
    deriving Eq

instance Show a => Show (Pretty a) where
    show (Pretty a) = TL.unpack ("\n" <> pShow a <> "\n")
