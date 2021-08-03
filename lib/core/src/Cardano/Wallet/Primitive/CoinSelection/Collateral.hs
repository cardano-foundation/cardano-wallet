{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Primitive.CoinSelection.Collateral
    (
    -- * Public API

      selectCollateral
    , SelectCollateral
    , SelectCollateralParams (..)
    , SelectCollateralResult (..)
    , SelectCollateralError (..)
    , SearchSpaceLimit (..)

    -- * Internal API

    -- ** Selecting collateral by giving priority to smallest values first
    , selectCollateralSmallest

    -- ** Selecting collateral by giving priority to largest values first
    , selectCollateralLargest

    -- ** Generating submaps
    , submaps

    -- ** Generating subsequences
    , subsequencesOfSize
    , numberOfSubsequencesOfSize

    -- ** Control flow
    , firstRight
    , takeUntil
    )
    where

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Data.Function
    ( (&) )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( listToMaybe, mapMaybe )
import Data.Ord
    ( Down (..) )
import Data.Semigroup
    ( sconcat )
import Data.Set
    ( Set )
import GHC.Generics
    ( Generic )

import Prelude

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

type SelectCollateral inputId =
    SelectCollateralParams inputId ->
    Either (SelectCollateralError inputId) (SelectCollateralResult inputId)

data SelectCollateralParams inputId = SelectCollateralParams
    { coinsAvailable
        :: Map inputId Coin
    , maximumSelectionSize
        :: Int
    , minimumSelectionAmount
        :: Coin
    , searchSpaceLimit
        :: SearchSpaceLimit
    }
    deriving (Eq, Generic, Show)

data SearchSpaceLimit
    = UnsafeNoSearchSpaceLimit
    | SearchSpaceLimit Int
    deriving (Eq, Show)

newtype SelectCollateralResult inputId = SelectCollateralResult
    { coinsSelected :: Map inputId Coin
    }
    deriving (Eq, Generic, Show)

newtype SelectCollateralError inputId = SelectCollateralError
    { largestCombinationAvailable :: Map inputId Coin
    }
    deriving (Eq, Generic, Show)

selectCollateral :: forall inputId. Ord inputId => SelectCollateral inputId
selectCollateral =
    -- Give priority to the strategy of selecting the smallest values possible,
    -- but fall back to the strategy of selecting larger values, if necessary:
    firstRight
        [ selectCollateralSmallest
        , selectCollateralLargest
        ]

--------------------------------------------------------------------------------
-- Internal API
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Selecting collateral by giving priority to smallest values first
--------------------------------------------------------------------------------

selectCollateralSmallest
    :: forall inputId. Ord inputId => SelectCollateral inputId
selectCollateralSmallest params =
    case smallestValidCombination of
        Just coinsSelected ->
            Right SelectCollateralResult {coinsSelected}
        Nothing ->
            Left (SelectCollateralError mempty)
  where
    coinsToConsider :: [(inputId, Coin)]
    coinsToConsider = coinsAvailable
        & Map.toList
        & L.sortOn snd
        & takeUntil ((>= minimumSelectionAmount) . snd)

    numberOfCoinsToConsider :: Int
    numberOfCoinsToConsider = length coinsToConsider

    smallestValidCombination :: Maybe (Map inputId Coin)
    smallestValidCombination =
        listToMaybe $ L.sortOn F.fold validCombinations
      where
        validCombinations :: [Map inputId Coin]
        validCombinations =
            mapMaybe smallestValidCombinationOfSize
            [1 .. maximumSelectionSize]

    smallestValidCombinationOfSize :: Int -> Maybe (Map inputId Coin)
    smallestValidCombinationOfSize size =
        guardSearchSpaceSize
            $ coinsToConsider
            & (`subsequencesOfSize` size)
            & fmap (\ics -> (ics, F.foldMap snd ics))
            & L.sortOn snd
            & L.dropWhile ((< minimumSelectionAmount) . snd)
            & listToMaybe
            & fmap (Map.fromList . fst)
      where
        -- If the number of combinations of the given size exceeds the maximum
        -- search space size, then return 'Nothing'.
        guardSearchSpaceSize :: Maybe a -> Maybe a
        guardSearchSpaceSize =
            case (requiredSearchSpaceSize, searchSpaceLimit) of
                (r, SearchSpaceLimit m) | r > m -> const Nothing
                _ -> id
          where
            requiredSearchSpaceSize =
                numberOfCoinsToConsider `numberOfSubsequencesOfSize` size

    SelectCollateralParams
        { coinsAvailable
        , maximumSelectionSize
        , minimumSelectionAmount
        , searchSpaceLimit
        } = params

--------------------------------------------------------------------------------
-- Selecting collateral by giving priority to largest values first
--------------------------------------------------------------------------------

selectCollateralLargest
    :: forall inputId. Ord inputId => SelectCollateral inputId
selectCollateralLargest params =
    case smallestValidSubmapOfLargestCombinationAvailable of
        Just coinsSelected ->
            Right SelectCollateralResult {coinsSelected}
        Nothing ->
            Left SelectCollateralError {largestCombinationAvailable}
  where
    largestCombinationAvailable :: Map inputId Coin
    largestCombinationAvailable =
        coinsAvailable
            & Map.toList
            & L.sortOn (Down . snd)
            & L.take maximumSelectionSize
            & Map.fromList

    smallestValidSubmapOfLargestCombinationAvailable :: Maybe (Map inputId Coin)
    smallestValidSubmapOfLargestCombinationAvailable =
        largestCombinationAvailable
            & submaps
            & Set.toList
            & fmap (\ics -> (ics, F.fold ics))
            & L.sortOn snd
            & L.dropWhile ((< minimumSelectionAmount) . snd)
            & fmap fst
            & listToMaybe

    SelectCollateralParams
        { coinsAvailable
        , maximumSelectionSize
        , minimumSelectionAmount
        } = params

--------------------------------------------------------------------------------
-- Generating submaps
--------------------------------------------------------------------------------

submaps :: forall a b. (Ord a, Ord b) => Map a b -> Set (Map a b)
submaps m = Set.map (Map.restrictKeys m) (Set.powerSet (Map.keysSet m))

--------------------------------------------------------------------------------
-- Generating subsequences
--------------------------------------------------------------------------------

numberOfSubsequencesOfSize :: Int -> Int -> Int
numberOfSubsequencesOfSize = choose
  where
    choose _ 0 = 1
    choose 0 _ = 0
    choose n k = choose (n - 1) (k - 1) * n `div` k

subsequencesOfSize :: [a] -> Int -> [[a]]
subsequencesOfSize xs k
    | k <= 0 =
        []
    | k > length xs =
        []
    | otherwise =
        case drop k (subsequencesBySize xs) of
            result : _ -> result
            [] -> []
  where
    subsequencesBySize [] = [[[]]]
    subsequencesBySize (y : ys) =
        zipWith
            (++)
            ([] : map (map (y :)) next)
            (next ++ [[]])
      where
        next = subsequencesBySize ys

--------------------------------------------------------------------------------
-- Control flow
--------------------------------------------------------------------------------

firstRight :: NonEmpty (a -> Either e r) -> (a -> Either e r)
firstRight = sconcat

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = foldr (\x ys -> x : if p x then [] else ys) []
