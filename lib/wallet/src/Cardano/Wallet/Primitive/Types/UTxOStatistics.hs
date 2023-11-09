{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Copyright: © 2018-2022 IOHK, 2023 Cardano Foundation
-- License: Apache-2.0
--
-- This module provides the 'UTxOStatistics' type.
--
module Cardano.Wallet.Primitive.Types.UTxOStatistics
    ( UTxOStatistics (..)
    , BoundType
    , HistogramBar (..)
    , compute
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..)
    )
import Control.DeepSeq
    ( NFData (..)
    )
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Data.Map.Strict
    ( Map
    )
import Data.Word
    ( Word64
    )
import Fmt
    ( Buildable (..)
    , blockListF'
    , padRightF
    , tupleF
    )
import GHC.Generics
    ( Generic
    )

import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as TxOut
import qualified Control.Foldl as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

data UTxOStatistics = UTxOStatistics
    { histogram :: ![HistogramBar]
    , allStakes :: !Word64
    , boundType :: BoundType
    } deriving (Show, Generic, Ord)

instance NFData UTxOStatistics

-- Example output:
--
-- @
--    = Total value of 14061000005 lovelace across 7 UTxOs
--     ... 10                2
--     ... 100               0
--     ... 1000              0
--     ... 10000             0
--     ... 100000            0
--     ... 1000000           0
--     ... 10000000          0
--     ... 100000000         2
--     ... 1000000000        0
--     ... 10000000000       3
--     ... 100000000000      0
--     ... 1000000000000     0
--     ... 10000000000000    0
--     ... 100000000000000   0
--     ... 1000000000000000  0
--     ... 10000000000000000 0
--     ... 45000000000000000 0
--  @
instance Buildable UTxOStatistics where
    build (UTxOStatistics hist val _) = mconcat
        [ "= Total value of "
        , build val
        , " lovelace across "
        , wordF $ sum $ map bucketCount hist
        , " UTxOs"
        , "\n"
        , blockListF' "" buildBar hist
        ]
      where
        buildBar (HistogramBar b c) =
            -- NOTE: Picked to fit well with the max value of Lovelace.
            "... " <> (padRightF 17 ' ' b) <> " " <> wordF c

        -- This is a workaround for the fact that:
        -- > fmt (build (0::Word)) == "-0"
        wordF = build . toInteger

instance Eq UTxOStatistics where
    (UTxOStatistics h s _) == (UTxOStatistics h' s' _) =
        s == s' && sorted h == sorted h'
      where
        sorted :: [HistogramBar] -> [HistogramBar]
        sorted = L.sortOn (\(HistogramBar key _) -> key)

-- | A 'HistogramBar' captures the value of a particular bucket. It specifies
-- the bucket upper bound, and its corresponding distribution (on the y-axis).
data HistogramBar = HistogramBar
    { bucketUpperBound :: !Word64
    , bucketCount      :: !Word64
    } deriving (Show, Eq, Ord, Generic)

instance NFData HistogramBar

instance Buildable HistogramBar where
    build (HistogramBar k v) = tupleF (k, v)

-- | A method of distributing values into buckets.
data BoundType = Log10 deriving (Eq, Show, Ord, Generic)

instance NFData BoundType

-- | Computes a 'UTxOStatistics' object from a 'UTxO' set.
compute :: UTxO -> UTxOStatistics
compute
    = computeWith (pure . Coin.unsafeToWord64 . TxOut.coin) Log10
    . Map.elems
    . unUTxO

-- | Computes a 'UTxOStatistics' object from an abstract source of values.
computeWith :: (a -> [Word64]) -> BoundType -> [a] -> UTxOStatistics
computeWith getCoins btype utxos =
    (F.fold foldStatistics (mconcat $ getCoins <$> utxos)) btype
  where
    foldStatistics :: F.Fold Word64 (BoundType -> UTxOStatistics)
    foldStatistics = UTxOStatistics
        <$> foldBuckets (generateBounds btype)
        <*> F.sum

    foldBuckets :: NonEmpty Word64 -> F.Fold Word64 [HistogramBar]
    foldBuckets bounds = F.Fold step initial extract
      where
        step :: Map Word64 Word64 -> Word64 -> Map Word64 Word64
        step x a = case Map.lookupGE a x of
            Just (k, v) -> Map.insert k (v+1) x
            Nothing -> Map.adjust (+1) (NE.head bounds) x
        initial :: Map Word64 Word64
        initial = Map.fromList $ map (, 0) (NE.toList bounds)
        extract :: Map Word64 Word64 -> [HistogramBar]
        extract = map (uncurry HistogramBar) . Map.toList

    generateBounds :: BoundType -> NonEmpty Word64
    generateBounds = \case
        Log10 -> NE.fromList $ map (10 ^!) [1..16] ++ [45 * (10 ^! 15)]

    (^!) :: Word64 -> Word64 -> Word64
    (^!) = (^)
