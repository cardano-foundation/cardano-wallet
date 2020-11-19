{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module provides the main 'UTxO' data type used by the wallet.
--
module Cardano.Wallet.Primitive.Types.UTxO
    (
    -- * UTxO Types
      UTxO (..)
    , UTxOStatistics (..)

    -- * Auxilliary Types
    , BoundType
    , Dom (..)
    , HistogramBar (..)

    -- * Functions
    , balance
    , balance'
    , computeStatistics
    , computeUtxoStatistics
    , excluding
    , isSubsetOf
    , log10
    , pickRandom
    , restrictedBy
    , restrictedTo

    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn, TxOut (..) )
import Control.DeepSeq
    ( NFData (..) )
import Data.List
    ( foldl' )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Set
    ( Set )
import Data.Word
    ( Word64 )
import Fmt
    ( Buildable (..)
    , blockListF'
    , padRightF
    , tupleF
    )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import System.Random
    ( randomRIO )

import qualified Control.Foldl as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

newtype UTxO = UTxO { getUTxO :: Map TxIn TxOut }
    deriving stock (Show, Generic, Eq, Ord)
    deriving newtype (Semigroup, Monoid)

instance NFData UTxO

-- | Allows us to define the "domain" of any type — @UTxO@ in particular — and
-- use 'dom' to refer to the /inputs/ of an /utxo/.
--
class Dom a where
    type DomElem a :: *
    dom :: a -> Set (DomElem a)

instance Dom UTxO where
    type DomElem UTxO = TxIn
    dom (UTxO utxo) = Map.keysSet utxo

instance Buildable UTxO where
    build (UTxO utxo) =
        blockListF' "-" utxoF (Map.toList utxo)
      where
        utxoF (inp, out) = build inp <> " => " <> build out

-- | Pick a random element from a UTxO, returns 'Nothing' if the UTxO is empty.
-- Otherwise, returns the selected entry and, the UTxO minus the selected one.
pickRandom
    :: UTxO
    -> IO (Maybe (TxIn, TxOut), UTxO)
pickRandom (UTxO utxo)
    | Map.null utxo =
        return (Nothing, UTxO utxo)
    | otherwise = do
        ix <- randomRIO (0, toEnum (Map.size utxo - 1))
        return (Just $ Map.elemAt ix utxo, UTxO $ Map.deleteAt ix utxo)

-- | Compute the balance of a UTxO
balance :: UTxO -> Natural
balance =
    Map.foldl' fn 0 . getUTxO
  where
    fn :: Natural -> TxOut -> Natural
    fn tot out = tot + fromIntegral (getCoin (coin out))

-- | Compute the balance of a unwrapped UTxO
balance' :: [(TxIn, TxOut)] -> Word64
balance' =
    foldl' fn 0
  where
    fn :: Word64 -> (TxIn, TxOut) -> Word64
    fn tot (_, out) = tot + getCoin (coin out)

-- | ins⋪ u
excluding :: UTxO -> Set TxIn ->  UTxO
excluding (UTxO utxo) =
    UTxO . Map.withoutKeys utxo

-- | a ⊆ b
isSubsetOf :: UTxO -> UTxO -> Bool
isSubsetOf (UTxO a) (UTxO b) =
    a `Map.isSubmapOf` b

-- | ins⊲ u
restrictedBy :: UTxO -> Set TxIn -> UTxO
restrictedBy (UTxO utxo) =
    UTxO . Map.restrictKeys utxo

-- | u ⊳ outs
restrictedTo :: UTxO -> Set TxOut -> UTxO
restrictedTo (UTxO utxo) outs =
    UTxO $ Map.filter (`Set.member` outs) utxo

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

-- An 'HistogramBar' captures the value of a particular bucket. It specifies
-- the bucket upper bound, and its corresponding distribution (on the y-axis).
data HistogramBar = HistogramBar
    { bucketUpperBound :: !Word64
    , bucketCount      :: !Word64
    } deriving (Show, Eq, Ord, Generic)

instance NFData HistogramBar

instance Buildable HistogramBar where
    build (HistogramBar k v) = tupleF (k, v)

--  Buckets boundaries can be constructed in different ways
data BoundType = Log10 deriving (Eq, Show, Ord, Generic)

instance NFData BoundType

-- | Smart-constructor to create bounds using a log-10 scale
log10 :: BoundType
log10 = Log10
{-# INLINE log10 #-}

-- | Compute UtxoStatistics from UTxOs
computeUtxoStatistics :: BoundType -> UTxO -> UTxOStatistics
computeUtxoStatistics btype =
    computeStatistics (pure . getCoin . coin) btype . Map.elems . getUTxO

-- | A more generic function for computing UTxO statistics on some other type of
-- data that maps to UTxO's values.
computeStatistics :: (a -> [Word64]) -> BoundType -> [a] -> UTxOStatistics
computeStatistics getCoins btype utxos =
    (F.fold foldStatistics (mconcat $ getCoins <$> utxos)) btype
  where
    foldStatistics :: F.Fold Word64 (BoundType -> UTxOStatistics)
    foldStatistics = UTxOStatistics
        <$> foldBuckets (generateBounds btype)
        <*> F.sum

    foldBuckets :: NonEmpty Word64 -> F.Fold Word64 [HistogramBar]
    foldBuckets bounds =
        let
            step :: Map Word64 Word64 -> Word64 -> Map Word64 Word64
            step x a = case Map.lookupGE a x of
                Just (k, v) -> Map.insert k (v+1) x
                Nothing -> Map.adjust (+1) (NE.head bounds) x
            initial :: Map Word64 Word64
            initial =
                Map.fromList $ zip (NE.toList bounds) (repeat 0)
            extract :: Map Word64 Word64 -> [HistogramBar]
            extract =
                map (uncurry HistogramBar) . Map.toList
        in
            F.Fold step initial extract

    generateBounds :: BoundType -> NonEmpty Word64
    generateBounds = \case
        Log10 -> NE.fromList $ map (10 ^!) [1..16] ++ [45 * (10 ^! 15)]

    (^!) :: Word64 -> Word64 -> Word64
    (^!) = (^)
