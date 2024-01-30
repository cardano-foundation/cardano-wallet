{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module provides the main 'UTxO' data type used by the wallet.
--
module Cardano.Wallet.Primitive.Types.UTxO
    (
    -- * UTxO
      UTxO (..)

    , dom
    , null
    , size
    , balance
    , isSubsetOf
    , isProperSubsetOf
    , empty
    , disjoint
    , excluding
    , restrictedBy
    , restrictedTo
    , difference
    , intersection
    , partition
    , lookup
    , filter
    , filterByAddressM
    , filterByAddress
    , toList

    -- * UTxO delta encoding
    , DeltaUTxO
    , excluded
    , received
    , excludingD
    , receiveD

    -- * Queries
    , assetIds
    , txIds

    -- * Transformations
    , mapAssetIds
    , mapTxIds
    , removeAssetId

    ) where

import Prelude hiding
    ( filter
    , lookup
    , null
    )

import Cardano.Wallet.Primitive.Types.Address
    ( Address
    )
import Cardano.Wallet.Primitive.Types.AssetId
    ( AssetId
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxId
    , TxIn
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..)
    )
import Control.DeepSeq
    ( NFData (..)
    )
import Data.Bifunctor
    ( bimap
    , first
    )
import Data.Delta
    ( Delta (..)
    )
import Data.Functor.Identity
    ( runIdentity
    )
import Data.Generics.Internal.VL.Lens
    ( over
    , view
    )
import Data.Map.Strict
    ( Map
    )
import Data.Set
    ( Set
    )
import Fmt
    ( Buildable (..)
    , blockListF'
    , blockMapF
    )
import GHC.Generics
    ( Generic
    )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TB
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as TxOut
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- UTxO
--------------------------------------------------------------------------------

newtype UTxO = UTxO { unUTxO :: Map TxIn TxOut }
    deriving stock (Show, Generic, Eq, Ord)
    deriving newtype (Semigroup, Monoid)

instance NFData UTxO

instance Buildable UTxO where
    build (UTxO utxo) =
        blockListF' "-" utxoF (Map.toList utxo)
      where
        utxoF (inp, out) = buildMap
            [ ("input"
              , build inp)
            , ("output"
              , build out)
            ]
        buildMap = blockMapF . fmap (first $ id @String)

-- | Domain of a 'UTxO' = the set of /inputs/ of the /utxo/.
dom :: UTxO -> Set TxIn
dom (UTxO utxo) = Map.keysSet utxo

-- | Compute the balance of a UTxO
balance :: UTxO -> TokenBundle
balance =
    Map.foldl' fn mempty . unUTxO
  where
    fn :: TokenBundle -> TxOut -> TokenBundle
    fn tot out = tot `TB.add` view #tokens out

difference :: UTxO -> UTxO -> UTxO
difference a b = a `excluding` Map.keysSet (unUTxO b)

intersection :: UTxO -> UTxO -> UTxO
intersection (UTxO a) (UTxO b) = UTxO $ Map.intersection a b

-- | Indicates whether a pair of UTxO sets are disjoint.
--
disjoint :: UTxO -> UTxO -> Bool
disjoint u1 u2 = unUTxO u1 `Map.disjoint` unUTxO u2

-- | ins⋪ u
excluding :: UTxO -> Set TxIn ->  UTxO
excluding (UTxO utxo) =
    UTxO . Map.withoutKeys utxo

-- | a ⊆ b
isSubsetOf :: UTxO -> UTxO -> Bool
isSubsetOf (UTxO a) (UTxO b) = Map.isSubmapOf a b

-- | a ⊂ b
isProperSubsetOf :: UTxO -> UTxO -> Bool
isProperSubsetOf (UTxO a) (UTxO b) = Map.isProperSubmapOf a b

-- | ins⊲ u
restrictedBy :: UTxO -> Set TxIn -> UTxO
restrictedBy (UTxO utxo) =
    UTxO . Map.restrictKeys utxo

-- | u ⊳ outs
restrictedTo :: UTxO -> Set TxOut -> UTxO
restrictedTo (UTxO utxo) outs =
    UTxO $ Map.filter (`Set.member` outs) utxo

empty :: UTxO
empty = UTxO Map.empty

null :: UTxO -> Bool
null (UTxO u) = Map.null u

size :: UTxO -> Int
size (UTxO u) = Map.size u

-- | Filters a UTxO set according to a condition.
filter :: (TxIn -> Bool) -> UTxO -> UTxO
filter f (UTxO u) = UTxO $ Map.filterWithKey (const . f) u

-- | Lookup an input in the UTXO
lookup :: TxIn -> UTxO -> Maybe TxOut
lookup i (UTxO u) = Map.lookup i u

-- | Filters a 'UTxO' set with an indicator function on 'Address' values.
--
-- Returns the subset of UTxO entries that have addresses for which the given
-- indicator function returns 'True'.
filterByAddressM :: forall f. Monad f => (Address -> f Bool) -> UTxO -> f UTxO
filterByAddressM isOursF (UTxO m) =
    UTxO <$> Map.traverseMaybeWithKey filterFunc m
  where
    filterFunc :: TxIn -> TxOut -> f (Maybe TxOut)
    filterFunc _txin txout = do
        ours <- isOursF $ view #address txout
        pure $ if ours then Just txout else Nothing

-- | Filters a 'UTxO' set with an indicator function on 'Address' values.
--
-- Returns the subset of UTxO entries that have addresses for which the given
-- indicator function returns 'True'.
--
-- filterByAddress f u = runIdentity $ filterByAddressM (pure . f) u
-- filterByAddress (const True) u = u
-- filterByAddress (const False) u = mempty
-- filterByAddress f mempty = mempty
-- filterByAddress f u `isSubsetOf` u
filterByAddress :: (Address -> Bool) -> UTxO -> UTxO
filterByAddress f = runIdentity . filterByAddressM (pure . f)

-- | Partitions a UTxO set according to a condition.
--
-- > filter p a == a && filter (not . p) b == b
-- >   where (a,b) = partition p utxo
partition :: (TxIn -> Bool) -> UTxO -> (UTxO, UTxO)
partition f (UTxO u) = bimap UTxO UTxO $ Map.partitionWithKey (const . f) u

-- | Converts a UTxO set into a list of UTxO elements.
--
toList :: UTxO -> [(TxIn, TxOut)]
toList = Map.toList . unUTxO

{-------------------------------------------------------------------------------
    Delta encodings of UTxO
-------------------------------------------------------------------------------}
-- | Efficient delta encoding for 'UTxO'.
data DeltaUTxO = DeltaUTxO
    { excluded :: !(Set TxIn) -- ^ First exclude these inputs
    , received :: !UTxO       -- ^ Then receive these additional outputs.
    } deriving (Generic, Eq, Show)

instance Delta DeltaUTxO where
    type Base DeltaUTxO = UTxO
    du `apply` u = (u `excluding` excluded du) <> received du

-- | Left argument is applied /after/ right argument.
instance Semigroup DeltaUTxO where
    db <> da = DeltaUTxO
        { excluded = excluded da <> excluded'db
        , received = received'da <> received db
        }
      where
        received'da = received da `excluding` excluded db
        excluded'db = excluded db `excludingS` received da

-- | Exclude the inputs of a 'UTxO' from a 'Set' of inputs.
excludingS :: Set TxIn -> UTxO -> Set TxIn
excludingS a (UTxO b) = Set.filter (not . (`Map.member` b)) a

-- | Restrict a 'Set' of inputs by the inputs of a 'UTxO'.
restrictedByS :: Set TxIn -> UTxO -> Set TxIn
restrictedByS a (UTxO b) = Set.filter (`Map.member` b) a

instance Monoid DeltaUTxO where
    mempty = DeltaUTxO { excluded = mempty, received = mempty }

-- | Exclude a set of transaction inputs, typically because we spend them.
excludingD :: UTxO -> Set TxIn -> (DeltaUTxO, UTxO)
excludingD u ins = (du, u `excluding` spent)
  where
    spent = ins `restrictedByS` u
    du = DeltaUTxO { excluded = spent, received = mempty }

-- | Receive additional 'UTxO' / union.
receiveD :: UTxO -> UTxO -> (DeltaUTxO, UTxO)
receiveD a b = (da, a <> new)
  where
    new =  b `excluding` dom a
    da = DeltaUTxO { excluded = mempty, received = new}

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

assetIds :: UTxO -> Set AssetId
assetIds (UTxO u) = foldMap TxOut.assetIds u

txIds :: UTxO -> Set TxId
txIds (UTxO u) = Set.map (view #inputId) (Map.keysSet u)

--------------------------------------------------------------------------------
-- Transformations
--------------------------------------------------------------------------------

mapAssetIds :: (AssetId -> AssetId) -> UTxO -> UTxO
mapAssetIds f (UTxO u) = UTxO $ Map.map (TxOut.mapAssetIds f) u

-- | Applies a mapping on transaction identifiers to a 'UTxO' set.
--
-- If the provided mapping gives rise to a collision within the 'TxIn' key set,
-- then only the smallest 'TxOut' is retained, according to the 'Ord' instance
-- for 'TxOut'.
--
mapTxIds :: (TxId -> TxId) -> UTxO -> UTxO
mapTxIds f (UTxO u) = UTxO $ Map.mapKeysWith min (over #inputId f) u

removeAssetId :: UTxO -> AssetId -> UTxO
removeAssetId (UTxO u) a = UTxO $ Map.map (`TxOut.removeAssetId` a) u
