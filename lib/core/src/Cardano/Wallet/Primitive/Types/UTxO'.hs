{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Primitive.Types.UTxO'
   ( UTxO'
   -- * Constructors
   , applyFirstTx

   -- * Operations
   , applyTx
   , difference

   -- * Observations
   , balance
   ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx, TxIn (..), TxOut (..) )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Control.Lens
    ( view )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Map.Strict
    ( Map (..) )
import Data.Set
    ( Set )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.Tx as Tx
import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

newtype UTxO' = UTxO' { _utxo :: UTxO }
    deriving newtype (Semigroup, Monoid)

-- semigroup/associativity:
--   x <> (y <> z) = (x <> y) <> z
-- semigroup/balance/distributes:
--   (u1 <> u2) = balance u1 <> balance u2
-- monoid/right identity:
--   x <> mempty = x
-- monoid/left identity:
--   mempty <> x = x
-- monoid/concatenation:
--   mconcat = foldr (<>) mempty
-- monoid/balance:
--   balance mempty = mempty
-- foldable/1:
--   foldr f z t = appEndo (foldMap (Endo . f) t ) z
-- foldable/2:
--   foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
-- foldable/3:
--   fold = foldMap id
-- foldable/4:
--   length = getSum . foldMap (Sum . const  1)

-- | Construct a UTxO from a transaction
--
-- balance (u <> applyFirstTx tx) = balance (applyTx tx u)
-- balance (applyFirstTx tx) = balance (applyTx tx mempty)
applyFirstTx :: Tx -> UTxO'
applyFirstTx tx =
    let
        indexedOutputs = zip [0..] (view #outputs tx)
        utxo = UTxO
               . Map.fromList
               . fmap (Bifunctor.first $ TxIn $ view #txId tx)
               $ indexedOutputs
    in
        UTxO' utxo

-- | Apply a transaction to a UTxO.
--
-- balance (applyTx tx u)
--    = balance u `TokenBundle.add` foldMap tokens (outputs tx)
applyTx :: Tx -> UTxO' -> UTxO'
applyTx tx u =
    let
        existingUTxO :: UTxO
        existingUTxO = _utxo u

        transactionUTxO :: UTxO
        transactionUTxO = _utxo $ applyFirstTx tx

        collateralIns = Set.fromList (fst <$> tx ^. #resolvedCollateral)
        transactionIns = Set.fromList (Tx.inputs tx)
  
        newUTxO = case tx ^. #isValidScript of
                Just False ->
                    existingUTxO `UTxO.excluding` collateralIns
                _ ->
                    (existingUTxO <> transactionUTxO)
                        `UTxO.excluding` transactionIns
    in
      UTxO' newUTxO

-- | Get the elements in u1 that are not in u2. In the case that elements are in
-- both, get the difference of the value of the TxOut (the TokenBundle value) in
-- both entries, TODO removing any entries that are fully spent.
--
-- balance (u1 `difference` u2) = balance u1 `TokenBundle.difference` balance u2
difference :: UTxO' -> UTxO' -> UTxO'
difference u1 u2 =
    let
        u1' = getUTxO . _utxo $ u1
        u2' = getUTxO . _utxo $ u2

        diffFunc :: TxOut -> TxOut -> Maybe TxOut
        diffFunc a b =
            let
                tokens1 = tokens a
                tokens2 = tokens b
                diff = tokens1 `TokenBundle.difference` tokens2
            in
                if diff == mempty
                then Nothing
                else Just $ TxOut (address a) diff
    in
        UTxO' (UTxO $ Map.differenceWith diffFunc u1' u2')

-- | Filter the TxOut addresses in a UTxO.
--
-- balance (filterUTxO (const $ pure True) u) = balance u
-- balance (filterUTxO (const $ pure False) u) = mempty
-- balance (filterUTxO f mempty) = mempty
-- balance (filterUTxO f (applyTx tx mempty)) =
--   foldMap (\o -> do
--               ours <- f (address o)
--               if ours then tokens o else mempty
--            ) (outputs tx)
filterUTxO :: forall f. Monad f => (Address -> f Bool) -> UTxO' -> f UTxO'
filterUTxO isOurs (UTxO' (UTxO m)) =
    UTxO' . UTxO <$> Map.traverseMaybeWithKey filterFunc m
    where
        filterFunc :: TxIn -> TxOut -> f (Maybe TxOut)
        filterFunc _txin txout = do
            ours <- isOurs $ view #address txout
            pure $ if ours then Just txout else Nothing

--------------------------------------------------------------------------------
-- Observations
--
-- When adding an observation, you should also add a law to each operation in
-- terms of that new observation, unless the new observation can trivially be
-- expressed in terms of some existing observation, in which case that law
-- should be stated alongside the (new) observation.
--------------------------------------------------------------------------------

-- Get the balance of the UTxO.
--
-- balance (applyFirstTx tx) = foldMap tokens (outputs tx)
balance :: UTxO' -> TokenBundle
balance = UTxO.balance . _utxo

txInputsInUTxO :: Tx -> UTxO' -> Set TxIn
txInputsInUTxO = undefined

txOutputsInUTxO :: Tx -> UTxO' -> Set TxOut
txOutputsInUTxO = undefined

--------------------------------------------------------------------------------
-- Helpers (TODO don't belong here)
--------------------------------------------------------------------------------

