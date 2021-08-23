{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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

   -- * Observations
   , balance
   ) where

import Prelude

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

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.Tx as Tx
import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data UTxO' = UTxO'
    { _utxo :: !UTxO
    , _lastTx :: !Tx
    }

applyFirstTx :: Tx -> UTxO'
applyFirstTx tx =
    let
        indexedOutputs = zip [0..] (view #outputs tx)
        utxo = UTxO
               . Map.fromList
               . fmap (Bifunctor.first $ TxIn $ view #txId tx)
               $ indexedOutputs
    in
        UTxO' utxo tx

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
      UTxO' newUTxO tx

balance :: UTxO' -> TokenBundle
balance = UTxO.balance . _utxo

-- Get the elements in u2 that are not in u1. In the case that elements are in
-- both, get the difference of the value of the TxOut (the TokenBundle value) in
-- both entries, removing any entries that are fully spent.
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
        UTxO' (UTxO $ Map.differenceWith diffFunc u2' u1') (_lastTx u2)
