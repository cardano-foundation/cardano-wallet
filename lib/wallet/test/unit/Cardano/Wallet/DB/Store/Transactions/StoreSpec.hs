{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.DB.Store.Transactions.StoreSpec (spec) where

import Prelude

import Cardano.DB.Sqlite
    ( ForeignKeysSetting (..) )
import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Fixtures
    ( StoreProperty, logScale, withDBInMemory, withStoreProp )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (TxId) )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( DeltaTxHistory (..)
    , TxHistoryF (..)
    , WithTxOut (..)
    , collateralIns
    , decorateWithTxOuts
    , ins
    , mkTxCollateral
    , mkTxHistory
    , mkTxIn
    , mkTxOut
    , undecorateFromTxOuts
    )
import Cardano.Wallet.DB.Store.Transactions.Store
    ( mkStoreTransactions )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..) )
import Control.Monad.State.Strict
    ( MonadState (get), evalState, modify )
import Data.Generics.Internal.VL
    ( set )
import List.Transformer
    ( select )
import Test.DBVar
    ( GenDelta, prop_StoreUpdates )
import Test.Hspec
    ( Spec, around, describe, it, shouldBe )
import Test.QuickCheck
    ( Property, arbitrary, elements, forAll, frequency, property )

import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Control.Foldl as L
import qualified Data.Map.Strict as Map
import qualified List.Transformer as LT

spec :: Spec
spec = do
    around (withDBInMemory ForeignKeysEnabled) $ do
        describe "Transactions store" $ do
            it "respects store laws" $
                property . prop_StoreLaws

    describe "TxOut decoration" $ do
        it "respects order and content of transactions" $
            property prop_DecorateIsInvertible
        it
            "reports a transaction where inputs point \
            \to all other transactions output"
            $ property prop_DecorateLinksTxInToTxOuts
        it
            "reports a transaction where collateral inputs point \
            \to all other transactions output"
            $ property prop_DecorateLinksTxCollateralsToTxOuts

prop_DecorateIsInvertible :: [Tx] -> Bool
prop_DecorateIsInvertible transactions =
    let txh = mkTxHistory transactions
     in undecorateFromTxOuts (decorateWithTxOuts txh) == txh

{- | We check that `decorateWithTxOuts` indeed decorates transaction inputs.
We do this by generating a set of random transactions, as well as a
"guinea pig" transaction, whose inputs point to all outputs
of the other transactions. Then, we expect that decorating the history
will decorate all inputs of the "guinea pig" transaction.
-}
prop_DecorateLinksTxInToTxOuts :: Property
prop_DecorateLinksTxInToTxOuts = do
    let transactionsGen = do
            transactions :: [W.Tx] <- arbitrary
            guinea <- arbitrary
            let guineaId = TxId $ txId guinea
                txouts = flip evalState 0
                    . L.purely LT.fold L.list
                    $ do
                        Tx{..} <- select transactions
                        txOut@(txOutPos, _) <- select $ zip [0 ..] outputs
                        position <- get
                        modify succ
                        let newInput = (W.TxIn txId txOutPos, W.Coin 0)
                        pure
                            ( newInput
                            ,
                                ( mkTxIn guineaId (position, newInput)
                                , mkTxOut (TxId txId) txOut
                                )
                            )
            let guinea' = set #resolvedInputs (fst <$> txouts) guinea
                TxHistoryF result =
                    decorateWithTxOuts $
                        mkTxHistory (guinea' : transactions)
            pure (guineaId, result, txouts)
    forAll transactionsGen $ \(idx, result, txouts) -> do
        ins (result Map.! idx)
            `shouldBe` [ WithTxOut txin (Just txout)
                       | (_, (txin, txout)) <- txouts
                       ]

{- | We check that `decorateWithTxOuts` indeed decorates transaction inputs.
We do this by generating a set of random transactions, as well as a
"guinea pig" transaction, whose collaterals point to all outputs
of the other transactions. Then, we expect that decorating the history
will decorate all collaterals of the "guinea pig" transaction.
-}
prop_DecorateLinksTxCollateralsToTxOuts :: Property
prop_DecorateLinksTxCollateralsToTxOuts = do
    let transactionsGen = do
            transactions :: [W.Tx] <- arbitrary
            guinea <- arbitrary
            let guineaId = TxId $ txId guinea
                txouts = flip evalState 0
                    . L.purely LT.fold L.list
                    $ do
                        Tx{..} <- select transactions
                        txOut@(txOutPos, _) <- select $ zip [0 ..] outputs
                        position <- get
                        modify succ
                        let newInput = (W.TxIn txId txOutPos, W.Coin 0)
                        pure
                            ( newInput
                            ,
                                ( mkTxCollateral guineaId (position, newInput)
                                , mkTxOut (TxId txId) txOut
                                )
                            )
            let guinea' =
                    set
                        #resolvedCollateralInputs
                        (fst <$> txouts)
                        guinea
                TxHistoryF result =
                    decorateWithTxOuts $
                        mkTxHistory (guinea' : transactions)
            pure (guineaId, result, txouts)
    forAll transactionsGen $ \(idx, result, txouts) -> do
        collateralIns (result Map.! idx)
            `shouldBe` [ WithTxOut txin (Just txout)
                       | (_, (txin, txout)) <- txouts
                       ]

prop_StoreLaws :: StoreProperty
prop_StoreLaws = withStoreProp $ \runQ ->
    prop_StoreUpdates
        runQ
        mkStoreTransactions
        (pure mempty)
        (logScale . genDeltas)

-- | Generate interesting changes to 'TxHistory'.
genDeltas :: GenDelta DeltaTxHistory
genDeltas (TxHistoryF history) =
    frequency
        [ (8, Append . mkTxHistory <$> arbitrary)
        , (1, DeleteTx . TxId <$> arbitrary)
        ,
            ( 2
            , DeleteTx
                <$> if null history
                    then TxId <$> arbitrary
                    else elements (Map.keys history)
            )
        ]
