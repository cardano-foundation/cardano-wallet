{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.DB.Store.Transactions.StoreSpec
    ( spec
    ) where

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
    ( DeltaTxSet (..)
    , TxSet (..)
    , collateralIns
    , decorateTxIns
    , ins
    , lookupTxOutForTxCollateral
    , lookupTxOutForTxIn
    , mkTxSet
    )
import Cardano.Wallet.DB.Store.Transactions.Store
    ( mkStoreTransactions )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..) )
import Data.Generics.Internal.VL
    ( set )
import Test.DBVar
    ( GenDelta, prop_StoreUpdates )
import Test.Hspec
    ( Spec, around, describe, it )
import Test.QuickCheck
    ( Gen, Property, arbitrary, elements, forAll, frequency, property, (===) )

import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    around (withDBInMemory ForeignKeysEnabled) $ do
        describe "Transactions store" $ do
            it "respects store laws" $
                property . prop_StoreLaws

    describe "TxOut decoration" $ do
        it
            "reports a transaction where inputs point \
            \to all other transactions output"
            $ property prop_DecorateLinksTxInToTxOuts
        it
            "reports a transaction where collateral inputs point \
            \to all other transactions output"
            $ property prop_DecorateLinksTxCollateralsToTxOuts

{-----------------------------------------------------------------------------
    Properties
------------------------------------------------------------------------------}
{- | We check that `decorateTxIns` indeed decorates transaction inputs.
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
                (txins, txouts) = unzip
                    [ (txin, txout)
                    | Tx{txId,outputs} <- transactions
                    , (txOutPos, txout) <- zip [0 ..] outputs
                    , let txin = (W.TxIn txId txOutPos, W.Coin 0)
                    ]
            let guinea' = set #resolvedInputs txins guinea
            pure (guineaId, mkTxSet (guinea' : transactions), txouts)

    forAll transactionsGen $ \(txid, TxSet pile, txouts) ->
        let guinea = pile Map.! txid
            deco   = decorateTxIns (TxSet pile) guinea
        in  [ lookupTxOutForTxIn txin deco | txin <- ins guinea]
            === map Just txouts

{- | We check that `decorateTxIns` indeed decorates transaction inputs.
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
                (txins, txouts) = unzip
                    [ (txin, txout)
                    | Tx{txId,outputs} <- transactions
                    , (txOutPos, txout) <- zip [0 ..] outputs
                    , let txin = (W.TxIn txId txOutPos, W.Coin 0)
                    ]
            let guinea' = set #resolvedCollateralInputs txins guinea
            pure (guineaId, mkTxSet (guinea' : transactions), txouts)

    forAll transactionsGen $ \(txid, TxSet pile, txouts) ->
        let guinea = pile Map.! txid
            deco   = decorateTxIns (TxSet pile) guinea
        in  [ lookupTxOutForTxCollateral txcol deco
            | txcol <- collateralIns guinea
            ]
            === map Just txouts

prop_StoreLaws :: StoreProperty
prop_StoreLaws = withStoreProp $ \runQ ->
    prop_StoreUpdates
        runQ
        mkStoreTransactions
        (pure mempty)
        (logScale . genDeltas)

addCBOR :: Tx -> Gen Tx
addCBOR tx = do
    mcbor <- arbitrary
    pure $ tx{txCBOR = mcbor}

-- | Generate interesting changes to 'TxSet'.
genDeltas :: GenDelta DeltaTxSet
genDeltas (TxSet pile) =
    frequency
        [ (8, Append . mkTxSet <$> (arbitrary >>= mapM addCBOR))
        , (1, DeleteTx . TxId <$> arbitrary)
        ,
            ( 2
            , DeleteTx
                <$> if null pile
                    then TxId <$> arbitrary
                    else elements (Map.keys pile)
            )
        ]
