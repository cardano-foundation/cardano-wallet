{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns#-}

module Cardano.Wallet.DB.Store.Transactions.StoreSpec
    ( spec
    ) where

import Prelude

import Cardano.DB.Sqlite
    ( ForeignKeysSetting (..)
    , runQuery
    )
import Cardano.Wallet.DB
    ( DBOpen (..)
    )
import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Fixtures
    ( StoreProperty
    , assertWith
    , logScale
    , queryLaw
    , withDBInMemory
    , withStoreProp
    )
import Cardano.Wallet.DB.Layer
    ( DefaultFieldValues (..)
    , withDBOpenFromFile
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (TxId)
    )
import Cardano.Wallet.DB.Store.Transactions.Decoration
    ( DecoratedTxIns
    , decorateTxInsForRelation
    , lookupTxOut
    , mkTxOutKey
    , mkTxOutKeyCollateral
    )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( DeltaTxSet (..)
    , TxRelation (..)
    , TxSet (..)
    , collateralIns
    , ins
    , mkTxSet
    )
import Cardano.Wallet.DB.Store.Transactions.Store
    ( mkStoreTransactions
    , selectTx
    )
import Cardano.Wallet.DB.Store.Transactions.TransactionInfo
    ( mkTxCBOR
    )
import Cardano.Wallet.Flavor
    ( Flavored (..)
    , WalletFlavorS (..)
    )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..)
    )
import Cardano.Wallet.Read.Tx.CBOR
    ( roundTripTxCBor
    )
import Control.Monad
    ( forM_
    , (<=<)
    , (>=>)
    )
import Control.Tracer
    ( nullTracer
    )
import Data.Delta
    ( Delta (..)
    )
import Data.Foldable
    ( toList
    )
import Data.Functor.Identity
    ( Identity (..)
    )
import Data.Generics.Internal.VL
    ( set
    )
import Data.Maybe
    ( mapMaybe
    )
import Data.Store
    ( Store (..)
    )
import System.Directory
    ( copyFile
    )
import System.FilePath
    ( (</>)
    )
import System.IO.Temp
    ( withSystemTempDirectory
    )
import Test.Hspec
    ( Spec
    , around
    , describe
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( Gen
    , Property
    , arbitrary
    , choose
    , elements
    , forAll
    , frequency
    , property
    , vectorOf
    , (===)
    )
import Test.QuickCheck.Monadic
    ( forAllM
    , pick
    )
import Test.Store
    ( GenDelta
    , prop_StoreUpdate
    )
import Test.Utils.Paths
    ( getTestData
    )

import qualified Cardano.Wallet.DB.Store.Transactions.Layer as TxSet
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = do
    around (withDBInMemory ForeignKeysEnabled) $ do
        describe "Transactions store" $ do
            it "respects store laws" $
                property . prop_StoreTransactionsLaws
        describe "selectTx" $
            it "retrieves transaction that was written" $
                property . prop_selectTx
        describe "mkQueryStoreTxSet" $
            it "respects query law" $
                property . prop_QueryLaw

    describe "TxOut decoration" $ do
        it
            "reports a transaction where inputs point \
            \to all other transactions output"
            $ property prop_DecorateLinksTxInToTxOuts
        it
            "reports a transaction where collateral inputs point \
            \to all other transactions output"
            $ property prop_DecorateLinksTxCollateralsToTxOuts

    describe "Transaction CBOR roundtrip" $ do
        it "works on a golden files" $ forM_
            [ Flavored SharedWallet
                "api-bench/sha.a1d5337305630db051fac6da5f8038abf4067068.sqlite"
            , Flavored ShelleyWallet
                "api-bench/she.1ceb45b37a94c7022837b5ca14045f11a5927c65.sqlite"
            , Flavored ByronWallet
                "api-bench/rnd.423b423718660431ebfe9c761cd72e64ee5065ac.sqlite"
            ] $ \(Flavored wF relPath) ->
            withinCopiedFile relPath
                $ \path -> withDBOpenFromFile wF nullTracer
                    (Just defaultFieldValues) path
                $ \DBOpen{atomically} -> do
                    Right (TxSet txSet) <-
                        atomically $ loadS mkStoreTransactions
                    let cbors =
                            mapMaybe (cbor >=> mkTxCBOR) $ toList txSet
                        Right cbors' = mapM roundTripTxCBor cbors
                    cbors `shouldBe` cbors'

defaultFieldValues :: DefaultFieldValues
defaultFieldValues = DefaultFieldValues
    { defaultActiveSlotCoefficient = ActiveSlotCoefficient 1.0
    , defaultDesiredNumberOfPool = 0
    , defaultMinimumUTxOValue = Coin 1_000_000
    , defaultHardforkEpoch = Nothing
    , defaultKeyDeposit = Coin 2_000_000
    }

withinCopiedFile
    :: FilePath
    -> (FilePath -> IO a) -> IO a
withinCopiedFile dbName action = do
    let orig = $(getTestData) </> dbName
    withSystemTempDirectory "migration-db" $ \dir -> do
        let path = dir </> "db.sqlite"
        copyFile orig path
        action path

{-----------------------------------------------------------------------------
    Properties
------------------------------------------------------------------------------}
{- | We check that `decorateTxInsForRelation` indeed decorates transaction inputs.
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
                    , let txin = (W.TxIn txId txOutPos, Just txout)
                    ]
            let guinea' = set #resolvedInputs txins guinea
            pure (guineaId, mkTxSet (guinea' : transactions), txouts)

    forAll transactionsGen $ \(txid, TxSet pile, txouts) ->
        let guinea = pile Map.! txid
            deco   = decorateTxInsForRelation' (TxSet pile) guinea
        in  [ lookupTxOut (mkTxOutKey txin) deco | txin <- ins guinea]
            === map Just txouts

decorateTxInsForRelation' :: TxSet -> TxRelation -> DecoratedTxIns
decorateTxInsForRelation' (TxSet relations) =
    runIdentity . decorateTxInsForRelation lookupTx
  where
    lookupTx txid = Identity $ Map.lookup txid relations

{- | We check that `decorateTxInsForRelation` indeed decorates transaction inputs.
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
                    , let txin = (W.TxIn txId txOutPos, Just txout)
                    ]
            let guinea' = set #resolvedCollateralInputs txins guinea
            pure (guineaId, mkTxSet (guinea' : transactions), txouts)

    forAll transactionsGen $ \(txid, TxSet pile, txouts) ->
        let guinea = pile Map.! txid
            deco   = decorateTxInsForRelation' (TxSet pile) guinea
        in  [ lookupTxOut (mkTxOutKeyCollateral txcol) deco
            | txcol <- collateralIns guinea
            ]
            === map Just txouts

prop_StoreTransactionsLaws :: StoreProperty
prop_StoreTransactionsLaws db =
    prop_StoreUpdate
        (runQuery db)
        (pure mkStoreTransactions)
        (pure mempty)
        (logScale . genDeltas)

prop_selectTx :: StoreProperty
prop_selectTx =
    withStoreProp $ \runQ ->
        forAllM genTxSet $ \txs -> do
            runQ $ writeS mkStoreTransactions txs
            forM_ (Map.assocs $ relations txs) $ \(txId, tx) -> do
                Just tx' <- runQ $ selectTx txId
                assertWith "relation is consistent" $ Left tx == tx'

prop_QueryLaw :: StoreProperty
prop_QueryLaw =
    withStoreProp $ \runQ ->
        forAllM genTxSet $ \txs -> do
            runQ $ writeS TxSet.mkQueryStoreTxSet txs
            forM_ (take 10 $ Map.keys $ relations txs) $ \txId -> do
                assertWith "GetTxById" <=< runQ
                    $ queryLaw TxSet.mkQueryStoreTxSet txs
                    $ TxSet.GetByTxId txId
                index <- pick $ choose (0,5)
                assertWith "GetTxOut" <=< runQ
                    $ queryLaw TxSet.mkQueryStoreTxSet txs
                    $ TxSet.GetTxOut (txId,index)

{-----------------------------------------------------------------------------
    Generators
------------------------------------------------------------------------------}
addCBOR :: Tx -> Gen Tx
addCBOR tx = do
    let mcbor = Nothing
    pure $ tx{txCBOR = mcbor}

-- | Generate interesting changes to 'TxSet'.
genDeltas :: GenDelta DeltaTxSet
genDeltas (TxSet pile) =
    frequency
        [ (8, Append . mkTxSet <$> (arbitrary >>= mapM addCBOR))
        , (1, DeleteTxs . Set.singleton . TxId <$> arbitrary)
        ,
            ( 2
            , DeleteTxs
                <$> if null pile
                    then Set.singleton. TxId <$> arbitrary
                    else do
                        k <- elements [1,3]
                        fmap Set.fromList . vectorOf k . elements $ Map.keys pile
            )
        ]

genTxSet :: Gen TxSet
genTxSet = (`apply` mempty) <$> genDeltas mempty
