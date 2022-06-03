{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.DB.Transactions.Select
    ( selectTxHistory
    , selectTxMeta
    )
    where

import Cardano.DB.Sqlite
    ( chunkSize )
import Cardano.Wallet.DB.Sqlite.Schema
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (TxId), getTxId )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, interpretQuery, slotToUTCTime )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (AssetId) )
import Control.Monad.Extra
    ( concatMapM )
import Data.Foldable
    ( fold )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL
    ( (^.) )
import Data.List
    ( nub, sortOn )
import Data.List.Split
    ( chunksOf )
import Data.Map.Strict
    ( Map )
import qualified Data.Map.Strict as Map
import Data.Maybe
    ( listToMaybe )
import Data.Ord
    ( Down (Down) )
import Data.Quantity
    ( Quantity (Quantity), getQuantity )
import Data.Word
    ( Word32 )
import Database.Persist
    ( Filter
    , SelectOpt (Asc, Desc)
    , entityVal
    , selectFirst
    , selectList
    , (<-.)
    , (==.)
    , (>=.)
    )
import Database.Persist.Sql
    ( SqlPersistT )
import Prelude
import UnliftIO
    ( liftIO )

import Cardano.Wallet.DB.Transactions.Types
    ( TxRelation
    , TxRelationA
    , TxRelationF (TxRelationF)
    , WithTxOut (WithTxOut, withTxOut_value)
    )
import qualified Cardano.Wallet.Primitive.Model as W
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import Data.Functor.Identity
    ( Identity (Identity), runIdentity )

-- This relies on available information from the database to reconstruct coin
-- selection information for __outgoing__ payments. We can't however guarantee
-- that we have such information for __incoming__ payments (we usually don't
-- have it).
--
-- To reliably provide this information for incoming payments, it should be
-- looked up when applying blocks from the global ledger, but that is future
-- work.
--
-- See also: issue #573.

selectTxs ::
    [TxMeta] ->
    SqlPersistT IO TxRelationA
selectTxs metas = do
    TxRelationF _metas inputs collateral outputs collateralOutputs withdrawals <-
        selectTxs' metas
    resolvedInputs <- fmap toOutputMap $
        combineChunked inputs $ \inputsChunk ->
            traverse readTxOutTokens . fmap entityVal
                =<< selectList
                    [ TxOutputTxId
                        <-. (txInputSourceTxId . runIdentity <$> inputsChunk)
                    ]
                    [Asc TxOutputTxId, Asc TxOutputIndex]

    resolvedCollateral <- fmap toOutputMap $
        combineChunked collateral $ \collateralChunk ->
            traverse readTxOutTokens . fmap entityVal
                =<< selectList
                    [ TxOutputTxId
                        <-. ( txCollateralSourceTxId . runIdentity
                                <$> collateralChunk
                            )
                    ]
                    [Asc TxOutputTxId, Asc TxOutputIndex]
    let inputs' = inputs `resolveInputWith` resolvedInputs
        collateral' = collateral `resolveCollateralWith` resolvedCollateral
    pure $
        TxRelationF
            metas
            inputs'
            collateral'
            outputs
            collateralOutputs
            withdrawals

readTxOutTokens :: TxOut -> SqlPersistT IO (TxOut, [TxOutToken])
readTxOutTokens out =
    (out,) . fmap entityVal
        <$> selectList
            [ TxOutTokenTxId ==. txOutputTxId out,
              TxOutTokenTxIndex ==. txOutputIndex out
            ]
            []

toOutputMap ::
    [(TxOut, [TxOutToken])] ->
    Map (TxId, Word32) (TxOut, [TxOutToken])
toOutputMap = Map.fromList . fmap toEntry
  where
    toEntry (out, tokens) = (key, (out, tokens))
      where
        key = (txOutputTxId out, txOutputIndex out)

resolveInputWith ::
    [Identity TxIn] ->
    Map (TxId, Word32) (TxOut, [TxOutToken]) ->
    [WithTxOut TxIn]
resolveInputWith inputs resolvedInputs =
    [ (WithTxOut i $ Map.lookup key resolvedInputs)
      | Identity i <- inputs,
        let key = (txInputSourceTxId i, txInputSourceIndex i)
    ]

resolveCollateralWith ::
    [Identity TxCollateral] ->
    Map (TxId, Word32) (TxOut, [TxOutToken]) ->
    [WithTxOut TxCollateral]
resolveCollateralWith collateral resolvedCollateral =
    [ (WithTxOut i $ Map.lookup key resolvedCollateral)
      | Identity i <- collateral,
        let key =
                ( txCollateralSourceTxId i,
                  txCollateralSourceIndex i
                )
    ]

selectTxs' ::
    [TxMeta] ->
    SqlPersistT IO TxRelation
selectTxs' = fmap fold . mapM select . chunksOf chunkSize
  where
    select txmetas = do
        inputs <- select'
            [TxInputTxId <-. txids]
            [Asc TxInputTxId, Asc TxInputOrder]
        collateral <- select'
            [TxCollateralTxId <-. txids]
            [Asc TxCollateralTxId, Asc TxCollateralOrder]
        outputs <- traverse readTxOutTokens =<< select'
            [TxOutputTxId <-. txids]
            [Asc TxOutputTxId, Asc TxOutputIndex]
        collateralOutputs <- traverse readTxCollateralOutTokens =<< select'
            [TxCollateralOutTxId <-. txids]
            [Asc TxCollateralOutTxId]
        withdrawals <- select' [TxWithdrawalTxId <-. txids] []
        pure $
            TxRelationF
                txmetas
                do Identity <$> inputs
                do Identity <$> collateral
                outputs
                collateralOutputs
                withdrawals
      where
        txids = txMetaTxId <$> txmetas
    select' f q = fmap entityVal <$> selectList f q
    -- Fetch the complete set of tokens associated with a TxOut.
    -- Fetch the complete set of tokens associated with a TxCollateralOut.
    --
    readTxCollateralOutTokens ::
        TxCollateralOut ->
        SqlPersistT IO (TxCollateralOut, [TxCollateralOutToken])
    readTxCollateralOutTokens out =
        (out,) . fmap entityVal
            <$> selectList
                [TxCollateralOutTokenTxId ==. txCollateralOutTxId out]
                []

{- | Split a query's input values into chunks, run multiple smaller queries,
 and then concatenate the results afterwards. Used to avoid "too many SQL
 variables" errors for "SELECT IN" queries.
-}
combineChunked :: [a] -> ([a] -> SqlPersistT IO [b]) -> SqlPersistT IO [b]
combineChunked xs f = concatMapM f $ chunksOf chunkSize xs

selectTxHistory ::
    W.Wallet s ->
    TimeInterpreter IO ->
    W.WalletId ->
    Maybe W.Coin ->
    W.SortOrder ->
    [Filter TxMeta] ->
    SqlPersistT IO [W.TransactionInfo]
selectTxHistory cp ti wid minWithdrawal order conditions = do
    let txMetaFilter = (TxMetaWalletId ==. wid) : conditions
    metas <- case minWithdrawal of
        Nothing -> fmap entityVal <$> selectList txMetaFilter sortOpt
        Just coin -> do
            txids <-
                fmap (txWithdrawalTxId . entityVal)
                    <$> selectList [TxWithdrawalAmount >=. coin] []
            ms <-
                combineChunked
                    (nub txids)
                    ( \chunk ->
                        selectList
                            ((TxMetaTxId <-. chunk) : txMetaFilter)
                            []
                    )
            let sortTxId = case order of
                    W.Ascending -> sortOn (Down . txMetaTxId)
                    W.Descending -> sortOn txMetaTxId
            let sortSlot = case order of
                    W.Ascending -> sortOn txMetaSlot
                    W.Descending -> sortOn (Down . txMetaSlot)
            pure $ sortSlot $ sortTxId $ fmap entityVal ms

    relation <- selectTxs metas
    liftIO $ txHistoryFromEntity ti (W.currentTip cp) relation
  where
    -- Note: there are sorted indices on these columns.
    -- The secondary sort by TxId is to make the ordering stable
    -- so that testing with random data always works.
    sortOpt = case order of
        W.Ascending -> [Asc TxMetaSlot, Desc TxMetaTxId]
        W.Descending -> [Desc TxMetaSlot, Asc TxMetaTxId]

selectWalletMetas ::
    W.WalletId ->
    SqlPersistT IO [TxMeta]
selectWalletMetas wid = do
    let txMetaFilter = [TxMetaWalletId ==. wid]
    fmap entityVal <$> selectList txMetaFilter []

selectWalletTxRelation :: W.WalletId -> SqlPersistT IO TxRelationA 
selectWalletTxRelation wid = selectWalletMetas wid >>= selectTxs

selectTxMeta ::
    W.WalletId ->
    W.Hash "Tx" ->
    SqlPersistT IO (Maybe TxMeta)
selectTxMeta wid tid =
    fmap entityVal <$> selectFirst
        [ TxMetaWalletId ==. wid, TxMetaTxId ==. (TxId tid)]
        [ Desc TxMetaSlot ]
-- note: TxIn records must already be sorted by order
-- and TxOut records must already be sorted by index
txHistoryFromEntity ::
    Monad m =>
    TimeInterpreter m ->
    W.BlockHeader ->
    TxRelationA ->
    m [W.TransactionInfo]
txHistoryFromEntity ti tip (TxRelationF metas ins cins outs couts ws) =
    mapM mkItem metas
  where
    startTime' = interpretQuery ti . slotToUTCTime
    mkItem m =
        mkTxWith
            (txMetaTxId m)
            (txMetaFee m)
            (txMetadata m)
            (mkTxDerived m)
            (txMetaScriptValidity m)
    mkTxWith txid mfee meta derived isValid = do
        t <- startTime' (derived ^. #slotNo)
        return $
            W.TransactionInfo
                { W.txInfoId =
                    getTxId txid,
                  W.txInfoFee =
                    W.Coin . fromIntegral <$> mfee,
                  W.txInfoInputs =
                    map mkTxIn $
                        filter
                            ((== txid) . txInputTxId . withTxOut_value)
                            ins,
                  W.txInfoCollateralInputs =
                    map mkTxCollateral $
                        filter
                            ((== txid) . txCollateralTxId . withTxOut_value)
                            cins,
                  W.txInfoOutputs =
                    map mkTxOut $ filter ((== txid) . txOutputTxId . fst) outs,
                  W.txInfoCollateralOutput =
                    -- This conversion from a list is correct, as the primary key
                    -- for the TxCollateralOut table guarantees that there can be
                    -- at most one collateral output for a given transaction.
                    listToMaybe $
                        map mkTxCollateralOut $
                            filter
                                ((== txid) . txCollateralOutTxId . fst)
                                couts,
                  W.txInfoWithdrawals =
                    Map.fromList $
                        map mkTxWithdrawal $
                            filter ((== txid) . txWithdrawalTxId) ws,
                  W.txInfoMeta =
                    derived,
                  W.txInfoMetadata =
                    meta,
                  W.txInfoDepth =
                    Quantity $
                        fromIntegral $
                            if tipH > txH
                                then tipH - txH
                                else 0,
                  W.txInfoTime =
                    t,
                  W.txInfoScriptValidity =
                    isValid <&> \case
                        False -> W.TxScriptInvalid
                        True -> W.TxScriptValid
                }
      where
        txH = getQuantity (derived ^. #blockHeight)
        tipH = getQuantity (tip ^. #blockHeight)
    mkTxIn (WithTxOut tx out) =
        ( W.TxIn
            { W.inputId = getTxId (txInputSourceTxId tx),
              W.inputIx = txInputSourceIndex tx
            },
          txInputSourceAmount tx,
          mkTxOut <$> out
        )
    mkTxCollateral (WithTxOut tx out) =
        ( W.TxIn
            { W.inputId = getTxId (txCollateralSourceTxId tx),
              W.inputIx = txCollateralSourceIndex tx
            },
          txCollateralSourceAmount tx,
          mkTxOut <$> out
        )
    mkTxOut (out, tokens) =
        W.TxOut
            { W.address = txOutputAddress out,
              W.tokens =
                TokenBundle.fromFlatList
                    (txOutputAmount out)
                    (mkTxOutToken <$> tokens)
            }
    mkTxOutToken token =
        ( AssetId (txOutTokenPolicyId token) (txOutTokenName token),
          txOutTokenQuantity token
        )
    mkTxCollateralOut (out, tokens) =
        W.TxOut
            { W.address = txCollateralOutAddress out,
              W.tokens =
                TokenBundle.fromFlatList
                    (txCollateralOutAmount out)
                    (mkTxCollateralOutToken <$> tokens)
            }
    mkTxCollateralOutToken token =
        ( AssetId
            (txCollateralOutTokenPolicyId token)
            (txCollateralOutTokenName token),
          txCollateralOutTokenQuantity token
        )
    mkTxWithdrawal w =
        ( txWithdrawalAccount w,
          txWithdrawalAmount w
        )
    mkTxDerived m =
        W.TxMeta
            { W.status = txMetaStatus m,
              W.direction = txMetaDirection m,
              W.slotNo = txMetaSlot m,
              W.blockHeight = Quantity (txMetaBlockHeight m),
              W.amount = txMetaAmount m,
              W.expiry = txMetaSlotExpires m
            }
