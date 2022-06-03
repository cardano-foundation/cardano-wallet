{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE OverloadedLabels #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.DB.Transactions.Select where

import Prelude

import Cardano.DB.Sqlite
    ( chunkSize )
import Cardano.Wallet.DB.Sqlite.Schema
    ( EntityField (..)
    , TxCollateral (..)
    , TxCollateralOut (..)
    , TxCollateralOutToken (..)
    , TxIn (..)
    , TxMeta (..)
    , TxOut (..)
    , TxOutToken (..)
    , TxWithdrawal (..)
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (TxId), getTxId )
import Cardano.Wallet.DB.Transactions.Types
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, interpretQuery, slotToUTCTime )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (AssetId) )
import Control.Arrow
    ( (&&&) )
import Control.Monad.Extra
    ( concatMapM )
import Data.Foldable
    ( fold, toList )
import Data.Function
    ( (&) )
import Data.Functor
    ( (<&>) )
import Data.Functor.Identity
    ( Identity (Identity), runIdentity )
import Data.Generics.Internal.VL
    ( view, (^.) )
import Data.List
    ( nub, sortOn )
import Data.List.Split
    ( chunksOf )
import Data.Map.Strict
    ( Map )
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
    , PersistEntity (PersistEntityBackend)
    , SelectOpt (Asc, Desc)
    , entityVal
    , fieldLens
    , selectFirst
    , selectList
    , (<-.)
    , (==.)
    , (>=.)
    )
import Database.Persist.Sql
    ( SqlBackend, SqlPersistT )
import UnliftIO
    ( liftIO )

import qualified Cardano.Wallet.Primitive.Model as W
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict as M

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

-- Fetch the complete set of tokens associated with a TxOut.
readTxOutTokens :: TxOut -> SqlPersistT IO (TxOut, [TxOutToken])
readTxOutTokens out =
    (out,) . fmap entityVal
        <$> selectList
            [ TxOutTokenTxId ==. txOutputTxId out,
              TxOutTokenTxIndex ==. txOutputIndex out
            ]
            []

-- add txouts to any input2 `t`
addTxOuts
    :: (t -> TxId) -- transaction of the input
    -> (t -> Word32) -- index of the input
    -> [Identity t]
    -> SqlPersistT IO [WithTxOut t]
addTxOuts f g xs = do
    txOuts <- combineChunked xs $ \chunk ->
            traverse readTxOutTokens . fmap entityVal
                =<< selectList
                    [ TxOutputTxId <-. (f . runIdentity <$> chunk)]
                    [Asc TxOutputTxId, Asc TxOutputIndex]
    let txOutMap = Map.fromList . fmap toEntry $ txOuts
            where
            toEntry (out, tokens) = (key, (out, tokens))
                where
                key = (txOutputTxId out, txOutputIndex out)
    pure $ xs <&> \(Identity i) -> WithTxOut i
        $ Map.lookup (f &&& g $ i) txOutMap

-- merge information from utxo on inputs and collaterals
patchToTxRelationA
    :: [(TxMeta, TxRelationF Identity)]
    -> SqlPersistT IO [(TxMeta, TxRelationF WithTxOut)]
patchToTxRelationA rs = do
    ins <- addTxOuts
        do txInputSourceTxId
        do txInputSourceIndex
        do toList rs >>= txRelation_ins . snd
    colls  <- addTxOuts
        do txCollateralSourceTxId
        do txCollateralSourceIndex
        do toList rs >>= txRelation_colls . snd
    pure $ do
            (m, r) <- rs
            pure $ (m,) $ TxRelationF
                do ins
                do colls
                do txRelation_outs r
                do txRelation_collouts r
                do txRelation_withdraws r

-- Fetch the complete set of tokens associated with a TxCollateralOut.
readTxCollateralOutTokens ::
    TxCollateralOut ->
    SqlPersistT IO (TxCollateralOut, [TxCollateralOutToken])
readTxCollateralOutTokens out =
    (out,) . fmap entityVal
        <$> selectList
            [TxCollateralOutTokenTxId ==. txCollateralOutTxId out]
            []
-- select a txRelation starting from the list of meta
selectTxRelation ::
    [TxMeta] ->
    SqlPersistT IO TxHistory
selectTxRelation = fmap (TxHistoryF . fold) . mapM select . chunksOf chunkSize
  where
    select:: [TxMeta] -> SqlPersistT IO (Map TxId (TxMeta, TxRelationF Identity))
    select txmetas = do
        inputs <- select' TxInputTxId
            [Asc TxInputTxId, Asc TxInputOrder]
        collateral <- select' TxCollateralTxId
            [Asc TxCollateralTxId, Asc TxCollateralOrder]
        -- TODO: use join
        outputs <- traverse (traverse readTxOutTokens) =<< select'
            TxOutputTxId
            [Asc TxOutputTxId, Asc TxOutputIndex]
        -- TODO: use join
        collateralOutputs <-
            traverse (traverse readTxCollateralOutTokens) =<< select'
                TxCollateralOutTxId
                [Asc TxCollateralOutTxId]
        withdrawals <- select' TxWithdrawalTxId []
        pure $ fold $ do
                m <- txmetas
                let tid = m ^. #txMetaTxId
                    ofMeta :: Map TxId [a] -> [a]
                    ofMeta = Map.findWithDefault [] tid
                pure $ M.singleton tid $ (m,) $ TxRelationF
                    do Identity <$> ofMeta inputs
                    do Identity <$> ofMeta collateral
                    do ofMeta outputs
                    do ofMeta collateralOutputs
                    do ofMeta withdrawals
      where
        select' ::
            ( PersistEntity b, PersistEntityBackend b ~ SqlBackend)
            => EntityField b TxId
            -> [SelectOpt b]
            -> SqlPersistT IO (Map TxId [b])
        select' k q
            = Map.fromListWith (flip (<>))
                . fmap (view (fieldLens k) &&& pure . entityVal)
                <$> selectList [k <-. (txMetaTxId <$> txmetas)] q


filterTxHistory
    :: Maybe W.Coin
    -> W.SortOrder
    -> (TxMeta -> Bool)
    -> TxHistoryF f
    -> [(TxMeta,TxRelationF f)]
filterTxHistory minWithdrawal order conditions (TxHistoryF rs) = let
    sortTxId = txMetaTxId . fst  & case order of
        W.Ascending -> sortOn . fmap Down
        W.Descending -> sortOn
    sortSlot = txMetaSlot . fst  & case order of
        W.Ascending -> sortOn
        W.Descending -> sortOn . fmap Down
    byCoin = case minWithdrawal of
        Nothing -> id
        Just coin -> filter
            do any (\c -> txWithdrawalAmount c >= coin)
                    . txRelation_withdraws
                    . snd
    filterMeta = filter (conditions . fst)
    in sortSlot $ sortTxId $ byCoin $ filterMeta $ toList rs
{- | Split a query's input values into chunks, run multiple smaller queries,
 and then concatenate the results afterwards. Used to avoid "too many SQL
 variables" errors for "SELECT IN" queries.
-}
combineChunked :: [a] -> ([a] -> SqlPersistT IO [b]) -> SqlPersistT IO [b]
combineChunked xs f = concatMapM f $ chunksOf chunkSize xs

selectWalletTransactionInfoStore
    :: W.Wallet s
    -> TimeInterpreter IO
    -> Maybe W.Coin
    -> W.SortOrder
    -> (TxMeta -> Bool)
    -> TxHistory
    -> SqlPersistT IO [W.TransactionInfo]
selectWalletTransactionInfoStore cp ti minWithdrawal order conditions txs = do
    x' <- patchToTxRelationA $
            filterTxHistory minWithdrawal order conditions txs
    liftIO $ txHistoryFromEntity ti (W.currentTip cp) x'


selectWalletTransactionInfo ::
    W.Wallet s ->
    TimeInterpreter IO ->
    W.WalletId ->
    Maybe W.Coin ->
    W.SortOrder ->
    [Filter TxMeta] ->
    SqlPersistT IO [W.TransactionInfo]
selectWalletTransactionInfo cp ti wid minWithdrawal order conditions = do
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

    relation <- do
        TxHistoryF rs <- selectTxRelation metas
        patchToTxRelationA $ toList rs
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

selectWalletTxRelation :: W.WalletId -> SqlPersistT IO TxHistory
selectWalletTxRelation wid = selectWalletMetas wid >>= selectTxRelation

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
    [(TxMeta, TxRelationF WithTxOut )] ->
    m [W.TransactionInfo]
txHistoryFromEntity ti tip = mapM mkItem
  where
    startTime' = interpretQuery ti . slotToUTCTime
    mkItem (m, r) =
        mkTxWith r
            (txMetaTxId m)
            (txMetaFee m)
            (txMetadata m)
            (mkTxDerived m)
            (txMetaScriptValidity m)
    mkTxWith (TxRelationF ins cins outs couts ws)
            txid mfee meta derived isValid = do
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
