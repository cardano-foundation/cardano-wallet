{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
 Copyright: © 2018-2022 IOHK
 License: Apache-2.0

Pure model for the transactions ('Tx') and metadata about them ('TxMeta')
in a collection of wallets.

-}
module Cardano.Wallet.DB.Store.Wallets.Model
    ( DeltaTxWalletsHistory (..)
    , DeltaWalletsMetaWithSubmissions (..)
    , TxWalletsHistory
    , mkTransactionInfo
    , walletsLinkedTransactions
    , mkTxHistoryWithCBORs
    ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( TxCollateral (..)
    , TxCollateralOut (..)
    , TxCollateralOutToken (..)
    , TxIn (..)
    , TxMeta (..)
    , TxOut (..)
    , TxOutToken (..)
    , TxWithdrawal (..)
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..) )
import Cardano.Wallet.DB.Store.Meta.Model
    ( DeltaTxMetaHistory (..), TxMetaHistory (..), mkTxMetaHistory )
import Cardano.Wallet.DB.Store.Submissions.Model
    ( DeltaTxLocalSubmission (..), TxLocalSubmissionHistory (..) )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( Decoration (With)
    , TxHistoryF (..)
    , TxRelationF (..)
    , WithTxOut (..)
    , mkTxHistory
    )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, interpretQuery, slotToUTCTime )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (AssetId) )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..), TxCBOR, TxMeta (..), TxOut (..) )
import Data.Delta
    ( Delta (..) )
import Data.DeltaMap
    ( DeltaMap (Adjust, Insert) )
import Data.Foldable
    ( toList )
import Data.Function
    ( (&) )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL
    ( over, view, (^.) )
import Data.Map.Strict
    ( Map )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set )
import Fmt
    ( Buildable, build )

import qualified Cardano.Wallet.DB.Sqlite.Schema as DB
import Cardano.Wallet.DB.Store.CBOR.Model
    ( TxCBORHistory (TxCBORHistory) )
import qualified Cardano.Wallet.DB.Store.Meta.Model as TxMetaStore
import Cardano.Wallet.DB.Store.TransactionsWithCBOR.Model
    ( TxHistoryWithCBOR (..) )
import qualified Cardano.Wallet.DB.Store.TransactionsWithCBOR.Model as TxStore
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as WC
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.Tx as WT
import qualified Data.Map.Strict as Map
import Data.Maybe
    ( maybeToList )
import qualified Data.Set as Set

data DeltaTxWalletsHistory
    = ExpandTxWalletsHistory W.WalletId [(WT.Tx, WT.TxMeta)]
    | ChangeTxMetaWalletsHistory W.WalletId DeltaWalletsMetaWithSubmissions
    | GarbageCollectTxWalletsHistory
    | RemoveWallet W.WalletId
    deriving ( Show, Eq )

instance Buildable DeltaTxWalletsHistory where
    build = build . show

data DeltaWalletsMetaWithSubmissions
    = ChangeMeta DeltaTxMetaHistory
    | ChangeSubmissions DeltaTxLocalSubmission
    deriving ( Show, Eq )

type MetasAndSubmissionsHistory = (TxMetaHistory, TxLocalSubmissionHistory)

constraintSubmissions
    :: MetasAndSubmissionsHistory -> MetasAndSubmissionsHistory
constraintSubmissions (metas,submissions) =
    ( metas
    , over #relations (\m -> Map.restrictKeys m
                       $ Map.keysSet (metas ^. #relations)) submissions)

instance Delta DeltaWalletsMetaWithSubmissions where
    type Base DeltaWalletsMetaWithSubmissions = MetasAndSubmissionsHistory
    apply (ChangeMeta cm) (metas,submissions) =
        constraintSubmissions (apply cm metas, submissions)
    apply (ChangeSubmissions cs) (metas,submissions) =
        constraintSubmissions (metas, apply cs submissions)

type TxWalletsHistory =
    (TxHistoryWithCBOR, Map W.WalletId MetasAndSubmissionsHistory)

mkTxHistoryWithCBORs :: [WT.Tx] -> TxHistoryWithCBOR
mkTxHistoryWithCBORs cs = TxHistoryWithCBOR (mkTxHistory cs)
    $ TxCBORHistory $ Map.fromList
        [(TxId txId, cbor) | Tx{..} <- cs, cbor <- maybeToList txCBOR]

instance Delta DeltaTxWalletsHistory where
    type Base DeltaTxWalletsHistory = TxWalletsHistory
    apply (ExpandTxWalletsHistory wid cs) (txh,mtxmh) =
        ( apply (TxStore.Append $ mkTxHistoryWithCBORs $ fst <$> cs) txh
        , flip apply mtxmh $ case Map.lookup wid mtxmh of
              Nothing -> Insert wid (mkTxMetaHistory wid cs, mempty)
              Just _ ->
                  Adjust wid
                  $ ChangeMeta
                  $ TxMetaStore.Expand
                  $ mkTxMetaHistory wid cs)
    apply (ChangeTxMetaWalletsHistory wid change) (txh, mtxmh) =
        (txh, garbageCollectEmptyWallets
            $ mtxmh & apply (Adjust wid change)
            )
    apply GarbageCollectTxWalletsHistory
        (TxHistoryWithCBOR (TxHistoryF txh) (TxCBORHistory cborh) , mtxmh) =
            let gc :: Map TxId x -> Map TxId x
                gc x = Map.restrictKeys x
                    $ walletsLinkedTransactions mtxmh
            in (TxHistoryWithCBOR
                (TxHistoryF $ gc txh)
                (TxCBORHistory $ gc cborh)
                    , mtxmh)
    apply (RemoveWallet wid) (x , mtxmh) = (x, Map.delete wid mtxmh)


-- necessary because database will not distinguish between
-- a missing wallet in the map
-- and a wallet that has no meta-transactions
garbageCollectEmptyWallets :: Map k MetasAndSubmissionsHistory
    -> Map k MetasAndSubmissionsHistory
garbageCollectEmptyWallets = Map.filter (not . null . view #relations . fst)

linkedTransactions :: MetasAndSubmissionsHistory -> Set TxId
linkedTransactions (TxMetaHistory m,_) = Map.keysSet m

walletsLinkedTransactions
    :: Map W.WalletId MetasAndSubmissionsHistory -> Set TxId
walletsLinkedTransactions = Set.unions . toList . fmap linkedTransactions

-- | Compute a high level view of a transaction known as 'TransactionInfo'
-- from a 'TxMeta' and a 'TxRelationF'.
-- Assumes that these data refer to the same 'TxId', does /not/ check this.
mkTransactionInfo :: Monad m
    => TimeInterpreter m
    -> W.BlockHeader
    -> TxRelationF 'With
    -> Maybe TxCBOR
    -> DB.TxMeta
    -> m WT.TransactionInfo
mkTransactionInfo ti tip TxRelationF{..} txCBOR DB.TxMeta{..} = do
    txTime <- interpretQuery ti . slotToUTCTime $ txMetaSlot
    return
        $ WT.TransactionInfo
        { WT.txInfoId = getTxId txMetaTxId
        , WT.txInfoCBOR = txCBOR
        , WT.txInfoFee = WC.Coin . fromIntegral <$> txMetaFee
        , WT.txInfoInputs = mkTxIn <$> ins
        , WT.txInfoCollateralInputs = mkTxCollateral <$> collateralIns
        , WT.txInfoOutputs = mkTxOut <$> outs
        , WT.txInfoCollateralOutput = mkTxCollateralOut <$> collateralOuts
        , WT.txInfoWithdrawals = Map.fromList $ map mkTxWithdrawal withdrawals
        , WT.txInfoMeta = WT.TxMeta
              { WT.status = txMetaStatus
              , WT.direction = txMetaDirection
              , WT.slotNo = txMetaSlot
              , WT.blockHeight = Quantity (txMetaBlockHeight)
              , amount = txMetaAmount
              , WT.expiry = txMetaSlotExpires
              }
        , WT.txInfoMetadata = txMetadata
        , WT.txInfoDepth = Quantity
              $ fromIntegral
              $ if tipH > txMetaBlockHeight
                  then tipH - txMetaBlockHeight
                  else 0
        , WT.txInfoTime = txTime
        , WT.txInfoScriptValidity = txMetaScriptValidity <&> \case
              False -> WT.TxScriptInvalid
              True -> WT.TxScriptValid
        }
  where
    tipH = getQuantity $ tip ^. #blockHeight
    mkTxIn (WithTxOut tx out) =
        ( WT.TxIn
          { WT.inputId = getTxId (txInputSourceTxId tx)
          , WT.inputIx = txInputSourceIndex tx
          }
        , txInputSourceAmount tx
        , mkTxOut <$> out)
    mkTxCollateral (WithTxOut tx out) =
        ( WT.TxIn
          { WT.inputId = getTxId (txCollateralSourceTxId tx)
          , WT.inputIx = txCollateralSourceIndex tx
          }
        , txCollateralSourceAmount tx
        , mkTxOut <$> out)
    mkTxOut (out,tokens) =
        WT.TxOut
        { address = txOutputAddress out
        , WT.tokens = TokenBundle.fromFlatList
              (txOutputAmount out)
              (mkTxOutToken <$> tokens)
        }
    mkTxOutToken token =
        ( AssetId (txOutTokenPolicyId token) (txOutTokenName token)
        , txOutTokenQuantity token)
    mkTxCollateralOut (out,tokens) =
        WT.TxOut
        { address = txCollateralOutAddress out
        , WT.tokens = TokenBundle.fromFlatList
              (txCollateralOutAmount out)
              (mkTxCollateralOutToken <$> tokens)
        }
    mkTxCollateralOutToken token =
        ( AssetId
              (txCollateralOutTokenPolicyId token)
              (txCollateralOutTokenName token)
        , txCollateralOutTokenQuantity token)
    mkTxWithdrawal w = (txWithdrawalAccount w, txWithdrawalAmount w)
