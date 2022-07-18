{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
 Copyright: © 2018-2022 IOHK
 License: Apache-2.0

Pure model for the transactions ('Tx') and metadata about them ('TxMeta')
in a collection of wallets.

-}
module Cardano.Wallet.DB.Store.Wallets.Model
    ( DeltaTxWalletsHistory (..)
    , TxWalletsHistory
    , walletsLinkedTransactions
    , mkTransactionInfo
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
    ( TxId (getTxId) )
import Cardano.Wallet.DB.Store.Meta.Model
    ( DeltaTxMetaHistory (..)
    , ManipulateTxMetaHistory
    , TxMetaHistory (..)
    , mkTxMetaHistory
    )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( Decoration (With)
    , TxHistory
    , TxHistory
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
    ( TxMeta (..), TxOut (..) )
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
    ( (^.) )
import Data.Map.Strict
    ( Map )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set )
import Fmt
    ( Buildable, build )

import qualified Cardano.Wallet.DB.Sqlite.Schema as DB
import qualified Cardano.Wallet.DB.Store.Meta.Model as TxMetaStore
import qualified Cardano.Wallet.DB.Store.Transactions.Model as TxStore
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as WC
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.Tx as WT
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Verbs to change transactions store and wallet-indexed meta stores.
data DeltaTxWalletsHistory
    = ExpandTxWalletsHistory W.WalletId [(WT.Tx, WT.TxMeta)]
    -- ^ Add transactions and meta for a wallet.
    | ChangeTxMetaWalletsHistory W.WalletId ManipulateTxMetaHistory
    -- ^ Change metas for a wallet.
    | GarbageCollectTxWalletsHistory
    -- ^ Delete all transactions that have no metas.
    | RemoveWallet W.WalletId
    -- ^ Remove all metas of a wallet.
    deriving ( Show, Eq )

instance Buildable DeltaTxWalletsHistory where
    build = build . show

-- | Transactions history is a shared transactions store together with
-- a set of meta-transactions stores indexed by wallet.
type TxWalletsHistory = (TxHistory, Map W.WalletId TxMetaHistory)

instance Delta DeltaTxWalletsHistory where
    type Base DeltaTxWalletsHistory = TxWalletsHistory
    apply (ExpandTxWalletsHistory wid cs) (txh,mtxmh) =
        ( apply (TxStore.Append $ mkTxHistory $ fst <$> cs) txh
        , mtxmh & case Map.lookup wid mtxmh of
              Nothing -> apply @(DeltaMap _ DeltaTxMetaHistory)
                  $ Insert wid
                  $ mkTxMetaHistory wid cs
              Just _ -> apply @(DeltaMap _ DeltaTxMetaHistory)
                  $ Adjust wid
                  $ TxMetaStore.Expand
                  $ mkTxMetaHistory wid cs)
    apply (ChangeTxMetaWalletsHistory wid change) (txh, mtxmh) =
        (txh, garbageCollectEmptyWallets
            $ mtxmh & apply (Adjust wid $ Manipulate change))
    apply GarbageCollectTxWalletsHistory (TxHistoryF txh, mtxmh) =
        ( TxHistoryF $ Map.restrictKeys txh $ walletsLinkedTransactions mtxmh
        , mtxmh)
    apply (RemoveWallet wid) (TxHistoryF txh, mtxmh) =
        ( TxHistoryF txh, Map.delete wid mtxmh )

-- necessary because database will not distinuish between
-- a missing wallet in the map
-- and a wallet that has no meta-transactions
garbageCollectEmptyWallets :: Map k TxMetaHistory -> Map k TxMetaHistory
garbageCollectEmptyWallets = Map.filter (not . null . relations)

linkedTransactions :: TxMetaHistory -> Set TxId
linkedTransactions (TxMetaHistory m) = Map.keysSet m

walletsLinkedTransactions :: Map W.WalletId TxMetaHistory -> Set TxId
walletsLinkedTransactions = Set.unions . toList .  fmap linkedTransactions

-- | Compute a high level view of a transaction known as 'TransactionInfo'
-- from a 'TxMeta' and a 'TxRelationF'.
-- Assumes that these data refer to the same 'TxId', does /not/ check this.
mkTransactionInfo :: Monad m
    => TimeInterpreter m
    -> W.BlockHeader
    -> TxRelationF 'With
    -> DB.TxMeta
    -> m WT.TransactionInfo
mkTransactionInfo ti tip TxRelationF{..} DB.TxMeta{..} = do
    txTime <- interpretQuery ti . slotToUTCTime $ txMetaSlot
    return
        $ WT.TransactionInfo
        { WT.txInfoId = getTxId txMetaTxId
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
