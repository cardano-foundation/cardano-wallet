{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright: © 2022 IOHK
License: Apache-2.0

Data type 'TxSet' for storing a set of transactions.
Transactions are encoded "as" expressed in DB tables.

-}
module Cardano.Wallet.DB.Store.Transactions.Model
    ( DeltaTxSet (..)
    , TxSet (..)
    , TxRelation (..)
    , tokenCollateralOrd
    , tokenOutOrd
    , mkTxSet
    , WalletTransactions

    -- * Type conversion from wallet types
    , mkTxIn
    , mkTxCollateral
    , mkTxOut

    -- * Type conversions to wallet types
    , fromTxOut
    , fromTxCollateralOut
    , txCBORPrism
    ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( CBOR (..)
    , TxCollateral (..)
    , TxCollateralOut (..)
    , TxCollateralOutToken (..)
    , TxIn (..)
    , TxOut (..)
    , TxOutToken (..)
    , TxWithdrawal (..)
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..)
    )
import Cardano.Wallet.Primitive.Types.AssetId
    ( AssetId (AssetId)
    )
import Cardano.Wallet.Primitive.Types.AssetName
    ( TokenName
    )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity
    )
import Cardano.Wallet.Primitive.Types.Tx.Tx
    ( Tx (txCBOR)
    )
import Cardano.Wallet.Read.Eras.EraValue
    ( eraValueSerialize
    )
import Cardano.Wallet.Read.Tx.CBOR
    ( TxCBOR
    )
import Control.Arrow
    ( (&&&)
    , (***)
    )
import Data.Bifunctor
    ( bimap
    )
import Data.ByteString
    ( ByteString
    )
import Data.ByteString.Lazy.Char8
    ( fromStrict
    , toStrict
    )
import Data.Delta
    ( Delta (..)
    )
import Data.Foldable
    ( fold
    )
import Data.Generics.Internal.VL
    ( Iso'
    , Prism
    , fromIso
    , iso
    , match
    , prism
    , view
    , (^.)
    )
import Data.List
    ( sortOn
    )
import Data.Map.Strict
    ( Map
    )
import Data.Word
    ( Word16
    , Word32
    )
import Fmt
    ( Buildable (..)
    )
import GHC.Generics
    ( Generic
    )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.Tx.Tx as W
    ( Tx
    )
import qualified Cardano.Wallet.Primitive.Types.Tx.Tx as W.Tx
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W
    ( TxIn
    )
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W.TxIn
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
    ( TxOut (TxOut)
    )
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W.TxOut
import qualified Data.ByteString.Lazy as BL
import qualified Data.Generics.Internal.VL as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

{- | A low level definition of a transaction covering all transaction content
 by collecting all related-to-index database rows.
 Normalization is performed anyway after the first relation level.
 All values used here are records in the database.
 Foreign keys are used to group data correctly,
 but they are not removed from the data.
-}
data TxRelation =
    TxRelation
    { ins :: [TxIn]
    , collateralIns :: [TxCollateral]
    , outs :: [(TxOut, [TxOutToken])]
    , collateralOuts :: Maybe (TxCollateralOut, [TxCollateralOutToken])
    , withdrawals :: [TxWithdrawal]
    , cbor :: Maybe CBOR
    }
    deriving ( Generic, Eq, Show )

-- | A 'TxSet' is a map of 'TxRelation's indexed by their 'TxId'.
newtype TxSet =
    TxSet { relations :: Map TxId TxRelation }
    deriving ( Generic, Eq, Show )

instance Monoid TxSet where
    mempty = TxSet mempty

instance Semigroup TxSet where
    TxSet h1 <> TxSet h2 =
        TxSet $ h1 <> h2

instance Buildable TxSet where
    build txs = "TxSet " <> build (show $ relations txs)

-- | Verbs to change a 'TxSet'.
data DeltaTxSet
    = Append TxSet
    -- ^ Add new set of transactions.
    -- /Overwrites/ transactions whose id is already present in the 'TxSet'.
    | DeleteTxs (Set.Set TxId)
    -- ^ Try to remove the transactions with the given transaction ids.
    deriving ( Show, Eq, Generic )

instance Buildable DeltaTxSet where
    build action = build $ show action

instance Delta DeltaTxSet where
    type Base DeltaTxSet = TxSet
    -- transactions are immutable so here there should happen no rewriting
    -- but we mimic the repsert in the store
    apply (Append txs) h = txs <> h
    apply (DeleteTxs tids) (TxSet txs) =
        TxSet $ Map.withoutKeys txs tids

{-------------------------------------------------------------------------------
    Type conversions
    From wallet types -> to database tables
-------------------------------------------------------------------------------}
mkTxIn :: TxId -> (Int, (W.TxIn, Maybe W.TxOut)) -> TxIn
mkTxIn tid (ix, (txIn, txOut)) =
    TxIn
    { txInputTxId = tid
    , txInputOrder = ix
    , txInputSourceTxId = TxId (W.TxIn.inputId txIn)
    , txInputSourceIndex = W.TxIn.inputIx txIn
    , txInputSourceAmount = maybe (W.Coin 0) W.TxOut.coin txOut
    }

mkTxCollateral :: TxId
    -> (Int, (W.TxIn, Maybe W.TxOut))
    -> TxCollateral
mkTxCollateral tid (ix, (txCollateral, txOut)) =
    TxCollateral
    { txCollateralTxId = tid
    , txCollateralOrder = ix
    , txCollateralSourceTxId = TxId $ W.TxIn.inputId txCollateral
    , txCollateralSourceIndex = W.TxIn.inputIx txCollateral
    , txCollateralSourceAmount = maybe (W.Coin 0) W.TxOut.coin txOut
    }

-- The key to sort TxCollateralOutToken
tokenCollateralOrd :: TxCollateralOutToken -> (TokenPolicyId, TokenName)
tokenCollateralOrd = txCollateralOutTokenPolicyId &&& txCollateralOutTokenName

-- The key to sort TxOutToken
tokenOutOrd :: TxOutToken -> (TokenPolicyId, TokenName)
tokenOutOrd = txOutTokenPolicyId &&& txOutTokenName

mkTxOut
    :: TxId
    -> (Word32, W.TxOut) -- ^ (index, txout)
    -> (TxOut, [TxOutToken])
mkTxOut tid (ix,txOut) = (out, sortOn tokenOutOrd tokens)
  where
    out =
        TxOut
        { txOutputTxId = tid
        , txOutputIndex = ix
        , txOutputAddress = view #address txOut
        , txOutputAmount = W.TxOut.coin txOut
        }
    tokens =
        mkTxOutToken tid ix
        <$> snd (TokenBundle.toFlatList $ view #tokens txOut)

mkTxOutToken
    :: TxId
    -> Word32 -- ^ index
    -> (AssetId, TokenQuantity)
    -> TxOutToken
mkTxOutToken tid ix (AssetId policy token,quantity) =
    TxOutToken
    { txOutTokenTxId = tid
    , txOutTokenTxIndex = ix
    , txOutTokenPolicyId = policy
    , txOutTokenName = token
    , txOutTokenQuantity = quantity
    }

mkTxCollateralOut
    :: TxId
    -> W.TxOut
    -> (TxCollateralOut, [TxCollateralOutToken])
mkTxCollateralOut tid txCollateralOut = (out, sortOn tokenCollateralOrd tokens)
  where
    out =
        TxCollateralOut
        { txCollateralOutTxId = tid
        , txCollateralOutAddress = view #address txCollateralOut
        , txCollateralOutAmount = W.TxOut.coin txCollateralOut
        }
    tokens =
        mkTxCollateralOutToken tid
        <$> snd (TokenBundle.toFlatList $ view #tokens txCollateralOut)

mkTxCollateralOutToken
    :: TxId -> (AssetId, TokenQuantity) -> TxCollateralOutToken
mkTxCollateralOutToken tid (AssetId policy token,quantity) =
    TxCollateralOutToken
    { txCollateralOutTokenTxId = tid
    , txCollateralOutTokenPolicyId = policy
    , txCollateralOutTokenName = token
    , txCollateralOutTokenQuantity = quantity
    }

mkTxWithdrawal :: TxId -> (RewardAccount, W.Coin) -> TxWithdrawal
mkTxWithdrawal tid (txWithdrawalAccount,txWithdrawalAmount) =
    TxWithdrawal { txWithdrawalTxId, txWithdrawalAccount, txWithdrawalAmount }
  where
    txWithdrawalTxId = tid

mkTxRelation :: W.Tx -> TxRelation
mkTxRelation tx =
    TxRelation
    { ins = fmap (mkTxIn tid) $ indexed . W.Tx.resolvedInputs $ tx
    , collateralIns =
          fmap (mkTxCollateral tid) $ indexed $ W.Tx.resolvedCollateralInputs tx
    , outs = fmap (mkTxOut tid) $ indexed $ W.Tx.outputs tx
    , collateralOuts = mkTxCollateralOut tid <$> W.Tx.collateralOutput tx
    , withdrawals =
          fmap (mkTxWithdrawal tid) $ Map.toList $ W.Tx.withdrawals tx
    , cbor = fst . L.build txCBORPrism . (tid,) <$> txCBOR tx
    }
  where
    tid = TxId $ tx ^. #txId
    indexed :: (Enum a, Num a) => [b] -> [(a, b)]
    indexed = zip [0 .. ]

-- | Convert high level transactions definition in low level 'TxSet'.
mkTxSet :: [W.Tx] -> TxSet
mkTxSet txs = TxSet $ fold $ do
    tx <- txs
    let relation = mkTxRelation tx
    pure $ Map.singleton (TxId $ tx ^. #txId) relation

{-------------------------------------------------------------------------------
    Type conversions
    From database tables -> to wallet types
-------------------------------------------------------------------------------}
fromTxOut :: (TxOut, [TxOutToken]) -> W.TxOut
fromTxOut (out,tokens) =
    W.TxOut
    { W.TxOut.address = txOutputAddress out
    , W.TxOut.tokens = TokenBundle.fromFlatList
            (txOutputAmount out)
            (fromTxOutToken <$> tokens)
    }
  where
    fromTxOutToken token =
        ( AssetId (txOutTokenPolicyId token) (txOutTokenName token)
        , txOutTokenQuantity token
        )

fromTxCollateralOut :: (TxCollateralOut, [TxCollateralOutToken]) -> W.TxOut
fromTxCollateralOut (out,tokens) =
    W.TxOut
    { W.TxOut.address = txCollateralOutAddress out
    , W.TxOut.tokens = TokenBundle.fromFlatList
            (txCollateralOutAmount out)
            (fromTxCollateralOutToken <$> tokens)
    }
  where
    fromTxCollateralOutToken token =
        ( AssetId
            (txCollateralOutTokenPolicyId token)
            (txCollateralOutTokenName token)
        , txCollateralOutTokenQuantity token
        )

type TxCBORRaw = (BL.ByteString, Int)

i :: Iso' (BL.ByteString, Int) (ByteString, Word16)
i = iso (toStrict *** fromIntegral) (fromStrict *** fromIntegral)

toTxCBOR :: (TxId, TxCBOR) -> (CBOR, TxCBORRaw)
toTxCBOR (id', tx) =
    let r = L.build eraValueSerialize tx
    in (uncurry (CBOR id') $ r ^. i, r)

fromTxCBOR :: CBOR -> Either (CBOR, TxCBORRaw ) (TxId, TxCBOR)
fromTxCBOR s@CBOR {..} = bimap (s ,) (cborTxId ,) $
    match eraValueSerialize $ (cborTxCBOR, cborTxEra) ^. fromIso i

txCBORPrism :: Prism CBOR (CBOR, TxCBORRaw) (TxId, TxCBOR) (TxId, TxCBOR)
txCBORPrism = prism toTxCBOR fromTxCBOR

type WalletTransactions = Map W.WalletId TxSet
