{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright: © 2022 IOHK
License: Apache-2.0

Data type 'TxHistory' for storing a set of transactions.
Transactions are encoded "as" expressed in DB tables.

-}
module Cardano.Wallet.DB.Store.Transactions.Model
    ( DeltaTxHistory (..)
    , TxHistory (..)
    , TxRelation (..)
    , tokenCollateralOrd
    , tokenOutOrd
    , mkTxHistory

    -- * Decoration
    , DecoratedTxIns
    , lookupTxOutForTxIn
    , lookupTxOutForTxCollateral
    , decorateTxIns

    -- * Type conversion from wallet types
    , mkTxIn
    , mkTxCollateral
    , mkTxOut

    -- * Type conversions to wallet types
    , fromTxOut
    , fromTxCollateralOut
    ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( TxCollateral (..)
    , TxCollateralOut (..)
    , TxCollateralOutToken (..)
    , TxIn (..)
    , TxOut (..)
    , TxOutToken (..)
    , TxWithdrawal (..)
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (TxId) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (AssetId) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName, TokenPolicyId )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity )
import Control.Applicative
    ( (<|>) )
import Control.Arrow
    ( (&&&) )
import Control.Monad
    ( guard )
import Data.Delta
    ( Delta (..) )
import Data.Foldable
    ( fold )
import Data.Generics.Internal.VL
    ( view, (^.) )
import Data.List
    ( find, sortOn )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( catMaybes )
import Data.Word
    ( Word32 )
import Fmt
    ( Buildable (build) )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Data.Map.Strict as Map

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
    }
    deriving ( Generic, Eq, Show )

-- | Transactions history is 'TxRelation's indexed by 'TxId'
newtype TxHistory =
    TxHistory { relations :: Map TxId TxRelation }
    deriving ( Generic, Eq, Show )

instance Monoid TxHistory where
    mempty = TxHistory mempty

instance Semigroup TxHistory where
    TxHistory h1 <> TxHistory h2 =
        TxHistory $ h1 <> h2

instance Buildable TxHistory where
    build txs = "TxHistory " <> build (show $ relations txs)

-- | Verbs to change a 'TxHistory'.
data DeltaTxHistory
    = Append TxHistory
    -- ^ Add new set of transactions.
    -- Overwrites transactions whose id is already present in the 'TxHistory'.
    | DeleteTx TxId
    -- ^ Try to remove the transaction at the given transaction id.
    deriving ( Show, Eq, Generic )

instance Buildable DeltaTxHistory where
    build action = build $ show action

instance Delta DeltaTxHistory where
    type Base DeltaTxHistory = TxHistory
    -- transactions are immutable so here there should happen no rewriting
    -- but we mimic the repsert in the store
    apply (Append txs) h = txs <> h
    apply (DeleteTx tid) (TxHistory txs) =
        TxHistory $ Map.delete tid txs

{-------------------------------------------------------------------------------
    Type conversions
    From wallet types -> to database tables
-------------------------------------------------------------------------------}
mkTxIn :: TxId -> (Int, (W.TxIn, W.Coin)) -> TxIn
mkTxIn tid (ix,(txIn,amt)) =
    TxIn
    { txInputTxId = tid
    , txInputOrder = ix
    , txInputSourceTxId = TxId (W.inputId txIn)
    , txInputSourceIndex = W.inputIx txIn
    , txInputSourceAmount = amt
    }

mkTxCollateral :: TxId
    -> (Int, (W.TxIn, W.Coin))
    -> TxCollateral
mkTxCollateral tid (ix,(txCollateral,amt)) =
    TxCollateral
    { txCollateralTxId = tid
    , txCollateralOrder = ix
    , txCollateralSourceTxId = TxId $ W.inputId txCollateral
    , txCollateralSourceIndex = W.inputIx txCollateral
    , txCollateralSourceAmount = amt
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
        , txOutputAmount = W.txOutCoin txOut
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
        , txCollateralOutAmount = W.txOutCoin txCollateralOut
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
    { ins = fmap (mkTxIn tid) $ indexed . W.resolvedInputs $ tx
    , collateralIns =
          fmap (mkTxCollateral tid) $ indexed $ W.resolvedCollateralInputs tx
    , outs = fmap (mkTxOut tid) $ indexed $ W.outputs tx
    , collateralOuts = mkTxCollateralOut tid <$> W.collateralOutput tx
    , withdrawals =
          fmap (mkTxWithdrawal tid) $ Map.toList $ W.withdrawals tx
    }
  where
    tid = TxId $ tx ^. #txId
    indexed :: (Enum a, Num a) => [b] -> [(a, b)]
    indexed = zip [0 .. ]

-- | Convert high level transactions definition in low level DB history
mkTxHistory :: [W.Tx] -> TxHistory
mkTxHistory txs = TxHistory $ fold $ do
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
    { W.address = txOutputAddress out
    , W.tokens = TokenBundle.fromFlatList
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
    { W.address = txCollateralOutAddress out
    , W.tokens = TokenBundle.fromFlatList
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

{-------------------------------------------------------------------------------
    Decorating Tx inputs with outputs
-------------------------------------------------------------------------------}
type TxOutKey = (TxId, Word32)

toKeyTxIn :: TxIn -> TxOutKey
toKeyTxIn txin = (txInputSourceTxId txin, txInputSourceIndex txin)

toKeyTxCollateral :: TxCollateral -> TxOutKey
toKeyTxCollateral txcol =
    (txCollateralSourceTxId txcol, txCollateralSourceIndex txcol)

-- | A collection of Tx inputs
-- (regular or collateral, refered to by input and order)
-- that are decorated with the values of their corresponding Tx outputs.
newtype DecoratedTxIns = DecoratedTxIns
    { unDecoratedTxIns
        :: Map TxOutKey W.TxOut
    }

instance Semigroup DecoratedTxIns where
    (DecoratedTxIns a) <> (DecoratedTxIns b) = DecoratedTxIns (a <> b)

instance Monoid DecoratedTxIns where
    mempty = DecoratedTxIns mempty

lookupTxOutForTxIn
    :: TxIn -> DecoratedTxIns -> Maybe W.TxOut
lookupTxOutForTxIn tx = Map.lookup (toKeyTxIn tx) . unDecoratedTxIns

lookupTxOutForTxCollateral
    :: TxCollateral -> DecoratedTxIns -> Maybe W.TxOut
lookupTxOutForTxCollateral tx =
    Map.lookup (toKeyTxCollateral tx) . unDecoratedTxIns

-- | Decorate the Tx inputs of a given 'TxRelation'
-- by searching the 'TxHistory' for corresponding output values.
decorateTxIns
    :: TxHistory -> TxRelation -> DecoratedTxIns
decorateTxIns (TxHistory relations) TxRelation{ins,collateralIns} =
    DecoratedTxIns . Map.fromList . catMaybes $
        (lookupOutput . toKeyTxIn <$> ins)
        ++ (lookupOutput . toKeyTxCollateral <$> collateralIns)
  where
    lookupOutput key@(txid, index) = do
        tx <- Map.lookup txid relations
        out <- lookupTxOut tx index <|> lookupTxCollateralOut tx index
        pure (key, out)

    lookupTxOut tx index = fromTxOut <$>
        Data.List.find ((index ==) . txOutputIndex . fst) (outs tx)

    lookupTxCollateralOut tx index = do
        out <- collateralOuts tx
        let collateralOutputIndex = toEnum $ length (outs tx)
        guard $ index == collateralOutputIndex  -- Babbage leder spec
        pure $ fromTxCollateralOut out
