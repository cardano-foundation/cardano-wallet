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
    ( TxId (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (AssetId) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName, TokenPolicyId )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity )
import Cardano.Wallet.Primitive.Types.Tx.Tx
    ( Tx (txCBOR) )
import Cardano.Wallet.Read.Eras.EraValue
    ( eraValueSerialize )
import Cardano.Wallet.Read.Tx.CBOR
    ( TxCBOR )
import Control.Applicative
    ( (<|>) )
import Control.Arrow
    ( (&&&), (***) )
import Control.Monad
    ( guard )
import Data.Bifunctor
    ( bimap )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Lazy.Char8
    ( fromStrict, toStrict )
import Data.Delta
    ( Delta (..) )
import Data.Foldable
    ( fold )
import Data.Generics.Internal.VL
    ( Iso', Prism, fromIso, iso, match, prism, view, (^.) )
import Data.List
    ( find, sortOn )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( catMaybes )
import Data.Word
    ( Word16, Word32 )
import Fmt
    ( Buildable (..) )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.Tx.Tx as W
import qualified Data.ByteString.Lazy as BL
import qualified Data.Generics.Internal.VL as L
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
    | DeleteTx TxId
    -- ^ Try to remove the transaction at the given transaction id.
    deriving ( Show, Eq, Generic )

instance Buildable DeltaTxSet where
    build action = build $ show action

instance Delta DeltaTxSet where
    type Base DeltaTxSet = TxSet
    -- transactions are immutable so here there should happen no rewriting
    -- but we mimic the repsert in the store
    apply (Append txs) h = txs <> h
    apply (DeleteTx tid) (TxSet txs) =
        TxSet $ Map.delete tid txs

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
-- by searching the 'TxSet' for corresponding output values.
decorateTxIns
    :: TxSet -> TxRelation -> DecoratedTxIns
decorateTxIns (TxSet relations) TxRelation{ins,collateralIns} =
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
