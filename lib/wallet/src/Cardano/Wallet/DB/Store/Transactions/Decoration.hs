{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Definition of the decoration of transactions to map tx inputs to the
-- corrisponding (known) tx outputs
module Cardano.Wallet.DB.Store.Transactions.Decoration
    ( DecoratedTxIns

      -- * Observation
    , mkTxOutKey
    , mkTxOutKeyCollateral
    , TxOutKey
    , lookupTxOut

      -- * Construction
    , LookupFun
    , TxInDecorator
    , decorateTxInsForRelation
    , decorateTxInsForRelationFromLookupTxOut
    , decorateTxInsForReadTxFromLookupTxOut
    ) where

import Prelude hiding
    ( (.)
    )

import Cardano.Wallet.DB.Sqlite.Schema
    ( TxCollateral (..)
    , TxIn (..)
    , TxOut (..)
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..)
    )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( TxRelation (TxRelation, collateralIns, collateralOuts, ins, outs)
    , fromTxCollateralOut
    , fromTxOut
    )
import Cardano.Wallet.Read.Eras
    ( EraValue
    , extractEraValue
    )
import Cardano.Wallet.Read.Eras.EraFun
    ( applyEraFun
    )
import Cardano.Wallet.Read.Primitive.Tx.Features.CollateralInputs
    ( getCollateralInputs
    )
import Cardano.Wallet.Read.Primitive.Tx.Features.Inputs
    ( getInputs
    )
import Cardano.Wallet.Read.Tx.CollateralInputs
    ( getEraCollateralInputs
    )
import Cardano.Wallet.Read.Tx.Inputs
    ( getEraInputs
    )
import Control.Applicative
    ( (<|>)
    )
import Control.Category
    ( (.)
    )
import Control.Monad
    ( guard
    )
import Data.List
    ( find
    )
import Data.Map.Strict
    ( Map
    )
import Data.Maybe
    ( catMaybes
    )
import Data.Word
    ( Word32
    )

import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
import qualified Cardano.Wallet.Read.Tx as Read
import qualified Data.Map.Strict as Map

{-----------------------------------------------------------------------------
    Data type
------------------------------------------------------------------------------}
type TxOutKey = (TxId, Word32)

-- | A collection of Tx inputs
-- (regular or collateral, refered to by input and order)
-- that are decorated with the values of their corresponding Tx outputs.
newtype DecoratedTxIns = DecoratedTxIns
    {unDecoratedTxIns :: Map TxOutKey W.TxOut}

{-----------------------------------------------------------------------------
    Observation
------------------------------------------------------------------------------}
mkTxOutKey :: TxIn -> TxOutKey
mkTxOutKey txin = (txInputSourceTxId txin, txInputSourceIndex txin)

mkTxOutKeyCollateral :: TxCollateral -> TxOutKey
mkTxOutKeyCollateral txcol =
    (txCollateralSourceTxId txcol, txCollateralSourceIndex txcol)

instance Semigroup DecoratedTxIns where
    (DecoratedTxIns a) <> (DecoratedTxIns b) = DecoratedTxIns (a <> b)

instance Monoid DecoratedTxIns where
    mempty = DecoratedTxIns mempty

lookupTxOut
    :: TxOutKey -> DecoratedTxIns -> Maybe W.TxOut
lookupTxOut tx = Map.lookup tx . unDecoratedTxIns

{-----------------------------------------------------------------------------
    Construction
------------------------------------------------------------------------------}

-- | A monadic function to look up a value @v@ from a key @k@.
type LookupFun m k v = k -> m (Maybe v)

-- | A monadic function that can decorate the inputs of a given
-- transaction @tx@ by fetching transaction data in the monad @m@.
type TxInDecorator tx m = tx -> m DecoratedTxIns

decorateTxInsInternalLookupTxRelation
    :: forall m
     . Monad m
    => LookupFun m TxId TxRelation
    -> [(TxId, Word32)]
    -> [(TxId, Word32)]
    -> m DecoratedTxIns
decorateTxInsInternalLookupTxRelation =
    decorateTxInsInternal . lookupOutputFromLookupRelation

lookupOutputFromLookupRelation
    :: forall m
     . Monad m
    => LookupFun m TxId TxRelation
    -> LookupFun m (TxId, Word32) W.TxOut
lookupOutputFromLookupRelation lookupTx = lookupOutput
  where
    lookupOutput :: (TxId, Word32) -> m (Maybe W.TxOut)
    lookupOutput (txid, index) = do
        mtx <- lookupTx txid
        pure $ do
            tx <- mtx
            lookupTxOut' tx index <|> lookupTxCollateralOut tx index

    lookupTxOut' :: TxRelation -> Word32 -> Maybe W.TxOut
    lookupTxOut' tx index =
        fromTxOut
            <$> find ((index ==) . txOutputIndex . fst) (outs tx)

    lookupTxCollateralOut :: (Enum a, Eq a) => TxRelation -> a -> Maybe W.TxOut
    lookupTxCollateralOut tx index = do
        out <- collateralOuts tx
        let collateralOutputIndex = toEnum $ length (outs tx)
        guard $ index == collateralOutputIndex -- Babbage leder spec
        pure $ fromTxCollateralOut out

decorateTxInsInternal
    :: forall m
     . Monad m
    => LookupFun m (TxId, Word32) W.TxOut
    -> [(TxId, Word32)]
    -> [(TxId, Word32)]
    -> m DecoratedTxIns
decorateTxInsInternal lookupOut ins collateralIns = do
    mouts <- mapM lookupOutput (ins <> collateralIns)
    pure . DecoratedTxIns . Map.fromList $ catMaybes mouts
  where
    lookupOutput :: (TxId, Word32) -> m (Maybe ((TxId, Word32), W.TxOut))
    lookupOutput key = do
        mout <- lookupOut key
        case mout of
            Nothing -> pure Nothing
            Just out -> pure $ Just (key, out)

-- | Decorate the Tx inputs of a given 'TxRelation'
-- by searching the 'TxSet' for corresponding output values.
decorateTxInsForRelationFromLookupTxOut
    :: Monad m
    => LookupFun m (TxId, Word32) W.TxOut
    -> TxRelation
    -> m DecoratedTxIns
decorateTxInsForRelationFromLookupTxOut
    lookupOut
    TxRelation{ins, collateralIns} =
        decorateTxInsInternal
            lookupOut
            (mkTxOutKey <$> ins)
            (mkTxOutKeyCollateral <$> collateralIns)

-- | Decorate the Tx inputs of a given 'TxRelation'
-- by searching the 'TxSet' for corresponding output values.
decorateTxInsForRelation
    :: Monad m
    => LookupFun m TxId TxRelation
    -> TxRelation
    -> m DecoratedTxIns
decorateTxInsForRelation lookupTx TxRelation{ins, collateralIns} =
    decorateTxInsInternalLookupTxRelation
        lookupTx
        (mkTxOutKey <$> ins)
        (mkTxOutKeyCollateral <$> collateralIns)

-- | Decorate the Tx inputs of a given 'TxRelation'
-- by searching the 'TxSet' for corresponding output values.
decorateTxInsForReadTxFromLookupTxOut
    :: Monad m
    => LookupFun m (TxId, Word32) W.TxOut
    -> EraValue Read.Tx
    -> m DecoratedTxIns
decorateTxInsForReadTxFromLookupTxOut lookupTxOut' tx =
    decorateTxInsInternal
        lookupTxOut'
        ( fmap undoWTxIn
            $ extractEraValue
            $ applyEraFun (getInputs . getEraInputs) tx
        )
        ( fmap undoWTxIn
            $ extractEraValue
            $ applyEraFun
                (getCollateralInputs . getEraCollateralInputs)
                tx
        )
  where
    undoWTxIn :: W.TxIn -> (TxId, Word32)
    undoWTxIn (W.TxIn k n) = (TxId k, n)
