{-# LANGUAGE NamedFieldPuns #-}

-- | Indirection module that re-exports types
-- used for writing transactions to the blockchain,
-- in the most recent and the next future eras.
--
-- TODO: Match this up with the @Write@ hierarchy.
module Cardano.Wallet.Deposit.Write
    ( Address

    , Value

    , TxId
    , Tx (..)
    , TxBody (..)
    , TxIn
    , TxOut
    , TxWitness

    -- * Helper functions
    , mkAda
    , mkTxOut
    , mockTxId
    , toConwayTx
    , toReadTx
    ) where

import Prelude

import Cardano.Wallet.Deposit.Read
    ( Address
    , Ix
    , TxId
    , TxIn
    , TxOut
    , TxWitness
    , Value
    )
import Cardano.Wallet.Primitive.Ledger.Convert
    ( toConwayTxOut
    , toLedger
    )
import Data.Map
    ( Map
    )
import Data.Maybe.Strict
    ( StrictMaybe
    , maybeToStrictMaybe
    )
import Data.Sequence.Strict
    ( StrictSeq
    , fromList
    )
import Data.Set
    ( Set
    )
import Lens.Micro
    ( (&)
    , (.~)
    )

import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Api.Tx.In as L
import qualified Cardano.Wallet.Deposit.Read
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
import qualified Cardano.Wallet.Read as Read
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

{-----------------------------------------------------------------------------
    Type definitions
    with dummies
------------------------------------------------------------------------------}
data Tx = Tx
    { txbody :: TxBody
    , txwits :: TxWitness
    }
    deriving (Eq, Ord, Show)

data TxBody = TxBody
    { spendInputs :: Set TxIn
    , collInputs :: Set TxIn
    , txouts :: Map Ix TxOut
    , collRet :: Maybe TxOut
    }
    deriving (Eq, Ord, Show)

mkAda :: Integer -> Value
mkAda = W.fromCoin . W.unsafeFromIntegral

mkTxOut :: Address -> Value -> TxOut
mkTxOut = W.TxOut

toConwayTx :: TxId -> Tx -> Read.Tx Read.Conway
toConwayTx _ Tx{txbody} = Read.Tx $ L.mkBasicTx txBody
  where
    txBody :: L.TxBody L.Conway
    txBody =
        L.mkBasicTxBody
            & (L.inputsTxBodyL .~ Set.map toLedgerTxIn (spendInputs txbody))
            & (L.collateralInputsTxBodyL
                .~ Set.map toLedgerTxIn (collInputs txbody)
                )
            & (L.outputsTxBodyL .~ toLedgerTxOuts (txouts txbody))
            & (L.collateralReturnTxBodyL
                .~ toLedgerMaybeTxOut (collRet txbody)
                )

toLedgerTxIn :: TxIn -> L.TxIn L.StandardCrypto
toLedgerTxIn = toLedger

toLedgerTxOuts :: Map Ix TxOut -> StrictSeq (L.TxOut L.Conway)
toLedgerTxOuts = fromList . map (toConwayTxOut . snd) . Map.toAscList

toLedgerMaybeTxOut :: Maybe TxOut -> StrictMaybe (L.TxOut L.Conway)
toLedgerMaybeTxOut = fmap toConwayTxOut . maybeToStrictMaybe

toReadTx :: TxId -> Tx -> Cardano.Wallet.Deposit.Read.Tx
toReadTx txid Tx{txbody=TxBody{..}} =
    W.Tx
        { W.txId =
            W.Hash txid
        , W.txCBOR =
            Nothing
        , W.fee =
            Nothing
        , W.resolvedInputs =
            map (,Nothing) $ Set.toList spendInputs
        , W.resolvedCollateralInputs =
            map (,Nothing) $ Set.toList collInputs
        , W.outputs =
            map snd $ Map.toAscList txouts
        , W.collateralOutput =
            collRet
        , W.withdrawals =
            mempty
        , W.metadata =
            Nothing
        , W.scriptValidity =
            Nothing
        }

mockTxId :: Show a => a -> TxId
mockTxId = B8.pack . show
