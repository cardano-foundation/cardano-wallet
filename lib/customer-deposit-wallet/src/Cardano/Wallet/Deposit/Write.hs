-- | Indirection module that re-exports types
-- used for writing transactions to the blockchain,
-- in the most recent and the next future eras.
--
-- TODO: Match this up with the @Write@ hierarchy.
module Cardano.Wallet.Deposit.Write
    ( Address

    , Value

    , TxId
    , Tx
    , mkTx
    , TxBody (..)
    , TxIn
    , TxOut

    -- * Helper functions
    , mkAda
    , mkTxOut
    , mockTxId
    , toConwayTx
    ) where

import Prelude

import Cardano.Read.Ledger.Tx.Output
    ( Output (..)
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , Ix
    , TxId
    , TxIn
    , TxOut
    , Value
    )
import Cardano.Wallet.Read.Hash
    ( hashFromBytesShort
    )
import Cardano.Wallet.Read.Tx
    ( toConwayOutput
    , txIdFromHash
    )
import Data.Map
    ( Map
    )
import Data.Maybe
    ( fromJust
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
import qualified Cardano.Wallet.Read as Read
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

{-----------------------------------------------------------------------------
    Type definitions
    with dummies
------------------------------------------------------------------------------}
type Tx = Read.Tx Read.Conway

data TxBody = TxBody
    { spendInputs :: Set TxIn
    , collInputs :: Set TxIn
    , txouts :: Map Ix TxOut
    , collRet :: Maybe TxOut
    }
    deriving (Show)

mkAda :: Integer -> Value
mkAda = Read.injectCoin . Read.CoinC

mkTxOut :: Address -> Value -> TxOut
mkTxOut = Read.mkBasicTxOut

toConwayTx :: Tx -> Read.Tx Read.Conway
toConwayTx = id

mkTx :: TxBody -> Tx
mkTx txbody = Read.Tx $ L.mkBasicTx txBody
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
toLedgerTxIn = id

toLedgerTxOuts :: Map Ix TxOut -> StrictSeq (L.TxOut L.Conway)
toLedgerTxOuts = fromList . map (toConwayTxOut . snd) . Map.toAscList

toLedgerMaybeTxOut :: Maybe TxOut -> StrictMaybe (L.TxOut L.Conway)
toLedgerMaybeTxOut = fmap toConwayTxOut . maybeToStrictMaybe

toConwayTxOut :: TxOut -> L.TxOut L.Conway
toConwayTxOut txout =
    case toConwayOutput txout of
        Output o -> o

mockTxId :: Show a => a -> TxId
mockTxId x =
    txIdFromHash
    . fromJust
    . hashFromBytesShort
    . SBS.pack
    $ take 32 (BS.unpack (B8.pack $ show x) <> repeat 0)
