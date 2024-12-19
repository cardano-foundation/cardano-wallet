{-# LANGUAGE DataKinds #-}

-- | Indirection module that re-exports types
-- used for writing transactions to the blockchain,
-- in the most recent and the next future eras.
--
-- TODO: Match this up with the @Write@ hierarchy.
module Cardano.Wallet.Deposit.Write
    ( -- * Basic types
      Address
    , Value
    , TxId
    , Tx
    , Block
    , mkTx
    , TxBody (..)
    , TxIn
    , TxOut
    , Coin

      -- * Transaction balancing
    , Write.IsRecentEra
    , Write.Conway
    , L.PParams
    , Write.UTxOAssumptions (..)
    , Write.ChangeAddressGen (..)
    , Write.StakeKeyDepositLookup (..)
    , Write.TimelockKeyWitnessCounts (..)
    , Write.UTxOIndex
    , Write.constructUTxOIndex
    , Write.UTxO
    , toConwayUTxO
    , Write.PartialTx (..)
    , Write.ErrBalanceTx (..)
    , Write.ErrBalanceTxAssetsInsufficientError (..)
    , Write.ErrBalanceTxInsufficientCollateralError (..)
    , Write.ErrBalanceTxInternalError (..)
    , Write.ErrBalanceTxOutputError (..)
    , Write.ErrBalanceTxOutputErrorInfo (..)
    , Write.ErrBalanceTxUnableToCreateChangeError (..)
    , Write.ErrAssignRedeemers (..)
    , Write.balanceTx

      -- * Signing
    , addSignature

      -- ** Time interpreter
    , Write.TimeTranslation

      -- * Helper functions
    , mkAda
    , mkTxOut
    , txOutsL
    , toConwayTx
    , addTxIn
    , addTxOut
    , emptyTxBody
    , UTxO.resolvedTx
    , UTxO.resolvedInputs
    ) where

import Prelude

import Cardano.Ledger.Coin
    ( Coin
    )
import Cardano.Read.Ledger.Tx.Output
    ( Output (..)
    )
import Cardano.Wallet.Address.BIP32_Ed25519
    ( XPrv
    , sign
    , toXPub
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , Ix
    , SlotNo (..)
    , TxId
    , TxIn
    , TxOut
    , Value
    )
import Cardano.Wallet.Deposit.Write.Keys
    ( signedDSIGNfromXSignature
    , vkeyFromXPub
    )
import Cardano.Wallet.Read.Tx
    ( toConwayOutput
    )
import Control.Lens
    ( Lens'
    , lens
    , (%~)
    , (&)
    , (.~)
    )
import Data.Map
    ( Map
    )
import Data.Maybe.Strict
    ( StrictMaybe (..)
    , maybeToStrictMaybe
    )
import Data.Sequence.Strict
    ( StrictSeq
    , fromList
    )
import Data.Set
    ( Set
    )

import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Api.Tx.In as L
import qualified Cardano.Ledger.Slot as L
import qualified Cardano.Wallet.Deposit.Pure.UTxO.Tx as UTxO
import qualified Cardano.Wallet.Read as Read
import qualified Cardano.Wallet.Read.Hash as Hash
import qualified Cardano.Write.Eras as Write
import qualified Cardano.Write.Tx as Write
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}
type Tx = Read.Tx Read.Conway

type Block = Read.Block Read.Conway

{-----------------------------------------------------------------------------
    Signing
------------------------------------------------------------------------------}
-- | Add a signature to the transaction using the private key
addSignature :: XPrv -> Tx -> Tx
addSignature xprv tx@(Read.Tx ledgerTx) =
    Read.Tx
        (ledgerTx & (L.witsTxL . L.addrTxWitsL) %~ Set.insert witnessVKey)
  where
    txHash = Read.hashFromTxId $ Read.getTxId tx
    xpub = toXPub xprv
    xsign = sign xprv (Hash.hashToBytes txHash)
    witnessVKey =
        L.WitVKey (vkeyFromXPub xpub) (signedDSIGNfromXSignature xsign)

{-----------------------------------------------------------------------------
    Convenience TxBody
------------------------------------------------------------------------------}

data TxBody = TxBody
    { spendInputs :: Set TxIn
    , collInputs :: Set TxIn
    , txouts :: Map Ix TxOut
    , collRet :: Maybe TxOut
    , expirySlot :: Maybe SlotNo
    }
    deriving (Show)

txOutsL :: Lens' TxBody (Map Ix TxOut)
txOutsL = lens txouts (\s a -> s{txouts = a})

nextIx :: TxBody -> Ix
nextIx = maybe minBound (succ . fst) . Map.lookupMax . txouts

addTxOut :: TxOut -> TxBody -> (TxBody, Ix)
addTxOut txout txbody = (txBody', txIx)
  where
    txBody' = txbody & txOutsL .~ Map.insert txIx txout (txouts txbody)
    txIx = nextIx txbody

addTxIn :: TxIn -> TxBody -> TxBody
addTxIn txin txbody = txbody{spendInputs = Set.insert txin (spendInputs txbody)}

emptyTxBody :: TxBody
emptyTxBody = TxBody mempty mempty mempty Nothing Nothing

-- | Inject a number of ADA, i.e. a million lovelace.
mkAda :: Integer -> Value
mkAda = Read.injectCoin . Read.CoinC . (* 1000000)

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
            & L.inputsTxBodyL .~ Set.map toLedgerTxIn (spendInputs txbody)
            & L.collateralInputsTxBodyL
                .~ Set.map toLedgerTxIn (collInputs txbody)
            & L.outputsTxBodyL .~ toLedgerTxOuts (txouts txbody)
            & L.collateralReturnTxBodyL
                .~ toLedgerMaybeTxOut (collRet txbody)
            & L.vldtTxBodyL
                .~ L.ValidityInterval
                    SNothing
                    (toLedgerSlotNo <$> maybeToStrictMaybe (expirySlot txbody))

toLedgerSlotNo :: SlotNo -> L.SlotNo
toLedgerSlotNo (SlotNo n) = L.SlotNo (fromInteger $ fromIntegral n)

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

toConwayUTxO :: Map TxIn TxOut -> Write.UTxO L.Conway
toConwayUTxO = Write.UTxO . Map.map toConwayTxOut
