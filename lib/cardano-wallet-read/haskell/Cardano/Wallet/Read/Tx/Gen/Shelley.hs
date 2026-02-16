{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Read.Tx.Gen.Shelley
    ( mkShelleyTx
    , exampleShelleyTx
    , txouts
    , wdrls
    , certs
    , txfee
    , txins
    , upd
    , auxb
    , wits
    , mkShelleyPaymentPart
    )
where

import Cardano.Crypto.Hash
    ( Hash (..)
    )
import Cardano.Ledger.Address
    ( Addr (..)
    )
import Cardano.Ledger.Api
    ( Era
    , ShelleyEra
    )
import Cardano.Ledger.Api.Tx.In
    ( mkTxInPartial
    )
import Cardano.Ledger.BaseTypes
    ( Network (..)
    )
import Cardano.Ledger.Coin
    ( Coin (..)
    )
import Cardano.Ledger.Credential
    ( Credential (KeyHashObj)
    , PaymentCredential
    , StakeReference (..)
    )
import Cardano.Ledger.Hashes
    ( TxAuxDataHash
    )
import Cardano.Ledger.Keys
    ( KeyHash (..)
    )
import Cardano.Ledger.Shelley.API.Types
    ( ShelleyTxAuxData
    , ShelleyTxOut (ShelleyTxOut)
    , ShelleyTxWits
    , TxBody (ShelleyTxBody)
    , TxIn (..)
    , Withdrawals (Withdrawals)
    )
import Cardano.Ledger.Shelley.PParams
    ( Update
    )
import Cardano.Ledger.Shelley.Tx
    ( ShelleyTx (ShelleyTx)
    , Tx (MkShelleyTx)
    )
import Cardano.Ledger.Shelley.TxCert
    ( ShelleyTxCert
    )
import Cardano.Ledger.Slot
    ( SlotNo (..)
    )
import Cardano.Wallet.Read.Tx.Gen.Address
    ( decodeShelleyAddress
    )
import Cardano.Wallet.Read.Tx.Gen.TxParameters
    ( Address (..)
    , Index (..)
    , Lovelace (..)
    , TxParameters (..)
    , exampleTxParameters
    )
import Cardano.Wallet.Read.Tx.TxId
    ( TxId
    )
import Data.ByteString
    ( ByteString
    )
import Data.Foldable
    ( toList
    )
import Data.List.NonEmpty
    ( NonEmpty
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
import GHC.Stack
    ( HasCallStack
    )
import Prelude

import Cardano.Ledger.Core qualified as L
import Data.ByteString.Short qualified as B
import Data.Set qualified as Set

mkShelleyTx
    :: TxParameters
    -> L.Tx ShelleyEra
mkShelleyTx TxParameters{txInputs, txOutputs} =
    MkShelleyTx $ ShelleyTx (body txInputs txOutputs) wits aux

aux :: StrictMaybe (ShelleyTxAuxData ShelleyEra)
aux = maybeToStrictMaybe Nothing

wits :: L.EraScript era => ShelleyTxWits era
wits = mempty

body
    :: HasCallStack
    => NonEmpty (Index, TxId)
    -> NonEmpty (Address, Lovelace)
    -> L.TxBody ShelleyEra
body ins outs =
    ShelleyTxBody
        (txins ins)
        (txouts outs)
        certs
        wdrls
        txfee
        slot
        upd
        auxb

auxb :: StrictMaybe TxAuxDataHash
auxb = maybeToStrictMaybe Nothing

upd :: StrictMaybe (Update era)
upd = maybeToStrictMaybe Nothing

slot :: SlotNo
slot = SlotNo 0

txfee :: Coin
txfee = Coin 0

wdrls :: Withdrawals
wdrls = Withdrawals mempty

certs :: StrictSeq (ShelleyTxCert era)
certs = mempty

txouts
    :: ( Era era
       , L.Value era ~ Coin
       )
    => NonEmpty (Address, Lovelace)
    -> StrictSeq (ShelleyTxOut era)
txouts xs = fromList $ do
    (addr, Lovelace val) <- toList xs
    pure $ ShelleyTxOut (decodeShelleyAddress addr) $ Coin val

mkShelleyPaymentPart :: ByteString -> Addr
mkShelleyPaymentPart x = Addr Mainnet (payment x) staking

staking :: StakeReference
staking = StakeRefNull

payment
    :: ByteString
    -> PaymentCredential
payment x = KeyHashObj $ KeyHash $ UnsafeHash $ B.toShort x

txins :: HasCallStack => NonEmpty (Index, TxId) -> Set TxIn
txins = foldMap $ uncurry mkShelleyInput

mkShelleyInput
    :: HasCallStack
    => Index
    -> TxId
    -> Set TxIn
mkShelleyInput (Index idx) txid =
    Set.singleton
        $ mkTxInPartial txid
        $ fromIntegral idx

exampleShelleyTx :: L.Tx ShelleyEra
exampleShelleyTx = mkShelleyTx exampleTxParameters
