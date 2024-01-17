{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

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

import Prelude

import Cardano.Crypto.Hash
    ( Hash (..)
    )
import Cardano.Ledger.Address
    ( Addr (..)
    )
import Cardano.Ledger.Api.Era
    ( Era
    , ShelleyEra
    , StandardCrypto
    )
import Cardano.Ledger.Api.Tx.In
    ( mkTxInPartial
    )
import Cardano.Ledger.AuxiliaryData
    ( AuxiliaryDataHash
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
import Cardano.Ledger.Keys
    ( KeyHash (..)
    )
import Cardano.Ledger.SafeHash
    ( unsafeMakeSafeHash
    )
import Cardano.Ledger.Shelley.PParams
    ( Update
    )
import Cardano.Ledger.Shelley.Tx
    ( ShelleyTx (ShelleyTx)
    , ShelleyTxBody (ShelleyTxBody)
    , ShelleyTxOut (ShelleyTxOut)
    , TxIn (..)
    )
import Cardano.Ledger.Shelley.TxAuxData
    ( ShelleyTxAuxData
    )
import Cardano.Ledger.Shelley.TxCert
    ( ShelleyTxCert
    )
import Cardano.Ledger.Shelley.TxBody
    ( Withdrawals (Withdrawals)
    )
import Cardano.Ledger.Shelley.TxWits
    ( ShelleyTxWits
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
    , TxId (..)
    , TxParameters (..)
    , exampleTxParameters
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

import qualified Cardano.Ledger.Core as L
import qualified Cardano.Ledger.TxIn as L
import qualified Data.ByteString.Short as B
import qualified Data.Set as Set

mkShelleyTx
    :: TxParameters
    -> ShelleyTx (ShelleyEra StandardCrypto)
mkShelleyTx TxParameters{txInputs, txOutputs} =
    ShelleyTx (body txInputs txOutputs) wits aux

aux :: StrictMaybe (ShelleyTxAuxData (ShelleyEra StandardCrypto))
aux = maybeToStrictMaybe Nothing

wits :: L.EraScript (era StandardCrypto) => ShelleyTxWits (era StandardCrypto)
wits = mempty

body
    :: HasCallStack
    => NonEmpty (Index, TxId)
    -> NonEmpty (Address, Lovelace)
    -> ShelleyTxBody (ShelleyEra StandardCrypto)
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

auxb :: StrictMaybe (AuxiliaryDataHash StandardCrypto)
auxb = maybeToStrictMaybe Nothing

upd :: StrictMaybe (Update era)
upd = maybeToStrictMaybe Nothing

slot :: SlotNo
slot = SlotNo 0

txfee :: Coin
txfee = Coin 0

wdrls :: Withdrawals StandardCrypto
wdrls = Withdrawals mempty

certs :: StrictSeq (ShelleyTxCert (era StandardCrypto))
certs = mempty

txouts
    :: ( Era era
       , L.Value era ~ Coin
       , L.EraCrypto era ~ StandardCrypto
       )
    => NonEmpty (Address, Lovelace)
    -> StrictSeq (ShelleyTxOut era)
txouts xs = fromList $ do
    (addr, Lovelace val) <- toList xs
    pure $ ShelleyTxOut (decodeShelleyAddress addr) $ Coin val

mkShelleyPaymentPart :: ByteString -> Addr StandardCrypto
mkShelleyPaymentPart x = Addr Mainnet (payment x) staking

staking :: StakeReference StandardCrypto
staking = StakeRefNull

payment
    :: ByteString
    -> PaymentCredential StandardCrypto
payment x = KeyHashObj $ KeyHash $ UnsafeHash $ B.toShort x

txins :: HasCallStack => NonEmpty (Index, TxId) -> Set (TxIn StandardCrypto)
txins = foldMap $ uncurry mkShelleyInput

mkShelleyInput
    :: HasCallStack
    => Index
    -> TxId
    -> Set (TxIn StandardCrypto)
mkShelleyInput (Index idx) (TxId h) =
    Set.singleton
        $ mkTxInPartial (L.TxId $ unsafeMakeSafeHash $ UnsafeHash $ B.toShort h)
        $ fromIntegral idx

exampleShelleyTx :: ShelleyTx (ShelleyEra StandardCrypto)
exampleShelleyTx = mkShelleyTx exampleTxParameters
