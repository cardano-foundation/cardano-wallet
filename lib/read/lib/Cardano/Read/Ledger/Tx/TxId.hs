{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
module Cardano.Read.Ledger.Tx.TxId
    ( TxIdType
    , TxId (..)
    , getEraTxId
    )
    where

import Prelude

import Cardano.Chain.UTxO
    ( taTx
    )
import Cardano.Crypto.Hashing
    ( serializeCborHash
    )
import Cardano.Ledger.Core
    ( bodyTxL
    , txIdTxBody
    )
import Cardano.Ledger.Crypto
    ( StandardCrypto
    )
import Cardano.Wallet.Read
    ( Tx
    )
import Cardano.Wallet.Read.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Era (..)
    , IsEra (..)
    , Mary
    , Shelley
    )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx
    )
import Control.Lens
    ( (^.)
    )

import qualified Cardano.Chain.UTxO as BY
import qualified Cardano.Ledger.Core as SH.Core
import qualified Cardano.Ledger.TxIn as SH.TxIn

type family TxIdType era where
    TxIdType Byron = BY.TxId
    TxIdType Shelley = SH.TxIn.TxId StandardCrypto
    TxIdType Allegra = SH.TxIn.TxId StandardCrypto
    TxIdType Mary = SH.TxIn.TxId StandardCrypto
    TxIdType Alonzo = SH.TxIn.TxId StandardCrypto
    TxIdType Babbage = SH.TxIn.TxId StandardCrypto
    TxIdType Conway = SH.TxIn.TxId StandardCrypto

newtype TxId era = TxId {unTxId :: TxIdType era}

{-# INLINEABLE getEraTxId #-}
getEraTxId :: forall era. IsEra era => Tx era -> TxId era
getEraTxId = case theEra :: Era era of
    Byron -> TxId . onTx byronTxId
    Shelley -> TxId . onTx shelleyTxId
    Allegra -> TxId . onTx shelleyTxId
    Mary -> TxId . onTx shelleyTxId
    Alonzo -> TxId . onTx shelleyTxId
    Babbage -> TxId . onTx shelleyTxId
    Conway -> TxId . onTx shelleyTxId

byronTxId :: BY.ATxAux a -> BY.TxId
byronTxId = serializeCborHash . taTx

shelleyTxId
    :: SH.Core.EraTx era
    => SH.Core.Tx era
    -> SH.TxIn.TxId (SH.Core.EraCrypto era)
shelleyTxId tx = txIdTxBody (tx ^. bodyTxL)
