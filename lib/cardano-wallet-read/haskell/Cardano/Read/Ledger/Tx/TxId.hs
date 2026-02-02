{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Era-indexed transaction identifier extraction.
-}
module Cardano.Read.Ledger.Tx.TxId
    ( -- * Transaction ID type
      TxIdType
    , TxId (..)

      -- * Extraction
    , getEraTxId
    )
where

import Prelude

import Cardano.Chain.UTxO
    ( taTx
    )
import Cardano.Chain.UTxO qualified as BY
import Cardano.Crypto.Hashing
    ( serializeCborHash
    )
import Cardano.Ledger.Core
    ( bodyTxL
    , txIdTxBody
    )
import Cardano.Ledger.Core qualified as SH.Core
import Cardano.Ledger.TxIn qualified as SH.TxIn
import Cardano.Read.Ledger.Eras
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
import Cardano.Read.Ledger.Tx.Eras
    ( onTx
    )
import Cardano.Read.Ledger.Tx.Tx
    ( Tx
    )
import Control.Lens
    ( (^.)
    )

-- | Era-specific transaction ID type.
type family TxIdType era where
    TxIdType Byron = BY.TxId
    TxIdType Shelley = SH.TxIn.TxId
    TxIdType Allegra = SH.TxIn.TxId
    TxIdType Mary = SH.TxIn.TxId
    TxIdType Alonzo = SH.TxIn.TxId
    TxIdType Babbage = SH.TxIn.TxId
    TxIdType Conway = SH.TxIn.TxId

-- | Era-indexed transaction ID wrapper.
newtype TxId era = TxId {unTxId :: TxIdType era}

{-# INLINEABLE getEraTxId #-}

-- | Extract the transaction ID from a transaction in any era.
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
    -> SH.TxIn.TxId
shelleyTxId tx = txIdTxBody (tx ^. bodyTxL)
