{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Cardano.Read.Ledger.Tx.Hash
-- Copyright   : © 2024 Cardano Foundation
-- License     : Apache-2.0
--
-- Transaction hash extraction as raw bytes. This provides the hash
-- digest for a transaction across all eras.
module Cardano.Read.Ledger.Tx.Hash
    ( -- * Hash extraction
      getEraTxHash

      -- * Era-specific helpers
    , byronTxHash
    , shelleyTxHash
    , fromShelleyTxId
    )
where

import Prelude

import Cardano.Chain.UTxO
    ( ATxAux
    , taTx
    )
import Cardano.Crypto
    ( serializeCborHash
    )
import qualified Cardano.Crypto as CryptoC
import qualified Cardano.Crypto.Hash as Crypto
import Cardano.Ledger.Core
    ( bodyTxL
    , txIdTxBody
    )
import qualified Cardano.Ledger.Core as SL.Core
import qualified Cardano.Ledger.Hashes as SafeHash
import Cardano.Ledger.TxIn
    ( TxId (..)
    )
import Cardano.Read.Ledger.Eras
    ( Era (..)
    , IsEra (..)
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

{-# INLINE getEraTxHash #-}

-- | Extract the hash of a transaction in any era.
getEraTxHash :: forall era. IsEra era => Tx era -> Crypto.ByteString
getEraTxHash = case theEra @era of
    Byron -> onTx byronTxHash
    Shelley -> mkShelleyHash
    Allegra -> mkShelleyHash
    Mary -> mkShelleyHash
    Alonzo -> mkShelleyHash
    Babbage -> mkShelleyHash
    Conway -> mkShelleyHash
  where
    mkShelleyHash = onTx $ \tx -> shelleyTxHash tx

-- | Extract the transaction hash from a Shelley-era (or later) transaction.
shelleyTxHash
    :: SL.Core.EraTx era => SL.Core.Tx era -> Crypto.ByteString
shelleyTxHash tx = fromShelleyTxId $ txIdTxBody (tx ^. bodyTxL)

-- | Extract the transaction hash from a Byron-era transaction.
byronTxHash :: ATxAux a -> Crypto.ByteString
byronTxHash = CryptoC.hashToBytes . serializeCborHash . taTx

-- | Convert a Shelley transaction ID to raw bytes.
fromShelleyTxId :: TxId -> Crypto.ByteString
fromShelleyTxId (TxId h) =
    Crypto.hashToBytes $ SafeHash.extractHash h
