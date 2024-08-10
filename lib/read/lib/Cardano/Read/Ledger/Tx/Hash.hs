{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Read.Ledger.Tx.Hash
    ( byronTxHash
    , shelleyTxHash
    , fromShelleyTxId
    , getEraTxHash
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
import Cardano.Ledger.Core
    ( bodyTxL
    , txIdTxBody
    )
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

import qualified Cardano.Crypto as CryptoC
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Ledger.Core as SL.Core
import qualified Cardano.Ledger.SafeHash as SafeHash

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

shelleyTxHash :: SL.Core.EraTx era => SL.Core.Tx era -> Crypto.ByteString
shelleyTxHash tx = fromShelleyTxId $ txIdTxBody (tx ^. bodyTxL)

byronTxHash :: ATxAux a -> Crypto.ByteString
byronTxHash = CryptoC.hashToBytes . serializeCborHash . taTx

fromShelleyTxId :: TxId crypto -> Crypto.ByteString
fromShelleyTxId (TxId h) =
    Crypto.hashToBytes $ SafeHash.extractHash h
