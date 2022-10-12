
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Wallet.Read.Tx.Hash
    ( byronTxHash
    , alonzoTxHash
    , shelleyTxHash
    , fromShelleyTxId
    , getEraTxHash
    )
    where

import Prelude

import Cardano.Binary
    ( ToCBOR (..) )
import Cardano.Chain.UTxO
    ( ATxAux, taTx )
import Cardano.Crypto
    ( serializeCborHash )
import Cardano.Ledger.Core
    ( AuxiliaryData )
import Cardano.Ledger.Era
    ( Era (..) )
import Cardano.Ledger.Shelley.TxBody
    ( EraIndependentTxBody )
import Cardano.Wallet.Read
    ( Tx )
import Cardano.Wallet.Read.Eras
    ( EraFun (..), K (..) )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx )

import qualified Cardano.Crypto as CryptoC
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Babbage.Tx as Babbage hiding
    ( ScriptIntegrityHash, TxBody )
import qualified Cardano.Ledger.Core as SL.Core
import qualified Cardano.Ledger.Crypto as SL
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.ShelleyMA as MA
import qualified Cardano.Ledger.TxIn as TxIn

getEraTxHash :: EraFun Tx (K Crypto.ByteString)
getEraTxHash = EraFun
    { byronFun = onTx $ K . byronTxHash
    , shelleyFun = onTx $ K . shelleyTxHash
    , allegraFun = onTx $ K . shelleyTxHash
    , maryFun = onTx $ K . shelleyTxHash
    , alonzoFun = onTx $ K . alonzoTxHash
    , babbageFun = onTx $ K . alonzoTxHash
    }

byronTxHash :: ATxAux a -> Crypto.ByteString
byronTxHash = CryptoC.hashToBytes . serializeCborHash . taTx

alonzoTxHash
    :: ( Crypto.HashAlgorithm (SL.HASH crypto)
       , SafeHash.HashAnnotated
             (SL.Core.TxBody era)
             EraIndependentTxBody
             crypto)
    => Babbage.ValidatedTx era
    -> Crypto.ByteString
alonzoTxHash (Alonzo.ValidatedTx bod _ _ _) = fromShelleyTxId $ TxIn.txid bod

shelleyTxHash
    :: ( Era x
       , ToCBOR (AuxiliaryData x)
       , ToCBOR (SL.Core.TxBody x)
       , ToCBOR (SL.Core.Witnesses x))
    => MA.Tx x
    -> Crypto.ByteString
shelleyTxHash
    (SL.Tx bod _ _) = fromShelleyTxId $ TxIn.txid bod

fromShelleyTxId :: SL.TxId crypto -> Crypto.ByteString
fromShelleyTxId (SL.TxId h) =
    Crypto.hashToBytes $ SafeHash.extractHash h
