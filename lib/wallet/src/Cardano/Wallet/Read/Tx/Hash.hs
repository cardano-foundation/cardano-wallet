
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

import Cardano.Chain.UTxO
    ( ATxAux, taTx )
import Cardano.Crypto
    ( serializeCborHash )
import Cardano.Ledger.Block
    ( txid )
import Cardano.Ledger.Core
    ( bodyTxL )
import Cardano.Ledger.TxIn
    ( TxId (..) )
import Cardano.Wallet.Read
    ( Tx )
import Cardano.Wallet.Read.Eras
    ( EraFun (..), K (..) )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx )
import Control.Lens
    ( (^.) )

import qualified Cardano.Crypto as CryptoC
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Ledger.Core as SL.Core
import qualified Cardano.Ledger.SafeHash as SafeHash


getEraTxHash :: EraFun Tx (K Crypto.ByteString)
getEraTxHash = EraFun
    { byronFun = onTx $ K . byronTxHash
    , shelleyFun = onTx $ \tx -> K . fromShelleyTxId $ txid (tx ^. bodyTxL)
    , allegraFun = onTx $ \tx -> K . fromShelleyTxId $ txid (tx ^. bodyTxL)
    , maryFun = onTx $ \tx -> K . fromShelleyTxId $ txid (tx ^. bodyTxL)
    , alonzoFun = onTx $ \tx -> K . fromShelleyTxId $ txid (tx ^. bodyTxL)
    , babbageFun = onTx $ \tx -> K . fromShelleyTxId $ txid (tx ^. bodyTxL)
    }

byronTxHash :: ATxAux a -> Crypto.ByteString
byronTxHash = CryptoC.hashToBytes . serializeCborHash . taTx

alonzoTxHash
    :: SL.Core.EraTx era
    => SL.Core.Tx era
    -> Crypto.ByteString
alonzoTxHash tx = fromShelleyTxId $ txid (tx ^. bodyTxL)

shelleyTxHash
    :: SL.Core.EraTx era
    => SL.Core.Tx era
    -> Crypto.ByteString
shelleyTxHash tx = fromShelleyTxId $ txid (tx ^. bodyTxL)

fromShelleyTxId :: TxId crypto -> Crypto.ByteString
fromShelleyTxId (TxId h) =
    Crypto.hashToBytes $ SafeHash.extractHash h
