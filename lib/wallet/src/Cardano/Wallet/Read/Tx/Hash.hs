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
{-# LANGUAGE NoMonomorphismRestriction #-}

module Cardano.Wallet.Read.Tx.Hash
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
import Cardano.Ledger.Block
    ( txid
    )
import Cardano.Ledger.Core
    ( bodyTxL
    )
import Cardano.Ledger.TxIn
    ( TxId (..)
    )
import Cardano.Wallet.Read
    ( Tx
    )
import Cardano.Wallet.Read.Eras
    ( EraFun (..)
    , K (..)
    )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx
    )
import Control.Lens
    ( (^.)
    )

import qualified Cardano.Crypto as CryptoC
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Ledger.Core as SL.Core
import qualified Cardano.Ledger.SafeHash as SafeHash

-- | Extract the hash of a transaction in any era.
getEraTxHash :: EraFun Tx (K Crypto.ByteString)
getEraTxHash =
    EraFun
        { byronFun = onTx $ K . byronTxHash
        , shelleyFun = mkShelleyHash
        , allegraFun = mkShelleyHash
        , maryFun = mkShelleyHash
        , alonzoFun = mkShelleyHash
        , babbageFun = mkShelleyHash
        , conwayFun = mkShelleyHash
        }
  where
    mkShelleyHash = onTx $ \tx -> K $ shelleyTxHash tx

shelleyTxHash :: SL.Core.EraTx era => SL.Core.Tx era -> Crypto.ByteString
shelleyTxHash tx = fromShelleyTxId $ txid (tx ^. bodyTxL)

byronTxHash :: ATxAux a -> Crypto.ByteString
byronTxHash = CryptoC.hashToBytes . serializeCborHash . taTx

fromShelleyTxId :: TxId crypto -> Crypto.ByteString
fromShelleyTxId (TxId h) =
    Crypto.hashToBytes $ SafeHash.extractHash h
