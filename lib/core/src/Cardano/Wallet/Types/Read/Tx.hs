{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright: © 2022 IOHK
License: Apache-2.0

The 'Tx' type represents transactions as they are read from the mainnet ledger.
It is compatible with the era-specific types from @cardano-ledger@.
-}
module Cardano.Wallet.Types.Read.Tx
    ( -- * Transactions
      TxEra
    , Tx (..)
    , TxId
    , computeTxId
    ) where

import Prelude

import Cardano.Api
    ( AllegraEra
    , AlonzoEra
    , BabbageEra
    , ByronEra
    , CardanoEra (..)
    , MaryEra
    , ShelleyEra
    )

import qualified Cardano.Api as Api
import qualified Cardano.Api.Shelley as Api
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Crypto as CryptoC
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.Shelley.API as Shelley
import qualified Cardano.Ledger.TxIn as Shelley
import qualified Cardano.Wallet.Primitive.Types.Hash as W

{-------------------------------------------------------------------------------
    Transaction type
-------------------------------------------------------------------------------}
-- | Closed type family returning the ledger 'Tx' type for each known @era@.
type family TxEra era where
    TxEra ByronEra = Byron.ATxAux ()
    TxEra ShelleyEra = Shelley.Tx (Api.ShelleyLedgerEra ShelleyEra)
    TxEra AllegraEra = Shelley.Tx (Api.ShelleyLedgerEra AllegraEra)
    TxEra MaryEra = Shelley.Tx (Api.ShelleyLedgerEra MaryEra)
    TxEra AlonzoEra = Alonzo.ValidatedTx (Api.ShelleyLedgerEra AlonzoEra)
    TxEra BabbageEra = Alonzo.ValidatedTx (Api.ShelleyLedgerEra BabbageEra)

{- Note [CardanoEra]

The `CardanoEra` from `Cardano.Api` enumerates the known Cardano eras.
It's very convenient, because it's a GADT and
brings a type level `era` in scope.

This `era` is not immediately suitable for use with the ledger types,
but the type family `Api.ShelleyLedgerEra` performs the conversion.
-}

-- | A transaction as it can be read from the Cardano mainnet.
--
-- This transaction can come from any era, old (Byron) and new (Babbage).
data Tx where
    Tx
        :: forall era. Api.IsCardanoEra era
        => Api.CardanoEra era
        -> TxEra era
        -> Tx

{- Note [SeeminglyRedundantPatternMatches]

When writing code that handles multiple eras, it is highly recommended
that you pattern match on each era invidiually and handle the specialized
types that arise.

When doing these pattern matches, you may find that some cases
may seem highly redundant.
However, the types of the patterns are wildly different due to
type-level programming, so the cases are not actually redundant.

In other words, it is best to keep each case separate,
even if this seemingly duplicates code,
and *not* attempt to refactor them into common function,
as the type signatures for those functions are more complicated
than any simplification that would result from avoided repetition.

Look, you have been warned!
-}

{-------------------------------------------------------------------------------
    TxId
-------------------------------------------------------------------------------}
type TxId = W.Hash "Tx"

-- | Compute the 'txId' of a transaction by hashing the transaction body.
computeTxId :: Tx -> TxId
computeTxId (Tx era tx) = case era of
    -- See Note [SeeminglyRedundantPatternMatches]

    ByronEra -> txHashByron tx

    ShelleyEra -> case tx of
        Shelley.Tx bod _ _ -> fromShelleyTxId $ Shelley.txid bod
    MaryEra -> case tx of
        Shelley.Tx bod _ _ -> fromShelleyTxId $ Shelley.txid bod
    AllegraEra -> case tx of
        Shelley.Tx bod _ _ -> fromShelleyTxId $ Shelley.txid bod

    AlonzoEra -> case tx of
        Alonzo.ValidatedTx bod _ _ _ -> fromShelleyTxId $ Shelley.txid bod
    BabbageEra -> case tx of
        Alonzo.ValidatedTx bod _ _ _ -> fromShelleyTxId $ Shelley.txid bod

txHashByron :: Byron.ATxAux a -> W.Hash tag
txHashByron =
    W.Hash . CryptoC.hashToBytes . CryptoC.serializeCborHash . Byron.taTx

fromShelleyTxId :: Shelley.TxId crypto -> W.Hash "Tx"
fromShelleyTxId (Shelley.TxId h) =
    W.Hash $ Crypto.hashToBytes $ SafeHash.extractHash h
