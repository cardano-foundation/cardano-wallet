{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Read.Primitive.Tx.Shelley
    ( fromShelleyTxIn
    , fromShelleyTxOut
    , fromShelleyCoin
    , fromShelleyWdrl
    , fromShelleyMD
    , fromShelleyTx
    , fromShelleyAddress
    )
    where

import Prelude

import Cardano.Address.Script
    ( KeyHash (..), KeyRole (..), Script (..) )
import Cardano.Api
    ( ShelleyEra )
import Cardano.Api.Shelley
    ( fromShelleyMetadata )
import Cardano.Crypto.Hash
    ( hashToBytes )
import Cardano.Ledger.Era
    ( Era (..) )
import Cardano.Wallet.Read.Eras
    ( inject, shelley )
import Cardano.Wallet.Read.Primitive.Tx.Features.Certificates
    ( anyEraCerts, fromStakeCredential )
import Cardano.Wallet.Read.Primitive.Tx.Features.Validity
    ( shelleyValidityInterval )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.CBOR
    ( renderTxToCBOR )
import Cardano.Wallet.Read.Tx.Hash
    ( fromShelleyTxId, shelleyTxHash )
import Cardano.Wallet.Transaction
    ( AnyScript (..)
    , TokenMapWithScripts (..)
    , ValidityIntervalExplicit (..)
    , WitnessCount (..)
    , emptyTokenMapWithScripts
    )
import Data.Bifunctor
    ( bimap )
import Data.Foldable
    ( toList )
import Data.Map.Strict
    ( Map )
import Data.Word
    ( Word16, Word32, Word64 )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.Address as SL
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Core as SL.Core
import qualified Cardano.Ledger.Crypto as SL
import qualified Cardano.Ledger.Keys as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.Tx as SL
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.RewardAccount as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W
    ( TxIn (TxIn) )
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
    ( TxOut (TxOut) )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

fromShelleyTxIn
    :: SL.TxIn crypto
    -> W.TxIn
fromShelleyTxIn (SL.TxIn txid (SL.TxIx ix)) =
    W.TxIn (W.Hash $ fromShelleyTxId txid) (unsafeCast ix)
  where
    -- During the Vasil hard-fork the cardano-ledger team moved from
    -- representing transaction indices with Word16s, to using Word64s (see
    -- commit
    -- https://github.com/input-output-hk/cardano-ledger/commit/4097a9055e6ea57161755e6a8cbfcf719b65e9ab).
    -- However, the valid range is still 0 <= x <= (maxBound :: Word16), so we
    -- reflect that here.
    unsafeCast :: Word64 -> Word32
    unsafeCast txIx =
        if txIx > fromIntegral (maxBound :: Word16)
        then error $ "Value for wallet TxIx is out of a valid range: " <> show txIx
        else fromIntegral txIx

fromShelleyTxOut
    :: ( Era era
       , SL.Core.Value era ~ SL.Coin
       )
    => SL.TxOut era
    -> W.TxOut
fromShelleyTxOut (SL.TxOut addr amount) = W.TxOut
    (fromShelleyAddress addr)
    (TokenBundle.fromCoin $ fromShelleyCoin amount)

fromShelleyAddress :: SL.Addr crypto -> W.Address
fromShelleyAddress = W.Address . SL.serialiseAddr

fromShelleyCoin :: SL.Coin -> W.Coin
fromShelleyCoin (SL.Coin c) = Coin.unsafeFromIntegral c

-- NOTE: For resolved inputs we have to pass in a dummy value of 0.
fromShelleyTx
    :: SL.Tx (Cardano.ShelleyLedgerEra ShelleyEra)
    -> ( W.Tx
       , [W.Certificate]
       , TokenMapWithScripts
       , TokenMapWithScripts
       , Maybe ValidityIntervalExplicit
       , WitnessCount
       )
fromShelleyTx tx =
    ( W.Tx
        { txId =
            W.Hash $ shelleyTxHash tx
        , txCBOR =
            Just $ renderTxToCBOR $ inject shelley $ Tx tx
        , fee =
            Just $ fromShelleyCoin fee
        , resolvedInputs =
            (,Nothing) . fromShelleyTxIn <$> toList ins
        , resolvedCollateralInputs =
            []
        , outputs =
            map fromShelleyTxOut (toList outs)
        , collateralOutput =
            -- Collateral outputs are not supported in Shelley.
            Nothing
        , withdrawals =
            fromShelleyWdrl wdrls
        , metadata =
            fromShelleyMD <$> SL.strictMaybeToMaybe mmd
        , scriptValidity =
            Nothing
        }
    , anyEraCerts certs
    , emptyTokenMapWithScripts
    , emptyTokenMapWithScripts
    , Just $ shelleyValidityInterval ttl
    , countWits
    )
  where
    SL.Tx (SL.TxBody ins outs certs wdrls fee ttl _ _) wits mmd = tx
    countWits = WitnessCount
        (fromIntegral $ Set.size $ SL.addrWits wits)
        (fmap (NativeScript . fromLedgerScript Payment) $ Map.elems $ SL.scriptWits wits)
        (fromIntegral $ Set.size $ SL.bootWits wits)

fromShelleyWdrl :: SL.Wdrl crypto -> Map W.RewardAccount W.Coin
fromShelleyWdrl (SL.Wdrl wdrl) = Map.fromList $
    bimap (fromStakeCredential . SL.getRwdCred) fromShelleyCoin
        <$> Map.toList wdrl

fromShelleyMD :: SL.Metadata c -> Cardano.TxMetadata
fromShelleyMD (SL.Metadata m) =
    Cardano.makeTransactionMetadata . fromShelleyMetadata $ m

fromLedgerScript
    :: SL.Crypto crypto
    => KeyRole
    -> SL.MultiSig crypto
    -> Script KeyHash
fromLedgerScript keyrole = fromLedgerScript'
  where
    fromLedgerScript' (SL.RequireSignature (SL.KeyHash h)) =
        RequireSignatureOf (KeyHash keyrole (hashToBytes h))
    fromLedgerScript' (SL.RequireAllOf contents) =
        RequireAllOf $ map fromLedgerScript' $ toList contents
    fromLedgerScript' (SL.RequireAnyOf contents) =
        RequireAnyOf $ map fromLedgerScript' $ toList contents
    fromLedgerScript' (SL.RequireMOf num contents) =
        RequireSomeOf (fromIntegral num) $ fromLedgerScript' <$> toList contents
