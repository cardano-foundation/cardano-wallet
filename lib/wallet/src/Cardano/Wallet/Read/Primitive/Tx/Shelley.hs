{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Conversion functions and static chain settings for Shelley.
module Cardano.Wallet.Read.Primitive.Tx.Shelley
    ( fromShelleyTxIn
    , fromShelleyTxOut
    , fromShelleyCoin
    , fromShelleyWdrl
    , fromShelleyMD
    , fromShelleyCert
    , fromShelleyTx
    , fromShelleyAddress
    )
  where

import Prelude

import Cardano.Api
    ( CardanoEra (..), ShelleyEra )
import Cardano.Api.Shelley
    ( fromShelleyMetadata )
import Cardano.Crypto.Hash.Class
    ( hashToBytes )
import Cardano.Ledger.BaseTypes
    ( strictMaybeToMaybe, urlToText )
import Cardano.Ledger.Era
    ( Era (..) )
import Cardano.Slotting.Slot
    ( EpochNo (..) )
import Cardano.Wallet.Primitive.Types
    ( PoolCertificate (..)
    , PoolRegistrationCertificate (..)
    , PoolRetirementCertificate (..)
    )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.CBOR
    ( getTxCBOR )
import Cardano.Wallet.Read.Tx.Hash
    ( fromShelleyTxId, shelleyTxHash )
import Cardano.Wallet.Transaction
    ( TokenMapWithScripts (..)
    , ValidityIntervalExplicit (..)
    , emptyTokenMapWithScripts
    )
import Cardano.Wallet.Util
    ( internalError )
import Data.Bifunctor
    ( bimap )
import Data.Foldable
    ( toList )
import Data.Map.Strict
    ( Map )
import Data.Quantity
    ( Percentage, Quantity (..), mkPercentage )
import Data.Word
    ( Word16, Word32, Word64 )
import Fmt
    ( (+||), (||+) )
import GHC.Stack
    ( HasCallStack )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.Address as SL
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Core as SL.Core
import qualified Cardano.Ledger.Credential as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.API as SLAPI
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.RewardAccount as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Ouroboros.Network.Block as O

fromShelleyTxIn
    :: SL.TxIn crypto
    -> W.TxIn
fromShelleyTxIn (SL.TxIn txid (SL.TxIx ix)) =
    W.TxIn (fromShelleyTxId txid) (unsafeCast ix)
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
    => SLAPI.TxOut era
    -> W.TxOut
fromShelleyTxOut (SLAPI.TxOut addr amount) = W.TxOut
    (fromShelleyAddress addr)
    (TokenBundle.fromCoin $ fromShelleyCoin amount)

fromShelleyAddress :: SL.Addr crypto -> W.Address
fromShelleyAddress = W.Address
    . SL.serialiseAddr

fromShelleyCoin :: SL.Coin -> W.Coin
fromShelleyCoin (SL.Coin c) = Coin.unsafeFromIntegral c

-- NOTE: For resolved inputs we have to pass in a dummy value of 0.
fromShelleyTx
    :: SLAPI.Tx (Cardano.ShelleyLedgerEra ShelleyEra)
    -> ( W.Tx
       , [W.Certificate]
       , TokenMapWithScripts
       , TokenMapWithScripts
       , Maybe ValidityIntervalExplicit
       )
fromShelleyTx tx =
    ( W.Tx
        { txId =
            shelleyTxHash tx
        , txCBOR =
            Just $ getTxCBOR $ Tx ShelleyEra tx
        , fee =
            Just $ fromShelleyCoin fee
        , resolvedInputs =
            map ((,W.Coin 0) . fromShelleyTxIn) (toList ins)
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
    , map fromShelleyCert (toList certs)
    , emptyTokenMapWithScripts
    , emptyTokenMapWithScripts
    , Just (ValidityIntervalExplicit (Quantity 0) (Quantity ttl))
    )
  where
    SL.Tx (SL.TxBody ins outs certs wdrls fee (O.SlotNo ttl) _ _) _ mmd = tx


fromShelleyWdrl :: SL.Wdrl crypto -> Map W.RewardAccount W.Coin
fromShelleyWdrl (SL.Wdrl wdrl) = Map.fromList $
    bimap (fromStakeCredential . SL.getRwdCred) fromShelleyCoin
        <$> Map.toList wdrl

fromShelleyMD :: SL.Metadata c -> Cardano.TxMetadata
fromShelleyMD (SL.Metadata m) =
    Cardano.makeTransactionMetadata . fromShelleyMetadata $ m

fromShelleyCert
    :: SL.DCert crypto
    -> W.Certificate
fromShelleyCert = \case
    SL.DCertDeleg (SL.Delegate delegation)  ->
        W.CertificateOfDelegation $ W.CertDelegateFull
            (fromStakeCredential (SL._delegator delegation))
            (fromPoolKeyHash (SL._delegatee delegation))

    SL.DCertDeleg (SL.DeRegKey credentials) ->
        W.CertificateOfDelegation $ W.CertDelegateNone (fromStakeCredential credentials)

    SL.DCertDeleg (SL.RegKey cred) ->
        W.CertificateOfDelegation $ W.CertRegisterKey $ fromStakeCredential cred

    SL.DCertPool (SL.RegPool pp) -> W.CertificateOfPool $ Registration
        ( W.PoolRegistrationCertificate
            { W.poolId = fromPoolKeyHash $ SL._poolId pp
            , W.poolOwners = fromOwnerKeyHash <$> Set.toList (SL._poolOwners pp)
            , W.poolMargin = fromUnitInterval (SL._poolMargin pp)
            , W.poolCost = toWalletCoin (SL._poolCost pp)
            , W.poolPledge = toWalletCoin (SL._poolPledge pp)
            , W.poolMetadata = fromPoolMetadata <$> strictMaybeToMaybe (SL._poolMD pp)
            }
        )

    SL.DCertPool (SL.RetirePool pid (EpochNo e)) ->
        W.CertificateOfPool $ Retirement $ PoolRetirementCertificate (fromPoolKeyHash pid)
        (W.EpochNo $ fromIntegral e)

    SL.DCertGenesis{} -> W.CertificateOther W.GenesisCertificate

    SL.DCertMir{}     -> W.CertificateOther W.MIRCertificate

toWalletCoin :: HasCallStack => SL.Coin -> W.Coin
toWalletCoin (SL.Coin c) = Coin.unsafeFromIntegral c

fromPoolMetadata :: SL.PoolMetadata -> (W.StakePoolMetadataUrl, W.StakePoolMetadataHash)
fromPoolMetadata meta =
    ( W.StakePoolMetadataUrl (urlToText (SL._poolMDUrl meta))
    , W.StakePoolMetadataHash (SL._poolMDHash meta)
    )

-- | Convert a stake credentials to a 'RewardAccount' type.
--
-- Unlike with Jörmungandr, the reward account payload doesn't represent a
-- public key but a HASH of a public key.
--
fromStakeCredential :: SL.Credential 'SL.Staking crypto -> W.RewardAccount
fromStakeCredential = \case
    SL.ScriptHashObj (SL.ScriptHash h) ->
        W.RewardAccount (hashToBytes h)
    SL.KeyHashObj (SL.KeyHash h) ->
        W.RewardAccount (hashToBytes h)

fromPoolKeyHash :: SL.KeyHash rol sc -> W.PoolId
fromPoolKeyHash (SL.KeyHash h) =
    W.PoolId (hashToBytes h)

fromOwnerKeyHash :: SL.KeyHash 'SL.Staking crypto -> W.PoolOwner
fromOwnerKeyHash (SL.KeyHash h) =
    W.PoolOwner (hashToBytes h)

fromUnitInterval :: HasCallStack => SL.UnitInterval -> Percentage
fromUnitInterval x =
    either bomb id . mkPercentage . toRational . SL.unboundRational $ x
  where
    bomb = internalError $
        "fromUnitInterval: encountered invalid parameter value: "+||x||+""
