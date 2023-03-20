{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Read.Primitive.Tx.Features.Certificates
    ( certificates
    , anyEraCerts
    , fromStakeCredential
    )
 where

import Prelude

import Cardano.Crypto.Hash.Class
    ( hashToBytes )
import Cardano.Ledger.BaseTypes
    ( strictMaybeToMaybe, urlToText )
import Cardano.Ledger.Shelley.TxBody
    ( DCert )
import Cardano.Pool.Metadata.Types
    ( StakePoolMetadataHash (..), StakePoolMetadataUrl (..) )
import Cardano.Pool.Types
    ( PoolId (PoolId), PoolOwner (PoolOwner) )
import Cardano.Slotting.Slot
    ( EpochNo (..) )
import Cardano.Wallet.Primitive.Types
    ( PoolCertificate (..)
    , PoolRegistrationCertificate (..)
    , PoolRetirementCertificate (..)
    )
import Cardano.Wallet.Read.Eras
    ( EraFun (..), K (..) )
import Cardano.Wallet.Read.Tx.Certificates
    ( Certificates (..), CertificatesType )
import Cardano.Wallet.Util
    ( internalError )
import Data.Foldable
    ( toList )
import Data.Quantity
    ( Percentage, mkPercentage )
import Fmt
    ( (+||), (||+) )
import GHC.Stack
    ( HasCallStack )

import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Credential as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.RewardAccount as W
import qualified Data.Set as Set

certificates :: EraFun Certificates (K [W.Certificate])
certificates = EraFun
    { byronFun = const $ K []
    , shelleyFun = mkCertsK
    , allegraFun = mkCertsK
    , maryFun = mkCertsK
    , alonzoFun = mkCertsK
    , babbageFun = mkCertsK
    , conwayFun = mkCertsK
    }

mkCertsK :: (Foldable t, CertificatesType era ~ t (DCert crypto))
    => Certificates era
    -> K [W.Certificate] b
mkCertsK (Certificates cs) = K . anyEraCerts $ cs

anyEraCerts :: Foldable t => t (DCert crypto) -> [W.Certificate]
anyEraCerts cs = fromShelleyCert <$> toList cs

fromShelleyCert
    :: SL.DCert crypto
    -> W.Certificate
fromShelleyCert = \case
    SL.DCertDeleg (SL.Delegate delegation)  ->
        W.CertificateOfDelegation $ W.CertDelegateFull
            (fromStakeCredential (SL._delegator delegation))
            (fromPoolKeyHash (SL._delegatee delegation))

    SL.DCertDeleg (SL.DeRegKey credentials) ->
        W.CertificateOfDelegation $ W.CertDelegateNone
            (fromStakeCredential credentials)

    SL.DCertDeleg (SL.RegKey cred) ->
        W.CertificateOfDelegation $ W.CertRegisterKey $ fromStakeCredential cred

    SL.DCertPool (SL.RegPool pp) -> W.CertificateOfPool $ Registration
        ( PoolRegistrationCertificate
            { poolId = fromPoolKeyHash $ SL._poolId pp
            , poolOwners = fromOwnerKeyHash <$> Set.toList (SL._poolOwners pp)
            , poolMargin = fromUnitInterval (SL._poolMargin pp)
            , poolCost = toWalletCoin (SL._poolCost pp)
            , poolPledge = toWalletCoin (SL._poolPledge pp)
            , poolMetadata = fromPoolMetadata <$> strictMaybeToMaybe
                (SL._poolMD pp)
            }
        )

    SL.DCertPool (SL.RetirePool pid (EpochNo e)) ->
        W.CertificateOfPool $ Retirement $ PoolRetirementCertificate (fromPoolKeyHash pid)
        (W.EpochNo $ fromIntegral e)

    SL.DCertGenesis{} -> W.CertificateOther W.GenesisCertificate

    SL.DCertMir{}     -> W.CertificateOther W.MIRCertificate

fromPoolMetadata :: SL.PoolMetadata -> (StakePoolMetadataUrl, StakePoolMetadataHash)
fromPoolMetadata meta =
    ( StakePoolMetadataUrl (urlToText (SL._poolMDUrl meta))
    , StakePoolMetadataHash (SL._poolMDHash meta)
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

fromPoolKeyHash :: SL.KeyHash rol sc -> PoolId
fromPoolKeyHash (SL.KeyHash h) =
    PoolId (hashToBytes h)

fromOwnerKeyHash :: SL.KeyHash 'SL.Staking crypto -> PoolOwner
fromOwnerKeyHash (SL.KeyHash h) =
    PoolOwner (hashToBytes h)

fromUnitInterval :: HasCallStack => SL.UnitInterval -> Percentage
fromUnitInterval x =
    either bomb id . mkPercentage . toRational . SL.unboundRational $ x
  where
    bomb = internalError $
        "fromUnitInterval: encountered invalid parameter value: "+||x||+""

toWalletCoin :: HasCallStack => SL.Coin -> W.Coin
toWalletCoin (SL.Coin c) = Coin.unsafeFromIntegral c
