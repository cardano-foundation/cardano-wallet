{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Certificates
    ( primitiveCertificates
    , anyEraCerts
    , fromStakeCredential
    , fromConwayCert
    )
where

import Prelude hiding
    ( (.)
    )

import Cardano.Api
    ( ConwayEra
    )
import Cardano.Crypto.Hash.Class
    ( hashToBytes
    )
import Cardano.Ledger.BaseTypes
    ( strictMaybeToMaybe
    , urlToText
    )
import Cardano.Ledger.Shelley.API
    ( PoolMetadata (..)
    , PoolParams (..)
    )
import Cardano.Ledger.Shelley.TxCert
    ( ShelleyTxCert
    )
import Cardano.Slotting.Slot
    ( EpochNo (..)
    )
import Cardano.Wallet.Primitive.Types.Certificates
    ( PoolCertificate (..)
    , PoolRegistrationCertificate (..)
    , PoolRetirementCertificate (..)
    )
import Cardano.Wallet.Primitive.Types.Pool
    ( PoolId (PoolId)
    , PoolOwner (PoolOwner)
    )
import Cardano.Wallet.Primitive.Types.StakePoolMetadata
    ( StakePoolMetadataHash (..)
    , StakePoolMetadataUrl (..)
    )
import Cardano.Wallet.Read.Eras
    ( EraFun (..)
    , K (..)
    )
import Cardano.Wallet.Read.Tx
    ( Tx
    )
import Cardano.Wallet.Read.Tx.Certificates
    ( Certificates (..)
    , CertificatesType
    , getEraCertificates
    )
import Cardano.Wallet.Util
    ( internalError
    )
import Control.Category
    ( (.)
    )
import Data.Foldable
    ( toList
    )
import Data.Percentage
    ( Percentage
    )
import Fmt
    ( (+||)
    , (||+)
    )
import GHC.Stack
    ( HasCallStack
    )

import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Credential as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Wallet.Primitive.Types.Certificates as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.EpochNo as W
import qualified Cardano.Wallet.Primitive.Types.RewardAccount as W
import qualified Data.Percentage as Percentage
import qualified Data.Set as Set

anyEraCerts :: EraFun Tx (K [W.Certificate])
anyEraCerts = primitiveCertificates . getEraCertificates

-- | Compute wallet primitive certificates from ledger certificates
primitiveCertificates :: EraFun Certificates (K [W.Certificate])
primitiveCertificates =
    EraFun
        { byronFun = const $ K []
        , shelleyFun = mkShelleyCertsK
        , allegraFun = mkShelleyCertsK
        , maryFun = mkShelleyCertsK
        , alonzoFun = mkShelleyCertsK
        , babbageFun = mkShelleyCertsK
        , conwayFun = mkConwayCertsK
        }

mkConwayCertsK
    :: Certificates ConwayEra
    -> K [W.Certificate] ConwayEra
mkConwayCertsK (Certificates cs) = K $ fromConwayCert <$> toList cs

fromConwayCert
    :: Ledger.ConwayEraTxCert era
    => Ledger.TxCert era
    -> W.Certificate
fromConwayCert = \case
    Ledger.RegPoolTxCert pp -> mkPoolRegistrationCertificate pp
    Ledger.RetirePoolTxCert pid en -> mkPoolRetirementCertificate pid en
    Ledger.RegTxCert cred -> mkRegisterKeyCertificate cred
    Ledger.UnRegTxCert cred -> mkDelegationNone cred
    Ledger.RegDepositTxCert _ _ -> error "TODO: Conway, ADP-3065"
    Ledger.UnRegDepositTxCert _ _ -> error "TODO: Conway, ADP-3065"
    Ledger.DelegTxCert _ _ -> error "TODO: Conway delegation, ADP-3065"
    {-
        Ledger.DelegStakeTxCert delegator pool ->
        W.CertificateOfDelegation
            $ W.CertDelegateFull
                (fromStakeCredential delegator)
                (fromPoolKeyHash pool)
    -}
    Ledger.RegDepositDelegTxCert _ _ _ -> error "TODO: Conway, ADP-3065"
    Ledger.AuthCommitteeHotKeyTxCert _ _ -> error "TODO: Conway other, ADP-3065"
    Ledger.ResignCommitteeColdTxCert _ _ -> error "TODO: Conway other, ADP-3065"
    Ledger.RegDRepTxCert _ _ _ -> error "TODO: Conway other, ADP-3065"
    Ledger.UnRegDRepTxCert _ _ -> error "TODO: Conway other, ADP-3065"
    _ -> error "impossible pattern"

mkShelleyCertsK
    :: ( Foldable t
       , CertificatesType era ~ t (ShelleyTxCert era')
       , Ledger.ShelleyEraTxCert era'
       , Ledger.ProtVerAtMost era' 8
       , Ledger.TxCert era' ~ ShelleyTxCert era'
       )
    => Certificates era
    -> K [W.Certificate] b
mkShelleyCertsK (Certificates cs) = K . map fromShelleyCert $ toList cs

mkPoolRegistrationCertificate :: PoolParams c -> W.Certificate
mkPoolRegistrationCertificate pp =
    W.CertificateOfPool
        $ Registration
        $ PoolRegistrationCertificate
            { poolId = fromPoolKeyHash (ppId pp)
            , poolOwners = fromOwnerKeyHash <$> Set.toList (ppOwners pp)
            , poolMargin = fromUnitInterval (ppMargin pp)
            , poolCost = toWalletCoin (ppCost pp)
            , poolPledge = toWalletCoin (ppPledge pp)
            , poolMetadata =
                fromPoolMetadata
                    <$> strictMaybeToMaybe (ppMetadata pp)
            }

mkPoolRetirementCertificate :: (SL.KeyHash rol sc) -> EpochNo -> W.Certificate
mkPoolRetirementCertificate pid (EpochNo e) =
    W.CertificateOfPool
        $ Retirement
        $ PoolRetirementCertificate
            { poolId = fromPoolKeyHash pid
            , retirementEpoch = W.EpochNo $ fromIntegral e
            }

mkRegisterKeyCertificate :: SL.Credential 'SL.Staking crypto -> W.Certificate
mkRegisterKeyCertificate =
    W.CertificateOfDelegation
        . W.CertRegisterKey
        . fromStakeCredential

mkDelegationNone :: SL.Credential 'SL.Staking crypto -> W.Certificate
mkDelegationNone credentials =
    W.CertificateOfDelegation
        $ W.CertDelegateNone (fromStakeCredential credentials)

fromShelleyCert
    :: ( Ledger.ShelleyEraTxCert era
       , Ledger.ProtVerAtMost era 8
       , Ledger.TxCert era ~ ShelleyTxCert era
    )
    => Ledger.TxCert era -> W.Certificate
fromShelleyCert = \case
    Ledger.DelegStakeTxCert delegator pool ->
        W.CertificateOfDelegation
            $ W.CertDelegateFull
                (fromStakeCredential delegator)
                (fromPoolKeyHash pool)
    Ledger.RegTxCert cred -> mkRegisterKeyCertificate cred
    Ledger.UnRegTxCert cred -> mkDelegationNone cred
    Ledger.RegPoolTxCert pp -> mkPoolRegistrationCertificate pp
    Ledger.RetirePoolTxCert pid en -> mkPoolRetirementCertificate pid en
    Ledger.GenesisDelegTxCert _ _ _ -> W.CertificateOther W.GenesisCertificate
    Ledger.MirTxCert _ -> W.CertificateOther W.MIRCertificate

fromPoolMetadata :: SL.PoolMetadata
    -> (StakePoolMetadataUrl, StakePoolMetadataHash)
fromPoolMetadata meta =
    ( StakePoolMetadataUrl (urlToText (pmUrl meta))
    , StakePoolMetadataHash (pmHash meta)
    )

-- | Convert a stake credentials to a 'RewardAccount' type.
--
-- Unlike with Jörmungandr, the reward account payload doesn't represent a
-- public key but a HASH of a public key.
fromStakeCredential :: SL.Credential 'SL.Staking crypto -> W.RewardAccount
fromStakeCredential = \case
    SL.ScriptHashObj (SL.ScriptHash h) ->
        W.FromScriptHash (hashToBytes h)
    SL.KeyHashObj (SL.KeyHash h) ->
        W.FromKeyHash (hashToBytes h)

fromPoolKeyHash :: SL.KeyHash rol sc -> PoolId
fromPoolKeyHash (SL.KeyHash h) =
    PoolId (hashToBytes h)

fromOwnerKeyHash :: SL.KeyHash 'SL.Staking crypto -> PoolOwner
fromOwnerKeyHash (SL.KeyHash h) =
    PoolOwner (hashToBytes h)

fromUnitInterval :: HasCallStack => SL.UnitInterval -> Percentage
fromUnitInterval x
    = either bomb id
    . Percentage.fromRational
    . toRational
    . SL.unboundRational
    $ x
  where
    bomb = internalError $
        "fromUnitInterval: encountered invalid parameter value: " +|| x ||+ ""

toWalletCoin :: HasCallStack => SL.Coin -> W.Coin
toWalletCoin (SL.Coin c) = Coin.unsafeFromIntegral c
