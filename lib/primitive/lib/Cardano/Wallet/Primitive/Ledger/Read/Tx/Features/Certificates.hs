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
    ( Certificate (..)
    , NonWalletCertificate (..)
    , PoolCertificate (..)
    , PoolRegistrationCertificate (..)
    , PoolRetirementCertificate (..)
    )
import Cardano.Wallet.Primitive.Types.DRep
    ( DRep (..)
    , DRepID (..)
    , DRepKeyHash (..)
    , DRepScriptHash (..)
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
import qualified Cardano.Ledger.DRep as Ledger
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
    Ledger.RegTxCert cred -> mkRegisterKeyCertificate Nothing cred
    Ledger.UnRegTxCert cred -> mkDelegationNone Nothing cred
    Ledger.RegDepositTxCert cred coin ->
        mkRegisterKeyCertificate (Just $ fromLedgerCoin coin) cred
    Ledger.UnRegDepositTxCert cred coin ->
        mkDelegationNone (Just $ fromLedgerCoin coin) cred
    Ledger.DelegTxCert cred delegatee ->
        mkDelegationVoting Nothing cred delegatee
    Ledger.RegDepositDelegTxCert cred delegatee coin ->
        mkDelegationVoting (Just $ fromLedgerCoin coin) cred delegatee
    Ledger.AuthCommitteeHotKeyTxCert _ _ ->
        CertificateOther AuthCommitteeHotKey
    Ledger.ResignCommitteeColdTxCert _ _ ->
        CertificateOther ResignCommitteeColdKey
    Ledger.RegDRepTxCert {} ->
        CertificateOther RegDRep
    Ledger.UnRegDRepTxCert _ _ ->
        CertificateOther UnRegDRep
    _ -> error "impossible pattern"

fromLedgerCoin :: HasCallStack => SL.Coin -> W.Coin
fromLedgerCoin (SL.Coin c) = Coin.unsafeFromIntegral c

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

mkRegisterKeyCertificate
    :: Maybe W.Coin
    -> SL.Credential 'SL.Staking crypto
    -> W.Certificate
mkRegisterKeyCertificate deposit =
    W.CertificateOfDelegation deposit
        . W.CertRegisterKey
        . fromStakeCredential

mkDelegationNone
    :: Maybe W.Coin
    -> SL.Credential 'SL.Staking crypto
    -> W.Certificate
mkDelegationNone deposit credentials =
    W.CertificateOfDelegation deposit
        $ W.CertDelegateNone (fromStakeCredential credentials)

mkDelegationVoting
    :: Maybe W.Coin
    -> SL.Credential 'SL.Staking crypto
    -> Ledger.Delegatee crypto
    -> W.Certificate
mkDelegationVoting deposit cred = \case
    Ledger.DelegStake pool ->
        W.CertificateOfDelegation deposit
            $ W.CertVoteAndDelegate (fromStakeCredential cred)
            (Just $ fromPoolKeyHash pool) Nothing
    Ledger.DelegVote vote ->
        W.CertificateOfDelegation deposit
            $ W.CertVoteAndDelegate (fromStakeCredential cred)
            Nothing (Just $ fromLedgerDRep vote)
    Ledger.DelegStakeVote pool vote ->
        W.CertificateOfDelegation deposit
            $ W.CertVoteAndDelegate (fromStakeCredential cred)
            (Just $ fromPoolKeyHash pool) (Just $ fromLedgerDRep vote)

fromLedgerDRep :: Ledger.DRep crypto -> DRep
fromLedgerDRep = \case
    Ledger.DRepAlwaysAbstain -> Abstain
    Ledger.DRepAlwaysNoConfidence -> NoConfidence
    Ledger.DRepCredential (SL.ScriptHashObj (SL.ScriptHash scripthash)) ->
        FromDRepID (DRepFromScriptHash (DRepScriptHash $ hashToBytes scripthash))
    Ledger.DRepCredential (SL.KeyHashObj (SL.KeyHash keyhash)) ->
        FromDRepID (DRepFromKeyHash (DRepKeyHash $ hashToBytes keyhash))

fromShelleyCert
    :: ( Ledger.ShelleyEraTxCert era
       , Ledger.ProtVerAtMost era 8
       , Ledger.TxCert era ~ ShelleyTxCert era
    )
    => Ledger.TxCert era -> W.Certificate
fromShelleyCert = \case
    Ledger.DelegStakeTxCert delegator pool ->
        W.CertificateOfDelegation Nothing
            $ W.CertVoteAndDelegate
                (fromStakeCredential delegator)
                (Just $ fromPoolKeyHash pool) Nothing
    Ledger.RegTxCert cred -> mkRegisterKeyCertificate Nothing cred
    Ledger.UnRegTxCert cred -> mkDelegationNone Nothing cred
    Ledger.RegPoolTxCert pp -> mkPoolRegistrationCertificate pp
    Ledger.RetirePoolTxCert pid en -> mkPoolRetirementCertificate pid en
    Ledger.GenesisDelegTxCert {} -> W.CertificateOther W.GenesisCertificate
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
