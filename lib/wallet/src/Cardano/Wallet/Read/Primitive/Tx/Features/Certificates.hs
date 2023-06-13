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
    , fromConwayCerts
    )
 where

import Prelude

import Cardano.Api
    ( ConwayEra )
import Cardano.Crypto.Hash.Class
    ( hashToBytes )
import Cardano.Ledger.Api
    ( StandardCrypto )
import Cardano.Ledger.BaseTypes
    ( strictMaybeToMaybe, urlToText )
import Cardano.Ledger.Conway.Delegation.Certificates
    ( ConwayDCert (..), ConwayDelegCert (..) )
import Cardano.Ledger.Shelley.API
    ( PoolCert (..), PoolMetadata (..), PoolParams (..) )
import Cardano.Ledger.Shelley.TxBody
    ( DCert )
import Cardano.Pool.Metadata.Types
    ( StakePoolMetadataHash (..), StakePoolMetadataUrl (..) )
import Cardano.Pool.Types
    ( PoolId (PoolId), PoolOwner (PoolOwner) )
import Cardano.Slotting.Slot
    ( EpochNo (..) )
import Cardano.Wallet.Primitive.Types
    ( PoolCertificate (..), PoolRegistrationCertificate (..),
    PoolRetirementCertificate (..) )
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
    , shelleyFun = mkShelleyCertsK
    , allegraFun = mkShelleyCertsK
    , maryFun = mkShelleyCertsK
    , alonzoFun = mkShelleyCertsK
    , babbageFun = mkShelleyCertsK
    , conwayFun = mkConwayCertsK
    }

mkConwayCertsK :: Certificates ConwayEra
    -> K [W.Certificate] ConwayEra
mkConwayCertsK (Certificates cs) = K $ fromConwayCerts <$> toList cs

fromConwayCerts :: ConwayDCert StandardCrypto -> W.Certificate
fromConwayCerts = \case
    ConwayDCertDeleg cdc -> case cdc of
        ConwayDeleg _cre _del _co -> error "TODO: ConwayDeleg, ADP-3065"
        ConwayReDeleg _cre _del -> error "TODO: ConwayReDeleg, ADP-3065"
        ConwayUnDeleg cre _co -> mkDelegationNone cre
            -- "TODO: ConwayUnDeleg, ADP-3065"
    ConwayDCertPool pc -> case pc of
        RegPool pp -> mkPoolRegistrationCertificate pp
        RetirePool kh en -> mkPoolRetirementCertificate kh en
    ConwayDCertConstitutional _cdc ->
        error "TODO: ConwayDCertConstitutional, ADP-3065"

mkShelleyCertsK :: (Foldable t, CertificatesType era ~ t (DCert crypto))
    => Certificates era
    -> K [W.Certificate] b
mkShelleyCertsK (Certificates cs) = K . anyEraCerts $ cs

anyEraCerts :: Foldable t => t (DCert crypto) -> [W.Certificate]
anyEraCerts cs = fromShelleyCert <$> toList cs

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
    W.CertificateOfPool $ Retirement $ PoolRetirementCertificate
        { poolId = fromPoolKeyHash pid
        , retirementEpoch = W.EpochNo $ fromIntegral e
        }

mkRegisterKeyCertificate :: SL.Credential 'SL.Staking crypto -> W.Certificate
mkRegisterKeyCertificate =
    W.CertificateOfDelegation
        . W.CertRegisterKey
        . fromStakeCredential

mkDelegationNone :: SL.Credential 'SL.Staking crypto -> W.Certificate
mkDelegationNone credentials = W.CertificateOfDelegation
    $ W.CertDelegateNone (fromStakeCredential credentials)

fromShelleyCert
    :: SL.DCert crypto
    -> W.Certificate
fromShelleyCert = \case
    SL.DCertDeleg (SL.Delegate delegation)  ->
        W.CertificateOfDelegation $ W.CertDelegateFull
            (fromStakeCredential (SL.dDelegator delegation))
            (fromPoolKeyHash (SL.dDelegatee delegation))

    SL.DCertDeleg (SL.DeRegKey credentials) -> mkDelegationNone credentials

    SL.DCertDeleg (SL.RegKey cred) -> mkRegisterKeyCertificate cred

    SL.DCertPool (SL.RegPool pp) -> mkPoolRegistrationCertificate pp

    SL.DCertPool (SL.RetirePool pid en) ->
        mkPoolRetirementCertificate pid en

    SL.DCertGenesis{} -> W.CertificateOther W.GenesisCertificate

    SL.DCertMir{}     -> W.CertificateOther W.MIRCertificate

fromPoolMetadata :: SL.PoolMetadata -> (StakePoolMetadataUrl, StakePoolMetadataHash)
fromPoolMetadata meta =
    ( StakePoolMetadataUrl (urlToText (pmUrl meta))
    , StakePoolMetadataHash (pmHash meta)
    )

-- | Convert a stake credentials to a 'RewardAccount' type.
--
-- Unlike with Jörmungandr, the reward account payload doesn't represent a
-- public key but a HASH of a public key.
--
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
fromUnitInterval x =
    either bomb id . mkPercentage . toRational . SL.unboundRational $ x
  where
    bomb = internalError $
        "fromUnitInterval: encountered invalid parameter value: "+||x||+""

toWalletCoin :: HasCallStack => SL.Coin -> W.Coin
toWalletCoin (SL.Coin c) = Coin.unsafeFromIntegral c
