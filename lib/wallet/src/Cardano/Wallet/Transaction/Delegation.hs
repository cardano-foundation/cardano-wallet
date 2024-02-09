{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Tools for creating transactions that change the delegation status.
--
module Cardano.Wallet.Transaction.Delegation
    ( certificateFromDelegationAction
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPub
    , xpubPublicKey
    )
import Cardano.Address.Script
    ( KeyHash (..)
    , Script (..)
    )
import Cardano.Crypto.Hash.Class
    ( Hash (UnsafeHash)
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( toCardanoSimpleScript
    )
import Cardano.Wallet.Primitive.Types.Pool
    ( PoolId (..)
    )
import Cardano.Wallet.Transaction
    ( DelegationAction (..)
    )
import Crypto.Hash.Extra
    ( blake2b224
    )
import Data.ByteString.Short
    ( toShort
    )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.Keys as Ledger
import qualified Internal.Cardano.Write.Tx as Write
    ( CardanoApiEra
    , RecentEra (RecentEraBabbage, RecentEraConway)
    )

{-----------------------------------------------------------------------------
    Cardano.Certificate
------------------------------------------------------------------------------}
certificateFromDelegationAction
    :: Write.RecentEra era
        -- ^ Era in which we create the certificate
    -> Either XPub (Script KeyHash)
        -- ^ Our staking credential
    -> DelegationAction
        -- ^ Delegation action that we plan to take
    -> [Cardano.Certificate (Write.CardanoApiEra era)]
        -- ^ Certificates representing the action
certificateFromDelegationAction Write.RecentEraBabbage cred = \case
    Join poolId ->
        [ Cardano.makeStakeAddressDelegationCertificate
            $ Cardano.StakeDelegationRequirementsPreConway
                babbageWitness
                (toCardanoStakeCredential cred)
                (toCardanoPoolId poolId)
        ]
    JoinRegisteringKey poolId ->
        [ Cardano.makeStakeAddressRegistrationCertificate
            $ Cardano.StakeAddrRegistrationPreConway
                babbageWitness
                (toCardanoStakeCredential cred)
        , Cardano.makeStakeAddressDelegationCertificate
            $ Cardano.StakeDelegationRequirementsPreConway
                babbageWitness
                (toCardanoStakeCredential cred)
                (toCardanoPoolId poolId)
        ]
    Quit ->
        [ Cardano.makeStakeAddressUnregistrationCertificate
            $ Cardano.StakeAddrRegistrationPreConway
                babbageWitness
                (toCardanoStakeCredential cred)
        ]
  where
    babbageWitness = Cardano.ShelleyToBabbageEraBabbage
certificateFromDelegationAction Write.RecentEraConway _cred =
    error "certificateFromDelegationAction: not supported in Conway yet"
  where
    _conwayWitness = Cardano.ConwayEraOnwardsConway

{-----------------------------------------------------------------------------
    Cardano.StakeCredential
------------------------------------------------------------------------------}

toCardanoStakeCredential
    :: Either XPub (Script KeyHash) -> Cardano.StakeCredential
toCardanoStakeCredential = \case
    Left xpub ->
        Cardano.StakeCredentialByKey
        . toHashStakeKey
        $ xpub
    Right script ->
        Cardano.StakeCredentialByScript
        . Cardano.hashScript
        . Cardano.SimpleScript
        $ toCardanoSimpleScript script

toCardanoPoolId :: PoolId -> Cardano.PoolId
toCardanoPoolId (PoolId pid) =
    Cardano.StakePoolKeyHash . Ledger.KeyHash . UnsafeHash $ toShort pid

toHashStakeKey :: XPub -> Cardano.Hash Cardano.StakeKey
toHashStakeKey =
    Cardano.StakeKeyHash
    . Ledger.KeyHash
    . UnsafeHash
    . toShort
    . blake2b224
    . xpubPublicKey
