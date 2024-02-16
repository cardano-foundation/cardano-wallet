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
import Cardano.Wallet.Primitive.Ledger.Convert
    ( toLedgerDelegatee
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( toCardanoLovelace
    , toCardanoSimpleScript
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin
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
import Internal.Cardano.Write.Tx
    ( CardanoApiEra
    , RecentEra (RecentEraBabbage, RecentEraConway)
    )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.ReexposeLedger as Ledger
import qualified Cardano.Api.Shelley as Cardano

{-----------------------------------------------------------------------------
    Cardano.Certificate
------------------------------------------------------------------------------}
certificateFromDelegationAction
    :: RecentEra era
        -- ^ Era in which we create the certificate
    -> Either XPub (Script KeyHash)
        -- ^ Our staking credential
    -> Maybe Coin
        -- ^ Optional deposit value
    -> DelegationAction
        -- ^ Delegation action that we plan to take
    -> [Cardano.Certificate (CardanoApiEra era)]
        -- ^ Certificates representing the action
certificateFromDelegationAction RecentEraBabbage cred _ da = case da of
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
certificateFromDelegationAction RecentEraConway cred depositM da =
    case (da, depositM) of
       (Join poolId, _) ->
           [ Cardano.makeStakeAddressDelegationCertificate
               $ Cardano.StakeDelegationRequirementsConwayOnwards
                   conwayWitness
                   (toCardanoStakeCredential cred)
                   (toLedgerDelegatee (Just poolId) Nothing)
           ]
       (JoinRegisteringKey poolId, Just deposit) ->
           [ Cardano.makeStakeAddressRegistrationCertificate
               $ Cardano.StakeAddrRegistrationConway
                   conwayWitness
                   (toCardanoLovelace deposit)
                   (toCardanoStakeCredential cred)
           , Cardano.makeStakeAddressDelegationCertificate
               $ Cardano.StakeDelegationRequirementsConwayOnwards
                   conwayWitness
                   (toCardanoStakeCredential cred)
                   (toLedgerDelegatee (Just poolId) Nothing)
           ]
       (JoinRegisteringKey _, Nothing) ->
           error "certificateFromDelegationAction: deposit value required in \
                 \Conway era when registration is carried out (joining)"
       (Quit, Just deposit) ->
           [ Cardano.makeStakeAddressUnregistrationCertificate
               $ Cardano.StakeAddrRegistrationConway
                   conwayWitness
                   (toCardanoLovelace deposit)
                   (toCardanoStakeCredential cred)
           ]
       (Quit, Nothing) ->
           error "certificateFromDelegationAction: deposit value required in \
                 \Conway era when registration is carried out (quitting)"
  where
    conwayWitness = Cardano.ConwayEraOnwardsConway

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
