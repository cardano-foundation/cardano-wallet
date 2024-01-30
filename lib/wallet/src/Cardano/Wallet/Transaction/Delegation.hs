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
    ( toCardanoLovelace
    , toCardanoSimpleScript
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin
    )
import Cardano.Wallet.Primitive.Types.DRep
    ( DRep (..)
    , DRepKeyHash (..)
    , DRepScriptHash (..)
    , VoteAction (..)
    )
import Cardano.Wallet.Primitive.Types.Pool
    ( PoolId (..)
    )
import Cardano.Wallet.Transaction
    ( DelegationAction (..)
    , VotingAction (..)
    )
import Crypto.Hash.Extra
    ( blake2b224
    )
import Data.ByteString.Short
    ( toShort
    )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.ReexposeLedger as Ledger
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.Hashes as SL
import qualified Internal.Cardano.Write.Tx as Write


{-----------------------------------------------------------------------------
    Cardano.Certificate
------------------------------------------------------------------------------}
certificateFromDelegationAction
    :: Write.RecentEra era
        -- ^ Era in which we create the certificate
    -> Either XPub (Script KeyHash)
        -- ^ Our staking credential
    -> Maybe DelegationAction
        -- ^ Delegation action that we plan to take
    -> Maybe VotingAction
        -- ^ Optional vote action in Conway era onwards
    -> Maybe Coin
       -- ^ Optional deposit
    -> [Cardano.Certificate (Write.CardanoApiEra era)]
        -- ^ Certificates representing the action
certificateFromDelegationAction Write.RecentEraBabbage cred daM vaM _ =
    case (daM, vaM) of
    (Just (Join poolId), Nothing) ->
        [ Cardano.makeStakeAddressDelegationCertificate
            $ Cardano.StakeDelegationRequirementsPreConway
                babbageWitness
                (toCardanoStakeCredential cred)
                (toCardanoPoolId poolId)
        ]
    (Just (JoinRegisteringKey poolId), Nothing) ->
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
    (Just (Quit), Nothing) ->
        [ Cardano.makeStakeAddressUnregistrationCertificate
            $ Cardano.StakeAddrRegistrationPreConway
                babbageWitness
                (toCardanoStakeCredential cred)
        ]
    (_, Just _) ->
        error "certificateFromDelegationAction: voting not allowed in Babbage era"
    (Nothing, Nothing) -> []
  where
    babbageWitness = Cardano.ShelleyToBabbageEraBabbage
certificateFromDelegationAction Write.RecentEraConway cred daM vaM depoM =
    case (daM, vaM, depoM) of
       -- just delegation
       (Just (Join poolId), Nothing, _) ->
           [ Cardano.makeStakeAddressDelegationCertificate
               $ Cardano.StakeDelegationRequirementsConwayOnwards
                   conwayWitness
                   (toCardanoStakeCredential cred)
                   (toLedgerDelegatee (Just $ toCardanoPoolId poolId) Nothing)
           ]
       (Just (JoinRegisteringKey poolId), Nothing, Just deposit) ->
           [ Cardano.makeStakeAddressRegistrationCertificate
               $ Cardano.StakeAddrRegistrationConway
                   conwayWitness
                   (toCardanoLovelace deposit)
                   (toCardanoStakeCredential cred)
           , Cardano.makeStakeAddressDelegationCertificate
               $ Cardano.StakeDelegationRequirementsConwayOnwards
                   conwayWitness
                   (toCardanoStakeCredential cred)
                   (toLedgerDelegatee (Just $ toCardanoPoolId poolId) Nothing)
           ]
       (Just (JoinRegisteringKey _), Nothing, Nothing) ->
           error "certificateFromDelegationAction: deposit value required in \
                 \Conway era when registration is carried out (joining)"
       (Just Quit, Nothing, Just deposit) ->
           [ Cardano.makeStakeAddressUnregistrationCertificate
               $ Cardano.StakeAddrRegistrationConway
                   conwayWitness
                   (toCardanoLovelace deposit)
                   (toCardanoStakeCredential cred)
           ]
       (Just Quit, Nothing, Nothing) ->
           error "certificateFromDelegationAction: deposit value required in \
                 \Conway era when registration is carried out (quitting)"

       -- just voting
       (Nothing, Just (Vote action), _) ->
           [ Cardano.makeStakeAddressDelegationCertificate
               $ Cardano.StakeDelegationRequirementsConwayOnwards
                   conwayWitness
                   (toCardanoStakeCredential cred)
                   (toLedgerDelegatee Nothing (Just action))
           ]
       (Nothing, Just (VoteRegisteringKey action), Just deposit) ->
           [ Cardano.makeStakeAddressRegistrationCertificate
               $ Cardano.StakeAddrRegistrationConway
                   conwayWitness
                   (toCardanoLovelace deposit)
                   (toCardanoStakeCredential cred)
           , Cardano.makeStakeAddressDelegationCertificate
               $ Cardano.StakeDelegationRequirementsConwayOnwards
                   conwayWitness
                   (toCardanoStakeCredential cred)
                   (toLedgerDelegatee Nothing (Just action))
           ]
       (Nothing, Just (VoteRegisteringKey _), Nothing) ->
           error "certificateFromDelegationAction: deposit value required in \
                 \Conway era when registration is carried out (voting)"

       -- both delegation and voting
       (Just (Join poolId), Just (Vote action), _) ->
           [ Cardano.makeStakeAddressDelegationCertificate
               $ Cardano.StakeDelegationRequirementsConwayOnwards
                   conwayWitness
                   (toCardanoStakeCredential cred)
                   (toLedgerDelegatee (Just $ toCardanoPoolId poolId) (Just action))
           ]
       (Just (JoinRegisteringKey poolId), Just (VoteRegisteringKey action), Just deposit) ->
           registerDelegateAndVote poolId action deposit
       (Just (Join poolId), Just (VoteRegisteringKey action), Just deposit) ->
           registerDelegateAndVote poolId action deposit
       (Just (JoinRegisteringKey poolId), Just (Vote action), Just deposit) ->
           registerDelegateAndVote poolId action deposit
       (Just (JoinRegisteringKey _), Just (VoteRegisteringKey _), Nothing) ->
           error "certificateFromDelegationAction: deposit value required in \
                 \Conway era when registration is carried out (joining reg and voting reg)"
       (Just (Join _), Just (VoteRegisteringKey _), Nothing) ->
           error "certificateFromDelegationAction: deposit value required in \
                 \Conway era when registration is carried out (joining and voting reg)"
       (Just (JoinRegisteringKey _), Just (Vote _), Nothing) ->
           error "certificateFromDelegationAction: deposit value required in \
                 \Conway era when registration is carried out (joining reg and voting)"
       (Just Quit, Just (Vote _), _) ->
           error "certificateFromDelegationAction: quitting and voting in the same  \
                 \transaction in Conway era is forbidden"
       (Just Quit, Just (VoteRegisteringKey action), _) ->
           [ Cardano.makeStakeAddressDelegationCertificate
               $ Cardano.StakeDelegationRequirementsConwayOnwards
                   conwayWitness
                   (toCardanoStakeCredential cred)
                   (toLedgerDelegatee Nothing (Just action))
           ]
       _ -> []
  where
    conwayWitness = Cardano.ConwayEraOnwardsConway
    registerDelegateAndVote poolId action deposit =
        [ Cardano.makeStakeAddressRegistrationCertificate
            $ Cardano.StakeAddrRegistrationConway
                conwayWitness
                (toCardanoLovelace deposit)
                (toCardanoStakeCredential cred)
        , Cardano.makeStakeAddressDelegationCertificate
            $ Cardano.StakeDelegationRequirementsConwayOnwards
                conwayWitness
                (toCardanoStakeCredential cred)
                (toLedgerDelegatee (Just $ toCardanoPoolId poolId) (Just action))
        ]

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

toLedgerDelegatee
    :: Maybe Cardano.PoolId
    -> Maybe VoteAction
    -> Ledger.Delegatee Write.StandardCrypto
toLedgerDelegatee poolM vaM = case (poolM, vaM) of
    (Just poolId, Nothing) ->
        Ledger.DelegStake (Cardano.unStakePoolKeyHash poolId)
    (Nothing, Just vote) ->
        Ledger.DelegVote (toLedgerDRep vote)
    (Just poolId, Just vote) ->
        Ledger.DelegStakeVote (Cardano.unStakePoolKeyHash poolId) (toLedgerDRep vote)
    _ ->
        error "toLedgerDelegatee: wrong use, at least pool or vote action must be present"

toLedgerDRep
    :: VoteAction -> Ledger.DRep Write.StandardCrypto
toLedgerDRep = \case
    Abstain -> Ledger.DRepAlwaysAbstain
    NoConfidence -> Ledger.DRepAlwaysNoConfidence
    VoteTo (DRepFromKeyHash (DRepKeyHash keyhash)) ->
        Ledger.DRepCredential . Ledger.KeyHashObj . Ledger.KeyHash . UnsafeHash $
        toShort keyhash
    VoteTo (DRepFromScriptHash (DRepScriptHash scripthash)) ->
        Ledger.DRepCredential . Ledger.ScriptHashObj . SL.ScriptHash . UnsafeHash $
        toShort scripthash
