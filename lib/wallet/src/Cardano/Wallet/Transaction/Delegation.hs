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
    , DRepID (..)
    , DRepKeyHash (..)
    , DRepScriptHash (..)
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
           error "certificateFromDelegationAction: deposit value required in Conway era when registration is carried out"
       (Just Quit, Nothing, Just deposit) ->
           [ Cardano.makeStakeAddressUnregistrationCertificate
               $ Cardano.StakeAddrRegistrationConway
                   conwayWitness
                   (toCardanoLovelace deposit)
                   (toCardanoStakeCredential cred)
           ]
       (Just Quit, Nothing, Nothing) ->
           error "certificateFromDelegationAction: deposit value required in Conway era when deregistration is carried out"

  {--
       -- waiting until cardano-api is updated
       -- we will need here also deposit value sneaked in
       (Nothing, Just (VoteRegisteringKey _action)) -> undefined
            --[ toStakeKeyRegCert cred deposit
            --, toStakePoolDlgCert cred Nothing (Just action)
            --]
       (Nothing, Just (Vote _action)) -> undefined
            --[ toStakePoolDlgCert cred Nothing (Just action) ]
       (Just (Join _poolId), Just (Vote _action)) -> undefined
            --[ toStakePoolDlgCert cred (Just poolId) (Just action) ]
       (Just (JoinRegisteringKey _poolId), Just (VoteRegisteringKey _action)) -> undefined
           --[ toStakeKeyRegCert cred deposit
           --, toStakePoolDlgCert cred (Just poolId) (Just action)
           --]
       (Just Quit, Just (Vote _action)) -> undefined
            --[ toStakePoolDlgCert cred Nothing (Just action) ]

       (Just (Join _poolId), Just (VoteRegisteringKey _action)) -> undefined
            -- this should not happen
       (Just (JoinRegisteringKey _poolId), Just (Vote _action)) -> undefined
            -- this should not happen
--}
       _ -> []

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

toLedgerDelegatee
    :: Maybe Cardano.PoolId
    -> Maybe VotingAction
    -> Ledger.Delegatee Write.StandardCrypto
toLedgerDelegatee poolM vaM = case (poolM, vaM) of
    (Just poolId, Nothing) ->
        Ledger.DelegStake (Cardano.unStakePoolKeyHash poolId)

toLedgerDRep
    :: DRep -> Ledger.DRep Write.StandardCrypto
toLedgerDRep = \case
    Abstain -> Ledger.DRepAlwaysAbstain
    NoConfidence -> Ledger.DRepAlwaysNoConfidence
    FromDRepID (DRepFromKeyHash (DRepKeyHash keyhash)) ->
        Ledger.DRepCredential . Ledger.KeyHashObj . Ledger.KeyHash . UnsafeHash $
        toShort keyhash
    FromDRepID (DRepFromScriptHash (DRepScriptHash scripthash)) ->
        Ledger.DRepCredential . Ledger.ScriptHashObj . SL.ScriptHash . UnsafeHash $
        toShort scripthash
