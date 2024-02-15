{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Tools for creating transactions that change the voting status.
--
module Cardano.Wallet.Transaction.Voting
    ( certificateFromVotingAction
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
import Cardano.Wallet.Transaction
    ( VotingAction (..)
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
certificateFromVotingAction
    :: Write.RecentEra era
        -- ^ Era in which we create the certificate
    -> Either XPub (Script KeyHash)
        -- ^ Our staking credential
    -> Maybe Coin
       -- ^ Deposit
    -> VotingAction
        -- ^ Voting action in Conway era onwards
    -> [Cardano.Certificate (Write.CardanoApiEra era)]
        -- ^ Certificates representing the voting action
certificateFromVotingAction Write.RecentEraBabbage _cred _depositM _va  =
    []
certificateFromVotingAction Write.RecentEraConway cred depositM va=
    case (va, depositM) of
        (Vote action,_) ->
            [ Cardano.makeStakeAddressDelegationCertificate
                $ Cardano.StakeDelegationRequirementsConwayOnwards
                    conwayWitness
                    (toCardanoStakeCredential cred)
                    (toLedgerDelegatee Nothing (Just action))
            ]
        (VoteRegisteringKey action, Just deposit) ->
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
        (VoteRegisteringKey _, Nothing) ->
           error "certificateFromVotingAction: deposit value required in \
                 \Conway era when registration is carried out"
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
    -> Maybe DRep
    -> Ledger.Delegatee Ledger.StandardCrypto
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
    :: DRep -> Ledger.DRep Ledger.StandardCrypto
toLedgerDRep = \case
    Abstain -> Ledger.DRepAlwaysAbstain
    NoConfidence -> Ledger.DRepAlwaysNoConfidence
    FromDRepID (DRepFromKeyHash (DRepKeyHash keyhash)) ->
        Ledger.DRepCredential . Ledger.KeyHashObj . Ledger.KeyHash . UnsafeHash $
        toShort keyhash
    FromDRepID (DRepFromScriptHash (DRepScriptHash scripthash)) ->
        Ledger.DRepCredential . Ledger.ScriptHashObj . SL.ScriptHash . UnsafeHash $
        toShort scripthash
