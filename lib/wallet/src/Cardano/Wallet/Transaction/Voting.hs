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
import Cardano.Wallet.Transaction
    ( VotingAction (..)
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
certificateFromVotingAction
    :: RecentEra era
        -- ^ Era in which we create the certificate
    -> Either XPub (Script KeyHash)
        -- ^ Our staking credential
    -> Maybe Coin
       -- ^ Deposit
    -> VotingAction
        -- ^ Voting action in Conway era onwards
    -> [Cardano.Certificate (CardanoApiEra era)]
        -- ^ Certificates representing the voting action
certificateFromVotingAction RecentEraBabbage _cred _depositM _va  =
    []
certificateFromVotingAction RecentEraConway cred depositM va=
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
