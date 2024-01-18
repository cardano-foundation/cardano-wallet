-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Tools for creating transactions that change the delegation status.
--
module Cardano.Wallet.Transaction.Delegation
    ( toStakePoolDlgCert
    , toStakeKeyRegCert
    , toStakeKeyDeregCert
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
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( toCardanoSimpleScript
    )
import Cardano.Wallet.Primitive.Types.Pool
    ( PoolId (..)
    , PoolOwner (..)
    )
import Crypto.Hash.Extra
    ( blake2b224
    )
import Data.ByteString.Short
    ( toShort
    )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano

{-----------------------------------------------------------------------------
    Cardano.Certificate
------------------------------------------------------------------------------}

toStakePoolDlgCert
    :: Either XPub (Script KeyHash) -> PoolId -> Cardano.Certificate
toStakePoolDlgCert cred (PoolId pid) = case cred of
    Left xpub ->
        Cardano.makeStakeAddressDelegationCertificate
        (Cardano.StakeCredentialByKey $ Cardano.StakeKeyHash (toKeyHash xpub))
        (Cardano.StakePoolKeyHash pool)
    Right script ->
        Cardano.makeStakeAddressDelegationCertificate
        (Cardano.StakeCredentialByScript
        . Cardano.hashScript
        . Cardano.SimpleScript
        $ toCardanoSimpleScript script)
        (Cardano.StakePoolKeyHash pool)
  where
    toKeyHash = SL.KeyHash . UnsafeHash . toShort . blake2b224 . xpubPublicKey
    pool = SL.KeyHash $ UnsafeHash $ toShort pid

toStakeKeyRegCert :: Either XPub (Script KeyHash) -> Cardano.Certificate
toStakeKeyRegCert cred = case cred of
    Left xpub ->
        Cardano.makeStakeAddressRegistrationCertificate
        . Cardano.StakeCredentialByKey
        . Cardano.StakeKeyHash
        . SL.KeyHash
        . UnsafeHash
        . toShort
        . blake2b224
        $ xpubPublicKey xpub
    Right script ->
        Cardano.makeStakeAddressRegistrationCertificate
        . Cardano.StakeCredentialByScript
        . Cardano.hashScript
        . Cardano.SimpleScript
        $ toCardanoSimpleScript script

toStakeKeyDeregCert :: Either XPub (Script KeyHash) -> Cardano.Certificate
toStakeKeyDeregCert = \case
    Left xpub ->
        Cardano.makeStakeAddressUnregistrationCertificate
        . Cardano.StakeCredentialByKey
        . Cardano.StakeKeyHash
        . SL.KeyHash
        . UnsafeHash
        . toShort
        . blake2b224
        $ xpubPublicKey xpub
    Right script ->
        Cardano.makeStakeAddressUnregistrationCertificate
        . Cardano.StakeCredentialByScript
        . Cardano.hashScript
        . Cardano.SimpleScript
        $ toCardanoSimpleScript script
