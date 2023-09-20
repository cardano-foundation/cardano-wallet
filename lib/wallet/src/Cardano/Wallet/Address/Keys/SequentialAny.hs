{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.Address.Keys.SequentialAny
    ( mkSeqAnyState
    , mkSeqStateFromRootXPrv
    )
where

import Prelude

import Cardano.Wallet.Address.Derivation
    ( Depth (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index (..)
    )
import Cardano.Wallet.Address.Derivation.MintBurn
    ( derivePolicyPrivateKey
    )
import Cardano.Wallet.Address.Derivation.SharedKey
    ( SharedKey (..)
    )
import Cardano.Wallet.Address.Discovery.Sequential
    ( AddressPoolGap
    , SeqState
    , SupportsDiscovery
    , mkSeqStateFromAccountXPub
    )
import Cardano.Wallet.Flavor
    ( Excluding
    , KeyFlavorS
    )
import GHC.TypeLits
    ( Nat
    )

import Cardano.Wallet.Address.Derivation.Byron
    ( ByronKey (..)
    )
import Cardano.Wallet.Address.Discovery.SequentialAny
    ( SeqAnyState (..)
    )
import Cardano.Wallet.Address.Keys.WalletKey
    ( getRawKey
    , liftRawKey
    , publicKey
    )
import Cardano.Wallet.Primitive.Types.Credentials
    ( ClearCredentials
    , RootCredentials (..)
    )

-- | Initialize the HD random address discovery state from a root key and RNG
-- seed.
--
-- The type parameter is expected to be a ratio of addresses we ought to simply
-- recognize as ours. It is expressed in per-myriad, so "1" means 0.01%,
-- "100" means 1% and 10000 means 100%.
mkSeqAnyState
    :: forall (p :: Nat) n k.
        ( SupportsDiscovery n k
        , Excluding '[SharedKey, ByronKey] k
        )
    => KeyFlavorS k
    -> ClearCredentials k
    -> Index 'Hardened 'PurposeK
    -> AddressPoolGap
    -> SeqAnyState n k p
mkSeqAnyState kF credentials purpose poolGap =
    SeqAnyState
        {innerState = mkSeqStateFromRootXPrv kF credentials purpose poolGap False}

-- | Construct a Sequential state for a wallet
-- from root private key and password.
mkSeqStateFromRootXPrv
    :: forall n k.
        ( SupportsDiscovery n k
        , Excluding '[ByronKey, SharedKey] k
        )
    => KeyFlavorS k
    -> ClearCredentials k
    -> Index 'Hardened 'PurposeK
    -> AddressPoolGap
    -> Bool
    -> SeqState n k
mkSeqStateFromRootXPrv kF (RootCredentials rootXPrv pwd) =
    mkSeqStateFromAccountXPub
        (publicKey kF $ deriveAccountPrivateKey pwd rootXPrv minBound)
            $ Just
            $ publicKey kF
            $ liftRawKey kF
            $ derivePolicyPrivateKey pwd (getRawKey kF rootXPrv) minBound
