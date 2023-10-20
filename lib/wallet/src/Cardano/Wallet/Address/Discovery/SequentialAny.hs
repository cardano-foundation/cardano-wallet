{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- We intentionally specify the constraint  (k == SharedKey) ~ 'False
-- in some exports.

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- An implementation of address discovery for the sequential address derivation
-- scheme specified in BIP-0044.
--
-- The management of _accounts_ is left-out for this implementation focuses on
-- a single account. In practice, one wants to manage a set of pools, one per
-- account.

module Cardano.Wallet.Address.Discovery.SequentialAny
    (

    -- ** Benchmarking
    SeqAnyState (..)
    , Discoveries (..)
    , Prologue (..)
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPub
    )
import Cardano.Wallet.Address.Book
    ( AddressBookIso (..)
    )
import Cardano.Wallet.Address.Derivation
    ( Depth (..)
    , DerivationIndex (..)
    , HardDerivation (..)
    , Index (..)
    , KeyFingerprint (..)
    , PaymentAddress (..)
    , SoftDerivation (..)
    )
import Cardano.Wallet.Address.Derivation.Shared
    ( SharedKey
    )
import Cardano.Wallet.Address.Discovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , IsOurs (..)
    , KnownAddresses (..)
    )
import Cardano.Wallet.Address.Discovery.Sequential
    ( SeqAddressPool (..)
    , SeqState (..)
    , SupportsDiscovery
    )
import Cardano.Wallet.Address.States.Families
    ( CredFromOf
    , KeyOf
    , NetworkOf
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId (..)
    , NetworkDiscriminant
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount
    )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toLedger
    )
import Control.DeepSeq
    ( NFData (..)
    )
import Control.Lens
    ( iso
    , withIso
    )
import Data.Digest.CRC32
    ( crc32
    )
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Type.Equality
    ( type (==)
    )
import Data.Word
    ( Word32
    )
import GHC.Generics
    ( Generic
    )
import GHC.TypeLits
    ( KnownNat
    , Nat
    , natVal
    )

import qualified Cardano.Wallet.Address.Discovery.Sequential as Seq
import qualified Cardano.Wallet.Address.Pool as AddressPool
import qualified Internal.Cardano.Write.UTxOAssumptions as UTxOAssumptions

{-------------------------------------------------------------------------------
    SeqAnyState

    For benchmarking and testing arbitrary large sequential wallets.
-------------------------------------------------------------------------------}

-- | An "unsound" alternative that can be used for benchmarking and stress
-- testing. It re-uses the same underlying structure as the `SeqState` but
-- it discovers addresses based on an arbitrary ratio instead of respecting
-- BIP-44 discovery.
--
-- The proportion is stored as a type-level parameter so that we don't have to
-- alter the database schema to store it. It simply exists and depends on the
-- caller creating the wallet to define it.
newtype SeqAnyState (network :: NetworkDiscriminant) key (p :: Nat) =
    SeqAnyState { innerState :: SeqState network key }
    deriving (Generic)

deriving instance
    ( Show (k 'AccountK XPub)
    , Show (k 'CredFromKeyK XPub)
    , Show (k 'PolicyK XPub)
    , Show (KeyFingerprint "payment" k)
    )
    => Show (SeqAnyState n k p)

instance
    ( NFData (k 'AccountK XPub)
    , NFData (k 'CredFromKeyK XPub)
    , NFData (k 'PolicyK XPub)
    , NFData (KeyFingerprint "payment" k)
    )
    => NFData (SeqAnyState n k p)

instance KnownNat p => IsOurs (SeqAnyState n k p) Address where
    isOurs (Address bytes) st@(SeqAnyState inner)
        | crc32 bytes < p && correctAddressType =
            let
                pool = getPool $ externalPool inner
                ix = toEnum $ AddressPool.size pool - AddressPool.gap pool
                addr = AddressPool.addressFromIx pool ix
                pool' = AddressPool.update addr pool
                path = DerivationIndex (getIndex ix) :| []
            in
                ( Just path
                , SeqAnyState $ inner{externalPool = SeqAddressPool pool'}
                )
        | otherwise =
            (Nothing, st)
      where
        p = floor (double sup * double (natVal (Proxy @p)) / 10_000)
          where
            sup = maxBound :: Word32

        double :: Integral a => a -> Double
        double = fromIntegral

        correctAddressType =
            UTxOAssumptions.validateAddress
                UTxOAssumptions.AllKeyPaymentCredentials
                (toLedger $ Address bytes)

instance IsOurs (SeqAnyState n k p) RewardAccount where
    isOurs _account state = (Nothing, state)

instance
    ( SoftDerivation k
    , AddressCredential k ~ 'CredFromKeyK
    ) => GenChange (SeqAnyState n k p)
  where
    type ArgGenChange (SeqAnyState n k p) = ArgGenChange (SeqState n k)
    genChange a (SeqAnyState s) = SeqAnyState <$> genChange a s

instance SupportsDiscovery n k => CompareDiscovery (SeqAnyState n k p) where
    compareDiscovery (SeqAnyState s) = compareDiscovery s

instance (PaymentAddress k 'CredFromKeyK, HasSNetworkId n)
    => KnownAddresses (SeqAnyState n k p) where
    knownAddresses (SeqAnyState s) = knownAddresses s

-- piggy-back on SeqState existing instance, to simulate the same behavior.
instance ( (key == SharedKey) ~ 'False, Eq (SeqState n key))
    => AddressBookIso (SeqAnyState n key p)
  where
    data Prologue (SeqAnyState n key p) = PS (Prologue (Seq.SeqState n key))
    data Discoveries (SeqAnyState n key p) = DS (Discoveries (Seq.SeqState n key))

    addressIso = withIso addressIso $ \from to ->
        let from2 st = let (a,b) = from $ innerState st in (PS a, DS b)
            to2 (PS a, DS b) = SeqAnyState $ to (a,b)
        in  iso from2 to2

instance Eq (Seq.SeqState n k) => Eq (Prologue (SeqAnyState n k p)) where
    (PS a) == (PS b) = a == b

instance Eq (Seq.SeqState n k) => Eq (Discoveries (SeqAnyState n k p)) where
    (DS a) == (DS b) = a == b

type instance CredFromOf (SeqAnyState n key p) = 'CredFromKeyK
type instance KeyOf (SeqAnyState n k p) = k
type instance NetworkOf (SeqAnyState n k p) = n
