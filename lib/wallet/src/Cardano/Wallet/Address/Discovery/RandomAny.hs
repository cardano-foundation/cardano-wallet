
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- An implementation of address discovery for the random address
-- scheme as used by the legacy Cardano wallets.

module Cardano.Wallet.Address.Discovery.RandomAny
    (

    -- ** Benchmarking
    RndAnyState (..)
    , mkRndAnyState
    , Prologue (..)
    , Discoveries (..)
    ) where
import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Wallet.Address.Book
    ( AddressBookIso (..) )
import Cardano.Wallet.Address.Derivation
    ( Depth (..) )
import Cardano.Wallet.Address.Derivation.Byron
    ( ByronKey (..) )
import Cardano.Wallet.Address.Discovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , IsOurs (isOurs)
    , KnownAddresses (..)
    )
import Cardano.Wallet.Address.Discovery.Random
    ( RndState (..)
    , RndStateLike (..)
    , addDiscoveredAddress
    , findUnusedPath
    , toDerivationIndexes
    )
import Cardano.Wallet.Address.States.Families
    ( CredFromOf, KeyOf, NetworkOf )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId, NetworkDiscriminant )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..), AddressState (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toLedger )
import Control.Arrow
    ( second )
import Control.DeepSeq
    ( NFData (..) )
import Control.Lens
    ( iso, withIso )
import Data.Digest.CRC32
    ( crc32 )
import Data.Proxy
    ( Proxy (..) )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( KnownNat, Nat, natVal )
import System.Random
    ( mkStdGen )

import qualified Cardano.Wallet.Address.Discovery.Random as Rnd
import qualified Cardano.Write.UTxOAssumptions as UTxOAssumptions

--------------------------------------------------------------------------------
--
-- RndAnyState
--
-- For benchmarking and testing arbitrary large random wallets.

-- | An "unsound" alternative that can be used for benchmarking and stress
-- testing. It re-uses the same underlying structure as the `RndState` but
-- it discover addresses based on an arbitrary ratio instead of decrypting the
-- derivation path.
--
-- The proportion is stored as a type-level parameter so that we don't have to
-- alter the database schema to store it. It simply exists and depends on the
-- caller creating the wallet to define it.
newtype RndAnyState (network :: NetworkDiscriminant) (p :: Nat) = RndAnyState
    { innerState :: RndState network
    } deriving (Generic, Show)

instance NFData (RndAnyState n p)

-- | Initialize the HD random address discovery state from a root key and RNG
-- seed.
--
-- The type parameter is expected to be a ratio of addresses we ought to simply
-- recognize as ours. It is expressed in per-myriad, so "1" means 0.01%,
-- "100" means 1% and 10000 means 100%.
mkRndAnyState
    :: forall (p :: Nat) n. ()
    => ByronKey 'RootK XPrv
    -> Int
    -> RndAnyState n p
mkRndAnyState key seed = RndAnyState
    { innerState = RndState
        { hdPassphrase = payloadPassphrase key
        , accountIndex = minBound
        , discoveredAddresses = mempty
        , pendingAddresses = mempty
        , gen = mkStdGen seed
        }
    }

instance RndStateLike (RndAnyState n p) where
    importAddress addr (RndAnyState inner) =
        RndAnyState <$> importAddress addr inner

    addPendingAddress addr path (RndAnyState inner) =
        RndAnyState $ addPendingAddress addr path inner

    unavailablePaths (RndAnyState inner) =
        unavailablePaths inner

    defaultAccountIndex (RndAnyState inner) =
        defaultAccountIndex inner

    withRNG (RndAnyState inner) action =
        second RndAnyState $ withRNG inner action

instance KnownNat p => IsOurs (RndAnyState n p) Address where
    isOurs addr@(Address bytes) st@(RndAnyState inner) =
        case isOurs addr inner of
            (Just path, inner') ->
                (Just path, RndAnyState inner')

            (Nothing, _) | crc32 bytes < p && correctAddressType ->
                let
                    (path, gen') = findUnusedPath
                        (gen inner) (accountIndex inner) (unavailablePaths inner)

                    inner' = addDiscoveredAddress
                        addr Used path (inner { gen = gen' })
                in
                (Just (toDerivationIndexes path), RndAnyState inner')

            (Nothing, _) ->
                (Nothing, st)
      where
        p = floor (double (maxBound :: Word32) * double (natVal (Proxy @p)) / 10000)

        double :: Integral a => a -> Double
        double = fromIntegral

        correctAddressType =
            UTxOAssumptions.validateAddress
                UTxOAssumptions.AllByronKeyPaymentCredentials
                (toLedger $ Address bytes)

instance IsOurs (RndAnyState n p) RewardAccount where
    isOurs _account state = (Nothing, state)

instance HasSNetworkId n => GenChange (RndAnyState n p) where
    type ArgGenChange (RndAnyState n p) = ArgGenChange (RndState n)
    genChange a (RndAnyState s) = RndAnyState <$> genChange a s

instance CompareDiscovery (RndAnyState n p) where
    compareDiscovery (RndAnyState s) = compareDiscovery s

instance KnownAddresses (RndAnyState n p) where
    knownAddresses (RndAnyState s) = knownAddresses s

{-------------------------------------------------------------------------------
    HD Random address book
-------------------------------------------------------------------------------}
-- piggy-back on SeqState existing instance, to simulate the same behavior.
instance AddressBookIso (RndAnyState n p)
  where
    data Prologue (RndAnyState n p) = PR (Prologue (Rnd.RndState n))
    data Discoveries (RndAnyState n p) = DR (Discoveries (Rnd.RndState n))

    addressIso = withIso addressIso $ \from to ->
        let from2 st = let (a,b) = from $ innerState st in (PR a, DR b)
            to2 (PR a, DR b) = RndAnyState $ to (a,b)
        in  iso from2 to2

instance Eq (Prologue (RndAnyState n p)) where PR a == PR b = a == b

instance Eq (Discoveries (RndAnyState n p)) where DR a == DR b = a == b

type instance CredFromOf (RndAnyState n p) = 'CredFromKeyK
type instance KeyOf (RndAnyState n p) = ByronKey
type instance NetworkOf (RndAnyState n p) = n
