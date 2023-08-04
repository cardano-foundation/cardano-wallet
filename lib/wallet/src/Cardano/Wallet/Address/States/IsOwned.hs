{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Cardano.Wallet.Address.States.IsOwned
  ( isOwned
  )
where

import Cardano.Crypto.Wallet
  ( XPrv
  )
import Cardano.Wallet.Address.Derivation
  ( Depth (..)
  )
import Cardano.Wallet.Address.Derivation.Shared
  (
  )
import Cardano.Wallet.Address.Discovery.Random qualified as Rnd
import Cardano.Wallet.Address.Discovery.Sequential qualified as Seq
import Cardano.Wallet.Address.Discovery.Shared qualified as Sha
import Cardano.Wallet.Address.States.Families
  ( CredFromOf
  , KeyOf
  , NetworkOf
  )
import Cardano.Wallet.Address.States.Features
  ( TestFeatures (isOwnedTest)
  )
import Cardano.Wallet.Flavor
  ( WalletFlavorS (..)
  )
import Cardano.Wallet.Primitive.NetworkId
  ( HasSNetworkId
  )
import Cardano.Wallet.Primitive.Passphrase.Types
  ( Passphrase (..)
  )
import Cardano.Wallet.Primitive.Types.Address
  ( Address (..)
  )
import Prelude

-- | More powerful than 'isOurs', this abstractions offer the underlying state
-- the ability to find / compute the address private key corresponding to a
-- given known address.
--
-- Requiring 'IsOwned' as a constraint supposed that there is a way to recover
-- the root private key of a particular wallet. This isn't true for externally
-- owned wallet which would delegate its key management to a third party (like
-- a hardware Ledger or Trezor).
--
-- Derive the private key corresponding to an address. Careful, this
-- operation can be costly. Note that the state is discarded from this
-- function as we do not intend to discover any addresses from this
-- operation; This is merely a lookup from known addresses.
isOwned
  :: forall s
   . HasSNetworkId (NetworkOf s)
  => WalletFlavorS s
  -> s
  -> (KeyOf s 'RootK XPrv, Passphrase "encryption")
  -> Address
  -> Maybe (KeyOf s (CredFromOf s) XPrv, Passphrase "encryption")
isOwned ByronWallet = Rnd.isOwned
isOwned IcarusWallet = Seq.isOwned
isOwned ShelleyWallet = Seq.isOwned
isOwned SharedWallet = Sha.isOwned
isOwned BenchByronWallet = \_ _ _ -> Nothing
isOwned BenchShelleyWallet = \_ _ _ -> Nothing
isOwned (TestStateS fs) = isOwnedTest fs
