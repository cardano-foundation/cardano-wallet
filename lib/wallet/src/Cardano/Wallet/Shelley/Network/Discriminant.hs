{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Wallet.Shelley.Network.Discriminant
    ( SomeNetworkDiscriminant (..)
    , networkDiscriminantToId
    , discriminantNetwork
    , networkIdVal
    ) where

import Prelude

import Cardano.Api.Shelley
    ( NetworkId )
import Cardano.Wallet.Read.NetworkId
    ( HasSNetworkId (sNetworkId)
    , NetworkDiscriminant (..)
    , SNetworkId (..)
    , fromSNat
    )
import Control.Arrow
    ( (>>>) )
import Data.Proxy
    ( Proxy (..) )
import Data.Typeable
    ( Typeable )

import qualified Cardano.Api as Cardano
import qualified Cardano.Ledger.BaseTypes as Ledger

-- | Encapsulate a network discriminant and the necessary constraints it should
-- satisfy.
data SomeNetworkDiscriminant where
    SomeNetworkDiscriminant
        :: forall (n :: NetworkDiscriminant)
         . (HasSNetworkId n, Typeable n)
        => Proxy n
        -> SomeNetworkDiscriminant

deriving instance Show SomeNetworkDiscriminant

networkDiscriminantToId :: SomeNetworkDiscriminant -> NetworkId
networkDiscriminantToId (SomeNetworkDiscriminant (Proxy :: Proxy n))
    = networkIdVal (sNetworkId @n)

discriminantNetwork :: SomeNetworkDiscriminant -> Ledger.Network
discriminantNetwork = networkDiscriminantToId >>> \case
    Cardano.Mainnet -> Ledger.Mainnet
    Cardano.Testnet _magic -> Ledger.Testnet

-- | Class to extract a @NetworkId@ from @NetworkDiscriminant@.
networkIdVal :: SNetworkId n -> NetworkId
networkIdVal SMainnet = Cardano.Mainnet
networkIdVal (STestnet snat) = Cardano.Testnet networkMagic
      where
        networkMagic =
            Cardano.NetworkMagic . fromIntegral $ fromSNat snat
