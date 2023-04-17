{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Wallet.Shelley.Network.Discriminant
    ( SomeNetworkDiscriminant (..)
    , networkIdVal
    ) where

import Prelude

import Cardano.Api.Shelley
    ( NetworkId )
import Cardano.Wallet.Read.NetworkId
    ( HasSNetworkId, NetworkDiscriminant (..), SNetworkId (..), fromSNat )
import Data.Proxy
    ( Proxy (..) )
import Data.Typeable
    ( Typeable )

import qualified Cardano.Api as Cardano

-- | Encapsulate a network discriminant and the necessary constraints it should
-- satisfy.
data SomeNetworkDiscriminant where
    SomeNetworkDiscriminant
        :: forall (n :: NetworkDiscriminant)
         . (HasSNetworkId n, Typeable n)
        => Proxy n
        -> SomeNetworkDiscriminant

deriving instance Show SomeNetworkDiscriminant

-- | Class to extract a @NetworkId@ from @NetworkDiscriminant@.
networkIdVal :: SNetworkId n -> NetworkId
networkIdVal SMainnet = Cardano.Mainnet
networkIdVal (STestnet snat) = Cardano.Testnet networkMagic
      where
        networkMagic =
            Cardano.NetworkMagic . fromIntegral $ fromSNat snat
