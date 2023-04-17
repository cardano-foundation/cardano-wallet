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
    , withSNetworkId
    , networkIdVal
    ) where

import Prelude

import Cardano.Api.Shelley
    ( NetworkId )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DelegationAddress, Depth (..), PaymentAddress )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
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
        :: forall (n :: NetworkDiscriminant).
            ( PaymentAddress n IcarusKey 'CredFromKeyK
            , PaymentAddress n ByronKey 'CredFromKeyK
            , PaymentAddress n ShelleyKey 'CredFromKeyK
            , DelegationAddress n ShelleyKey 'CredFromKeyK
            , HasSNetworkId n
            , Typeable n
            )
        => Proxy n
        -> SomeNetworkDiscriminant

deriving instance Show SomeNetworkDiscriminant

networkDiscriminantToId :: SomeNetworkDiscriminant -> NetworkId
networkDiscriminantToId some = withSNetworkId some networkIdVal

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

withSNetworkId
    :: SomeNetworkDiscriminant
    -> (forall (n :: NetworkDiscriminant). HasSNetworkId n => SNetworkId n -> r)
    -> r
withSNetworkId (SomeNetworkDiscriminant (_proxy :: Proxy n)) f = f $ sNetworkId @n
