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
    , EncodeAddress (..)
    , EncodeStakeAddress (..)
    , DecodeAddress (..)
    , DecodeStakeAddress (..)
    , HasNetworkId (..)
    , withSNetworkId
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
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Read.NetworkId
    ( HasSNetworkId (sNetworkId), NetworkDiscriminant (..), SNetworkId )
import Control.Arrow
    ( (>>>) )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( TextDecodingError )
import Data.Typeable
    ( Typeable )
import GHC.TypeLits
    ( KnownNat, natVal )

import qualified Cardano.Api as Cardano
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Wallet.Primitive.Types.RewardAccount as W

-- | Encapsulate a network discriminant and the necessary constraints it should
-- satisfy.
data SomeNetworkDiscriminant where
    SomeNetworkDiscriminant
        :: forall (n :: NetworkDiscriminant).
            ( PaymentAddress n IcarusKey 'CredFromKeyK
            , PaymentAddress n ByronKey 'CredFromKeyK
            , PaymentAddress n ShelleyKey 'CredFromKeyK
            , EncodeAddress n
            , DecodeAddress n
            , EncodeStakeAddress n
            , DecodeStakeAddress n
            , DelegationAddress n ShelleyKey 'CredFromKeyK
            , HasNetworkId n
            , HasSNetworkId n
            , Typeable n
            )
        => Proxy n
        -> SomeNetworkDiscriminant

deriving instance Show SomeNetworkDiscriminant

-- | An abstract class to allow encoding of addresses depending on the target
-- backend used.
class EncodeAddress (n :: NetworkDiscriminant) where
    encodeAddress :: Address -> Text

-- | An abstract class to allow decoding of addresses depending on the target
-- backend used.
class DecodeAddress (n :: NetworkDiscriminant) where
    decodeAddress :: Text -> Either TextDecodingError Address

class EncodeStakeAddress (n :: NetworkDiscriminant) where
    encodeStakeAddress :: W.RewardAccount -> Text

class DecodeStakeAddress (n :: NetworkDiscriminant) where
    decodeStakeAddress :: Text -> Either TextDecodingError W.RewardAccount

networkDiscriminantToId :: SomeNetworkDiscriminant -> NetworkId
networkDiscriminantToId (SomeNetworkDiscriminant proxy) = networkIdVal proxy

discriminantNetwork :: SomeNetworkDiscriminant -> Ledger.Network
discriminantNetwork = networkDiscriminantToId >>> \case
    Cardano.Mainnet -> Ledger.Mainnet
    Cardano.Testnet _magic -> Ledger.Testnet

-- | Class to extract a @NetworkId@ from @NetworkDiscriminant@.
class HasNetworkId (n :: NetworkDiscriminant) where
    networkIdVal :: Proxy n -> NetworkId

instance HasNetworkId 'Mainnet where
    networkIdVal _ = Cardano.Mainnet

instance KnownNat protocolMagic => HasNetworkId ('Testnet protocolMagic) where
    networkIdVal _ = Cardano.Testnet networkMagic
      where
        networkMagic =
            Cardano.NetworkMagic . fromIntegral . natVal $ Proxy @protocolMagic

withSNetworkId
    :: SomeNetworkDiscriminant
    -> (forall (n :: NetworkDiscriminant). HasSNetworkId n => SNetworkId n -> r)
    -> r
withSNetworkId (SomeNetworkDiscriminant (_proxy :: Proxy n)) f = f $ sNetworkId @n
