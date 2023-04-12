{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Wallet.Read.NetworkId
    ( NetworkDiscriminant (..)
    , NetworkDiscriminantVal
    , networkDiscriminantVal
    , NetworkDiscriminantBits
    , networkDiscriminantBits
    , NetworkDiscriminantCheck (..)
    , networkDescription
    , SNetworkId (..)
    , HasSNetworkId (..)
    , fromSNat
    , toSNat
    , fromSNetworkId
    , toSNetworkId
    )
where

import Prelude

import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Typeable
    ( Typeable )
import Data.Word
    ( Word8 )
import GHC.Natural
    ( Natural )
import GHC.TypeNats
    ( KnownNat, Nat, SomeNat (..), natVal, someNatVal )

import qualified Data.Text as T

{-------------------------------------------------------------------------------
                             Network Discrimination
-------------------------------------------------------------------------------}

-- | Available network options.
--
-- - @Mainnet@: is a shortcut for quickly pointing to mainnet. On Byron and
--              Shelley, it assumes no discrimination. It has a known magic and
--              known genesis parameters.
--
-- - @Testnet@: can be used to identify any network that has a custom genesis
--              and, that requires _explicit_ network discrimination in
--              addresses. Genesis file needs to be passed explicitly when
--              starting the application.
--
data NetworkDiscriminant = Mainnet | Testnet Nat
    deriving (Typeable)

class NetworkDiscriminantVal (n :: NetworkDiscriminant) where
    networkDiscriminantVal :: Text

instance NetworkDiscriminantVal 'Mainnet where
    networkDiscriminantVal =
        "mainnet"

instance KnownNat pm => NetworkDiscriminantVal ('Testnet pm) where
    networkDiscriminantVal =
        "testnet (" <> T.pack (show $ natVal $ Proxy @pm) <> ")"

class NetworkDiscriminantBits (n :: NetworkDiscriminant) where
    networkDiscriminantBits :: Word8

instance NetworkDiscriminantBits 'Mainnet where
    networkDiscriminantBits = 0b00000001

instance NetworkDiscriminantBits ('Testnet pm) where
    networkDiscriminantBits = 0b00000000

class NetworkDiscriminantCheck (n :: NetworkDiscriminant) k where
    networkDiscriminantCheck :: Word8 -> Bool

-- | Helper function that can be called without @AllowAmbiguousTypes@.
networkDescription
    :: forall n. NetworkDiscriminantVal n => Proxy n -> Text
networkDescription _ = networkDiscriminantVal @n

{-----------------------------------------------------------------------------
    Value level network discrimination
------------------------------------------------------------------------------}

data NetworkId
    = NMainnet
    | NTestnet Natural
    deriving (Eq, Show)

data SNat (n :: Nat) where
    SNat :: KnownNat n => Proxy n -> SNat n

fromSNat :: SNat n -> Natural
fromSNat p@(SNat _) = natVal p

toSNat :: Natural -> (forall (n :: Nat). SNat n -> a) -> a
toSNat nat f = case someNatVal nat of
    SomeNat proxy -> f (SNat proxy)

{-----------------------------------------------------------------------------
    singleton for NetworkDiscriminant
------------------------------------------------------------------------------}

-- | A singleton for 'NetworkDiscriminant'.
data SNetworkId (n :: NetworkDiscriminant) where
    SMainnet :: SNetworkId 'Mainnet
    STestnet :: SNat i -> SNetworkId ('Testnet i)

-- | A class for extracting the singleton for 'NetworkDiscriminant'.
class HasSNetworkId n where
    sNetworkId :: SNetworkId n

instance HasSNetworkId 'Mainnet where
    sNetworkId = SMainnet

instance KnownNat i => HasSNetworkId ('Testnet i) where
    sNetworkId = STestnet $ SNat Proxy

{-----------------------------------------------------------------------------
   conversions
------------------------------------------------------------------------------}

-- | Convert a 'NetworkDiscriminant' singleton to a 'NetworkId' value .
fromSNetworkId :: SNetworkId n -> NetworkId
fromSNetworkId SMainnet = NMainnet
fromSNetworkId (STestnet p) = NTestnet $ fromSNat p

-- | Run a function on a 'NetworkDiscriminant' singleton given a network id.
toSNetworkId
    :: NetworkId
    -> (forall (n :: NetworkDiscriminant). SNetworkId n -> a)
    -> a
toSNetworkId NMainnet f = f SMainnet
toSNetworkId (NTestnet i) f = toSNat i $  f . STestnet
