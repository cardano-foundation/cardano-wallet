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
    , networkDiscriminantVal
    , networkDiscriminantBits
    , NetworkDiscriminantCheck (..)
    , SNetworkId (..)
    , HasSNetworkId (..)
    , NetworkId (..)
    , fromSNat
    , toSNat
    , fromSNetworkId
    , toSNetworkId
    , sNetworkIdOfProxy
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

class NetworkDiscriminantCheck k where
    networkDiscriminantCheck :: SNetworkId n -> Word8 -> Bool

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
   Type level network discrimination
------------------------------------------------------------------------------}

networkDiscriminantVal :: SNetworkId n -> Text
networkDiscriminantVal SMainnet = "mainnet"
networkDiscriminantVal (STestnet pm)
    = "testnet (" <> T.pack (show $ fromSNat pm) <> ")"

networkDiscriminantBits :: SNetworkId n -> Word8
networkDiscriminantBits SMainnet = 0b00000001
networkDiscriminantBits (STestnet _) = 0b00000000

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

sNetworkIdOfProxy :: forall n. HasSNetworkId n => Proxy n -> SNetworkId n
sNetworkIdOfProxy Proxy = sNetworkId @n
