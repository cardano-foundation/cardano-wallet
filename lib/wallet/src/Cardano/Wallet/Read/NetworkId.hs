{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
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
    , fromSNetworkId
    , withSNetworkId
    , networkIdVal
    )
where

import Prelude

import Data.Proxy
    ( Proxy (..)
    )
import Data.Text
    ( Text
    )
import Data.Typeable
    ( Typeable
    )
import Data.Word
    ( Word8
    )
import GHC.Natural
    ( Natural
    )
import GHC.TypeNats
    ( KnownNat
    , Nat
    , SomeNat (..)
    , natVal
    , someNatVal
    )

import qualified Cardano.Api as Cardano
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

deriving instance Show (SNat n)
deriving instance Eq (SNat n)

fromSNat :: SNat n -> Natural
fromSNat p@(SNat _) = natVal p

withSNat :: Natural -> (forall (n :: Nat). KnownNat n => SNat n -> a) -> a
withSNat nat f = case someNatVal nat of
    SomeNat proxy -> f (SNat proxy)

{-----------------------------------------------------------------------------
    singleton for NetworkDiscriminant
------------------------------------------------------------------------------}

-- | A singleton for 'NetworkDiscriminant'.
data SNetworkId (n :: NetworkDiscriminant) where
    SMainnet :: SNetworkId 'Mainnet
    STestnet :: SNat i -> SNetworkId ('Testnet i)

deriving instance Show (SNetworkId n)
deriving instance Eq (SNetworkId n)

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
networkDiscriminantVal (STestnet pm) =
    "testnet (" <> T.pack (show $ fromSNat pm) <> ")"

networkDiscriminantBits :: SNetworkId n -> Word8
networkDiscriminantBits SMainnet = 0b00000001
networkDiscriminantBits (STestnet _) = 0b00000000

-- | Extract a Cardano.NetworkId from NetworkDiscriminant singleton.
networkIdVal :: SNetworkId n -> Cardano.NetworkId
networkIdVal SMainnet = Cardano.Mainnet
networkIdVal (STestnet snat) = Cardano.Testnet networkMagic
  where
    networkMagic =
        Cardano.NetworkMagic . fromIntegral $ fromSNat snat

{-----------------------------------------------------------------------------
   conversions
------------------------------------------------------------------------------}

-- | Convert a 'NetworkDiscriminant' singleton to a 'NetworkId' value .
fromSNetworkId :: SNetworkId n -> NetworkId
fromSNetworkId SMainnet = NMainnet
fromSNetworkId (STestnet p) = NTestnet $ fromSNat p

-- | Run a function on a 'NetworkDiscriminant' singleton given a network id.
withSNetworkId
    :: NetworkId
    -> ( forall (n :: NetworkDiscriminant)
          . (Typeable n, HasSNetworkId n)
         => SNetworkId n
         -> a
       )
    -> a
withSNetworkId NMainnet f = f SMainnet
withSNetworkId (NTestnet i) f = withSNat i $ f . STestnet
