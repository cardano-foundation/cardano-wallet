{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Read.NetworkId
    ( NetworkDiscriminant (..)
    , NetworkDiscriminantVal
    , networkDiscriminantVal
    , NetworkDiscriminantBits
    , networkDiscriminantBits
    , NetworkDiscriminantCheck (..)
    , networkDescription
    )
where

import Prelude

import Data.Text
    ( Text )
import Data.Typeable
    ( Proxy (..), Typeable )
import Data.Word
    ( Word8 )
import GHC.TypeLits
    ( KnownNat, Nat, natVal )

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
