{-# LANGUAGE DataKinds #-}

-- | Module containing 'KeyStore'.
--
-- Under construction (ADP-2675).
module Internal.Cardano.Write.Tx.Sign.KeyStore
    (
      -- * KeyStore
      KeyStore (..)
    , keyStoreFromKeyHashLookup
    , keyStoreFromAddressLookup
    , keyStoreFromBootstrapAddressLookup
    , keyStoreFromMaybeXPrv
    , keyStoreFromXPrv

      -- * KeyHash
    , KeyHash'
    , keyHashFromBytes
    , keyHashFromXPrv
    , keyHashToBytes

    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPrv
    , toXPub
    , xpubPublicKey
    )
import Cardano.Ledger.Api
    ( Addr
    , BootstrapAddress
    , KeyRole (Witness)
    , StandardCrypto
    )
import Cardano.Ledger.Shelley.API
    ( KeyHash (..)
    )
import Control.Applicative
    ( (<|>)
    )
import Crypto.Hash.Extra
    ( blake2b224
    )
import Data.ByteString
    ( ByteString
    )
import Data.Maybe
    ( fromMaybe
    )

import qualified Cardano.Crypto.Hash as Hash

--------------------------------------------------------------------------------
-- KeyStore
--------------------------------------------------------------------------------

-- | 'KeyStore' allow wallets and other users to encapsulate all their
-- signing-capabilities into a single value, which then can be used with a
-- function like:
--
-- @@
--     signTx
--         :: KeyStore
--         -> UTxO era
--         -> Tx era
--         -> Tx era
-- @@
--
data KeyStore = KeyStore
    { -- | Main workhorse of 'KeyStore'.
      resolveKeyHash
        :: KeyHash' -> Maybe XPrv

      -- | An additional way of resolving address keys based on 'Addr' instead
      -- of 'KeyHash'. This is less powerful than 'resolveKeyHash', but required
      -- for supporting 'cardano-wallet'.
    , resolveAddress
        :: Addr StandardCrypto -> Maybe XPrv

      -- | Allows resolving keys for bootstrap addresses, which is something
      -- 'resolveKeyHash' cannot do.
    , resolveBootstrapAddress
        :: BootstrapAddress StandardCrypto -> Maybe XPrv
    }

-- Inefficient; don't overuse
instance Semigroup KeyStore where
    (KeyStore f g h) <> (KeyStore f' g' h') =
        KeyStore (alt1 f f') (alt1 g g') (alt1 h h')
      where
        alt1 a b x = a x <|> b x

instance Monoid KeyStore where
    mempty = KeyStore (const Nothing) (const Nothing) (const Nothing)

keyStoreFromKeyHashLookup
    :: (KeyHash' -> Maybe XPrv)
    -> KeyStore
keyStoreFromKeyHashLookup f = mempty { resolveKeyHash = f }

keyStoreFromXPrv
    :: XPrv
    -> KeyStore
keyStoreFromXPrv xprv =
    let
        keyHash = keyHashFromXPrv xprv
    in
        keyStoreFromKeyHashLookup $ \h ->
            if h == keyHash then Just xprv else Nothing

keyStoreFromMaybeXPrv
    :: Maybe XPrv
    -> KeyStore
keyStoreFromMaybeXPrv = maybe mempty keyStoreFromXPrv

keyStoreFromAddressLookup
    :: (Addr StandardCrypto -> Maybe XPrv)
    -> KeyStore
keyStoreFromAddressLookup f = mempty { resolveAddress = f }

keyStoreFromBootstrapAddressLookup
    :: (BootstrapAddress StandardCrypto -> Maybe XPrv)
    -> KeyStore
keyStoreFromBootstrapAddressLookup f = mempty { resolveBootstrapAddress = f }

--------------------------------------------------------------------------------
-- KeyHash
--------------------------------------------------------------------------------

type KeyHash' = KeyHash 'Witness StandardCrypto

keyHashFromBytes :: ByteString -> Maybe KeyHash'
keyHashFromBytes = fmap KeyHash . Hash.hashFromBytes

keyHashToBytes :: KeyHash' -> ByteString
keyHashToBytes (KeyHash h) = Hash.hashToBytes h

keyHashFromXPrv :: XPrv -> KeyHash'
keyHashFromXPrv = fromMaybe (error "keyHashFromXPrv: impossible")
    . keyHashFromBytes
    . blake2b224
    . xpubPublicKey
    . toXPub
