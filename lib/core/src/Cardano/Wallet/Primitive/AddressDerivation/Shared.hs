{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Implementation of address derivation for 'Shared' Keys.

module Cardano.Wallet.Primitive.AddressDerivation.Shared
    ( -- * Types
      SharedKey(..)

    -- * Generation and derivation
    , generateKeyFromSeed
    , unsafeGenerateKeyFromSeed

    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPrv, toXPub, unXPrv, unXPub, xPrvChangePass, xprv, xpub )
import Cardano.Mnemonic
    ( SomeMnemonic )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Passphrase (..)
    , PersistPrivateKey (..)
    , PersistPublicKey (..)
    , SoftDerivation (..)
    , WalletKey (..)
    , fromHex
    , hex
    )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( deriveAccountPrivateKeyShelley
    , deriveAddressPrivateKeyShelley
    , deriveAddressPublicKeyShelley
    , unsafeGenerateKeyFromSeedShelley
    )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( GetPurpose (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.SharedState
    ( purposeCIP1854 )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( (<=<) )
import Crypto.Hash
    ( hash )
import Data.ByteString
    ( ByteString )
import GHC.Generics
    ( Generic )

{-------------------------------------------------------------------------------
                            Sequential Derivation
-------------------------------------------------------------------------------}

-- | A cryptographic key for Shared address derivation, with phantom-types to
-- disambiguate derivation paths
--
-- @
-- let rootPrivateKey = SharedKey 'RootK XPrv
-- let accountPubKey = SharedKey 'AccountK XPub
-- let addressPubKey = SharedKey 'AddressK XPub
-- @
newtype SharedKey (depth :: Depth) key =
    SharedKey { getKey :: key }
    deriving stock (Generic, Show, Eq)

instance (NFData key) => NFData (SharedKey depth key)

-- | Generate a root key from a corresponding seed.
-- The seed should be at least 16 bytes.
generateKeyFromSeed
    :: (SomeMnemonic, Maybe SomeMnemonic)
       -- ^ The actual seed and its recovery / generation passphrase
    -> Passphrase "encryption"
    -> SharedKey 'RootK XPrv
generateKeyFromSeed = unsafeGenerateKeyFromSeed

-- | Generate a new key from seed. Note that the @depth@ is left open so that
-- the caller gets to decide what type of key this is. This is mostly for
-- testing, in practice, seeds are used to represent root keys, and one should
-- use 'generateKeyFromSeed'.
unsafeGenerateKeyFromSeed
    :: (SomeMnemonic, Maybe SomeMnemonic)
        -- ^ The actual seed and its recovery / generation passphrase
    -> Passphrase "encryption"
    -> SharedKey depth XPrv
unsafeGenerateKeyFromSeed mnemonics pwd =
    SharedKey $ unsafeGenerateKeyFromSeedShelley mnemonics pwd

instance HardDerivation SharedKey where
    type AddressIndexDerivationType SharedKey = 'Soft

    deriveAccountPrivateKey pwd (SharedKey rootXPrv) ix =
        SharedKey $ deriveAccountPrivateKeyShelley purposeCIP1854 pwd rootXPrv ix

    deriveAddressPrivateKey pwd (SharedKey accXPrv) role ix =
        SharedKey $ deriveAddressPrivateKeyShelley pwd accXPrv role ix

instance SoftDerivation SharedKey where
    deriveAddressPublicKey (SharedKey accXPub) role ix =
        SharedKey $ deriveAddressPublicKeyShelley accXPub role ix

{-------------------------------------------------------------------------------
                            WalletKey implementation
-------------------------------------------------------------------------------}

instance WalletKey SharedKey where
    changePassphrase (Passphrase oldPwd) (Passphrase newPwd) (SharedKey prv) =
        SharedKey $ xPrvChangePass oldPwd newPwd prv

    publicKey (SharedKey prv) =
        SharedKey (toXPub prv)

    digest (SharedKey pub) =
        hash (unXPub pub)

    getRawKey =
        getKey

    liftRawKey =
        SharedKey

    keyTypeDescriptor _ =
        "shared"

{-------------------------------------------------------------------------------
                         Relationship Key / Address
-------------------------------------------------------------------------------}

instance GetPurpose SharedKey where
    getPurpose = purposeCIP1854

{-------------------------------------------------------------------------------
                          Storing and retrieving keys
-------------------------------------------------------------------------------}

instance PersistPrivateKey (SharedKey 'RootK) where
    serializeXPrv (k, h) =
        ( hex . unXPrv . getKey $ k
        , hex . getHash $ h
        )

    unsafeDeserializeXPrv (k, h) = either err id $ (,)
        <$> fmap SharedKey (xprvFromText k)
        <*> fmap Hash (fromHex h)
      where
        xprvFromText = xprv <=< fromHex @ByteString
        err _ = error "unsafeDeserializeXPrv: unable to deserialize SharedKey"

instance PersistPublicKey (SharedKey depth) where
    serializeXPub =
        hex . unXPub . getKey

    unsafeDeserializeXPub =
        either err SharedKey . (xpub <=< fromHex @ByteString)
      where
        err _ = error "unsafeDeserializeXPub: unable to deserialize SharedKey"
