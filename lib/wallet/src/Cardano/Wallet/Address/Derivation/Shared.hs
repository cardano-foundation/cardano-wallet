{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Implementation of address derivation for 'Shared' Keys.
module Cardano.Wallet.Address.Derivation.Shared
    ( -- * Types
      SharedKey (..)

      -- * Generation and derivation
    , generateKeyFromSeed
    , unsafeGenerateKeyFromSeed
    , purposeCIP1854
    ) where

import Prelude

import Cardano.Address.Derivation
    ( xpubPublicKey
    )
import Cardano.Crypto.Wallet
    ( XPrv
    , XPub
    , unXPub
    , xpub
    )
import Cardano.Mnemonic
    ( SomeMnemonic
    )
import Cardano.Wallet.Address.Derivation
    ( AddressParts (..)
    , Depth (..)
    , DerivationType (..)
    , ErrMkKeyFingerprint (..)
    , HardDerivation (..)
    , KeyFingerprint (..)
    , MkKeyFingerprint (..)
    , PersistPublicKey (..)
    , SoftDerivation (..)
    , fromHex
    , hex
    , toAddressParts
    )
import Cardano.Wallet.Address.Derivation.SharedKey
    ( SharedKey (..)
    , purposeCIP1854
    )
import Cardano.Wallet.Address.Derivation.Shelley
    ( deriveAccountPrivateKeyShelley
    , deriveAddressPrivateKeyShelley
    , deriveAddressPublicKeyShelley
    , unsafeGenerateKeyFromSeedShelley
    )
import Cardano.Wallet.Address.Discovery
    ( GetPurpose (..)
    )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    )
import Cardano.Wallet.Read.NetworkId
    ( NetworkDiscriminant
    )
import Control.Monad
    ( (<=<)
    )
import Crypto.Hash.Algorithms
    ( Blake2b_224 (..)
    )
import Crypto.Hash.IO
    ( HashAlgorithm (hashDigestSize)
    )
import Crypto.Hash.Utils
    ( blake2b224
    )
import Data.ByteString
    ( ByteString
    )
import Data.Proxy
    ( Proxy (..)
    )

import qualified Data.ByteString as BS

{-------------------------------------------------------------------------------
                            Sequential Derivation
-------------------------------------------------------------------------------}

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
    type AddressCredential SharedKey = 'CredFromScriptK

    deriveAccountPrivateKey pwd (SharedKey rootXPrv) ix =
        SharedKey $ deriveAccountPrivateKeyShelley purposeCIP1854 pwd rootXPrv ix

    deriveAddressPrivateKey pwd (SharedKey accXPrv) role ix =
        SharedKey $ deriveAddressPrivateKeyShelley pwd accXPrv role ix

instance SoftDerivation SharedKey where
    deriveAddressPublicKey (SharedKey accXPub) role ix =
        SharedKey $ deriveAddressPublicKeyShelley accXPub role ix

{-------------------------------------------------------------------------------
                         Relationship Key / Address
-------------------------------------------------------------------------------}

instance GetPurpose SharedKey where
    getPurpose = purposeCIP1854

{-------------------------------------------------------------------------------
                          Storing and retrieving keys
-------------------------------------------------------------------------------}

instance PersistPublicKey (SharedKey depth) where
    serializeXPub =
        hex . unXPub . getKey

    unsafeDeserializeXPub =
        either err SharedKey . (xpub <=< fromHex @ByteString)
      where
        err _ = error "unsafeDeserializeXPub: unable to deserialize SharedKey"

instance MkKeyFingerprint SharedKey Address where
    paymentKeyFingerprint addr =
        let AddressParts{..} = toAddressParts addr
            baseAddr = 0b00110000 -- scripthash; scripthash
            enterpriseAddr = 0b01110000 -- scripthash
            rewardAcct = 0b11110000 -- scripthash
        in  if addrType `elem` [baseAddr, enterpriseAddr, rewardAcct]
                then Right $ KeyFingerprint $ BS.take hashSize rest
                else Left $ ErrInvalidAddress addr (Proxy @SharedKey)

instance
    MkKeyFingerprint
        SharedKey
        ( Proxy (n :: NetworkDiscriminant)
        , SharedKey 'CredFromScriptK XPub
        )
    where
    paymentKeyFingerprint (_, paymentK) =
        Right $ KeyFingerprint $ blake2b224 $ xpubPublicKey $ getKey paymentK

{-------------------------------------------------------------------------------
                                 Internals
-------------------------------------------------------------------------------}

hashSize :: Int
hashSize = hashDigestSize Blake2b_224
