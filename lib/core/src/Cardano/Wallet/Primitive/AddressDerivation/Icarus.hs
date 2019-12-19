{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Implementation of address derivation for 'Icarus' keys. This uses the Byron
-- derivation for addresses, but on top of the derivation scheme V2.

module Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( -- * Types
      IcarusKey(..)

    -- * Generation and derivation
    , generateKeyFromSeed
    , unsafeGenerateKeyFromSeed
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( DerivationScheme (..)
    , XPrv
    , XPub
    , deriveXPrv
    , deriveXPub
    , generateNew
    , toXPub
    , unXPrv
    , unXPub
    , xPrvChangePass
    , xprv
    , xpub
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , ErrMkKeyFingerprint (..)
    , HardDerivation (..)
    , Index (..)
    , KeyFingerprint (..)
    , MkKeyFingerprint (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PaymentAddress (..)
    , PersistPrivateKey (..)
    , PersistPublicKey (..)
    , SoftDerivation (..)
    , WalletKey (..)
    , fromHex
    , hex
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( decodeLegacyAddress )
import Cardano.Wallet.Primitive.Types
    ( Address (..), Hash (..), invariant )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( (<=<) )
import Crypto.Hash
    ( hash )
import Data.Bifunctor
    ( bimap )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )

import qualified Cardano.Byron.Codec.Cbor as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

-- | A cryptographic key for sequential-scheme address derivation, with
-- phantom-types to disambiguate key types.
--
-- @
-- let rootPrivateKey = IcarusKey 'RootK XPrv
-- let accountPubKey = IcarusKey 'AccountK XPub
-- let addressPubKey = IcarusKey 'AddressK XPub
-- @
newtype IcarusKey (depth :: Depth) key =
    IcarusKey { getKey :: key }
    deriving stock (Generic, Show, Eq)

instance (NFData key) => NFData (IcarusKey depth key)

-- | Purpose is a constant set to 44' (or 0x8000002C) following the original
-- BIP-44 specification.
--
-- It indicates that the subtree of this node is used according to this
-- specification.
--
-- Hardened derivation is used at this level.
purposeIndex :: Word32
purposeIndex = 0x8000002C

-- | One master node (seed) can be used for unlimited number of independent
-- cryptocoins such as Bitcoin, Litecoin or Namecoin. However, sharing the
-- same space for various cryptocoins has some disadvantages.
--
-- This level creates a separate subtree for every cryptocoin, avoiding reusing
-- addresses across cryptocoins and improving privacy issues.
--
-- Coin type is a constant, set for each cryptocoin. For Cardano this constant
-- is set to 1815' (or 0x80000717). 1815 is the birthyear of our beloved Ada
-- Lovelace.
--
-- Hardened derivation is used at this level.
coinTypeIndex :: Word32
coinTypeIndex = 0x80000717

-- | The minimum seed length for 'generateKeyFromSeed' and 'unsafeGenerateKeyFromSeed'.
minSeedLengthBytes :: Int
minSeedLengthBytes = 16

{-------------------------------------------------------------------------------
                               Key Generation
-------------------------------------------------------------------------------}

-- | Generate a root key from a corresponding seed.
-- The seed should be at least 16 bytes.
generateKeyFromSeed
    :: Passphrase "seed"
        -- ^ The actual seed
    -> Passphrase "encryption"
        -- ^ Master encryption passphrase
    -> IcarusKey 'RootK XPrv
generateKeyFromSeed = unsafeGenerateKeyFromSeed

-- | Generate a new key from seed. Note that the @depth@ is left open so that
-- the caller gets to decide what type of key this is. This is mostly for
-- testing, in practice, seeds are used to represent root keys, and one should
-- use 'generateKeyFromSeed'.
unsafeGenerateKeyFromSeed
    :: Passphrase "seed"
        -- ^ The actual seed
    -> Passphrase "encryption"
        -- ^ Master encryption passphrase
    -> IcarusKey depth XPrv
unsafeGenerateKeyFromSeed (Passphrase seed) (Passphrase pwd) =
    let
        seed' = invariant
            ("seed length : "
                <> show (BA.length seed)
                <> " in (Passphrase \"seed\") is not valid"
            )
            seed
            (\s -> BA.length s >= minSeedLengthBytes && BA.length s <= 255)
    in IcarusKey $ generateNew seed' (mempty :: ByteString) pwd

{-------------------------------------------------------------------------------
                          Hard / Soft Key Derivation
-------------------------------------------------------------------------------}

instance HardDerivation IcarusKey where
    type AddressIndexDerivationType IcarusKey = 'Soft

    deriveAccountPrivateKey
            (Passphrase pwd) (IcarusKey rootXPrv) (Index accIx) =
        let
            purposeXPrv = -- lvl1 derivation; hardened derivation of purpose'
                deriveXPrv DerivationScheme2 pwd rootXPrv purposeIndex
            coinTypeXPrv = -- lvl2 derivation; hardened derivation of coin_type'
                deriveXPrv DerivationScheme2 pwd purposeXPrv coinTypeIndex
            acctXPrv = -- lvl3 derivation; hardened derivation of account' index
                deriveXPrv DerivationScheme2 pwd coinTypeXPrv accIx
        in
            IcarusKey acctXPrv

    deriveAddressPrivateKey
            (Passphrase pwd) (IcarusKey accXPrv) accountingStyle (Index addrIx) =
        let
            changeCode =
                fromIntegral $ fromEnum accountingStyle
            changeXPrv = -- lvl4 derivation; soft derivation of change chain
                deriveXPrv DerivationScheme2 pwd accXPrv changeCode
            addrXPrv = -- lvl5 derivation; soft derivation of address index
                deriveXPrv DerivationScheme2 pwd changeXPrv addrIx
        in
            IcarusKey addrXPrv

instance SoftDerivation IcarusKey where
    deriveAddressPublicKey (IcarusKey accXPub) accountingStyle (Index addrIx) =
        fromMaybe errWrongIndex $ do
            let changeCode = fromIntegral $ fromEnum accountingStyle
            changeXPub <- -- lvl4 derivation in bip44 is derivation of change chain
                deriveXPub DerivationScheme2 accXPub changeCode
            addrXPub <- -- lvl5 derivation in bip44 is derivation of address chain
                deriveXPub DerivationScheme2 changeXPub addrIx
            return $ IcarusKey addrXPub
      where
        errWrongIndex = error $
            "deriveAddressPublicKey failed: was given an hardened (or too big) \
            \index for soft path derivation ( " ++ show addrIx ++ "). This is \
            \either a programmer error, or, we may have reached the maximum \
            \number of addresses for a given wallet."

{-------------------------------------------------------------------------------
                            WalletKey implementation
-------------------------------------------------------------------------------}

instance WalletKey IcarusKey where
    keyTypeDescriptor _ = "ica"

    changePassphrase (Passphrase old) (Passphrase new) (IcarusKey prv) =
        IcarusKey $ xPrvChangePass old new prv

    publicKey (IcarusKey prv) =
        IcarusKey (toXPub prv)

    digest (IcarusKey prv) =
        hash (unXPub prv)

    getRawKey =
        getKey

    dummyKey =
        let Right pub = xpub (BS.replicate 64 0) in IcarusKey pub

{-------------------------------------------------------------------------------
                         Relationship Key / Address
-------------------------------------------------------------------------------}

instance PaymentAddress 'Mainnet IcarusKey where
    paymentAddress k = Address
        $ CBOR.toStrictByteString
        $ CBOR.encodeAddress (getKey k) []
    liftPaymentAddress (KeyFingerprint bytes) =
        Address bytes

instance MkKeyFingerprint IcarusKey Address where
    paymentKeyFingerprint addr@(Address bytes) =
        case decodeLegacyAddress bytes of
            Just _  -> Right $ KeyFingerprint bytes
            Nothing -> Left $ ErrInvalidAddress addr (Proxy @IcarusKey)

instance MkKeyFingerprint IcarusKey (IcarusKey 'AddressK XPub) where
    paymentKeyFingerprint k =
        bimap (const err) coerce
        . paymentKeyFingerprint @IcarusKey
        . paymentAddress @'Mainnet
        $ k
      where
        err = ErrInvalidAddress k Proxy

{-------------------------------------------------------------------------------
                          Storing and retrieving keys
-------------------------------------------------------------------------------}

instance PersistPrivateKey (IcarusKey 'RootK) where
    serializeXPrv (k, h) =
        ( hex . unXPrv . getKey $ k
        , hex . getHash $ h
        )

    unsafeDeserializeXPrv (k, h) = either err id $ (,)
        <$> fmap IcarusKey (xprvFromText k)
        <*> fmap Hash (fromHex h)
      where
        xprvFromText = xprv <=< fromHex @ByteString
        err _ = error "unsafeDeserializeXPrv: unable to deserialize IcarusKey"

instance PersistPublicKey (IcarusKey depth) where
    serializeXPub =
        hex . unXPub . getKey

    unsafeDeserializeXPub =
        either err IcarusKey . xpubFromText
      where
        xpubFromText = xpub <=< fromHex @ByteString
        err _ = error "unsafeDeserializeXPub: unable to deserialize IcarusKey"
