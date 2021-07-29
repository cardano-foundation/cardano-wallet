{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Implementation of address derivation for 'Icarus' keys. This uses the Byron
-- derivation for addresses, but on top of the derivation scheme V2.

module Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( -- * Types
      IcarusKey(..)

    -- * Generation and derivation
    , generateKeyFromSeed
    , generateKeyFromHardwareLedger
    , unsafeGenerateKeyFromSeed
    , minSeedLengthBytes
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
import Cardano.Mnemonic
    ( SomeMnemonic (..), entropyToBytes, mnemonicToEntropy, mnemonicToText )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , ErrMkKeyFingerprint (..)
    , HardDerivation (..)
    , Index (..)
    , KeyFingerprint (..)
    , MkKeyFingerprint (..)
    , NetworkDiscriminant (..)
    , PaymentAddress (..)
    , PersistPrivateKey (..)
    , PersistPublicKey (..)
    , RewardAccount (..)
    , SoftDerivation (..)
    , WalletKey (..)
    , fromHex
    , hex
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( GetPurpose (..), IsOurs (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState, coinTypeAda, purposeBIP44 )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..), PassphraseHash (..), changePassphraseXPrv )
import Cardano.Wallet.Primitive.Types
    ( testnetMagic )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Util
    ( invariant )
import Control.Arrow
    ( first, left )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( (<=<) )
import Crypto.Error
    ( eitherCryptoError )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( SHA256 (..), SHA512 (..) )
import Crypto.MAC.HMAC
    ( HMAC, hmac )
import Data.Bifunctor
    ( bimap )
import Data.Bits
    ( clearBit, setBit, testBit )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Function
    ( (&) )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( KnownNat )

import qualified Cardano.Byron.Codec.Cbor as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Crypto.ECC.Edwards25519 as Ed25519
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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

-- | The minimum seed length for 'generateKeyFromSeed' and 'unsafeGenerateKeyFromSeed'.
minSeedLengthBytes :: Int
minSeedLengthBytes = 16

{-------------------------------------------------------------------------------
                               Key Generation
-------------------------------------------------------------------------------}

-- | Generate a root key from a corresponding seed.
-- The seed should be at least 16 bytes.
generateKeyFromSeed
    :: SomeMnemonic
        -- ^ The root mnemonic
    -> Passphrase "encryption"
        -- ^ Master encryption passphrase
    -> IcarusKey 'RootK XPrv
generateKeyFromSeed = unsafeGenerateKeyFromSeed

-- | Hardware Ledger devices generates keys from mnemonic using a different
-- approach (different from the rest of Cardano).
--
-- It is a combination of:
--
-- - [SLIP 0010](https://github.com/satoshilabs/slips/blob/master/slip-0010.md)
-- - [BIP 0032](https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki)
-- - [BIP 0039](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki)
-- - [RFC 8032](https://tools.ietf.org/html/rfc8032#section-5.1.5)
-- - What seems to be arbitrary changes from Ledger regarding the calculation of
--   the initial chain code and generation of the root private key.
generateKeyFromHardwareLedger
    :: SomeMnemonic
        -- ^ The root mnemonic
    -> Passphrase "encryption"
        -- ^ Master encryption passphrase
    -> IcarusKey 'RootK XPrv
generateKeyFromHardwareLedger (SomeMnemonic mw) (Passphrase pwd) = unsafeFromRight $ do
    let seed = pbkdf2HmacSha512
            $ T.encodeUtf8
            $ T.intercalate " "
            $ mnemonicToText mw

    -- NOTE
    -- SLIP-0010 refers to `iR` as the chain code. Here however, the chain code
    -- is obtained as a hash of the initial seed whereas iR is used to make part
    -- of the root private key itself.
    let cc = hmacSha256 (BS.pack [1] <> seed)
    let (iL, iR) = first pruneBuffer $ hashRepeatedly seed
    pA <- ed25519ScalarMult iL

    prv <- left show $ xprv $ iL <> iR <> pA <> cc
    pure $ IcarusKey (xPrvChangePass (mempty :: ByteString) pwd prv)
  where
    -- Errors yielded in the body of 'generateKeyFromHardwareLedger' are
    -- programmer errors (out-of-range byte buffer access or, invalid length for
    -- cryptographic operations). Therefore, we throw badly if we encounter any.
    unsafeFromRight :: Either String a -> a
    unsafeFromRight = either error id

    -- This is the algorithm described in SLIP 0010 for master key generation
    -- with an extra step to discard _some_ of the potential private keys. Why
    -- this extra step remains a mystery as of today.
    --
    --      1. Generate a seed byte sequence S of 512 bits according to BIP-0039.
    --         (done in a previous step, passed as argument).
    --
    --      2. Calculate I = HMAC-SHA512(Key = "ed25519 seed", Data = S)
    --
    --      3. Split I into two 32-byte sequences, IL and IR.
    --
    -- extra *******************************************************************
    -- *                                                                       *
    -- *    3.5 If the third highest bit of the last byte of IL is not zero    *
    -- *        S = I and go back to step 2.                                   *
    -- *                                                                       *
    -- *************************************************************************
    --
    --      4. Use parse256(IL) as master secret key, and IR as master chain code.
    hashRepeatedly :: ByteString -> (ByteString, ByteString)
    hashRepeatedly bytes = case BS.splitAt 32 (hmacSha512 bytes) of
        (iL, iR) | isInvalidKey iL -> hashRepeatedly (iL <> iR)
        (iL, iR) -> (iL, iR)
      where
        isInvalidKey k = testBit (k `BS.index` 31) 5

    -- - Clear the lowest 3 bits of the first byte
    -- - Clear the highest bit of the last byte
    -- - Set the second highest bit of the last byte
    --
    -- As described in [RFC 8032 - 5.1.5](https://tools.ietf.org/html/rfc8032#section-5.1.5)
    pruneBuffer :: ByteString -> ByteString
    pruneBuffer bytes =
        let
            (firstByte, rest) = fromMaybe (error "pruneBuffer: no first byte") $
                BS.uncons bytes

            (rest', lastByte) = fromMaybe (error "pruneBuffer: no last byte") $
                BS.unsnoc rest

            firstPruned = firstByte
                & (`clearBit` 0)
                & (`clearBit` 1)
                & (`clearBit` 2)

            lastPruned = lastByte
                & (`setBit` 6)
                & (`clearBit` 7)
        in
            (firstPruned `BS.cons` BS.snoc rest' lastPruned)

    ed25519ScalarMult :: ByteString -> Either String ByteString
    ed25519ScalarMult bytes = do
        scalar <- left show $ eitherCryptoError $ Ed25519.scalarDecodeLong bytes
        pure $ Ed25519.pointEncode $ Ed25519.toPoint scalar

    -- As described in [BIP 0039 - From Mnemonic to Seed](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki#from-mnemonic-to-seed)
    pbkdf2HmacSha512 :: ByteString -> ByteString
    pbkdf2HmacSha512 bytes = PBKDF2.generate
        (PBKDF2.prfHMAC SHA512)
        (PBKDF2.Parameters 2048 64)
        bytes
        ("mnemonic" :: ByteString)

    hmacSha256 :: ByteString -> ByteString
    hmacSha256 =
        BA.convert @(HMAC SHA256) . hmac salt

    -- As described in [SLIP 0010 - Master Key Generation](https://github.com/satoshilabs/slips/blob/master/slip-0010.md#master-key-generation)
    hmacSha512 :: ByteString -> ByteString
    hmacSha512 =
        BA.convert @(HMAC SHA512) . hmac salt

    salt :: ByteString
    salt = "ed25519 seed"

-- | Generate a new key from seed. Note that the @depth@ is left open so that
-- the caller gets to decide what type of key this is. This is mostly for
-- testing, in practice, seeds are used to represent root keys, and one should
-- use 'generateKeyFromSeed'.
unsafeGenerateKeyFromSeed
    :: SomeMnemonic
        -- ^ The root mnemonic
    -> Passphrase "encryption"
        -- ^ Master encryption passphrase
    -> IcarusKey depth XPrv
unsafeGenerateKeyFromSeed (SomeMnemonic mw) (Passphrase pwd) =
    let
        seed  = entropyToBytes $ mnemonicToEntropy mw
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
                deriveXPrv DerivationScheme2 pwd rootXPrv (getIndex purposeBIP44)
            coinTypeXPrv = -- lvl2 derivation; hardened derivation of coin_type'
                deriveXPrv DerivationScheme2 pwd purposeXPrv (getIndex coinTypeAda)
            acctXPrv = -- lvl3 derivation; hardened derivation of account' index
                deriveXPrv DerivationScheme2 pwd coinTypeXPrv accIx
        in
            IcarusKey acctXPrv

    deriveAddressPrivateKey
            (Passphrase pwd) (IcarusKey accXPrv) role (Index addrIx) =
        let
            changeCode =
                fromIntegral $ fromEnum role
            changeXPrv = -- lvl4 derivation; soft derivation of change chain
                deriveXPrv DerivationScheme2 pwd accXPrv changeCode
            addrXPrv = -- lvl5 derivation; soft derivation of address index
                deriveXPrv DerivationScheme2 pwd changeXPrv addrIx
        in
            IcarusKey addrXPrv

instance SoftDerivation IcarusKey where
    deriveAddressPublicKey (IcarusKey accXPub) role (Index addrIx) =
        fromMaybe errWrongIndex $ do
            let changeCode = fromIntegral $ fromEnum role
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

    changePassphrase old new (IcarusKey prv) =
        IcarusKey $ changePassphraseXPrv old new prv

    publicKey (IcarusKey prv) =
        IcarusKey (toXPub prv)

    digest (IcarusKey prv) =
        hash (unXPub prv)

    getRawKey =
        getKey

    liftRawKey =
        IcarusKey

{-------------------------------------------------------------------------------
                         Relationship Key / Address
-------------------------------------------------------------------------------}

instance GetPurpose IcarusKey where
    getPurpose = purposeBIP44

instance PaymentAddress 'Mainnet IcarusKey where
    paymentAddress k = Address
        $ CBOR.toStrictByteString
        $ CBOR.encodeAddress (getKey k) []
    liftPaymentAddress (KeyFingerprint bytes) =
        Address bytes

instance KnownNat pm => PaymentAddress ('Testnet pm) IcarusKey where
    paymentAddress k = Address
        $ CBOR.toStrictByteString
        $ CBOR.encodeAddress (getKey k)
            [ CBOR.encodeProtocolMagicAttr (testnetMagic @pm)
            ]
    liftPaymentAddress (KeyFingerprint bytes) =
        Address bytes

instance MkKeyFingerprint IcarusKey Address where
    paymentKeyFingerprint addr@(Address bytes) =
        case CBOR.deserialiseCbor CBOR.decodeAddressPayload bytes of
            Just _  -> Right $ KeyFingerprint bytes
            Nothing -> Left $ ErrInvalidAddress addr (Proxy @IcarusKey)

instance PaymentAddress n IcarusKey
    => MkKeyFingerprint IcarusKey (Proxy (n :: NetworkDiscriminant), IcarusKey 'AddressK XPub)
  where
    paymentKeyFingerprint (proxy, k) =
        bimap (const err) coerce
        . paymentKeyFingerprint @IcarusKey
        . paymentAddress @n
        $ k
      where
        err = ErrInvalidAddress (proxy, k) Proxy

instance IsOurs (SeqState n IcarusKey) RewardAccount where
    isOurs _account state = (Nothing, state)

{-------------------------------------------------------------------------------
                          Storing and retrieving keys
-------------------------------------------------------------------------------}

instance PersistPrivateKey (IcarusKey 'RootK) where
    serializeXPrv (k, h) =
        ( hex . unXPrv . getKey $ k
        , hex . getPassphraseHash $ h
        )

    unsafeDeserializeXPrv (k, h) = either err id $ (,)
        <$> fmap IcarusKey (xprvFromText k)
        <*> fmap PassphraseHash (fromHex h)
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
