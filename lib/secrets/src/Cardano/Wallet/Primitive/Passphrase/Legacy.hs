{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Support for verifying hashed passwords from the old wallet codebase.
--
-- These passwords were encrypted by the @scrypt@ package using the following
-- parameters:
--  - logN = 14
--  - r = 8
--  - p = 1
--  - outputLength = 64
--
-- It is possible to disable support for legacy password hashing by compiling
-- with the @-scrypt@ Cabal flag.

module Cardano.Wallet.Primitive.Passphrase.Legacy
    ( -- * Legacy passphrases
      checkPassphrase
    , preparePassphrase

      -- * Testing-only helper
    , haveScrypt

      -- * Internal functions, exposed for testing
    , PassphraseHashLength
    , encryptPassphraseTestingOnly
    , checkPassphraseCryptonite
    , checkPassphraseScrypt
    , getSalt
    , genSalt
    ) where

import Prelude

import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase (..)
    , PassphraseHash (..)
    )
import Crypto.Hash.Extra
    ( blake2b256
    )
import Crypto.Random.Types
    ( MonadRandom (..)
    )
import Data.ByteArray.Encoding
    ( Base (..)
    , convertFromBase
    , convertToBase
    )
import Data.ByteString
    ( ByteString
    )
import Data.Word
    ( Word64
    )

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Crypto.KDF.Scrypt as Scrypt
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as B8

#if HAVE_SCRYPT
import Crypto.Scrypt
    ( EncryptedPass (..)
    , Pass (..)
    , Salt (Salt)
    , encryptPass
    , scryptParamsLen
    , verifyPass'
    )

haveScrypt :: Bool
haveScrypt = True

#else

haveScrypt :: Bool
haveScrypt = False

#endif

{- NOTE [LegacyScrypt]

We need to transition away from the unmaintained `scrypt` package.
Specifically, the `scrypt` package is not supported on `aarch64-darwin`.

We can use the `cryptonite` package instead.
In order to ensure that the new package produces the same result as the
old one, we proceed as follows:

    * in production:
        * use `cryptonite`
    * in testing:
        * generate random passphrases,
            encrypted with `scrypt` package if available
        * check that these encrypted passphrases
            * are verified correctly with the `cryptonite` package
            * are verified correctly with the `scrypt` package

These tests ensure that the code using `cryptonite` can verify
hashed passphrases that were created with `scrypt`.

-}

{-----------------------------------------------------------------------------
    Passphrase hashing and verification using the Scrypt algorithm
------------------------------------------------------------------------------}
-- | Verify a wallet spending password using the legacy Byron scrypt encryption
-- scheme.
checkPassphrase :: Passphrase "encryption" -> PassphraseHash -> Bool
checkPassphrase = checkPassphraseCryptonite

-- | Encrypt a wallet spending password using
-- the legacy Byron scrypt encryption scheme.
--
-- Do not use this function in production, only in unit tests!
encryptPassphraseTestingOnly
    :: MonadRandom m
    => PassphraseHashLength
    -> Passphrase "encryption"
    -> m PassphraseHash
#if HAVE_SCRYPT
encryptPassphraseTestingOnly len pwd = mkPassphraseHash <$> genSalt
  where
    mkPassphraseHash :: Passphrase "salt" -> PassphraseHash
    mkPassphraseHash (Passphrase salt) =
        PassphraseHash
        . BA.convert
        . getEncryptedPass
        $ encryptPass
            params
            (Salt $ BA.convert salt)
            (Pass . BA.convert . unPassphrase $ cborify pwd)

    params =
        case scryptParamsLen
                (fromIntegral logN)
                (fromIntegral r)
                (fromIntegral p)
                (fromIntegral len)
          of
            Just x -> x
            Nothing -> error "scryptParamsLen: unreachable code path"
#else
encryptPassphraseTestingOnly len pwd =
    encryptPassphraseCryptoniteWithLength len <$> genSalt <*> pure pwd
#endif

preparePassphrase :: Passphrase "user" -> Passphrase "encryption"
preparePassphrase = Passphrase . hashMaybe . unPassphrase
  where
    hashMaybe pw
        | pw == mempty = mempty
        | otherwise = BA.convert $ blake2b256 pw

{-----------------------------------------------------------------------------
    Passphrase verification
------------------------------------------------------------------------------}

checkPassphraseScrypt :: Passphrase "encryption" -> PassphraseHash -> Bool
#if HAVE_SCRYPT
checkPassphraseScrypt pwd stored =
    verifyPass' (Pass (BA.convert (cborify pwd))) encryptedPass
  where
    encryptedPass = EncryptedPass (BA.convert stored)
#else
checkPassphraseScrypt _ _ = error "checkPassphraseScrypt not available"
#endif

checkPassphraseCryptonite
    :: Passphrase "encryption" -> PassphraseHash -> Bool
checkPassphraseCryptonite pwd stored =
    case parsePassphraseHash stored of
        Just (salt, len) ->
            encryptPassphraseCryptoniteWithLength len salt pwd == stored
        Nothing -> False

-- | Extract salt field from pipe-delimited password hash.
-- This will fail unless there are exactly 5 fields
getSalt :: PassphraseHash -> Maybe (Passphrase "salt")
getSalt = fmap fst . parsePassphraseHash

parsePassphraseHash
    :: PassphraseHash
    -> Maybe (Passphrase "salt", PassphraseHashLength)
parsePassphraseHash (PassphraseHash stored) =
    case B8.split '|' (BA.convert stored) of
        [_logN, _r, _p, salt64, hash64]
            | Right salt <- convertFromBase Base64 salt64
            , Right hash <- convertFromBase Base64 hash64
            -> Just (Passphrase salt, B8.length hash)
        _ -> Nothing

{-----------------------------------------------------------------------------
    Passphrase hashing
------------------------------------------------------------------------------}
type PassphraseHashLength = Int

encryptPassphraseCryptoniteWithLength
    :: PassphraseHashLength
    -> Passphrase "salt"
    -> Passphrase "encryption"
    -> PassphraseHash
encryptPassphraseCryptoniteWithLength len (Passphrase salt) pwd =
    PassphraseHash $ BA.convert combined
  where
    combined = B8.intercalate "|"
        [ showBS logN
        , showBS r
        , showBS p
        , convertToBase Base64 salt
        , convertToBase Base64 passphraseHash
        ]

    passphraseHash :: ByteString
    passphraseHash = Scrypt.generate params (cborify pwd) salt

    params = Scrypt.Parameters ((2 :: Word64) ^ logN) r p len

    showBS = B8.pack . show

-- Scrypt parameters
logN, r, p :: Int
logN = 14
r = 8
p = 1

cborify :: Passphrase "encryption" -> Passphrase "encryption"
cborify = Passphrase . BA.convert . CBOR.toStrictByteString
    . CBOR.encodeBytes . BA.convert . unPassphrase

genSalt :: MonadRandom m => m (Passphrase "salt")
genSalt = Passphrase <$> getRandomBytes 32
