{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

-- | Provides support for AES 256.
--
-- AES is a subset of the family of block ciphers known as Rijndael ciphers.
--
-- That family includes no less than 15 variants, for three possible block
-- sizes (128, 192 and 256 bits).
--
-- Three are supported by cryptonite: AES128, AES192 and AES256 (used here).
--
-- There are five possible key sizes (128, 160, 192, 224 and 256 bits).
--
-- In case of the cryptonite implementation we have the following constraints:
-- (AES128 - 16 bytes, AES192 - 24 bytes, AES 256 - 32 bytes).
--
-- AES, as standardized by NIST, includes only three variants, all with 128-bit
-- blocks, and with keys of 128, 192, or 256 bits. (The same as adopted in
-- cryptonite.)
--
-- The block size of AES is always 128 bits, so a 256-bit initialisation vector
-- (IV) is not possible for most modes of operation. These are the reasons we
-- require the secret key to be 32 bytes and IV 16 bytes for AES256 used here.
--
-- Additionally, input length must be a multiple of block size, ie., 16 bytes.
--
-- In order to realize this PKCS#7 padding is made available as an option.
-- This padding is preferred because it is unambiguous and widely used (for
-- example in OpenSSL).
--
-- One can always read the last byte and remove that number of padding bytes,
-- and then detect if the decryption was successful by checking that all of
-- the bytes removed have the same value.
--
module Cryptography.Cipher.AES256CBC
    ( CipherMode (..)
    , CipherError (..)
    , encrypt
    , decrypt
    ) where

import Prelude

import Control.Monad
    ( when
    )
import Crypto.Cipher.AES
    ( AES256
    )
import Crypto.Cipher.Types
    ( BlockCipher (cbcDecrypt, cbcEncrypt)
    , Cipher (cipherInit)
    , IV
    , makeIV
    )
import Cryptography.Core
    ( CryptoError (CryptoError_IvSizeInvalid)
    , CryptoFailable (CryptoFailed, CryptoPassed)
    )
import Data.Bifunctor
    ( Bifunctor (bimap, first, second)
    )
import Data.ByteString
    ( ByteString
    )
import Data.Either.Extra
    ( maybeToEither
    )

import qualified Cryptography.Padding.PKCS7 as PKCS7
import qualified Data.ByteString as BS

data CipherMode =
    WithoutPadding | WithPadding
    deriving (Eq, Show)

data CipherError =
      FromCryptonite CryptoError
    | EmptyPayload
    | WrongPayloadSize
    | WrongSaltSize
    deriving (Eq, Show)

-- | Initialise a block cipher.
initCipher :: ByteString -> Either CryptoError AES256
initCipher k = case cipherInit k of
    CryptoFailed e -> Left e
    CryptoPassed a -> Right a

-- | Create an initialisation vector (IV).
createIV :: ByteString -> Either CryptoError (IV AES256)
createIV =
    maybeToEither CryptoError_IvSizeInvalid . makeIV

-- | Encrypt using AES256 using CBC mode
encrypt
    :: CipherMode
    -> ByteString
    -- ^ Secret key: must be 32 bytes.
    -> ByteString
    -- ^ Initialisation vector (IV): must be 16 bytes.
    -> Maybe ByteString
    -- ^ Optional salt. If specified, must be 8 bytes.
    -> ByteString
    -- ^ Payload: must be a multiple of a block size, ie., 16 bytes.
    -> Either CipherError ByteString
encrypt mode key iv saltM msg = do
    when (any ((/= 8) . BS.length) saltM) $
        Left WrongSaltSize
    when (mode == WithoutPadding && BS.length msg `mod` 16 /= 0) $
        Left WrongPayloadSize
    initedIV <- first FromCryptonite (createIV iv)
    let msgM = case mode of
            WithoutPadding -> Just msg
            WithPadding -> pad msg
    msg' <- maybeToEither EmptyPayload msgM
    case saltM of
        Nothing ->
            bimap FromCryptonite
            (\c -> cbcEncrypt c initedIV msg') (initCipher key)
        Just salt ->
            second (\c -> addSalt salt <> c) $
            bimap FromCryptonite
            (\c -> cbcEncrypt c initedIV msg') (initCipher key)
  where
    addSalt salt = saltPrefix <> salt
    pad payload
        | BS.null payload = Nothing
        | otherwise = Just (PKCS7.pad payload)

saltPrefix :: ByteString
saltPrefix = "Salted__"

-- | Decrypt using AES256 using CBC mode.
decrypt
    :: CipherMode
    -> ByteString
    -- ^ Secret key: must be 32 bytes.
    -> ByteString
    -- ^ Initialisation vector (IV): must be 16 bytes.
    -> ByteString
    -- ^ Payload: must be a multiple of a block size, ie., 16 bytes.
    -> Either CipherError (ByteString, Maybe ByteString)
    -- ^ Decrypted payload and optionally salt that was used for encryption.
decrypt mode key iv msg = do
    when (mode == WithoutPadding && BS.length msg `mod` 16 /= 0) $
        Left WrongPayloadSize
    initedIV <- first FromCryptonite (createIV iv)
    let (prefix,rest) = BS.splitAt 8 msg
    let saltDetected = prefix == saltPrefix
    if saltDetected then
        second (, Just $ BS.take 8 rest) $
        bimap FromCryptonite
        (\c -> cbcDecrypt c initedIV (BS.drop 8 rest)) (initCipher key) >>=
        unpad
    else
        second (, Nothing) $
        bimap FromCryptonite
        (\c -> cbcDecrypt c initedIV msg) (initCipher key) >>=
        unpad
  where
    unpad :: ByteString -> Either CipherError ByteString
    unpad p = case mode of
        WithoutPadding -> Right p
        WithPadding -> maybeToEither EmptyPayload (PKCS7.unpad p)
