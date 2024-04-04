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
    , padPKCS7
    , unpadPKCS7
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
import Data.ByteString
    ( ByteString
    )
import Data.Either.Combinators
    ( mapBoth
    , mapLeft
    , mapRight
    , maybeToRight
    )
import Data.Either.Extra
    ( maybeToEither
    )
import Data.Maybe
    ( fromJust
    , isJust
    )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

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
    -- ^ Optional salt, if specified must be 8 bytes
    -> ByteString
    -- ^ Payload: must be a multiple of a block size, ie., 16 bytes.
    -> Either CipherError ByteString
encrypt mode key iv saltM msg = do
   when (isJust saltM && BS.length (fromJust saltM) /= 8) $
       Left WrongSaltSize
   when (mode == WithoutPadding && BS.length msg `mod` 16 /= 0) $
       Left WrongPayloadSize
   initedIV <- mapLeft FromCryptonite (createIV iv)
   let msgM = case mode of
           WithoutPadding -> Just msg
           WithPadding -> padPKCS7 msg
   msg' <- maybeToRight EmptyPayload msgM
   case saltM of
       Nothing ->
           mapBoth FromCryptonite
           (\c -> cbcEncrypt c initedIV msg') (initCipher key)
       Just salt ->
           mapRight (\c -> addSalt salt <> c) $
           mapBoth FromCryptonite
           (\c -> cbcEncrypt c initedIV msg') (initCipher key)
 where
   addSalt salt = saltPrefix <> salt

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
    -- ^ Decrypted payload and optionally salt that was used upon encryption of the payment
decrypt mode key iv msg = do
   when (mode == WithoutPadding && BS.length msg `mod` 16 /= 0) $
       Left WrongPayloadSize
   initedIV <- mapLeft FromCryptonite (createIV iv)
   let (prefix,rest)= BS.splitAt 8 msg
   let saltDetected = prefix == saltPrefix
   let unpadding p = case mode of
           WithoutPadding -> Right p
           WithPadding -> maybeToRight EmptyPayload (unpadPKCS7 p)
   if saltDetected then
       mapRight (, Just $ BS.take 8 rest) $
       mapBoth FromCryptonite
       (\c -> cbcDecrypt c initedIV (BS.drop 8 rest)) (initCipher key) >>=
       unpadding
   else
       mapRight (, Nothing) $
       mapBoth FromCryptonite
       (\c -> cbcDecrypt c initedIV msg) (initCipher key) >>=
       unpadding

-- | Apply PKCS#7 padding to payload and end up with a multiple of a block
-- size, i.e., 16 bytes, according to
-- https://datatracker.ietf.org/doc/html/rfc5652#section-6.3.
-- The padding value is the number of padding bytes.
-- If 1 byte of padding is required, the padding is "01".
-- If 2 bytes of padding, it's "02 02".
-- If no padding is required, an extra block of 0x10 bytes is added,
-- i.e., sixteen copies of 16, which is the blocksize.
-- This means that payload can only fit 15 bytes into a single block with
-- padding. A 16 byte payload requires 2 blocks with padding.
padPKCS7 :: ByteString -> Maybe ByteString
padPKCS7 payload
    | BS.null payload = Nothing
    | otherwise = Just $ BS.append payload padding
  where
    padding = B8.replicate paddingLength (toEnum paddingLength)
    paddingLength = 16 - (BS.length payload `mod` 16)

unpadPKCS7 :: ByteString -> Maybe ByteString
unpadPKCS7 payload =
    stripPadding <$> BS.unsnoc payload
  where
    stripPadding (_, lastByte) = BS.dropEnd paddingLength payload
      where
        paddingLength :: Int
        paddingLength = fromEnum lastByte
