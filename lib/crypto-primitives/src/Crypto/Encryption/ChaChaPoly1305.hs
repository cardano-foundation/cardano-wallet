{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Crypto.Encryption.ChaChaPoly1305
    ( encryptPayload
    , decryptPayload
    , toSymmetricKey

    , CryptoError (..)
    , CryptoFailable (..)
    ) where

import Prelude

import Control.Monad
    ( when
    )
import Crypto.Error
    ( CryptoError (..)
    , CryptoFailable (..)
    )
import Crypto.Hash.Algorithms
    ( SHA256 (..)
    , SHA512 (..)
    )
import Data.ByteString
    ( ByteString
    )

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Crypto.Cipher.ChaChaPoly1305 as Poly
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

import Data.ByteArray.Encoding
    ( Base (Base16, Base64)
    , convertToBase
    )
import qualified Data.Text.Encoding as T
import qualified Debug.Trace as TR

import qualified Crypto.Encryption.AES256CBC as AES256CBC
import qualified Crypto.KeyDerivationFunction.PBKDF2 as My
import qualified Data.ByteString.Char8 as B8

-- | ChaCha20/Poly1305 encrypting the payload with passphrase and nonce.
-- | The caller must ensure that the passphrase length is 32 bytes and
-- | nonce is 12 bytes
encryptPayload
    :: ByteString
       -- ^ Symmetric key / passphrase, 32-byte long
    -> ByteString
       -- ^ Nonce, 12-byte long
    -> ByteString
        -- ^ Payload to be encrypted
    -> ByteString
        -- ^ Ciphertext with a 16-byte crypto-tag appended.
encryptPayload passphrase nonce payload = BS.drop 2 $ unsafeSerialize $ do
    nonced <- Poly.nonce12 nonce
    st1 <- Poly.finalizeAAD <$> Poly.initialize passphrase nonced
    let (out, st2) = Poly.encrypt payload st1
    return $ out <> BA.convert (Poly.finalize st2)
  where
    unsafeSerialize :: CryptoFailable ByteString -> ByteString
    unsafeSerialize =
        CBOR.toStrictByteString . CBOR.encodeBytes . useInvariant
    -- Encryption fails if the key is the wrong size, but that won't happen
    -- if the key was created with 'toSymmetricKey'.
    useInvariant = \case
        CryptoPassed res -> res
        CryptoFailed err -> error $ "encryptPayload: " ++ show err

-- | ChaCha20/Poly1305 decrypting with passphrase and nonce.
-- | The caller must ensure that the passphrase length is 32 bytes and
-- | nonce is 12 bytes
decryptPayload
    :: ByteString
       -- ^ Symmetric key / passphrase, 32-byte long
    -> ByteString
       -- ^ Nonce, 12-byte long
    -> ByteString
        -- ^ Payload to be decrypted
    -> CryptoFailable ByteString
decryptPayload passphrase nonce bytes = do
    let (payload, tag) = BS.splitAt (BS.length bytes - 16) bytes
    nonced <- Poly.nonce12 nonce
    st1 <- Poly.finalizeAAD <$> Poly.initialize passphrase nonced
    let (out, st2) = Poly.decrypt payload st1
    when (BA.convert (Poly.finalize st2) /= tag) $
        CryptoFailed CryptoError_MacKeyInvalid
    return out

-- | Derive a symmetric key for encrypting .
-- | PBKDF2 encryption using HMAC with the hash algorithm SHA512 is employed.

-- | $ echo -n "metadata-secret" | openssl enc -aes-256-cbc -pbkdf2 -md sha512 -pass stdin -P -S 3030303030303030 -iter 10000 -P
-- | salt=3030303030303030
-- | key=BF0450D025740732576B4BD264B12B16B4FFE04FFE39B954267E908B0E80BB6F
-- | iv =452A8874770B3C2984A700AAFA8A8F6C

-- |     salt = "00000000"
-- |    out =
-- |         PBKDF2.generate
-- |        (PBKDF2.prfHMAC SHA512)
-- |        (PBKDF2.Parameters 10000 32)
-- |       rawKey
-- |       salt
-- | rawKey"metadata-secret" out:"bf0450d025740732576b4bd264b12b16b4ffe04ffe39b954267e908b0e80bb6f" salt:"3030303030303030"

-- | $ echo -n "metadata-secret" | openssl enc -aes-256-cbc -pbkdf2 -md sha256 -pass stdin -P -S 3030303030303030 -iter 10000 -P
-- | salt=3030303030303030
-- | key=57FCB522B950BCB78138EECFE7FBE07881E5B49AAA2D1CD761D4495A09A5F16C
-- | iv =41D88E094C8F202C7DF6654B7F40E5AF

-- | $ echo -n "metadata-secret" | openssl enc -aes-256-cbc -pbkdf2 -pass stdin -P -S 3030303030303030 -iter 10000 -P
-- | salt=3030303030303030
-- | key=57FCB522B950BCB78138EECFE7FBE07881E5B49AAA2D1CD761D4495A09A5F16C
-- | iv =41D88E094C8F202C7DF6654B7F40E5AF
toSymmetricKey
    :: ByteString
    -> ByteString
toSymmetricKey rawKey = TR.trace ("rawKey"<> show rawKey <> " out:"<>show (hex out)<>" salt:"<> show (hex salt)<>"\nout1a:"<> show (hex out1a)<>"\nout1b:"<> show (hex out1b)<>"\nout2:"<> show (base64 out2)<>"\nout3:"<> show out3<>"\nsalt:"<> show (B8.pack "00000000")) $ out
  where
    salt = "00000000"
    msg1 = "[\"Invoice-No: 123456789\",\"Order-No: 7654321\",\"Email: john@doe.com\"]"
    out =
        PBKDF2.generate
        (PBKDF2.prfHMAC SHA256)
        (PBKDF2.Parameters 10000 48)
        rawKey
        salt
    (out1a, out1b) = My.generateKeyForMetadata rawKey (Just salt)
    (Right out2) = AES256CBC.encrypt out1a out1b (B8.pack "0000000000000000")
    (Right out3) = AES256CBC.decrypt out1a out1b out2
{--
    PBKDF2.generate
    (PBKDF2.prfHMAC SHA512)
    (PBKDF2.Parameters 500 32)
    rawKey
    ("metadata-encryption" :: ByteString)
--}

hex = T.decodeUtf8 . convertToBase Base16
base64 = T.decodeUtf8 . convertToBase Base64
