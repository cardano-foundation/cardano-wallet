{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Cryptography.Cipher.ChaChaPoly1305
    ( encrypt
    , decrypt
    ) where

import Prelude

import Control.Monad
    ( when
    )
import Crypto.Cipher.ChaChaPoly1305
    ( finalize
    , finalizeAAD
    , initialize
    , nonce12
    )
import Crypto.Error
    ( CryptoError (..)
    , CryptoFailable (..)
    )
import Data.ByteString
    ( ByteString
    )

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Crypto.Cipher.ChaChaPoly1305 as Poly
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

-- | ChaCha20/Poly1305 encrypting the payload with passphrase and nonce.
-- | The caller must ensure that the passphrase length is 32 bytes and
-- | nonce is 12 bytes
encrypt
    :: ByteString
       -- ^ Symmetric key / passphrase, 32-byte long
    -> ByteString
       -- ^ Nonce, 12-byte long
    -> ByteString
        -- ^ Payload to be encrypted
    -> ByteString
        -- ^ Ciphertext with a 16-byte crypto-tag appended.
encrypt passphrase nonce payload = BS.drop 2 $ unsafeSerialize $ do
    nonced <- nonce12 nonce
    st1 <- finalizeAAD <$> initialize passphrase nonced
    let (out, st2) = Poly.encrypt payload st1
    return $ out <> BA.convert (finalize st2)
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
decrypt
    :: ByteString
       -- ^ Symmetric key / passphrase, 32-byte long
    -> ByteString
       -- ^ Nonce, 12-byte long
    -> ByteString
        -- ^ Payload to be decrypted
    -> CryptoFailable ByteString
decrypt passphrase nonce bytes = do
    let (payload, tag) = BS.splitAt (BS.length bytes - 16) bytes
    nonced <- nonce12 nonce
    st1 <- finalizeAAD <$> initialize passphrase nonced
    let (out, st2) = Poly.decrypt payload st1
    when (BA.convert (finalize st2) /= tag) $
        CryptoFailed CryptoError_MacKeyInvalid
    return out
