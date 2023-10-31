{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Crypto.Encryption.ChaChaPoly1305
    ( encryptPayload
    ) where

import Prelude

import Crypto.Error
    ( CryptoFailable (..)
    )
import Data.ByteString
    ( ByteString
    )

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Crypto.Cipher.ChaChaPoly1305 as Poly
import qualified Data.ByteArray as BA


-- | ChaCha20/Poly1305 encrypting and signing the payload.
-- |
-- | The caller must ensure that the passphrase length is 32 bytes.
encryptPayload
    :: ByteString
       -- ^ Symmetric key / passphrase, 32-byte long
    -> ByteString
       -- ^ Nonce
    -> ByteString
        -- ^ Payload to be encrypted
    -> ByteString
        -- ^ Ciphertext with a 128-bit crypto-tag appended.
encryptPayload passphrase nounce payload = unsafeSerialize $ do
    nonce <- Poly.nonce12 nounce
    st1 <- Poly.finalizeAAD <$> Poly.initialize passphrase nonce
    let (out, st2) = Poly.encrypt payload st1
    return $ out <> BA.convert (Poly.finalize st2)
  where
    unsafeSerialize :: CryptoFailable ByteString -> ByteString
    unsafeSerialize =
        CBOR.toStrictByteString . CBOR.encodeBytes . useInvariant

    -- Encryption fails if the key is the wrong size, but that won't happen
    -- if the key was created with 'xxx'.
    useInvariant = \case
        CryptoPassed res -> res
        CryptoFailed err -> error $ "encryptPayload: " ++ show err
