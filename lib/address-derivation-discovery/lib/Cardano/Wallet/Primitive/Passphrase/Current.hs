{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Generating and verifying hashes of wallet passwords.
--

module Cardano.Wallet.Primitive.Passphrase.Current
    ( encryptPassphrase
    , checkPassphrase
    , preparePassphrase
    , genSalt
    ) where

import Prelude

import Cardano.Wallet.Primitive.Passphrase.Types
    ( ErrWrongPassphrase (..), Passphrase (..), PassphraseHash (..) )
import Control.Monad
    ( unless )
import Crypto.KDF.PBKDF2
    ( Parameters (..), fastPBKDF2_SHA512 )
import Crypto.Random.Types
    ( MonadRandom (..) )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Function
    ( on )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

-- | Encrypt a 'Passphrase' into a format that is suitable for storing on disk
encryptPassphrase
    :: MonadRandom m
    => Passphrase "encryption"
    -> m PassphraseHash
encryptPassphrase (Passphrase bytes) = mkPassphraseHash <$> genSalt
  where
    mkPassphraseHash (Passphrase salt) = PassphraseHash $ BA.convert $ mempty
        <> BS.singleton (fromIntegral (BA.length salt))
        <> BA.convert salt
        <> fastPBKDF2_SHA512 params bytes salt

    params = Parameters
        { iterCounts = 20000
        , outputLength = 64
        }

genSalt :: MonadRandom m => m (Passphrase "salt")
genSalt = Passphrase <$> getRandomBytes 16

preparePassphrase :: Passphrase "user" -> Passphrase "encryption"
preparePassphrase = coerce

checkPassphrase
    :: Passphrase "encryption"
    -> PassphraseHash
    -> Either ErrWrongPassphrase ()
checkPassphrase prepared stored = do
    salt <- getSalt (BA.convert stored)
    unless (constantTimeEq (encryptPassphrase prepared salt) stored) $
        Left ErrWrongPassphrase
  where
    getSalt :: ByteString -> Either ErrWrongPassphrase (Passphrase "salt")
    getSalt bytes = do
        len <- case BS.unpack (BS.take 1 bytes) of
            [len] -> Right $ fromIntegral len
            _ -> Left ErrWrongPassphrase
        Right $ Passphrase $ BA.convert $ BS.take len (BS.drop 1 bytes)

    constantTimeEq :: PassphraseHash -> PassphraseHash -> Bool
    constantTimeEq = (==) `on` BA.convert @_ @ScrubbedBytes
