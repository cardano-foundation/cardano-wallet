{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Hashing of wallet passwords.
--

module Cardano.Wallet.Primitive.Passphrase
    ( -- * Passphrases from the user
      Passphrase (..)
    , PassphraseMinLength (..)
    , PassphraseMaxLength (..)
    , validatePassphrase

      -- * Wallet passphrases stored as hashes
    , PassphraseHash (..)
    , PassphraseScheme (..)
    , currentPassphraseScheme
    , WalletPassphraseInfo (..)

      -- * Operations
    , encryptPassphrase
    , encryptPassphrase'
    , checkPassphrase
    , preparePassphrase
    , changePassphraseXPrv
    , checkAndChangePassphraseXPrv
    , ErrWrongPassphrase (..)
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPrv, xPrvChangePass )
import Cardano.Wallet.Primitive.Passphrase.Types
import Crypto.Random.Types
    ( MonadRandom )

import qualified Cardano.Wallet.Primitive.Passphrase.Current as PBKDF2
import qualified Cardano.Wallet.Primitive.Passphrase.Legacy as Scrypt

currentPassphraseScheme :: PassphraseScheme
currentPassphraseScheme = EncryptWithPBKDF2

-- | Hashes a 'Passphrase' into a format that is suitable for storing on
-- disk. It will always use the current scheme: pbkdf2-hmac-sha512.
encryptPassphrase
    :: MonadRandom m
    => Passphrase "user"
    -> m (PassphraseScheme, PassphraseHash)
encryptPassphrase = fmap (currentPassphraseScheme,)
    . encryptPassphrase' currentPassphraseScheme

encryptPassphrase'
    :: MonadRandom m
    => PassphraseScheme
    -> Passphrase "user"
    -> m PassphraseHash
encryptPassphrase' scheme = encrypt . preparePassphrase scheme
  where
    encrypt = case scheme of
        EncryptWithPBKDF2 -> PBKDF2.encryptPassphrase
        EncryptWithScrypt -> Scrypt.encryptPassphraseTestingOnly

-- | Manipulation done on legacy passphrases before used for encryption.
preparePassphrase
    :: PassphraseScheme
    -> Passphrase "user"
    -> Passphrase "encryption"
preparePassphrase = \case
    EncryptWithPBKDF2 -> PBKDF2.preparePassphrase
    EncryptWithScrypt -> Scrypt.preparePassphrase

-- | Check whether a 'Passphrase' matches with a stored 'Hash'
checkPassphrase
    :: PassphraseScheme
    -> Passphrase "user"
    -> PassphraseHash
    -> Either ErrWrongPassphrase ()
checkPassphrase scheme received stored = case scheme of
    EncryptWithPBKDF2 -> PBKDF2.checkPassphrase prepared stored
    EncryptWithScrypt -> case Scrypt.checkPassphrase prepared stored of
        Just True -> Right ()
        Just False -> Left ErrWrongPassphrase
        Nothing -> Left (ErrPassphraseSchemeUnsupported scheme)
  where
    prepared = preparePassphrase scheme received

-- | Re-encrypts a wallet private key with a new passphrase.
--
-- **Important**:
-- This function doesn't check that the old passphrase is correct! Caller is
-- expected to have already checked that. Using an incorrect passphrase here
-- will lead to very bad thing.
changePassphraseXPrv
    :: (PassphraseScheme, Passphrase "user")
       -- ^ Old passphrase
    -> (PassphraseScheme, Passphrase "user")
       -- ^ New passphrase
    -> XPrv
       -- ^ Key to re-encrypt
    -> XPrv
changePassphraseXPrv (oldS, old) (newS, new) = xPrvChangePass oldP newP
  where
    oldP = preparePassphrase oldS old
    newP = preparePassphrase newS new

-- | Re-encrypts a wallet private key with a new passphrase.
checkAndChangePassphraseXPrv
    :: MonadRandom m
    => ((PassphraseScheme, PassphraseHash), Passphrase "user")
       -- ^ Old passphrase
    -> Passphrase "user"
       -- ^ New passphrase
    -> XPrv
       -- ^ Key to re-encrypt
    -> m (Either ErrWrongPassphrase ((PassphraseScheme, PassphraseHash), XPrv))
checkAndChangePassphraseXPrv ((oldS, oldH), old) new key =
    case checkPassphrase oldS old oldH of
        Right () -> do
            (newS, newH) <- encryptPassphrase new
            let newKey = changePassphraseXPrv (oldS, old) (newS, new) key
            pure $ Right ((newS, newH), newKey)
        Left e -> pure $ Left e
