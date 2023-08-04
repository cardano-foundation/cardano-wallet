{-# LANGUAGE DataKinds #-}

module Cardano.Wallet.Primitive.Passphrase.Gen
  ( genUserPassphrase
  , shrinkUserPassphrase
  , genPassphraseScheme
  , genEncryptionPassphrase
  )
where

import Cardano.Wallet.Primitive.Passphrase
  ( Passphrase (..)
  , PassphraseMaxLength (..)
  , PassphraseMinLength (..)
  , PassphraseScheme (..)
  , preparePassphrase
  )
import Control.Monad
  ( replicateM
  )
import Data.ByteArray qualified as BA
import Data.ByteString.Char8 qualified as B8
import Data.Proxy
  ( Proxy (..)
  )
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Test.QuickCheck
  ( Gen
  , arbitraryPrintableChar
  , choose
  )
import Test.QuickCheck.Arbitrary.Generic
  ( genericArbitrary
  )
import Prelude

genUserPassphrase :: Gen (Passphrase "user")
genUserPassphrase = do
  n <- choose (passphraseMinLength p, passphraseMaxLength p)
  bytes <- T.encodeUtf8 . T.pack <$> replicateM n arbitraryPrintableChar
  return $ Passphrase $ BA.convert bytes
  where
    p = Proxy :: Proxy "user"

shrinkUserPassphrase :: Passphrase "user" -> [Passphrase "user"]
shrinkUserPassphrase (Passphrase bytes)
  | BA.length bytes <= passphraseMinLength p = []
  | otherwise =
      [ Passphrase
          $ BA.convert
          $ B8.take (passphraseMinLength p)
          $ BA.convert bytes
      ]
  where
    p = Proxy :: Proxy "user"

genPassphraseScheme :: Gen PassphraseScheme
genPassphraseScheme = genericArbitrary

genEncryptionPassphrase :: Gen (Passphrase "encryption")
genEncryptionPassphrase =
  preparePassphrase EncryptWithPBKDF2
    <$> genUserPassphrase
