{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.PassphraseSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Passphrase
    ( ErrWrongPassphrase (..)
    , Passphrase (..)
    , PassphraseHash (..)
    , PassphraseScheme (..)
    , checkPassphrase
    , encryptPassphrase
    )
import Cardano.Wallet.Primitive.Passphrase.Gen
    ( genEncryptionPassphrase
    , genPassphraseScheme
    , genUserPassphrase
    , shrinkUserPassphrase
    )
import Cardano.Wallet.Primitive.Passphrase.Legacy
    ( haveScrypt )
import Control.Monad.IO.Class
    ( liftIO )
import Data.Proxy
    ( Proxy (..) )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..), Property, counterexample, property, (===), (==>) )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, run )
import Test.Text.Roundtrip
    ( textRoundtrip )


spec :: Spec
spec = do
    parallel $ describe "Text Roundtrip" $ do
        textRoundtrip $ Proxy @(Passphrase "user")

    parallel $ describe "Passphrases" $ do
        it "checkPassphrase p h(p) == Right ()" $
            property prop_passphraseRoundtrip
        it "p /= p' => checkPassphrase p' h(p) == Left ErrWrongPassphrase" $
            property prop_passphraseRoundtripFail
        it "checkPassphrase fails when hash is malformed" $
            property prop_passphraseHashMalformed

{-------------------------------------------------------------------------------
                               Properties
-------------------------------------------------------------------------------}

prop_passphraseRoundtrip
    :: Passphrase "user"
    -> Property
prop_passphraseRoundtrip pwd = monadicIO $ liftIO $ do
    (scheme, hpwd) <- encryptPassphrase pwd
    scheme `shouldBe` EncryptWithPBKDF2
    checkPassphrase EncryptWithPBKDF2 pwd hpwd `shouldBe` Right ()

prop_passphraseRoundtripFail
    :: PassphraseScheme
    -> Passphrase "user"
    -> Passphrase "user"
    -> Property
prop_passphraseRoundtripFail scheme p p' =
    p /= p' ==> monadicIO $ do
        (_scheme, hp) <- run $ encryptPassphrase p
        assert $ checkPassphrase scheme p' hp ==
            whenSupported scheme (Left ErrWrongPassphrase)

prop_passphraseHashMalformed
    :: PassphraseScheme
    -> Passphrase "user"
    -> Property
prop_passphraseHashMalformed scheme pwd =
    counterexample ("haveScrypt = " <> show haveScrypt) $
    checkPassphrase scheme pwd (PassphraseHash mempty)
        === whenSupported scheme (Left ErrWrongPassphrase)

instance Arbitrary (Passphrase "user") where
    arbitrary = genUserPassphrase
    shrink = shrinkUserPassphrase

instance Arbitrary PassphraseScheme where
    arbitrary = genPassphraseScheme

instance Arbitrary (Passphrase "encryption") where
    arbitrary = genEncryptionPassphrase

-- | Helper that returns 'ErrPassphraseSchemeUnsupported' when the provided
-- scheme is 'EncryptWithScrypt' and 'haveScrypt' is false, and otherwise
-- returns the second argument.
whenSupported
    :: PassphraseScheme
    -> Either ErrWrongPassphrase ()
    -> Either ErrWrongPassphrase ()
whenSupported EncryptWithPBKDF2 = id
whenSupported EncryptWithScrypt
    | not haveScrypt
        = const . Left $ ErrPassphraseSchemeUnsupported EncryptWithScrypt
    | otherwise
        = id

