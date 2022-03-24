{-# LANGUAGE AllowAmbiguousTypes #-}
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
    , preparePassphrase
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
import Data.ByteString
    ( ByteString )
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

import qualified Data.ByteArray as BA

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
        describe "EncryptWithPBKDF2 goldens" $ do
            pbkdf2Golden passphraseGolden1
            pbkdf2Golden passphraseGolden2

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

pbkdf2Golden :: Golden -> Spec
pbkdf2Golden g = describe ("passphrase = " <> show (unwrap (passphrase g))) $ do
    it "preparePassphrase" $ do
        unwrap (preparePassphrase EncryptWithPBKDF2 (passphrase g))
            `shouldBe` unwrap (prepared g)
    it "encryptPassphrase" $ do
        unwrap (snd (encryptPassphrase (passphrase g) salt))
            `shouldBe` unwrap (hash g)

    it "checkPassphrase" $ do
        checkPassphrase EncryptWithPBKDF2 (passphrase g) (hash g)
            `shouldBe` Right ()


  where
    -- Generated with 'genSalt'
    salt :: Passphrase "salt"
    salt = Passphrase "\x85\x80\x1b#ÓèÚ\x1b\x86>\xd8ØHëÎ"

    -- To have actual values in counterexamples, rather than just
    -- <scrubbed bytes>:
    unwrap :: BA.ByteArrayAccess i => i -> ByteString
    unwrap = BA.convert @_ @ByteString

data Golden = Golden
    { passphrase :: Passphrase "user"
    , prepared :: Passphrase "encryption"
    , hash :: PassphraseHash
    }

passphraseGolden1 :: Golden
passphraseGolden1 = Golden
    { passphrase = Passphrase "passphrase"
    , prepared = Passphrase "passphrase"
    , hash = PassphraseHash
        "\SI\133\128\ESC#\211\232\218\ESC\134>\216\216H\235\206\206\211\134'\
        \\135\253\237\244\226\143\SYN\239!}\247\172\168\CAN\212\DC1\SI\255\235\
        \\215\241\DC4\181\133\177\232=\190\154\249\ETB\EOTd\176\149\249\216\133\
        \\141\188P\188s\159q\250\159^dj\STX\ACK$O@\208\138\236wp"
    }

passphraseGolden2 :: Golden
passphraseGolden2 = Golden
    { passphrase = Passphrase ""
    , prepared = Passphrase ""
    , hash = PassphraseHash
        "\SI\133\128\ESC#\211\232\218\ESC\134>\216\216H\235\206\130\173|\250\
        \\215\130\227^\155\216\176\NUL\145p\190P\CAN\254\155\190\140\DLE\208\
        \\194\207)X\171p*Y\170\192eiZ\243\\\222Mr\174\av\NAK\238\183\DC2\156\
        \\203\196\156,,\245X\161\242\160\148\217k\EM\234"
    }
