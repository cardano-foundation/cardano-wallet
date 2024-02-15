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
    , genUserPassphrase
    , shrinkUserPassphrase
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Data.ByteArray
    ( ByteArray
    )
import Data.ByteArray.Encoding
    ( Base (..)
    , convertFromBase
    )
import Data.ByteString
    ( ByteString
    )
import Data.Proxy
    ( Proxy (..)
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , property
    , (===)
    , (==>)
    )
import Test.QuickCheck.Monadic
    ( assert
    , monadicIO
    , run
    )
import Test.Text.Roundtrip
    ( textRoundtrip
    )

import qualified Data.ByteArray as BA

spec :: Spec
spec = do
    describe "Text Roundtrip" $ do
        textRoundtrip $ Proxy @(Passphrase "user")

    describe "Passphrases" $ do
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
    :: Passphrase "user"
    -> Passphrase "user"
    -> Property
prop_passphraseRoundtripFail p p' =
    p /= p' ==> monadicIO $ do
        (scheme, hp) <- run $ encryptPassphrase p
        assert $ checkPassphrase scheme p' hp == Left ErrWrongPassphrase

prop_passphraseHashMalformed
    :: Passphrase "user"
    -> Property
prop_passphraseHashMalformed pwd =
    checkPassphrase EncryptWithPBKDF2 pwd (PassphraseHash mempty)
        === Left ErrWrongPassphrase

instance Arbitrary (Passphrase "user") where
    arbitrary = genUserPassphrase
    shrink = shrinkUserPassphrase

instance Arbitrary (Passphrase "encryption") where
    arbitrary = genEncryptionPassphrase

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
    , hash = unsafeFromHex
        "0f85801b23d3e8da1b863ed8d848ebceced3862787fdedf4e28f16ef217df7ac\
        \a818d4110fffebd7f114b585b1e83dbe9af9170464b095f9d8858dbc50bc739f\
        \71fa9f5e646a0206244f40d08aec7770"
    }

passphraseGolden2 :: Golden
passphraseGolden2 = Golden
    { passphrase = Passphrase ""
    , prepared = Passphrase ""
    , hash = unsafeFromHex
        "0f85801b23d3e8da1b863ed8d848ebce82ad7cfad782e35e9bd8b0009170be50\
        \18fe9bbe8c10d0c2cf2958ab702a59aac065695af35cde4d72ae077615eeb712\
        \9ccbc49c2c2cf558a1f2a094d96b19ea"
    }

-- | Decode an hex-encoded 'ByteString' into raw bytes, or fail.
unsafeFromHex :: forall b. ByteArray b => ByteString -> b
unsafeFromHex = unsafeRight . convertFromBase @ByteString @b Base16
  where
    unsafeRight :: Either a b -> b
    unsafeRight = either (error "unsafeFromHex failed") id
