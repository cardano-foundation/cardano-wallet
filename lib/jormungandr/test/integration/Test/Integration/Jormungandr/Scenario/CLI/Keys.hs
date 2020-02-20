{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Integration.Jormungandr.Scenario.CLI.Keys
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , Passphrase (..)
    , SomeMnemonic (..)
    , WalletKey (..)
    , XPrv
    , hex
    , unXPrv
    , unXPrvStripPub
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.Mnemonic
    ( ConsistentEntropy, EntropySize, Mnemonic, entropyToMnemonic )
import Cardano.Wallet.Unsafe
    ( unsafeMkEntropy )
import Data.Proxy
    ( Proxy (..) )
import GHC.TypeLits
    ( natVal )
import System.Process
    ( readProcessWithExitCode )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , counterexample
    , frequency
    , property
    , vectorOf
    )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )

import qualified Cardano.Wallet.Primitive.AddressDerivation.Byron as Byron
import qualified Cardano.Wallet.Primitive.AddressDerivation.Icarus as Icarus
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shelley as Shelley
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

spec :: Spec
spec =
    describe "unXPrvStripPub" $ do
        it "is compatible with jcli (Shelley)" $
            property $ prop_keyToHexTextJcliCompatible @ShelleyKey
        it "is compatible with jcli (Icarus)" $
            property $ prop_keyToHexTextJcliCompatible @IcarusKey
        it "is compatible with jcli (Byron)" $
            property $ prop_keyToHexTextJcliCompatible @ByronKey

prop_keyToHexTextJcliCompatible
    :: WalletKey k
    => k 'RootK XPrv
    -> Property
prop_keyToHexTextJcliCompatible k = monadicIO $ do
    let Right hexXPrv = fmap (B8.unpack . hex) . unXPrvStripPub . getRawKey $ k
    monitor (counterexample $ "\nkey bytes = " ++ hexXPrv)
    (code, stdout, stderr) <- run $ jcliKeyFromHex hexXPrv
    monitor (counterexample $ "\n" ++ show code)
    monitor (counterexample $ "Stdout: " ++ show stdout)
    monitor (counterexample $ "Stderr: " ++ show stderr)
    assert (stderr == "")
  where
    jcliKeyFromHex = readProcessWithExitCode
        "jcli"
        ["key", "from-bytes", "--type", "ed25519bip32"]

instance Arbitrary (ShelleyKey 'RootK XPrv) where
    shrink _ = []
    arbitrary = do
        s <- SomeMnemonic <$> genMnemonic @15
        g <- fmap SomeMnemonic <$> genSecondFactor
        return $ Shelley.unsafeGenerateKeyFromSeed (s, g) encryptionPass
      where
        encryptionPass = Passphrase ""
        genSecondFactor = frequency
            [ (30, return Nothing)
            , (70, Just <$> genMnemonic @12)
            ]

instance Arbitrary (ByronKey 'RootK XPrv) where
    shrink _ = []
    arbitrary = Byron.unsafeGenerateKeyFromSeed ()
        <$> (SomeMnemonic <$> genMnemonic @12)
        <*> (pure mempty)

instance Arbitrary (IcarusKey 'RootK XPrv) where
    shrink _ = []
    arbitrary = Icarus.unsafeGenerateKeyFromSeed
        <$> (SomeMnemonic <$> genMnemonic @12)
        <*> (pure mempty)

instance Show XPrv where
    show = show . unXPrv

instance Eq XPrv where
    a == b = unXPrv a == unXPrv b

-- | Generates an arbitrary mnemonic of a size according to the type parameter.
--
-- E.g:
-- >>> arbitrary = SomeMnemonic <$> genMnemonic @12
--
-- NOTE: Duplicated with "Cardano.Wallet.Gen".
genMnemonic
    :: forall mw ent csz.
     ( ConsistentEntropy ent mw csz
     , EntropySize mw ~ ent
     )
    => Gen (Mnemonic mw)
genMnemonic = do
        let n = fromIntegral (natVal $ Proxy @(EntropySize mw)) `div` 8
        bytes <- BS.pack <$> vectorOf n arbitrary
        let ent = unsafeMkEntropy @(EntropySize mw) bytes
        return $ entropyToMnemonic ent
