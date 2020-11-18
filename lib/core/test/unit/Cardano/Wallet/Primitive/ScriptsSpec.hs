{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.ScriptsSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPub )
import Cardano.Address.Script
    ( KeyHash (..), Script (..) )
import Cardano.Mnemonic
    ( SomeMnemonic (..) )
import Cardano.Wallet.Gen
    ( genMnemonic )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index (..)
    , Passphrase (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    , Role (..)
    , SoftDerivation (..)
    , WalletKey (..)
    , hex
    , preparePassphrase
    )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..), generateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( DerivationPrefix (..)
    , SeqState (..)
    , coinTypeAda
    , defaultAddressPoolGap
    , emptyPendingIxs
    , mkAddressPool
    , purposeCIP1852
    , purposeCIP1852
    )
import Cardano.Wallet.Primitive.Scripts
    ( isShared, retrieveAllVerKeyHashes, toKeyHash )
import Cardano.Wallet.Primitive.Types
    ( PassphraseScheme (..) )
import Cardano.Wallet.Unsafe
    ( unsafeXPub )
import Control.Monad
    ( replicateM )
import Data.Proxy
    ( Proxy (..) )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Positive (..)
    , Property
    , arbitraryPrintableChar
    , choose
    , elements
    , property
    , scale
    , sized
    , vectorOf
    , (===)
    )

import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    describe "isShared" $ do
        it "script composed with all of our verification keys should discover them all"
            (property prop_scriptFromOurVerKeys)
        it "script composed with not our verification keys should not be discovered"
            (property prop_scriptFromNotOurVerKeys)

prop_scriptFromOurVerKeys
    :: AccountXPubWithScript
    -> Property
prop_scriptFromOurVerKeys (AccountXPubWithScript accXPub' script') = do
    let sciptHashes = retrieveAllVerKeyHashes script'
    let intPool = mkAddressPool accXPub' defaultAddressPoolGap []
    let extPool = mkAddressPool accXPub' defaultAddressPoolGap []
    let seqState =
            SeqState intPool extPool emptyPendingIxs dummyRewardAccount defaultPrefix Map.empty
    let (ourSharedKeys, _) = isShared script' seqState
    L.sort (L.nub $ map toKeyHash ourSharedKeys) === L.sort (L.nub sciptHashes)

prop_scriptFromNotOurVerKeys
    :: ShelleyKey 'AccountK XPub
    -> AccountXPubWithScript
    -> Property
prop_scriptFromNotOurVerKeys accXPub' (AccountXPubWithScript _accXPub script') = do
    let intPool = mkAddressPool accXPub' defaultAddressPoolGap []
    let extPool = mkAddressPool accXPub' defaultAddressPoolGap []
    let seqState =
            SeqState intPool extPool emptyPendingIxs dummyRewardAccount defaultPrefix Map.empty
    let (ourSharedKeys, _) = isShared script' seqState
    ourSharedKeys === []

data AccountXPubWithScript = AccountXPubWithScript
    { accXPub :: ShelleyKey 'AccountK XPub
    , script :: Script
    } deriving (Eq, Show)

defaultPrefix :: DerivationPrefix
defaultPrefix = DerivationPrefix
    ( purposeCIP1852
    , coinTypeAda
    , minBound
    )

dummyRewardAccount :: ShelleyKey 'AddressK XPub
dummyRewardAccount = ShelleyKey $ unsafeXPub $ B8.replicate 64 '0'

instance Ord KeyHash where
    compare (KeyHash kh1) (KeyHash kh2) =
        compare (T.decodeUtf8 $ hex kh1) (T.decodeUtf8 $ hex kh2)

{-------------------------------------------------------------------------------
                                Arbitrary Instances
-------------------------------------------------------------------------------}

genScript :: [KeyHash] -> Gen Script
genScript keyHashes =
    scale (`div` 3) $ sized scriptTree
      where
        scriptTree 0 = do
            keyH <- elements keyHashes
            pure $ RequireSignatureOf keyH
        scriptTree n = do
            Positive m <- arbitrary
            let n' = n `div` (m + 1)
            scripts <- vectorOf m (scriptTree n')
            atLeast <- choose (1, fromIntegral (m + 1))
            elements
                [ RequireAllOf scripts
                , RequireAnyOf scripts
                , RequireSomeOf atLeast scripts
                ]

instance Arbitrary (ShelleyKey 'AccountK XPub) where
    arbitrary = do
        mnemonics <- SomeMnemonic <$> genMnemonic @12
        encPwd <- arbitrary
        let rootXPrv = generateKeyFromSeed (mnemonics, Nothing) encPwd
        pure $ publicKey $ deriveAccountPrivateKey encPwd rootXPrv minBound

instance Arbitrary AccountXPubWithScript where
    arbitrary = do
        accXPub' <- arbitrary
        keyNum <- choose (2,8)
        let toVerKey ix = deriveAddressPublicKey accXPub' MultisigScript ix
        let minIndex = getIndex @'Soft minBound
        let verKeys =
                map (\ix -> toVerKey (toEnum (fromInteger $ toInteger $ minIndex + ix)))
                [0 .. keyNum]
        let verKeyHashes = map toKeyHash verKeys
        AccountXPubWithScript accXPub' <$> genScript verKeyHashes

instance Arbitrary (Passphrase "raw") where
    arbitrary = do
        n <- choose (passphraseMinLength p, passphraseMaxLength p)
        bytes <- T.encodeUtf8 . T.pack <$> replicateM n arbitraryPrintableChar
        return $ Passphrase $ BA.convert bytes
      where p = Proxy :: Proxy "raw"

    shrink (Passphrase bytes)
        | BA.length bytes <= passphraseMinLength p = []
        | otherwise =
            [ Passphrase
            $ BA.convert
            $ B8.take (passphraseMinLength p)
            $ BA.convert bytes
            ]
      where p = Proxy :: Proxy "raw"

instance Arbitrary (Passphrase "encryption") where
    arbitrary = preparePassphrase EncryptWithPBKDF2
        <$> arbitrary @(Passphrase "raw")
