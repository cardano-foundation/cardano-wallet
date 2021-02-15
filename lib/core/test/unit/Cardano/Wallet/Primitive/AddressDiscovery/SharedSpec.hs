{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.AddressDiscovery.SharedSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPub )
import Cardano.Address.Script
    ( Cosigner (..), Script (..), ScriptTemplate (..) )
import Cardano.Address.Style.Shelley
    ( mkNetworkDiscriminant )
import Cardano.Wallet.Gen
    ( genNatural )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index (..)
    , WalletKey (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..), unsafeGenerateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap (..), mkUnboundedAddressPoolGap )
import Cardano.Wallet.Primitive.AddressDiscovery.Shared
    ( constructAddressFromIx, isShared, keyHashFromAccXPubIx, newSharedState )
import Cardano.Wallet.Unsafe
    ( someDummyMnemonic )
import Data.Proxy
    ( Proxy (..) )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , arbitraryBoundedEnum
    , choose
    , property
    , suchThat
    , (.&&.)
    , (===)
    , (==>)
    )

import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "isShared for Catalyst" $ do
        it "address composed with our verification key should be discoverable if within pool gap"
            (property prop_addressWithScriptFromOurVerKeyIxIn)
        it "address composed with our verification key should not be discoverable if beyond pool gap"
            (property prop_addressWithScriptFromOurVerKeyIxBeyond)

prop_addressWithScriptFromOurVerKeyIxIn
    :: CatalystSharedState
    -> Index 'Soft 'ScriptK
    -> Property
prop_addressWithScriptFromOurVerKeyIxIn (CatalystSharedState accXPub' accIx' scriptTemplate' g) keyIx =
    fromIntegral (fromEnum keyIx) < threshold ==>
    keyIx' === keyIx .&&. keyHash' === keyHash
  where
    threshold =
        fromIntegral (fromEnum (minBound @(Index 'Soft 'ScriptK))) +
        getAddressPoolGap g
    (Right tag) = mkNetworkDiscriminant 1
    addr = constructAddressFromIx tag scriptTemplate' Nothing keyIx
    keyHash = keyHashFromAccXPubIx accXPub' keyIx
    sharedState = newSharedState accXPub' accIx' g scriptTemplate' Nothing
    ((Just (keyIx', keyHash')), _) = isShared addr sharedState

prop_addressWithScriptFromOurVerKeyIxBeyond
    :: CatalystSharedState
    -> Index 'Soft 'ScriptK
    -> Property
prop_addressWithScriptFromOurVerKeyIxBeyond (CatalystSharedState accXPub' accIx' scriptTemplate' g) keyIx =
    fromIntegral (fromEnum keyIx) > threshold ==>
    fst (isShared addr sharedState) === Nothing .&&.
    snd (isShared addr sharedState) === sharedState
  where
    threshold =
        fromIntegral (fromEnum (minBound @(Index 'Soft 'ScriptK))) +
        getAddressPoolGap g
    (Right tag) = mkNetworkDiscriminant 1
    addr = constructAddressFromIx tag scriptTemplate' Nothing keyIx
    sharedState = newSharedState accXPub' accIx' g scriptTemplate' Nothing


data CatalystSharedState = CatalystSharedState
    { accXPub :: ShelleyKey 'AccountK XPub
    , accIx :: Index 'Hardened 'AccountK
    , scriptTemplate :: ScriptTemplate
    , addrPoolGap :: AddressPoolGap
    } deriving (Eq, Show)

{-------------------------------------------------------------------------------
                                Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary CatalystSharedState where
    arbitrary = do
        let mw = someDummyMnemonic (Proxy @12)
        let rootXPrv = unsafeGenerateKeyFromSeed (mw, Nothing) mempty
        accIx' <- arbitrary
        let accXPub' = publicKey $ deriveAccountPrivateKey mempty rootXPrv accIx'
        slotUntil <- genNatural
        slotAfter <- genNatural `suchThat` (> slotUntil)
        let script' = RequireAllOf
                [ RequireSignatureOf (Cosigner 0)
                , RequireAnyOf [ ActiveUntilSlot slotUntil, ActiveFromSlot slotAfter] ]
        let scriptTemplate' =
                ScriptTemplate (Map.fromList [(Cosigner 0, getRawKey accXPub')]) script'
        CatalystSharedState accXPub' accIx' scriptTemplate' <$> arbitrary

instance Arbitrary AddressPoolGap where
    shrink _ = []
    arbitrary = mkUnboundedAddressPoolGap <$> choose (10, 20)

instance Arbitrary (Index 'Hardened depth) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'Soft depth) where
    shrink _ = []
    arbitrary = toEnum <$> choose (0, 100)
