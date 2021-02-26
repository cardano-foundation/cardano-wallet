{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.AddressDiscovery.SharedSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub )
import Cardano.Address.Script
    ( Cosigner (..), Script (..), ScriptTemplate (..) )
import Cardano.Wallet.Gen
    ( genNatural )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index (..)
    , NetworkDiscriminant (..)
    , WalletKey (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..), unsafeGenerateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDiscovery.Script
    ( constructAddressFromIx, keyHashFromAccXPubIx )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap (..), addresses, mkUnboundedAddressPoolGap )
import Cardano.Wallet.Primitive.AddressDiscovery.Shared
    ( SharedState (..), isShared, liftPaymentAddress, newSharedState )
import Cardano.Wallet.Primitive.Types.Address
    ( AddressState (..) )
import Cardano.Wallet.Unsafe
    ( someDummyMnemonic )
import Data.Maybe
    ( isJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Word
    ( Word32 )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , arbitraryBoundedEnum
    , choose
    , property
    , suchThat
    , (.&&.)
    , (===)
    , (==>)
    )
import Type.Reflection
    ( Typeable )

import qualified Data.List as L
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "isShared for Catalyst" $ do
        it "address composed with our verification key should be discoverable if within pool gap"
            (property (prop_addressWithScriptFromOurVerKeyIxIn @'Mainnet))
        it "address composed with our verification key must not be discoverable if beyond pool gap"
            (property (prop_addressWithScriptFromOurVerKeyIxBeyond @'Mainnet))
        it "first discovery enlarges ourAddresses and marks the address Used"
            (property (prop_addressDiscoveryMakesAddressUsed @'Mainnet))
{--
        it "multiple discovery of the same address is idempotent for state"
            (property prop_addressDoubleDiscovery)
        it "address composed with our verification key must not be discoverable with other account public key for the same key index"
            (property prop_addressDiscoveryImpossibleFromOtherAccXPub)
        it "address composed with our verification key must not be discoverable within the same account public key but different account index for same key index"
            (property prop_addressDiscoveryImpossibleFromOtherIxOfTheSameAccXPub)
        it "upon address discovery there is exact and consequitive number, amounting to  address pool gap number, of Unused addresses"
            (property prop_addressDiscoveryDoesNotChangeGapInvariance)
--}
prop_addressWithScriptFromOurVerKeyIxIn
    :: forall (n :: NetworkDiscriminant). Typeable n
    => CatalystSharedState
    -> Index 'Soft 'ScriptK
    -> Property
prop_addressWithScriptFromOurVerKeyIxIn (CatalystSharedState accXPub' accIx' scriptTemplate' g) keyIx =
    fromIntegral (fromEnum keyIx) < threshold g ==>
    keyIx' === keyIx .&&. keyHash' === keyHash
  where
    addr = constructAddressFromIx @n scriptTemplate' Nothing keyIx
    keyHash = keyHashFromAccXPubIx accXPub' keyIx
    sharedState = newSharedState @n accXPub' accIx' g scriptTemplate' Nothing
    ((Just (keyIx', keyHash')), _) = isShared @n addr sharedState

prop_addressWithScriptFromOurVerKeyIxBeyond
    :: forall (n :: NetworkDiscriminant). Typeable n
    => CatalystSharedState
    -> Index 'Soft 'ScriptK
    -> Property
prop_addressWithScriptFromOurVerKeyIxBeyond (CatalystSharedState accXPub' accIx' scriptTemplate' g) keyIx =
    fromIntegral (fromEnum keyIx) >= threshold g ==>
    fst (isShared @n addr sharedState) === Nothing .&&.
    snd (isShared @n addr sharedState) === sharedState
  where
    addr = constructAddressFromIx @n scriptTemplate' Nothing keyIx
    sharedState = newSharedState @n accXPub' accIx' g scriptTemplate' Nothing

prop_addressDiscoveryMakesAddressUsed
    :: forall (n :: NetworkDiscriminant).
    ( Typeable n )
    => CatalystSharedState
    -> Index 'Soft 'ScriptK
    -> Property
prop_addressDiscoveryMakesAddressUsed (CatalystSharedState accXPub' accIx' scriptTemplate' g) keyIx =
    fromIntegral (fromEnum keyIx) < threshold g ==>
    L.lookup addr ourAddrs === Just Used .&&.
    fromIntegral (L.length ourAddrs) === (fromIntegral (fromEnum ix + 1) + getAddressPoolGap g)
  where
    addr = constructAddressFromIx @n scriptTemplate' Nothing keyIx
    sharedState = newSharedState @n accXPub' accIx' g scriptTemplate' Nothing
    ((Just (ix,_)), sharedState') = isShared @n addr sharedState
    ourAddrs = addresses (liftPaymentAddress @n @ShelleyKey) $ sharedStateAddressPool sharedState'
{--
prop_addressDoubleDiscovery
    :: CatalystSharedState
    -> Index 'Soft 'ScriptK
    -> Property
prop_addressDoubleDiscovery (CatalystSharedState accXPub' accIx' scriptTemplate' g) keyIx =
    fromIntegral (fromEnum keyIx) < threshold g ==>
    isJust (fst sharedState') === True .&&.
    snd sharedState' === snd sharedState''
  where
    (Right tag) = mkNetworkDiscriminant 1
    addr = constructAddressFromIx tag scriptTemplate' Nothing keyIx
    sharedState = newSharedState accXPub' accIx' g scriptTemplate' Nothing
    sharedState' = isShared addr sharedState
    sharedState'' = isShared addr (snd sharedState')

prop_addressDiscoveryImpossibleFromOtherAccXPub
    :: CatalystSharedState
    -> Index 'Soft 'ScriptK
    -> ShelleyKey 'AccountK XPub
    -> Property
prop_addressDiscoveryImpossibleFromOtherAccXPub (CatalystSharedState _ accIx' scriptTemplate' g) keyIx accXPub' =
    fromIntegral (fromEnum keyIx) < threshold g ==>
    fst (isShared addr sharedState) === Nothing .&&.
    snd (isShared addr sharedState) === sharedState
  where
    (Right tag) = mkNetworkDiscriminant 1
    addr = constructAddressFromIx tag scriptTemplate' Nothing keyIx
    (ScriptTemplate _ script') = scriptTemplate'
    scriptTemplate'' = ScriptTemplate (Map.fromList [(Cosigner 0, getRawKey accXPub')]) script'
    sharedState = newSharedState accXPub' accIx' g scriptTemplate'' Nothing

prop_addressDiscoveryImpossibleFromOtherIxOfTheSameAccXPub
    :: CatalystSharedState
    -> Index 'Soft 'ScriptK
    -> (ShelleyKey 'RootK XPrv, Index 'Hardened 'AccountK)
    -> Property
prop_addressDiscoveryImpossibleFromOtherIxOfTheSameAccXPub (CatalystSharedState _ _ scriptTemplate' g) keyIx (rootXPrv, accIx') =
    fromIntegral (fromEnum keyIx) < threshold g ==>
    fst (isShared addr sharedState) === Nothing .&&.
    snd (isShared addr sharedState) === sharedState
  where
    (Right tag) = mkNetworkDiscriminant 1
    addr = constructAddressFromIx tag scriptTemplate' Nothing keyIx
    (ScriptTemplate _ script') = scriptTemplate'
    accXPub' = publicKey $ deriveAccountPrivateKey mempty rootXPrv accIx'
    scriptTemplate'' = ScriptTemplate (Map.fromList [(Cosigner 0, getRawKey accXPub')]) script'
    sharedState = newSharedState accXPub' accIx' g scriptTemplate'' Nothing

prop_addressDiscoveryDoesNotChangeGapInvariance
    :: CatalystSharedState
    -> Index 'Soft 'ScriptK
    -> Property
prop_addressDiscoveryDoesNotChangeGapInvariance (CatalystSharedState accXPub' accIx' scriptTemplate' g) keyIx =
    fromIntegral (fromEnum keyIx) < threshold g ==>
    fromIntegral (L.length mapOfConsecutiveUnused) === getAddressPoolGap g
  where
    (Right tag) = mkNetworkDiscriminant 1
    addr = constructAddressFromIx tag scriptTemplate' Nothing keyIx
    sharedState = newSharedState accXPub' accIx' g scriptTemplate' Nothing
    (_, sharedState') = isShared addr sharedState
    mapOfConsecutiveUnused =
        L.tail $
        L.dropWhile (\(_addr, (_ix, state)) -> state /= Used) $
        L.sortOn (\(_addr, (ix, _state)) -> ix) $
        Map.toList $ shareStateOurAddresses sharedState'
--}
data CatalystSharedState = CatalystSharedState
    { accXPub :: ShelleyKey 'AccountK XPub
    , accIx :: Index 'Hardened 'AccountK
    , scriptTemplate :: ScriptTemplate
    , addrPoolGap :: AddressPoolGap
    } deriving (Eq, Show)

threshold :: AddressPoolGap -> Word32
threshold g =
    fromIntegral (fromEnum (minBound @(Index 'Soft 'ScriptK))) +
    getAddressPoolGap g

{-------------------------------------------------------------------------------
                                Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary CatalystSharedState where
    arbitrary = do
        accIx' <- arbitrary
        accXPub' <- snd <$> genKeys accIx'
        slotUntil <- genNatural
        slotAfter <- genNatural `suchThat` (> slotUntil)
        let script' = RequireAllOf
                [ RequireSignatureOf (Cosigner 0)
                , RequireAnyOf [ ActiveUntilSlot slotUntil, ActiveFromSlot slotAfter] ]
        let scriptTemplate' =
                ScriptTemplate (Map.fromList [(Cosigner 0, getRawKey accXPub')]) script'
        CatalystSharedState accXPub' accIx' scriptTemplate' <$> arbitrary

instance Arbitrary (ShelleyKey 'AccountK XPub) where
    arbitrary = do
        accIx' <- arbitrary
        snd <$> genKeys accIx'

instance Arbitrary (ShelleyKey 'RootK XPrv) where
    arbitrary = do
        accIx' <- arbitrary
        fst <$> genKeys accIx'

instance Show XPrv where
    show = const "XPrv"

genKeys
    :: Index 'Hardened 'AccountK
    -> Gen (ShelleyKey 'RootK XPrv, ShelleyKey 'AccountK XPub)
genKeys accIx' = do
    let mw = someDummyMnemonic (Proxy @12)
    let rootXPrv = unsafeGenerateKeyFromSeed (mw, Nothing) mempty
    pure (rootXPrv, publicKey $ deriveAccountPrivateKey mempty rootXPrv accIx')

instance Arbitrary AddressPoolGap where
    shrink _ = []
    arbitrary = mkUnboundedAddressPoolGap <$> choose (10, 20)

instance Arbitrary (Index 'Hardened depth) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'Soft depth) where
    shrink _ = []
    arbitrary = toEnum <$> choose (0, 100)
