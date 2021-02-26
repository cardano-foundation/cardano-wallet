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
import Cardano.Wallet.Gen
    ( genScript )
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
        it "multiple discovery of the same address is idempotent for state"
            (property (prop_addressDoubleDiscovery @'Mainnet))
        it "address composed with our verification key must not be discoverable with other account public key for the same key index"
            (property (prop_addressDiscoveryImpossibleFromOtherAccXPub @'Mainnet))
        it "address composed with our verification key must not be discoverable within the same mnemonic when other of its account is used"
            (property (prop_addressDiscoveryImpossibleFromOtherAccountOfTheSameRootXPrv @'Mainnet))
        it "address composed with our verification key must not be discoverable within proper account is script changes structure"
            (property (prop_addressDiscoveryImpossibleWithinAccountButDifferentScript @'Mainnet))
        it "upon address discovery there is exact and consecutive number of Unused indices that amounts to the address pool gap number"
            (property (prop_addressDiscoveryDoesNotChangeGapInvariance @'Mainnet))

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
    :: forall (n :: NetworkDiscriminant). Typeable n
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

prop_addressDoubleDiscovery
    :: forall (n :: NetworkDiscriminant). Typeable n
    => CatalystSharedState
    -> Index 'Soft 'ScriptK
    -> Property
prop_addressDoubleDiscovery (CatalystSharedState accXPub' accIx' scriptTemplate' g) keyIx =
    fromIntegral (fromEnum keyIx) < threshold g ==>
    isJust (fst sharedState') === True .&&.
    snd sharedState' === snd sharedState''
  where
    addr = constructAddressFromIx @n scriptTemplate' Nothing keyIx
    sharedState = newSharedState @n accXPub' accIx' g scriptTemplate' Nothing
    sharedState' = isShared @n addr sharedState
    sharedState'' = isShared @n addr (snd sharedState')

prop_addressDiscoveryImpossibleFromOtherAccXPub
    :: forall (n :: NetworkDiscriminant). Typeable n
    => CatalystSharedState
    -> Index 'Soft 'ScriptK
    -> ShelleyKey 'AccountK XPub
    -> Property
prop_addressDiscoveryImpossibleFromOtherAccXPub (CatalystSharedState _ accIx' scriptTemplate' g) keyIx accXPub' =
    fromIntegral (fromEnum keyIx) < threshold g ==>
    fst (isShared addr sharedState) === Nothing .&&.
    snd (isShared addr sharedState) === sharedState
  where
    addr = constructAddressFromIx @n scriptTemplate' Nothing keyIx
    (ScriptTemplate _ script') = scriptTemplate'
    scriptTemplate'' = ScriptTemplate (Map.fromList [(Cosigner 0, getRawKey accXPub')]) script'
    sharedState = newSharedState @n accXPub' accIx' g scriptTemplate'' Nothing

prop_addressDiscoveryImpossibleFromOtherAccountOfTheSameRootXPrv
    :: forall (n :: NetworkDiscriminant). Typeable n
    => CatalystSharedState
    -> Index 'Soft 'ScriptK
    -> (ShelleyKey 'RootK XPrv, Index 'Hardened 'AccountK, Index 'Hardened 'AccountK)
    -> Property
prop_addressDiscoveryImpossibleFromOtherAccountOfTheSameRootXPrv (CatalystSharedState _ _ scriptTemplate' g) keyIx (rootXPrv, accIx', accIx'') =
    fromIntegral (fromEnum keyIx) < threshold g ==>
    fst (isShared addr sharedState) === Nothing .&&.
    snd (isShared addr sharedState) === sharedState
  where
    accXPub' = publicKey $ deriveAccountPrivateKey mempty rootXPrv accIx'
    accXPub'' = publicKey $ deriveAccountPrivateKey mempty rootXPrv accIx''
    (ScriptTemplate _ script') = scriptTemplate'
    scriptTemplate'' = ScriptTemplate (Map.fromList [(Cosigner 0, getRawKey accXPub')]) script'
    scriptTemplate''' = ScriptTemplate (Map.fromList [(Cosigner 0, getRawKey accXPub'')]) script'
    sharedState = newSharedState @n accXPub'' accIx'' g scriptTemplate'' Nothing
    addr = constructAddressFromIx @n scriptTemplate''' Nothing keyIx

prop_addressDiscoveryImpossibleWithinAccountButDifferentScript
    :: forall (n :: NetworkDiscriminant). Typeable n
    => CatalystSharedState
    -> Index 'Soft 'ScriptK
    -> OneCosignerScript
    -> Property
prop_addressDiscoveryImpossibleWithinAccountButDifferentScript (CatalystSharedState accXPub' accIx' scriptTemplate' g) keyIx (OneCosignerScript script') =
    fromIntegral (fromEnum keyIx) < threshold g ==>
    fst (isShared addr sharedState) === Nothing .&&.
    snd (isShared addr sharedState) === sharedState
  where
    (ScriptTemplate cosignerXpubs _) = scriptTemplate'
    scriptTemplate'' = ScriptTemplate cosignerXpubs script'
    sharedState = newSharedState @n accXPub' accIx' g scriptTemplate' Nothing
    addr = constructAddressFromIx @n scriptTemplate'' Nothing keyIx

prop_addressDiscoveryDoesNotChangeGapInvariance
    :: forall (n :: NetworkDiscriminant). Typeable n
    => CatalystSharedState
    -> Index 'Soft 'ScriptK
    -> Property
prop_addressDiscoveryDoesNotChangeGapInvariance (CatalystSharedState accXPub' accIx' scriptTemplate' g) keyIx =
    fromIntegral (fromEnum keyIx) < threshold g ==>
    fromIntegral (L.length mapOfConsecutiveUnused) === getAddressPoolGap g
  where
    addr = constructAddressFromIx @n scriptTemplate' Nothing keyIx
    sharedState = newSharedState @n accXPub' accIx' g scriptTemplate' Nothing
    (_, sharedState') = isShared @n addr sharedState
    mapOfConsecutiveUnused =
        L.tail $
        L.dropWhile (\(_addr, state) -> state /= Used) $
        addresses (liftPaymentAddress @n @ShelleyKey) $ sharedStateAddressPool sharedState'

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

newtype OneCosignerScript = OneCosignerScript
    { unScript :: Script Cosigner } deriving (Show, Eq)

instance Arbitrary OneCosignerScript where
    arbitrary = OneCosignerScript <$> genScript [Cosigner 0]

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
