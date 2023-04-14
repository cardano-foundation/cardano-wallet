{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
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
    ( Cosigner (..)
    , Script (..)
    , ScriptTemplate (..)
    , ValidationLevel (..)
    , validateScriptTemplate
    )
import Cardano.Wallet.Gen
    ( genNatural, genScript, genScriptTemplate )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index (..)
    , KeyFingerprint
    , Role (..)
    , WalletKey (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation.Shared
    ( unsafeGenerateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDerivation.SharedKey
    ( SharedKey (..), constructAddressFromIx )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..), KnownAddresses (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap (..), defaultAddressPoolGap, mkUnboundedAddressPoolGap )
import Cardano.Wallet.Primitive.AddressDiscovery.Shared
    ( Readiness (..)
    , SharedAddressPool (..)
    , SharedAddressPools (..)
    , SharedState (..)
    , isShared
    , liftPaymentAddress
    , mkSharedStateFromAccountXPub
    , mkSharedStateFromRootXPrv
    )
import Cardano.Wallet.Primitive.Passphrase.Gen
    ( genEncryptionPassphrase )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..), AddressState (..) )
import Cardano.Wallet.Read.NetworkId
    ( HasSNetworkId, NetworkDiscriminant (..) )
import Cardano.Wallet.Unsafe
    ( someDummyMnemonic )
import Control.Arrow
    ( first )
import Data.Either
    ( isRight )
import Data.Maybe
    ( isJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Word
    ( Word32, Word8 )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , arbitraryBoundedEnum
    , choose
    , elements
    , property
    , suchThat
    , (.&&.)
    , (===)
    , (==>)
    )

import qualified Cardano.Wallet.Address.Pool as AddressPool
import qualified Data.ByteString as BS
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

    describe "KnownAddresses" $ do
        it "addresses with wrong prefixes and our credentials are discovered via isOurs" $ do
            (property prop_oursUnexpectedPrefix)

prop_addressWithScriptFromOurVerKeyIxIn
    :: forall (n :: NetworkDiscriminant). HasSNetworkId n
    => CatalystSharedState
    -> Index 'Soft 'CredFromScriptK
    -> Property
prop_addressWithScriptFromOurVerKeyIxIn (CatalystSharedState accXPub' accIx' pTemplate' dTemplate' g) keyIx =
    preconditions keyIx g dTemplate' ==>
    keyIx' === keyIx
  where
    addr = constructAddressFromIx @n UtxoExternal pTemplate' dTemplate' keyIx
    sharedState = mkSharedStateFromAccountXPub @n accXPub' accIx' g pTemplate' dTemplate'
    (Just (keyIx', _), _) = isShared @n addr sharedState

prop_addressWithScriptFromOurVerKeyIxBeyond
    :: forall (n :: NetworkDiscriminant). HasSNetworkId n
    => CatalystSharedState
    -> Index 'Soft 'CredFromScriptK
    -> Property
prop_addressWithScriptFromOurVerKeyIxBeyond (CatalystSharedState accXPub' accIx' pTemplate' dTemplate' g) keyIx =
    fromIntegral (fromEnum keyIx) >= threshold g ==>
    fst (isShared @n addr sharedState) === Nothing .&&.
    snd (isShared @n addr sharedState) === sharedState
  where
    addr = constructAddressFromIx @n UtxoExternal pTemplate' dTemplate' keyIx
    sharedState = mkSharedStateFromAccountXPub @n accXPub' accIx' g pTemplate' dTemplate'

getAddrPool
    :: SharedState n k
    -> AddressPool.Pool (KeyFingerprint "payment" k) (Index 'Soft 'CredFromScriptK)
getAddrPool st = case ready st of
    Active (SharedAddressPools (SharedAddressPool pool) _ _) -> pool
    Pending -> error "expected active state"

prop_addressDiscoveryMakesAddressUsed
    :: forall (n :: NetworkDiscriminant). HasSNetworkId n
    => CatalystSharedState
    -> Index 'Soft 'CredFromScriptK
    -> Property
prop_addressDiscoveryMakesAddressUsed (CatalystSharedState accXPub' accIx' pTemplate' dTemplate' g) keyIx =
    preconditions keyIx g dTemplate' ==>
    (snd <$> Map.lookup addr ourAddrs) === Just Used .&&.
    fromIntegral (Map.size ourAddrs) === (fromIntegral (fromEnum ix + 1) + getAddressPoolGap g)
  where
    sharedState = mkSharedStateFromAccountXPub @n accXPub' accIx' g pTemplate' dTemplate'
    addr = AddressPool.addressFromIx (getAddrPool sharedState) keyIx
    (Just (ix, _), sharedState') = isShared @n (liftPaymentAddress @n addr) sharedState
    ourAddrs = AddressPool.addresses (getAddrPool sharedState')

prop_addressDoubleDiscovery
    :: forall (n :: NetworkDiscriminant). HasSNetworkId n
    => CatalystSharedState
    -> Index 'Soft 'CredFromScriptK
    -> Property
prop_addressDoubleDiscovery (CatalystSharedState accXPub' accIx' pTemplate' dTemplate' g) keyIx =
    preconditions keyIx g dTemplate' ==>
    isJust (fst sharedState') === True .&&.
    snd sharedState' === snd sharedState''
  where
    addr = constructAddressFromIx @n UtxoExternal pTemplate' dTemplate' keyIx
    sharedState = mkSharedStateFromAccountXPub @n accXPub' accIx' g pTemplate' dTemplate'
    sharedState' = isShared @n addr sharedState
    sharedState'' = isShared @n addr (snd sharedState')

prop_addressDiscoveryImpossibleFromOtherAccXPub
    :: forall (n :: NetworkDiscriminant). HasSNetworkId n
    => CatalystSharedState
    -> Index 'Soft 'CredFromScriptK
    -> SharedKey 'AccountK XPub
    -> Property
prop_addressDiscoveryImpossibleFromOtherAccXPub (CatalystSharedState _ accIx' pTemplate' dTemplate' g) keyIx accXPub' =
    preconditions keyIx g dTemplate' ==>
    fst (isShared addr sharedState) === Nothing .&&.
    snd (isShared addr sharedState) === sharedState
  where
    addr = constructAddressFromIx @n UtxoExternal pTemplate' dTemplate' keyIx
    (ScriptTemplate _ script') = pTemplate'
    pTemplate'' = ScriptTemplate (Map.fromList [(Cosigner 0, getRawKey accXPub')]) script'
    sharedState = mkSharedStateFromAccountXPub @n accXPub' accIx' g pTemplate'' dTemplate'

prop_addressDiscoveryImpossibleFromOtherAccountOfTheSameRootXPrv
    :: forall (n :: NetworkDiscriminant). HasSNetworkId n
    => CatalystSharedState
    -> Index 'Soft 'CredFromScriptK
    -> (SharedKey 'RootK XPrv, Index 'Hardened 'AccountK, Index 'Hardened 'AccountK)
    -> Property
prop_addressDiscoveryImpossibleFromOtherAccountOfTheSameRootXPrv (CatalystSharedState _ _ pTemplate' dTemplate' g) keyIx (rootXPrv, accIx', accIx'') =
    preconditions keyIx g dTemplate' ==>
    fst (isShared addr sharedState) === Nothing .&&.
    snd (isShared addr sharedState) === sharedState
  where
    accXPub' = publicKey $ deriveAccountPrivateKey mempty rootXPrv accIx'
    accXPub'' = publicKey $ deriveAccountPrivateKey mempty rootXPrv accIx''
    (ScriptTemplate _ script') = pTemplate'
    pTemplate'' = ScriptTemplate (Map.fromList [(Cosigner 0, getRawKey accXPub')]) script'
    pTemplate''' = ScriptTemplate (Map.fromList [(Cosigner 0, getRawKey accXPub'')]) script'
    sharedState = mkSharedStateFromAccountXPub @n accXPub'' accIx'' g pTemplate'' dTemplate'
    addr = constructAddressFromIx @n UtxoExternal pTemplate''' dTemplate' keyIx

prop_addressDiscoveryImpossibleWithinAccountButDifferentScript
    :: forall (n :: NetworkDiscriminant). HasSNetworkId n
    => CatalystSharedState
    -> Index 'Soft 'CredFromScriptK
    -> OneCosignerScript
    -> Property
prop_addressDiscoveryImpossibleWithinAccountButDifferentScript (CatalystSharedState accXPub' accIx' pTemplate' dTemplate' g) keyIx (OneCosignerScript script') =
    preconditions keyIx g dTemplate' ==>
    fst (isShared addr sharedState) === Nothing .&&.
    snd (isShared addr sharedState) === sharedState
  where
    (ScriptTemplate cosignerXpubs _) = pTemplate'
    pTemplate'' = ScriptTemplate cosignerXpubs script'
    sharedState = mkSharedStateFromAccountXPub @n accXPub' accIx' g pTemplate' dTemplate'
    addr = constructAddressFromIx @n UtxoExternal pTemplate'' dTemplate' keyIx

prop_addressDiscoveryDoesNotChangeGapInvariance
    :: forall (n :: NetworkDiscriminant). HasSNetworkId n
    => CatalystSharedState
    -> Index 'Soft 'CredFromScriptK
    -> Property
prop_addressDiscoveryDoesNotChangeGapInvariance (CatalystSharedState accXPub' accIx' pTemplate' dTemplate' g) keyIx =
    preconditions keyIx g dTemplate' ==>
    fromIntegral (L.length mapOfConsecutiveUnused) === getAddressPoolGap g
  where
    sharedState = mkSharedStateFromAccountXPub @n accXPub' accIx' g pTemplate' dTemplate'
    addr = AddressPool.addressFromIx (getAddrPool sharedState) keyIx
    (_, sharedState') = isShared @n (liftPaymentAddress @n addr) sharedState
    mapOfConsecutiveUnused
        = L.tail
        . L.dropWhile (== Unused)
        . L.map snd
        . L.sortOn fst
        . Map.elems . AddressPool.addresses
        $ getAddrPool sharedState'

preconditions
    :: Index 'Soft 'CredFromScriptK
    -> AddressPoolGap
    -> Maybe ScriptTemplate
    -> Bool
preconditions keyIx g dTemplate' =
    fromIntegral (fromEnum keyIx) < threshold g && templateValidated
  where
    templateValidated = case dTemplate' of
        Nothing -> True
        Just t -> isRight (validateScriptTemplate RecommendedValidation t)

threshold :: AddressPoolGap -> Word32
threshold g =
    fromIntegral (fromEnum (minBound @(Index 'Soft 'CredFromScriptK))) +
    getAddressPoolGap g

data CatalystSharedState = CatalystSharedState
    { accXPub :: SharedKey 'AccountK XPub
    , accIx :: Index 'Hardened 'AccountK
    , pTemplate :: ScriptTemplate
    , dTemplate :: Maybe ScriptTemplate
    , addrPoolGap :: AddressPoolGap
    } deriving (Eq, Show)

prop_oursUnexpectedPrefix
    :: SharedState 'Mainnet SharedKey
    -> UnexpectedPrefix
    -> Property
prop_oursUnexpectedPrefix s prefix =
    let
        (Address addr, _, _) = head $ knownAddresses s
        addr' = BS.cons (unWord8 prefix) (BS.tail addr)
    in
        first isJust (isOurs (Address addr') s) === (False, s)

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
        let pTemplate' =
                ScriptTemplate (Map.fromList [(Cosigner 0, getRawKey accXPub')]) script'
        (OneCosignerScript otherScript) <- arbitrary
        let _otherTemplate = ScriptTemplate (Map.fromList [(Cosigner 0, getRawKey accXPub')]) otherScript
        --dTemplate' <- elements [Nothing, Just pTemplate', Just otherTemplate]
        dTemplate' <- elements [Nothing, Just pTemplate']
        CatalystSharedState accXPub' accIx' pTemplate' dTemplate'  <$> arbitrary

instance Arbitrary (SharedKey 'AccountK XPub) where
    arbitrary = do
        accIx' <- arbitrary
        snd <$> genKeys accIx'

newtype OneCosignerScript = OneCosignerScript
    { unScript :: Script Cosigner } deriving (Show, Eq)

instance Arbitrary OneCosignerScript where
    arbitrary = OneCosignerScript <$> genScript [Cosigner 0]

instance Arbitrary (SharedKey 'RootK XPrv) where
    arbitrary = do
        accIx' <- arbitrary
        fst <$> genKeys accIx'

instance Show XPrv where
    show = const "XPrv"

genKeys
    :: Index 'Hardened 'AccountK
    -> Gen (SharedKey 'RootK XPrv, SharedKey 'AccountK XPub)
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

newtype UnexpectedPrefix = UnexpectedPrefix {unWord8 :: Word8}
    deriving (Eq, Show)

instance Arbitrary UnexpectedPrefix where
    arbitrary = do
        let baseAddr = 0b00110001       -- scripthash; scripthash, mainnet
            enterpriseAddr = 0b01110001 -- scripthash, mainnet
            rewardAcct = 0b11110001     -- scripthash, mainnet
            validPrefixesMainnet = [baseAddr, enterpriseAddr, rewardAcct]
        UnexpectedPrefix <$> arbitrary `suchThat` (`notElem` validPrefixesMainnet)

instance Arbitrary (SharedState 'Mainnet SharedKey) where
    arbitrary = do
        rootXPrv <- arbitrary
        pwd <- genEncryptionPassphrase
        accIx' <- arbitrary
        scriptTemplate <- genScriptTemplate
        pure $ mkSharedStateFromRootXPrv (rootXPrv, pwd) accIx'
            defaultAddressPoolGap scriptTemplate Nothing
