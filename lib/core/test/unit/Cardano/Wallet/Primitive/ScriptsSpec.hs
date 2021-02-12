{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.ScriptsSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPub )
import Cardano.Address.Script
    ( KeyHash (..), Script (..), ScriptHash, toScriptHash )
import Cardano.Wallet.Gen
    ( genScript )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , NetworkDiscriminant (..)
    , SoftDerivation
    , WalletKey (..)
    , deriveVerificationKey
    , hashVerificationKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..) )
import Cardano.Wallet.Primitive.AddressDerivationSpec
    ()
import Cardano.Wallet.Primitive.AddressDiscovery.Delegation
    ( DelegationState, mkEmptyDelegationState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap (..)
    , DerivationPrefix (..)
    , SeqState (..)
    , VerificationKeyPool (..)
    , coinTypeAda
    , defaultAddressPoolGap
    , emptyPendingIxs
    , getAddressPoolGap
    , mkAddressPool
    , newVerificationKeyPool
    , purposeCIP1852
    , purposeCIP1852
    , verPoolIndexedKeys
    , verPoolKnownScripts
    )
import Cardano.Wallet.Primitive.Scripts
    ( isShared, retrieveAllVerKeyHashes )
import Cardano.Wallet.Primitive.Types.Address
    ( AddressState (..) )
import Cardano.Wallet.Unsafe
    ( unsafeXPub )
import Data.Map.Strict
    ( Map )
import Data.Ord
    ( Down (..) )
import Data.Set
    ( Set )
import Data.Word
    ( Word32 )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , choose
    , property
    , vectorOf
    , (.&&.)
    , (===)
    , (==>)
    )

import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "isShared" $ do
        it "script composed with all of our verification keys should discover them all"
            (property prop_scriptFromOurVerKeys)
        it "script composed with not our verification keys should not be discovered"
            (property prop_scriptFromNotOurVerKeys)
        it "the same script discovered twice should have the same knownScripts imprint"
            (property prop_scriptDiscoveredTwice)
        it "knownScripts of the sequential state is populated properly"
            (property prop_scriptUpdatesStateProperly)
        it "scripts with our verification keys are discovered properly"
            (property prop_scriptsDiscovered)
        it "scripts with two account key verification keys are discovered properly"
            (property prop_scriptDiscoveredByTwo)
        it "discovering our verification keys make them mark Used"
            (property prop_markingDiscoveredVerKeys)
        it "discovering works after pool extension"
            (property prop_poolExtension)
        it "before and after discovery pool number of last consequitive Unused keys stays the same"
            (property prop_unusedVerKeysConstant)
        it "discovered verification keys in scripts are consistent between knownScripts and indexedKeys"
            (property prop_verKeysConsistent)

prop_scriptFromOurVerKeys
    :: AccountXPubWithScripts
    -> Property
prop_scriptFromOurVerKeys (AccountXPubWithScripts accXPub' scripts') = do
    let (script:_) = scripts'
    let scriptKeyHashes = retrieveAllVerKeyHashes script
    let seqState = initializeState accXPub'
    let (ourSharedKeys, _) = isShared script seqState
    L.sort (L.nub $ map hashVerificationKey ourSharedKeys) ===
        L.sort (L.nub scriptKeyHashes)

prop_scriptFromNotOurVerKeys
    :: ShelleyKey 'AccountK XPub
    -> AccountXPubWithScripts
    -> Property
prop_scriptFromNotOurVerKeys accXPub' (AccountXPubWithScripts _accXPub scripts') = do
    let (script:_) = scripts'
    let seqState = initializeState accXPub'
    let (ourSharedKeys, _) = isShared script seqState
    ourSharedKeys === []

prop_scriptUpdatesStateProperly
    :: AccountXPubWithScripts
    -> Property
prop_scriptUpdatesStateProperly (AccountXPubWithScripts accXPub' scripts') = do
    let (script:_) = scripts'
    let sciptKeyHashes = retrieveAllVerKeyHashes script
    let seqState = initializeState accXPub'
    let (_, seqState') = isShared script seqState
    let expected =
            if null sciptKeyHashes then
                Nothing
            else Just (Set.fromList (L.nub sciptKeyHashes))
    scriptKeyHashesInMap script accXPub' seqState' === expected

prop_scriptDiscoveredTwice
    :: AccountXPubWithScripts
    -> Property
prop_scriptDiscoveredTwice (AccountXPubWithScripts accXPub' scripts') = do
    let (script:_) = scripts'
    let seqState = initializeState accXPub'
    let (_, seqState') = isShared script seqState
    let (_, seqState'') = isShared script seqState'
    seqState' === seqState''

prop_scriptsDiscovered
    :: AccountXPubWithScripts
    -> Property
prop_scriptsDiscovered (AccountXPubWithScripts accXPub' scripts') = do
    let seqState0 = initializeState accXPub'
    let seqState = foldr (\script s -> snd $ isShared script s) seqState0 scripts'
    let scriptHashes = Set.fromList $ Map.keys $ getKnownScripts seqState
    let scriptsWithKeyHashes = filter (not . null . retrieveAllVerKeyHashes) scripts'
    scriptHashes === Set.fromList (map toScriptHash scriptsWithKeyHashes)

prop_scriptDiscoveredByTwo
    :: TwoAccountXPubsWithScript
    -> Property
prop_scriptDiscoveredByTwo (TwoAccountXPubsWithScript accXPub' accXPub'' script) = do
    let seqState0' = initializeState accXPub'
    let seqState0'' = initializeState accXPub''
    let (_, seqState') = isShared script seqState0'
    let (_, seqState'') = isShared script seqState0''
    let sciptKeyHashes = retrieveAllVerKeyHashes script
    let scriptKeyHashes' = scriptKeyHashesInMap script accXPub' seqState'
    let scriptKeyHashes'' = scriptKeyHashesInMap script accXPub'' seqState''
    let expected =
            if null sciptKeyHashes then
                Nothing
            else Just (Set.fromList sciptKeyHashes)
    (scriptKeyHashes' <> scriptKeyHashes'') === expected

prop_markingDiscoveredVerKeys
    :: AccountXPubWithScripts
    -> Property
prop_markingDiscoveredVerKeys (AccountXPubWithScripts accXPub' scripts') = do
    let (script:_) = scripts'
    let sciptKeyHashes = retrieveAllVerKeyHashes script
    let seqState = initializeState accXPub'
    let (_, SeqState _ _ _ _ verKeyPool _) =
            isShared script seqState
    let ourKeys = verPoolIndexedKeys verKeyPool
    let discoveredKeyMap =
            Map.filterWithKey (\k _ -> k `elem` sciptKeyHashes) ourKeys
    let addressStatesToCheck =
            map (\(_, (_, isUsed)) -> isUsed) $ Map.toList discoveredKeyMap
    L.all (== Used) addressStatesToCheck === True

prop_poolExtension
    :: AccountXPubWithScriptExtension
    -> Property
prop_poolExtension (AccountXPubWithScriptExtension accXPub' scripts') =
    not (any (null . retrieveAllVerKeyHashes) scripts') ==>
    scriptHashes == Set.fromList (map toScriptHash scripts') .&&.
        seqState3 == seqState0
  where
    seqState0 = initializeState accXPub'
    [script1,script2] = scripts'
    seqState1 = snd $ isShared script1 seqState0
    seqState2 = snd $ isShared script2 seqState1
    seqState3 = snd $ isShared script2 seqState0
    scriptHashes = Set.fromList $ Map.keys $ getKnownScripts seqState2

prop_unusedVerKeysConstant
    :: AccountXPubWithScripts
    -> Property
prop_unusedVerKeysConstant (AccountXPubWithScripts accXPub' scripts') = do
    let (script:_) = scripts'
    let seqState = initializeState accXPub'
    let (_, seqState') = isShared script seqState
    let lastUnusedKeys =
            L.takeWhile (\(_,isUsed) -> isUsed == Unused) .
            L.sortOn (Down . fst) .
            Map.elems .
            getVerKeyMap
    L.length (lastUnusedKeys seqState) === L.length (lastUnusedKeys seqState')

prop_verKeysConsistent
    :: AccountXPubWithScripts
    -> Property
prop_verKeysConsistent (AccountXPubWithScripts accXPub' scripts') = do
    let seqState0 = initializeState accXPub'
    let seqState = foldr (\script s -> snd $ isShared script s) seqState0 scripts'
    let verKeyIxs = L.nub $ concat $ Map.elems $ getKnownScripts seqState
    let verKeyHashes =  Map.keys $ Map.filter (\(_, isUsed) -> isUsed == Used) $
            getVerKeyMap seqState
    Set.fromList verKeyHashes  === Set.fromList (map (deriveKeyHash accXPub') verKeyIxs)

data AccountXPubWithScripts = AccountXPubWithScripts
    { accXPub :: ShelleyKey 'AccountK XPub
    , scripts :: [Script KeyHash]
    } deriving (Eq, Show)

data AccountXPubWithScriptExtension = AccountXPubWithScriptExtension
    { accXPub :: ShelleyKey 'AccountK XPub
    , scripts :: [Script KeyHash]
    } deriving (Eq, Show)

data TwoAccountXPubsWithScript = TwoAccountXPubsWithScript
    { accXPub1 :: ShelleyKey 'AccountK XPub
    , accXPub2 :: ShelleyKey 'AccountK XPub
    , scripts :: Script KeyHash
    } deriving (Eq, Show)

defaultPrefix :: DerivationPrefix
defaultPrefix = DerivationPrefix
    ( purposeCIP1852
    , coinTypeAda
    , minBound
    )

deriveKeyHash
    :: (SoftDerivation k, WalletKey k)
    => k 'AccountK XPub
    -> Index 'Soft 'ScriptK
    -> KeyHash
deriveKeyHash accXPub' =
    hashVerificationKey . (deriveVerificationKey accXPub')

dummyDelegSt :: DelegationState ShelleyKey
dummyDelegSt = mkEmptyDelegationState $
    ShelleyKey $ unsafeXPub $ B8.replicate 64 '0'

initializeState
    :: ShelleyKey 'AccountK XPub
    -> SeqState 'Mainnet ShelleyKey
initializeState accXPub' =
    let intPool = mkAddressPool accXPub' defaultAddressPoolGap []
        extPool = mkAddressPool accXPub' defaultAddressPoolGap []
        sPool = newVerificationKeyPool accXPub' defaultAddressPoolGap
    in SeqState intPool extPool emptyPendingIxs defaultPrefix sPool dummyDelegSt

getKnownScripts
    :: SeqState 'Mainnet ShelleyKey
    -> Map ScriptHash [Index 'Soft 'ScriptK]
getKnownScripts (SeqState _ _ _ _ verKeyPool _) =
    verPoolKnownScripts verKeyPool

getVerKeyMap
    :: SeqState 'Mainnet ShelleyKey
    -> Map KeyHash (Index 'Soft 'ScriptK, AddressState)
getVerKeyMap (SeqState _ _ _ _ verKeyPool _) =
    verPoolIndexedKeys verKeyPool

scriptKeyHashesInMap
    :: Script KeyHash
    -> ShelleyKey 'AccountK XPub
    -> SeqState 'Mainnet ShelleyKey
    -> Maybe (Set KeyHash)
scriptKeyHashesInMap script' accXPub' s =
    Set.fromList . map (deriveKeyHash accXPub') <$>
    Map.lookup (toScriptHash script') (getKnownScripts s)

{-------------------------------------------------------------------------------
                                Arbitrary Instances
-------------------------------------------------------------------------------}

prepareVerKeys
    :: ShelleyKey 'AccountK XPub
    -> [Word32]
    -> [ShelleyKey 'ScriptK XPub]
prepareVerKeys accXPub' =
    let minIndex = getIndex @'Soft minBound
    in map (\ix -> deriveVerificationKey accXPub' (toEnum (fromInteger $ toInteger $ minIndex + ix)))

instance Arbitrary AccountXPubWithScripts where
    arbitrary = do
        accXPub' <- arbitrary
        let g = getAddressPoolGap defaultAddressPoolGap
        kNum <- choose (2, g - 1)
        let verKeyHashes = map hashVerificationKey (prepareVerKeys accXPub' [0 .. kNum])
        scriptsNum <- choose (1,10)
        AccountXPubWithScripts accXPub' <$> vectorOf scriptsNum (genScript verKeyHashes)

instance Arbitrary AccountXPubWithScriptExtension where
    arbitrary = do
        accXPub' <- arbitrary

        -- the first script is expected to trigger extension to scriptPool
        let g = getAddressPoolGap defaultAddressPoolGap
        scriptTipping <-
            genScript (hashVerificationKey <$> (prepareVerKeys accXPub' [g - 1]))

        -- the next script is using extended indices that were not possible to be discovered
        -- earlier, but are supposed to be discovered now
        kNum <- choose (2,8)
        let verKeysNext = prepareVerKeys accXPub' [g .. g + kNum]
        scriptNext <- genScript (hashVerificationKey <$> verKeysNext)

        pure $ AccountXPubWithScriptExtension accXPub' [scriptTipping, scriptNext]

instance Arbitrary TwoAccountXPubsWithScript where
    arbitrary = do
        accXPub1' <- arbitrary
        accXPub2' <- arbitrary
        let g = getAddressPoolGap defaultAddressPoolGap
        kNum <- choose (1, g - 1)
        let verKeyHashes accXPub' =
                map hashVerificationKey (prepareVerKeys accXPub' [0 .. kNum])
        let bothVerKeyHashes =
                verKeyHashes accXPub1' ++ verKeyHashes accXPub2'
        TwoAccountXPubsWithScript accXPub1' accXPub2' <$> genScript bothVerKeyHashes
