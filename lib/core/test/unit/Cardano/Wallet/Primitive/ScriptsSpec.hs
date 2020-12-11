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
import Cardano.Mnemonic
    ( SomeMnemonic (..) )
import Cardano.Wallet.Gen
    ( genMnemonic )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    , SoftDerivation
    , WalletKey (..)
    , deriveVerificationKey
    , hashVerificationKey
    , preparePassphrase
    )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..), generateKeyFromSeed )
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
import Cardano.Wallet.Primitive.Types
    ( PassphraseScheme (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( AddressState (..) )
import Cardano.Wallet.Unsafe
    ( unsafeXPub )
import Control.Monad
    ( replicateM )
import Data.Map.Strict
    ( Map )
import Data.Ord
    ( Down (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Set
    ( Set )
import Data.Word
    ( Word32 )
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
    , (.&&.)
    , (===)
    )

import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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
    scriptKeyHashesInMap script accXPub' seqState'
        === Just (Set.fromList (L.nub sciptKeyHashes))

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
    scriptHashes === Set.fromList (map toScriptHash scripts')

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
    (scriptKeyHashes' <> scriptKeyHashes'')
        === Just (Set.fromList (L.nub sciptKeyHashes))

prop_markingDiscoveredVerKeys
    :: AccountXPubWithScripts
    -> Property
prop_markingDiscoveredVerKeys (AccountXPubWithScripts accXPub' scripts') = do
    let (script:_) = scripts'
    let sciptKeyHashes = retrieveAllVerKeyHashes script
    let seqState = initializeState accXPub'
    let (_, SeqState _ _ _ _ _ verKeyPool) =
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
prop_poolExtension (AccountXPubWithScriptExtension accXPub' scripts') = do
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
    , scripts :: [Script]
    } deriving (Eq, Show)

data AccountXPubWithScriptExtension = AccountXPubWithScriptExtension
    { accXPub :: ShelleyKey 'AccountK XPub
    , scripts :: [Script]
    } deriving (Eq, Show)

data TwoAccountXPubsWithScript = TwoAccountXPubsWithScript
    { accXPub1 :: ShelleyKey 'AccountK XPub
    , accXPub2 :: ShelleyKey 'AccountK XPub
    , scripts :: Script
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

dummyRewardAccount :: ShelleyKey 'AddressK XPub
dummyRewardAccount = ShelleyKey $ unsafeXPub $ B8.replicate 64 '0'

initializeState
    :: ShelleyKey 'AccountK XPub
    -> SeqState 'Mainnet ShelleyKey
initializeState accXPub' =
    let intPool = mkAddressPool accXPub' defaultAddressPoolGap []
        extPool = mkAddressPool accXPub' defaultAddressPoolGap []
        sPool = newVerificationKeyPool accXPub' defaultAddressPoolGap
    in SeqState intPool extPool emptyPendingIxs dummyRewardAccount defaultPrefix sPool

getKnownScripts
    :: SeqState 'Mainnet ShelleyKey
    -> Map ScriptHash [Index 'Soft 'ScriptK]
getKnownScripts (SeqState _ _ _ _ _ verKeyPool) =
    verPoolKnownScripts verKeyPool

getVerKeyMap
    :: SeqState 'Mainnet ShelleyKey
    -> Map KeyHash (Index 'Soft 'ScriptK, AddressState)
getVerKeyMap (SeqState _ _ _ _ _ verKeyPool) =
    verPoolIndexedKeys verKeyPool

scriptKeyHashesInMap
    :: Script
    -> ShelleyKey 'AccountK XPub
    -> SeqState 'Mainnet ShelleyKey
    -> Maybe (Set KeyHash)
scriptKeyHashesInMap script' accXPub' s =
    Set.fromList . map (deriveKeyHash accXPub') <$>
    Map.lookup (toScriptHash script') (getKnownScripts s)

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
            scripts' <- vectorOf m (scriptTree n')
            atLeast <- choose (1, fromIntegral (m + 1))
            elements
                [ RequireAllOf scripts'
                , RequireAnyOf scripts'
                , RequireSomeOf atLeast scripts'
                ]

prepareVerKeys
    :: ShelleyKey 'AccountK XPub
    -> [Word32]
    -> [ShelleyKey 'ScriptK XPub]
prepareVerKeys accXPub' =
    let minIndex = getIndex @'Soft minBound
    in map (\ix -> deriveVerificationKey accXPub' (toEnum (fromInteger $ toInteger $ minIndex + ix)))

instance Arbitrary (ShelleyKey 'AccountK XPub) where
    arbitrary = do
        mnemonics <- SomeMnemonic <$> genMnemonic @12
        encPwd <- arbitrary
        let rootXPrv = generateKeyFromSeed (mnemonics, Nothing) encPwd
        pure $ publicKey $ deriveAccountPrivateKey encPwd rootXPrv minBound

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
