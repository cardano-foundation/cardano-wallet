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
    ( KeyHash (..), Script (..), toScriptHash )
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
    , Role (..)
    , SoftDerivation (..)
    , WalletKey (..)
    , preparePassphrase
    )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..), generateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( DerivationPrefix (..)
    , SeqState (..)
    , VerificationKeyPool (..)
    , coinTypeAda
    , defaultAddressPoolGap
    , emptyPendingIxs
    , getAddressPoolGap
    , mkAddressPool
    , mkUnboundedAddressPoolGap
    , mkVerificationKeyPool
    , purposeCIP1852
    , purposeCIP1852
    , toVerKeyHash
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
import Data.Proxy
    ( Proxy (..) )
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
        it "discovering our verification keys make them mark Used"
            (property prop_markingDiscoveredVerKeys)
        it "discovering works after pool extension"
            (property prop_poolExtension)
        it "our verification keys beyond pool gap are not discovered"
            (property prop_scriptsOutsideNotDiscovered)

prop_scriptFromOurVerKeys
    :: AccountXPubWithScripts
    -> Property
prop_scriptFromOurVerKeys (AccountXPubWithScripts accXPub' scripts') = do
    let (script:_) = scripts'
    let sciptKeyHashes = retrieveAllVerKeyHashes script
    let seqState = initializeState accXPub'
    let (ourSharedKeys, _) = isShared script seqState
    L.sort (L.nub $ map toVerKeyHash ourSharedKeys) === L.sort (L.nub sciptKeyHashes)

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
    let scriptKeyHashesInMap =
            Set.fromList . map (toVerKeyHash . projectKey) <$>
            Map.lookup (toScriptHash script) (knownScripts seqState')
    scriptKeyHashesInMap === (Just $ Set.fromList (L.nub sciptKeyHashes))

prop_scriptDiscoveredTwice
    :: AccountXPubWithScripts
    -> Property
prop_scriptDiscoveredTwice (AccountXPubWithScripts accXPub' scripts') = do
    let (script:_) = scripts'
    let seqState = initializeState accXPub'
    let (_, seqState') = isShared script seqState
    let (_, seqState'') = isShared script seqState'
    knownScripts seqState' === knownScripts seqState''

prop_scriptsDiscovered
    :: AccountXPubWithScripts
    -> Property
prop_scriptsDiscovered (AccountXPubWithScripts accXPub' scripts') = do
    let seqState0 = initializeState accXPub'
    let seqState = foldr (\script s -> snd $ isShared script s) seqState0 scripts'
    let scriptHashes = Set.fromList $ Map.keys $ knownScripts seqState
    scriptHashes === Set.fromList (map toScriptHash scripts')

prop_markingDiscoveredVerKeys
    :: AccountXPubWithScripts
    -> Property
prop_markingDiscoveredVerKeys (AccountXPubWithScripts accXPub' scripts') = do
    let (script:_) = scripts'
    let sciptKeyHashes = retrieveAllVerKeyHashes script
    let seqState = initializeState accXPub'
    let (_, SeqState _ _ _ _ _ _ (VerificationKeyPool _ _ allOurVerKeys)) =
            isShared script seqState
    let discoveredKeyMap = Map.filterWithKey (\k _ -> k `elem` sciptKeyHashes) allOurVerKeys
    let addressStatesToCheck =
            map (\(_, (_, isUsed)) -> isUsed) $ Map.toList discoveredKeyMap
    L.all (== Used) addressStatesToCheck === True

prop_poolExtension
    :: AccountXPubWithScriptsExtension
    -> Property
prop_poolExtension (AccountXPubWithScriptsExtension accXPub' scripts') = do
    g' == mkUnboundedAddressPoolGap
        (getAddressPoolGap defaultAddressPoolGap + getAddressPoolGap g)
    .&&.
        scriptHashes == Set.fromList (map toScriptHash scripts')
  where
    (SeqState s1 s2 ixs rpk prefix scr (VerificationKeyPool _ g allOurVerKeys)) =
        initializeState accXPub'
    --make artificially all except last index Used
    isLastIndex ix =
        if ix == Index (minBound + getAddressPoolGap defaultAddressPoolGap - 1) then
            Unused
        else
            Used
    allOurVerKeys' =
        Map.mapWithKey (\_ (ix, _) -> (ix, isLastIndex ix)) allOurVerKeys
    seqState =
        SeqState s1 s2 ixs rpk prefix scr (VerificationKeyPool accXPub' g allOurVerKeys')
    (script:restScripts) = scripts'
    (_, seqState0@(SeqState _ _ _ _ _ _ (VerificationKeyPool _ g' _)) ) =
        isShared script seqState
    seqState' = foldr (\script' s -> snd $ isShared script' s) seqState0 restScripts
    scriptHashes = Set.fromList $ Map.keys $ knownScripts seqState'

prop_scriptsOutsideNotDiscovered
    :: AccountXPubWithScriptsOutside
    -> Property
prop_scriptsOutsideNotDiscovered (AccountXPubWithScriptsOutside accXPub' scripts') = do
    let seqState0 = initializeState accXPub'
    let seqState = foldr (\script s -> snd $ isShared script s) seqState0 scripts'
    let scriptHashes = Map.keys $ knownScripts seqState
    null scriptHashes === True

data AccountXPubWithScripts = AccountXPubWithScripts
    { accXPub :: ShelleyKey 'AccountK XPub
    , scripts :: [Script]
    } deriving (Eq, Show)

data AccountXPubWithScriptsExtension = AccountXPubWithScriptsExtension
    { accXPub :: ShelleyKey 'AccountK XPub
    , scripts :: [Script]
    } deriving (Eq, Show)

data AccountXPubWithScriptsOutside = AccountXPubWithScriptsOutside
    { accXPub :: ShelleyKey 'AccountK XPub
    , scripts :: [Script]
    } deriving (Eq, Show)

defaultPrefix :: DerivationPrefix
defaultPrefix = DerivationPrefix
    ( purposeCIP1852
    , coinTypeAda
    , minBound
    )

dummyRewardAccount :: ShelleyKey 'AddressK XPub
dummyRewardAccount = ShelleyKey $ unsafeXPub $ B8.replicate 64 '0'

projectKey :: ShelleyKey 'ScriptK XPub -> ShelleyKey 'AddressK XPub
projectKey (ShelleyKey k) = ShelleyKey k

initializeState
    :: ShelleyKey 'AccountK XPub
    -> SeqState 'Mainnet ShelleyKey
initializeState accXPub' =
    let intPool = mkAddressPool accXPub' defaultAddressPoolGap []
        extPool = mkAddressPool accXPub' defaultAddressPoolGap []
        multiPool = mkVerificationKeyPool accXPub' defaultAddressPoolGap Map.empty
    in SeqState intPool extPool emptyPendingIxs dummyRewardAccount defaultPrefix Map.empty multiPool

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
    -> [ShelleyKey 'AddressK XPub]
prepareVerKeys accXPub' =
    let toVerKey = deriveAddressPublicKey accXPub' MultisigScript
        minIndex = getIndex @'Soft minBound
    in map (\ix -> toVerKey (toEnum (fromInteger $ toInteger $ minIndex + ix)))

instance Arbitrary (ShelleyKey 'AccountK XPub) where
    arbitrary = do
        mnemonics <- SomeMnemonic <$> genMnemonic @12
        encPwd <- arbitrary
        let rootXPrv = generateKeyFromSeed (mnemonics, Nothing) encPwd
        pure $ publicKey $ deriveAccountPrivateKey encPwd rootXPrv minBound

instance Arbitrary AccountXPubWithScripts where
    arbitrary = do
        accXPub' <- arbitrary
        kNum <- choose (2,8)
        let verKeyHashes = map toVerKeyHash (prepareVerKeys accXPub' [0 .. kNum])
        scriptsNum <- choose (1,10)
        AccountXPubWithScripts accXPub' <$> vectorOf scriptsNum (genScript verKeyHashes)

instance Arbitrary AccountXPubWithScriptsExtension where
    arbitrary = do
        accXPub' <- arbitrary

        -- the first script is expected to trigger extension to multisigScript
        let g = getAddressPoolGap defaultAddressPoolGap
        scriptTipping <-
            genScript (toVerKeyHash <$> (prepareVerKeys accXPub' [g - 1]))

        -- the next scripts are using extended indices that were not possible to be discovered
        kNum <- choose (2,8)
        scriptsNum <- choose (1,10)
        let verKeysNext = prepareVerKeys accXPub' [g .. g + kNum]
        scriptsNext <-
            vectorOf scriptsNum (genScript (toVerKeyHash <$> verKeysNext))

        pure $ AccountXPubWithScriptsExtension accXPub' (scriptTipping : scriptsNext )

instance Arbitrary AccountXPubWithScriptsOutside where
    arbitrary = do
        accXPub' <- arbitrary
        kNum <- choose (2,8)
        let g = getAddressPoolGap defaultAddressPoolGap
        let verKeyHashes = map toVerKeyHash (prepareVerKeys accXPub' [g .. g + kNum])
        scriptsNum <- choose (1,10)
        AccountXPubWithScriptsOutside accXPub' <$> vectorOf scriptsNum (genScript verKeyHashes)

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
