{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.Ledger.ConvertSpec
    ( spec
    ) where

import Cardano.Address.KeyHash
    ( KeyHash (..)
    , KeyRole (..)
    )
import Cardano.Address.Script
    ( Script (..)
    )
import Cardano.Ledger.Allegra.Scripts
    ( Timelock
    )
import Cardano.Ledger.Api
    ( ConwayEra
    , DijkstraEra
    )
import Cardano.Ledger.Babbage
    ( BabbageEra
    )
import Cardano.Ledger.Credential
    ( Credential (KeyHashObj, ScriptHashObj)
    )
import Cardano.Ledger.Dijkstra.Scripts
    ( pattern RequireGuard
    )
import Cardano.Slotting.Slot
    ( SlotNo (..)
    )
import Cardano.Wallet.Primitive.Ledger.Convert
    ( Convert (..)
    , fromBabbageTxOutInEra
    , toBabbageTxOutInEra
    , toLedgerAssetName
    , toLedgerMintValue
    , toLedgerTimelockScript
    , toLedgerTokenPolicyId
    , toLedgerTokenQuantity
    , toLedgerUTxOInEra
    , toWalletAssetName
    , toWalletScript
    , toWalletTokenPolicyId
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Scripts
    ( conwayAnyExplicitScript
    , dijkstraAnyExplicitScript
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    )
import Cardano.Wallet.Primitive.Types.AssetId
    ( AssetId (..)
    )
import Cardano.Wallet.Primitive.Types.AssetName
    ( AssetName
    )
import Cardano.Wallet.Primitive.Types.AssetName.Gen
    ( genAssetNameLargeRange
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle
    )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundle
    , genTokenBundleSmallRange
    , shrinkTokenBundleSmallRange
    )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap
    )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genTokenMapSmallRange
    , shrinkTokenMap
    )
import Cardano.Wallet.Primitive.Types.TokenMapWithScripts
    ( ScriptReference (ViaSpending)
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId.Gen
    ( genTokenPolicyIdLargeRange
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..)
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
    ( genTokenQuantityFullRange
    , shrinkTokenQuantityFullRange
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn.Gen
    ( genTxIn
    , shrinkTxIn
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut.Gen
    ( genTxOutCoin
    , shrinkTxOutCoin
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..)
    )
import Cardano.Wallet.Primitive.Types.WitnessCount
    ( WitnessCountCtx (..)
    )
import Control.Exception
    ( ErrorCall (..)
    , evaluate
    , try
    )
import Control.Monad
    ( replicateM
    )
import Data.Char
    ( toLower
    )
import Data.List
    ( isInfixOf
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Set
    ( Set
    )
import Data.Typeable
    ( Typeable
    , typeRep
    )
import Test.Cardano.Ledger.Allegra.Arbitrary
    (
    )
import Test.Cardano.Ledger.Core.Arbitrary
    (
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldContain
    , shouldNotContain
    )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Positive (Positive)
    , Property
    , arbitrarySizedNatural
    , checkCoverage
    , choose
    , classify
    , conjoin
    , counterexample
    , cover
    , elements
    , forAll
    , frequency
    , generate
    , ioProperty
    , listOf1
    , oneof
    , property
    , resize
    , scale
    , sized
    , tabulate
    , vectorOf
    , (===)
    , (==>)
    )
import Prelude

import qualified Cardano.Ledger.Address as Ledger
    ( Addr
    )
import qualified Cardano.Ledger.Allegra.Scripts as LedgerScripts
    ( pattern RequireTimeExpire
    , pattern RequireTimeStart
    )
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Api as LedgerApi
import qualified Cardano.Ledger.Api.UTxO as Ledger
    ( UTxO (..)
    )
import qualified Cardano.Ledger.Babbage.TxOut as Babbage
import qualified Cardano.Ledger.Mary.Value as Ledger
import qualified Cardano.Ledger.Shelley.Scripts as LedgerScripts
    ( pattern RequireAllOf
    , pattern RequireAnyOf
    , pattern RequireMOf
    , pattern RequireSignature
    )
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set

spec :: Spec
spec = describe "Cardano.Wallet.Primitive.Ledger.ConvertSpec"
    $ modifyMaxSuccess (const 1000)
    $ do
        describe "recent-era tx output conversions" $ do
            it "round-trips a generated output at the Dijkstra era"
                $ property
                $ forAll genRecentEraOutput
                $ \o ->
                    fromBabbageTxOutInEra
                        (toBabbageTxOutInEra o :: Babbage.BabbageTxOut DijkstraEra)
                        === o

            it "agrees with the Conway conversion field by field"
                $ property
                $ forAll (genRecentEraOutputWith genTokenBundle)
                $ \o -> case ( toBabbageTxOutInEra o :: Babbage.BabbageTxOut ConwayEra
                             , toBabbageTxOutInEra o :: Babbage.BabbageTxOut DijkstraEra
                             ) of
                    (Babbage.BabbageTxOut ca cv _ _, Babbage.BabbageTxOut da dv _ _) ->
                        conjoin
                            [ (toWallet (ca :: Ledger.Addr) :: Address)
                                === (toWallet (da :: Ledger.Addr) :: Address)
                            , cv === dv
                            ]

            it "preserves the UTxO map at the Dijkstra era"
                $ property
                $ forAll genRecentEraUTxO
                $ \u -> case toLedgerUTxOInEra u :: Ledger.UTxO DijkstraEra of
                    Ledger.UTxO m' -> case u of
                        UTxO m ->
                            Map.mapKeys toWallet (fmap fromBabbageTxOutInEra m')
                                === m

            it "the era generator actually reaches the Dijkstra era"
                $ property prop_eraGeneratorReachesDijkstra

        describe "Roundtrip conversions" $ do
            ledgerRoundtrip $ Proxy @Coin
            ledgerRoundtrip $ Proxy @TokenBundle
            ledgerRoundtrip $ Proxy @AssetName
            ledgerRoundtrip $ Proxy @TokenPolicyId
            ledgerRoundtrip $ Proxy @TokenQuantity
            ledgerRoundtrip $ Proxy @TxIn

        describe
            "Timelock roundtrips (toLedgerTimelockScript, toWalletScript)"
            $ do
                let ledger = toLedgerTimelockScript @BabbageEra
                let wallet = toWalletScript (const Unknown)

                it "ledger . wallet . ledger == ledger" $ property $ \s -> do
                    -- Ignore key role by doing one extra conversion
                    ledger (wallet $ ledger s) === ledger s

                it "ledger . wallet == id" $ property $ \s -> do
                    ledger (wallet s) === s

        describe "dijkstraAnyExplicitScript (Dijkstra call site, end to end)" $ do
            let ctx = AnyWitnessCountCtx

            it "shared shapes decode to the same explicit script as the Conway decoding"
                $ property
                $ forAll genDijkstraSharedNativeScript $ \d ->
                    forAll arbitrary $ \(h :: LedgerApi.ScriptHash) ->
                        dijkstraAnyExplicitScript ctx (ViaSpending, h, Alonzo.NativeScript d)
                            === conwayAnyExplicitScript
                                ctx
                                (ViaSpending, h, Alonzo.NativeScript (toConwayCounterpart d))

            it "a guard script fails naming the era, without claiming the shape"
                $ property
                $ forAll genDijkstraGuardNativeScript $ \g ->
                    forAll arbitrary $ \(h :: LedgerApi.ScriptHash) ->
                        classify (guardCredentialForm g == "KeyHashObj") "KeyHashObj guard"
                            $ classify
                                (guardCredentialForm g == "ScriptHashObj")
                                "ScriptHashObj guard"
                            $ ioProperty
                                ( assertDijkstraConversionFails
                                    ( snd
                                        ( dijkstraAnyExplicitScript
                                            ctx
                                            (ViaSpending, h, Alonzo.NativeScript g)
                                        )
                                    )
                                )

            it "the generated shared-shape population contains every shared shape, nested"
                $ do
                    samples <-
                        generate
                            (replicateM 2000 (resize 25 genDijkstraSharedNativeScript))
                    Set.unions (map allSharedShapes samples)
                        `shouldBe` Set.fromList [minBound .. maxBound]
                    any hasNestedCompound samples `shouldBe` True

        describe "toLedgerMintValue" $ do
            it "is total for generated mint and burn maps"
                $ property
                    prop_mintValue_total

            it "preserves empty mint and burn maps"
                $ toLedgerMintValue mempty mempty === mempty

            it "translates pure mints as positive quantities"
                $ property
                    prop_mintValue_mintOnly

            it "translates pure burns as negative quantities"
                $ property
                    prop_mintValue_burnOnly

            it "nets mint and burn quantities per asset"
                $ property
                    prop_mintValue_netting

            it "does not introduce phantom asset keys"
                $ property
                    prop_mintValue_noPhantomKeys

            it "does not emit empty policy buckets"
                $ property
                    prop_mintValue_noEmptyBuckets

            it "roundtrips disjoint mints and burns"
                $ property
                    prop_mintValue_roundtripDisjoint

--------------------------------------------------------------------------------
-- Dijkstra native script conversion
--------------------------------------------------------------------------------

-- Standalone generators (no Arbitrary instances are defined in this
-- repository's tests). Leaves come from the ledger testlib's Arbitrary
-- instances for hashes.

-- | The six native script shapes Dijkstra shares with 'Timelock', including
-- nested compounds.
genDijkstraSharedNativeScript :: Gen (LedgerApi.NativeScript DijkstraEra)
genDijkstraSharedNativeScript = sized $ \(n :: Int) ->
    if n <= 1
        then genSharedLeaf
        else oneof [genSharedLeaf, genSharedNode]
  where
    genSharedLeaf = oneof
        [ LedgerScripts.RequireSignature <$> arbitrary
        , LedgerScripts.RequireTimeStart <$> genSlotNo
        , LedgerScripts.RequireTimeExpire <$> genSlotNo
        ]
    genSharedNode = oneof
        [ compound LedgerScripts.RequireAllOf
        , compound LedgerScripts.RequireAnyOf
        , do
            subs <- listOf1 sub
            Positive k <- arbitrary
            pure
                $ LedgerScripts.RequireMOf
                    (1 + (k - 1) `mod` length subs)
                    (StrictSeq.fromList subs)
        ]
    compound mk = mk . StrictSeq.fromList <$> listOf1 sub
    sub = scale (`div` 2) genDijkstraSharedNativeScript
    genSlotNo = SlotNo <$> choose (0, 2 ^ (40 :: Int))

-- | A guard script over a key-hash credential.
genDijkstraKeyHashGuard :: Gen (LedgerApi.NativeScript DijkstraEra)
genDijkstraKeyHashGuard = RequireGuard . KeyHashObj <$> arbitrary

-- | A guard script over a script-hash credential.
genDijkstraScriptHashGuard :: Gen (LedgerApi.NativeScript DijkstraEra)
genDijkstraScriptHashGuard = RequireGuard . ScriptHashObj <$> arbitrary

-- | Guard scripts over both credential forms.
genDijkstraGuardNativeScript :: Gen (LedgerApi.NativeScript DijkstraEra)
genDijkstraGuardNativeScript =
    oneof [genDijkstraKeyHashGuard, genDijkstraScriptHashGuard]

-- | The Conway counterpart of a shared-shape Dijkstra native script: the
-- identical structure over 'Timelock ConwayEra'. Shape-for-shape this mirrors
-- the ledger's own 'upgradeTimelock' mapping.
toConwayCounterpart :: LedgerApi.NativeScript DijkstraEra -> Timelock ConwayEra
toConwayCounterpart = \case
    LedgerScripts.RequireSignature kh -> LedgerScripts.RequireSignature kh
    LedgerScripts.RequireAllOf xs ->
        LedgerScripts.RequireAllOf (fmap toConwayCounterpart xs)
    LedgerScripts.RequireAnyOf xs ->
        LedgerScripts.RequireAnyOf (fmap toConwayCounterpart xs)
    LedgerScripts.RequireMOf n xs ->
        LedgerScripts.RequireMOf n (fmap toConwayCounterpart xs)
    LedgerScripts.RequireTimeStart s -> LedgerScripts.RequireTimeStart s
    LedgerScripts.RequireTimeExpire s -> LedgerScripts.RequireTimeExpire s
    s -> error ("no Conway counterpart: " <> show s)

data SharedShape
    = ShapeRequireSignature
    | ShapeRequireAllOf
    | ShapeRequireAnyOf
    | ShapeRequireMOf
    | ShapeRequireTimeStart
    | ShapeRequireTimeExpire
    deriving (Bounded, Enum, Eq, Ord, Show)

-- | Every shared shape occurring anywhere in the script, nested or not.
allSharedShapes :: LedgerApi.NativeScript DijkstraEra -> Set SharedShape
allSharedShapes = \case
    LedgerScripts.RequireSignature _ -> Set.singleton ShapeRequireSignature
    LedgerScripts.RequireAllOf xs ->
        Set.insert ShapeRequireAllOf (foldMap allSharedShapes xs)
    LedgerScripts.RequireAnyOf xs ->
        Set.insert ShapeRequireAnyOf (foldMap allSharedShapes xs)
    LedgerScripts.RequireMOf _ xs ->
        Set.insert ShapeRequireMOf (foldMap allSharedShapes xs)
    LedgerScripts.RequireTimeStart _ -> Set.singleton ShapeRequireTimeStart
    LedgerScripts.RequireTimeExpire _ -> Set.singleton ShapeRequireTimeExpire
    RequireGuard _ -> Set.empty
    s -> error ("unexpected script in shape census: " <> show s)

-- | True when some compound node contains another compound node below it.
hasNestedCompound :: LedgerApi.NativeScript DijkstraEra -> Bool
hasNestedCompound = go False
  where
    go belowCompound = \case
        LedgerScripts.RequireSignature _ -> False
        LedgerScripts.RequireAllOf xs -> belowCompound || any (go True) xs
        LedgerScripts.RequireAnyOf xs -> belowCompound || any (go True) xs
        LedgerScripts.RequireMOf _ xs -> belowCompound || any (go True) xs
        LedgerScripts.RequireTimeStart _ -> False
        LedgerScripts.RequireTimeExpire _ -> False
        RequireGuard _ -> False
        _ -> False

guardCredentialForm :: LedgerApi.NativeScript DijkstraEra -> String
guardCredentialForm = \case
    RequireGuard (KeyHashObj _) -> "KeyHashObj"
    RequireGuard (ScriptHashObj _) -> "ScriptHashObj"
    _ -> error "not a guard script"

-- | Requires the conversion of the given script to fail with an error message
-- that names the Dijkstra era, states that the script has no wallet
-- representation, and does not claim to identify which shape it caught.
assertDijkstraConversionFails :: Show a => a -> IO ()
assertDijkstraConversionFails result = do
    r <- try (evaluate (show result))
    case r of
        Left (ErrorCall msg) -> do
            msg `shouldContain` "Dijkstra"
            msg `shouldContain` "no wallet representation"
            map toLower msg `shouldNotContain` "guard"
        Right shown -> fail ("expected the conversion to fail, got: " <> shown)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

ledgerRoundtrip
    :: forall w l
     . (Arbitrary w, Eq w, Show w, Typeable w, Convert w l)
    => Proxy w
    -> Spec
ledgerRoundtrip proxy = it title
    $ property
    $ \a -> toWallet (toLedger @w a) === a
  where
    title =
        mconcat
            [ "Can perform roundtrip conversion for values of type '"
            , show (typeRep proxy)
            , "'"
            ]

prop_mintValue_total :: TestTokenMap -> TestTokenMap -> Property
prop_mintValue_total (TestTokenMap mint) (TestTokenMap burn) =
    ledgerMintMap (toLedgerMintValue mint burn)
        === expectedLedgerMintMap mint burn

prop_mintValue_mintOnly :: TestTokenMap -> Property
prop_mintValue_mintOnly (TestTokenMap mint) =
    ledgerMintMap (toLedgerMintValue mint mempty)
        === expectedLedgerMintMap mint mempty

prop_mintValue_burnOnly :: TestTokenMap -> Property
prop_mintValue_burnOnly (TestTokenMap burn) =
    ledgerMintMap (toLedgerMintValue mempty burn)
        === expectedLedgerMintMap mempty burn

prop_mintValue_netting :: TestTokenMap -> TestTokenMap -> Property
prop_mintValue_netting (TestTokenMap mint) (TestTokenMap burn) =
    ledgerMintMap (toLedgerMintValue mint burn)
        === expectedLedgerMintMap mint burn

prop_mintValue_noPhantomKeys
    :: TestTokenMap -> TestTokenMap -> Property
prop_mintValue_noPhantomKeys (TestTokenMap mint) (TestTokenMap burn) =
    outputKeys `Set.isSubsetOf` inputKeys
        === True
  where
    outputKeys = walletAssetKeys $ ledgerMintMap $ toLedgerMintValue mint burn
    inputKeys = TokenMap.getAssets mint <> TokenMap.getAssets burn

prop_mintValue_noEmptyBuckets
    :: TestTokenMap -> TestTokenMap -> Property
prop_mintValue_noEmptyBuckets (TestTokenMap mint) (TestTokenMap burn) =
    counterexample (show output) $ not (any Map.null output) === True
  where
    output = ledgerMintMap $ toLedgerMintValue mint burn

prop_mintValue_roundtripDisjoint
    :: TestTokenMap -> TestTokenMap -> Property
prop_mintValue_roundtripDisjoint (TestTokenMap mint) (TestTokenMap burn) =
    Set.null
        (TokenMap.getAssets mint `Set.intersection` TokenMap.getAssets burn)
        ==> fromLedgerMintValue (toLedgerMintValue mint burn) === (mint, burn)

ledgerMintMap
    :: Ledger.MultiAsset
    -> Map.Map Ledger.PolicyID (Map.Map Ledger.AssetName Integer)
ledgerMintMap (Ledger.MultiAsset assets) = assets

expectedLedgerMintMap
    :: TokenMap
    -> TokenMap
    -> Map.Map Ledger.PolicyID (Map.Map Ledger.AssetName Integer)
expectedLedgerMintMap mint burn =
    Map.mapMaybe nonEmpty
        $ Map.unionWith
            (Map.unionWith (+))
            (signedNested id mint)
            (signedNested negate burn)
  where
    signedNested sign =
        Map.map (Map.map (sign . toLedgerTokenQuantity))
            . toLedgerNestedMap

    nonEmpty inner =
        let nonZero = Map.filter (/= 0) inner
        in  if Map.null nonZero then Nothing else Just nonZero

walletAssetKeys
    :: Map.Map Ledger.PolicyID (Map.Map Ledger.AssetName Integer)
    -> Set AssetId
walletAssetKeys =
    Set.fromList
        . concatMap
            ( \(policy, assets) ->
                [ AssetId
                    (toWalletTokenPolicyId policy)
                    (toWalletAssetName asset)
                | asset <- Map.keys assets
                ]
            )
        . Map.toList

fromLedgerMintValue :: Ledger.MultiAsset -> (TokenMap, TokenMap)
fromLedgerMintValue (Ledger.MultiAsset assets) =
    (TokenMap.fromFlatList mints, TokenMap.fromFlatList burns)
  where
    (mints, burns) =
        foldMap
            splitQuantity
            [ (policy, asset, quantity)
            | (policy, inner) <- Map.toList assets
            , (asset, quantity) <- Map.toList inner
            ]

    splitQuantity (policy, asset, quantity)
        | quantity > 0 =
            ( pure
                ( walletAssetId policy asset
                , TokenQuantity $ fromInteger quantity
                )
            , []
            )
        | quantity < 0 =
            ( []
            , pure
                ( walletAssetId policy asset
                , TokenQuantity $ fromInteger $ abs quantity
                )
            )
        | otherwise =
            mempty

    walletAssetId policy asset =
        AssetId (toWalletTokenPolicyId policy) (toWalletAssetName asset)

toLedgerNestedMap
    :: TokenMap
    -> Map.Map Ledger.PolicyID (Map.Map Ledger.AssetName TokenQuantity)
toLedgerNestedMap =
    Map.mapKeys toLedgerTokenPolicyId
        . Map.map (Map.mapKeys toLedgerAssetName)
        . TokenMap.toNestedMap

--------------------------------------------------------------------------------
-- Arbitraries
--------------------------------------------------------------------------------

newtype TestTokenMap = TestTokenMap
    { getTestTokenMap :: TokenMap
    }
    deriving (Eq, Show)

instance Arbitrary TestTokenMap where
    arbitrary = TestTokenMap <$> genTokenMapSmallRange
    shrink = fmap TestTokenMap . shrinkTokenMap . getTestTokenMap

instance Arbitrary Coin where
    -- This instance is used to test roundtrip conversions, so it's important
    -- that we generate coins across the full range available.
    arbitrary = genTxOutCoin
    shrink = shrinkTxOutCoin

instance Arbitrary TokenBundle where
    arbitrary = genTokenBundleSmallRange
    shrink = shrinkTokenBundleSmallRange

instance Arbitrary AssetName where
    arbitrary = genAssetNameLargeRange

-- No shrinking

instance Arbitrary TokenPolicyId where
    arbitrary = genTokenPolicyIdLargeRange

-- No shrinking

instance Arbitrary TokenQuantity where
    arbitrary = genTokenQuantityFullRange
    shrink = shrinkTokenQuantityFullRange

instance Arbitrary TxIn where
    arbitrary = genTxIn
    shrink = shrinkTxIn

instance Arbitrary (Script KeyHash) where
    arbitrary = do
        keyHashes <- vectorOf 10 arbitrary
        genScript keyHashes
      where
        genScript :: [a] -> Gen (Script a)
        genScript elems = scale (`div` 3) $ sized scriptTree
          where
            scriptTree 0 =
                oneof
                    [ RequireSignatureOf <$> elements elems
                    , ActiveFromSlot <$> arbitrarySizedNatural
                    , ActiveUntilSlot <$> arbitrarySizedNatural
                    ]
            scriptTree n = do
                Positive m <- arbitrary
                let n' = n `div` (m + 1)
                scripts' <- vectorOf m (scriptTree n')
                atLeast <- choose (1, fromIntegral m)
                elements
                    [ RequireAllOf scripts'
                    , RequireAnyOf scripts'
                    , RequireSomeOf atLeast scripts'
                    ]

-- | A wallet output whose address is well-formed for the ledger decoder
-- (base address: header byte plus two 28-byte key hashes) and whose bundle
-- varies. The dummy addresses of genTxOut are deliberately not usable
-- here: the recent-era conversions decode the address.
genRecentEraOutput :: Gen TxOut
genRecentEraOutput =
    genRecentEraOutputWith genTokenBundleSmallRange

-- | The bundle generator is a parameter so the field-comparison properties
-- can widen the value coverage beyond the small-range default (R5).
genRecentEraOutputWith :: Gen TokenBundle -> Gen TxOut
genRecentEraOutputWith genBundle =
    TxOut
        <$> genWellFormedAddress
        <*> genBundle

genWellFormedAddress :: Gen Address
genWellFormedAddress = do
    payment <- BS.pack <$> vectorOf 28 arbitrary
    stake <- BS.pack <$> vectorOf 28 arbitrary
    pure $ Address (0x01 `BS.cons` (payment <> stake))

genRecentEraUTxO :: Gen UTxO
genRecentEraUTxO = sized $ \size -> do
    n <- choose (0, size)
    UTxO . Map.fromList
        <$> replicateM n ((,) <$> genTxIn <*> genRecentEraOutput)

instance Arbitrary KeyHash where
    arbitrary = do
        cred <- elements [Payment, Delegation, Policy, Unknown]
        KeyHash cred . BS.pack <$> vectorOf 28 arbitrary

--------------------------------------------------------------------------------
-- Era coverage for the recent-era output conversion
--------------------------------------------------------------------------------

data EraToken = EraBabbage | EraConway | EraDijkstra
    deriving (Eq, Show)

-- | The generated population is Dijkstra-heavy on purpose: the required
-- coverage is on the Dijkstra case, and 'checkCoverage' in
-- 'prop_eraGeneratorReachesDijkstra' fails the property when it falls below
-- 90%. Babbage and Conway stay reachable so regression there is still
-- visible.
instance Arbitrary EraToken where
    arbitrary =
        frequency
            [ (95, pure EraDijkstra)
            , (3, pure EraBabbage)
            , (2, pure EraConway)
            ]
    shrink = const []

-- | The era generator must actually reach the Dijkstra case. A property
-- whose generated population never produces the Dijkstra conversion is
-- blind to it at any number of runs; 'checkCoverage' fails this property
-- when the Dijkstra share drops below 90%.
prop_eraGeneratorReachesDijkstra :: EraToken -> Property
prop_eraGeneratorReachesDijkstra tok =
    forAll genRecentEraOutput $ \o ->
        checkCoverage
            $ cover
                90
                (tok == EraDijkstra)
                "Dijkstra era in generated population"
            $ tabulate "era" [show tok]
            $ convertInEra tok o === o
  where
    convertInEra t x = case t of
        EraBabbage ->
            fromBabbageTxOutInEra
                (toBabbageTxOutInEra x :: Babbage.BabbageTxOut BabbageEra)
        EraConway ->
            fromBabbageTxOutInEra
                (toBabbageTxOutInEra x :: Babbage.BabbageTxOut ConwayEra)
        EraDijkstra ->
            fromBabbageTxOutInEra
                (toBabbageTxOutInEra x :: Babbage.BabbageTxOut DijkstraEra)
