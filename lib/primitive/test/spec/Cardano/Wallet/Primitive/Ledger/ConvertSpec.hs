{-# LANGUAGE FlexibleInstances #-}
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
import Cardano.Ledger.Babbage
    ( BabbageEra
    )
import Cardano.Wallet.Primitive.Ledger.Convert
    ( Convert (..)
    , toLedgerAssetName
    , toLedgerMintValue
    , toLedgerTimelockScript
    , toLedgerTokenPolicyId
    , toLedgerTokenQuantity
    , toWalletAssetName
    , toWalletScript
    , toWalletTokenPolicyId
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
    ( genTokenBundleSmallRange
    , shrinkTokenBundleSmallRange
    )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap
    )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genTokenMapSmallRange
    , shrinkTokenMap
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
import Cardano.Wallet.Primitive.Types.Tx.TxOut.Gen
    ( genTxOutCoin
    , shrinkTxOutCoin
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
import Test.Hspec
    ( Spec
    , describe
    , it
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
    , choose
    , counterexample
    , elements
    , oneof
    , property
    , scale
    , sized
    , vectorOf
    , (===)
    , (==>)
    )
import Prelude

import qualified Cardano.Ledger.Mary.Value as Ledger
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = describe "Cardano.Wallet.Primitive.Ledger.ConvertSpec"
    $ modifyMaxSuccess (const 1000)
    $ do
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

instance Arbitrary KeyHash where
    arbitrary = do
        cred <- elements [Payment, Delegation, Policy, Unknown]
        KeyHash cred . BS.pack <$> vectorOf 28 arbitrary
