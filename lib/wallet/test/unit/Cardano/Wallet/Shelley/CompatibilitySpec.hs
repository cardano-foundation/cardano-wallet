{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Shelley.CompatibilitySpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub )
import Cardano.Address.Script
    ( KeyHash
    , KeyRole (..)
    , Script (..)
    , ScriptHash (..)
    , keyHashFromBytes
    , serializeScript
    , toScriptHash
    )
import Cardano.Crypto.Hash.Class
    ( digest )
import Cardano.Ledger.Crypto
    ( Crypto (..) )
import Cardano.Mnemonic
    ( ConsistentEntropy
    , EntropySize
    , Mnemonic
    , SomeMnemonic (..)
    , entropyToMnemonic
    )
import Cardano.Wallet.Api.Types
    ( DecodeAddress (..), DecodeStakeAddress (..), EncodeStakeAddress (..) )
import Cardano.Wallet.Byron.Compatibility
    ( maryTokenBundleMaxSize )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , Index (getIndex)
    , NetworkDiscriminant (..)
    , PaymentAddress (..)
    , WalletKey
    , delegationAddress
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..) )
import Cardano.Wallet.Primitive.Types
    ( SlotId (..), TokenBundleMaxSize (..), getDecentralizationLevel )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundle, genTokenBundleSmallRange, shrinkTokenBundleSmallRange )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TokenBundleSizeAssessment (..)
    , TokenBundleSizeAssessor (..)
    , TxSize (..)
    , txOutMaxCoin
    , txOutMinCoin
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut.Gen
    ( genTxOutTokenBundle )
import Cardano.Wallet.Shelley.Compatibility
    ( CardanoBlock
    , StandardCrypto
    , computeTokenBundleSerializedLengthBytes
    , decentralizationLevelFromPParams
    , fromCardanoValue
    , fromTip
    , inspectAddress
    , interval0
    , interval1
    , invertUnitInterval
    , toCardanoHash
    , toCardanoValue
    , toTip
    , tokenBundleSizeAssessor
    )
import Cardano.Wallet.Unsafe
    ( unsafeIntToWord, unsafeMkEntropy )
import Cardano.Wallet.Util
    ( tryInternalError )
import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Codec.Binary.Encoding
    ( fromBase16 )
import Control.Monad
    ( forM_ )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, encodeBase58 )
import Data.Either
    ( isLeft, isRight )
import Data.Function
    ( (&) )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Ratio
    ( Ratio, (%) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( toText )
import Data.Word
    ( Word16, Word32, Word64 )
import GHC.TypeLits
    ( natVal )
import Ouroboros.Network.Block
    ( BlockNo (..), SlotNo (..), Tip (..) )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldSatisfy )
import Test.Hspec.Core.Spec
    ( SpecWith )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..)
    , Blind (..)
    , Gen
    , NonNegative (..)
    , Property
    , Small (..)
    , checkCoverage
    , choose
    , chooseInt
    , conjoin
    , counterexample
    , cover
    , frequency
    , oneof
    , property
    , resize
    , vector
    , withMaxSuccess
    , (===)
    , (==>)
    )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )

import qualified Cardano.Api as Cardano
import qualified Cardano.Ledger.Address as SL
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Shelley as SL
import qualified Cardano.Ledger.Shelley as SLAPI
import qualified Cardano.Ledger.Shelley.PParams as SL
import qualified Cardano.Wallet.Primitive.AddressDerivation as AddressDerivation
import qualified Cardano.Wallet.Primitive.AddressDerivation.Byron as Byron
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shelley as Shelley
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    describe "Conversions" $ do
        it "toTip' . fromTip' == id" $ property $ \gh tip -> do
            let fromTip' = fromTip gh
            let toTip' = toTip gh :: W.BlockHeader -> Tip (CardanoBlock StandardCrypto)
            toTip' (fromTip' tip) === tip

        it "unsafeIntToWord" $
            property prop_unsafeIntToWord

    describe "Shelley StakeAddress" $ do
        prop "roundtrip / Mainnet" $ \x ->
            (decodeStakeAddress @'Mainnet . encodeStakeAddress @'Mainnet) x
            ===
            Right x

        prop "roundtrip / Testnet" $ \x ->
            (decodeStakeAddress @('Testnet 0) . encodeStakeAddress @('Testnet 0)) x
            ===
            Right x

    describe "Shelley Addresses" $ do
        prop "(Mainnet) can be deserialised by shelley ledger spec" $ \k -> do
            let Address addr = paymentAddress @'Mainnet @ShelleyKey @'CredFromKeyK k
            case SL.deserialiseAddr @StandardCrypto addr of
                Just _ -> property True
                Nothing -> property False

        prop "Shelley addresses from bech32" $ \k ->
            let addr@(Address bytes) = paymentAddress @'Mainnet @ShelleyKey @'CredFromKeyK k
            in  decodeAddress @'Mainnet (bech32 bytes) === Right addr
                    & counterexample (show $ bech32 bytes)

        prop "Shelley addresses with delegation from bech32" $ \k1 k2 ->
            let addr@(Address bytes) = delegationAddress @'Mainnet @ShelleyKey @'CredFromKeyK k1 k2
            in  decodeAddress @'Mainnet (bech32 bytes) === Right addr
                    & counterexample (show $ bech32 bytes)

        prop "Shelley addresses from bech32 - testnet" $ \k ->
            let addr@(Address raw) = paymentAddress @('Testnet 0) @ShelleyKey @'CredFromKeyK k
            in  decodeAddress @('Testnet 0) (bech32testnet raw) === Right addr
                   & counterexample (show $ bech32testnet raw)

        prop "Byron addresses from base58" $ \k ->
            let addr@(Address bytes) = paymentAddress @'Mainnet @ByronKey @'CredFromKeyK k
            in  decodeAddress @'Mainnet (base58 bytes) === Right addr
                    & counterexample (show $ base58 bytes)

    describe "decentralizationLevelFromPParams" $ do

        let mkDecentralizationParam :: SL.UnitInterval -> SLAPI.ShelleyPParams (SL.ShelleyEra StandardCrypto)
            mkDecentralizationParam i = SL.emptyPParams { SL._d = i }

        let testCases :: [(Ratio Word64, Text)]
            testCases =
                [ (10 % 10,   "0.00%")
                , ( 9 % 10,  "10.00%")
                , ( 5 % 10,  "50.00%")
                , ( 1 % 10,  "90.00%")
                , ( 0 % 10, "100.00%")
                ]

        forM_ testCases $ \(input, expectedOutput) -> do
            let title = show input <> " -> " <> show expectedOutput
            let output = input
                    & toRational
                    & unsafeBoundRational
                    & mkDecentralizationParam
                    & decentralizationLevelFromPParams
                    & getDecentralizationLevel
                    & toText
            it title $ output `shouldBe` expectedOutput

    describe "Cardano.Api.Value-TokenBundle conversion" $ do
        it "roundtrips" $ checkCoverage $ property $ \tb ->
            cover 20 (TokenBundle.getCoin tb /= Coin 0) "has ada" $
            cover 2 (TokenBundle.getCoin tb == Coin 0) "has no ada" $
            cover 10 (length (snd $ TokenBundle.toFlatList tb) > 3)
                "has some assets" $
            fromCardanoValue (toCardanoValue tb) === tb


    describe "Assessing the sizes of token bundles" $ do

        it "prop_assessTokenBundleSize_enlarge" $
            property prop_assessTokenBundleSize_enlarge
        it "prop_assessTokenBundleSize_shrink" $
            property prop_assessTokenBundleSize_shrink
        it "unit_assessTokenBundleSize_fixedSizeBundle_32" $
            property unit_assessTokenBundleSize_fixedSizeBundle_32
        it "unit_assessTokenBundleSize_fixedSizeBundle_48" $
            property unit_assessTokenBundleSize_fixedSizeBundle_48
        it "unit_assessTokenBundleSize_fixedSizeBundle_64" $
            property unit_assessTokenBundleSize_fixedSizeBundle_64
        it "unit_assessTokenBundleSize_fixedSizeBundle_128" $
            property unit_assessTokenBundleSize_fixedSizeBundle_128

    describe "Utilities" $ do

        describe "UnitInterval" $ do

            it "coverage adequate" $
                checkCoverage $ property $ \i ->
                    let half = unsafeBoundRational (1 % 2) in
                    cover 10 (i == half) "i = 0.5" $
                    cover 10 (i == interval0) "i = 0" $
                    cover 10 (i == interval1) "i = 1" $
                    cover 10 (i > interval0 && i < half) "0 < i < 0.5" $
                    cover 10 (half < i && i < interval1) "0.5 < i < 1"
                    True

            it "invertUnitInterval . invertUnitInterval == id" $
                property $ \i ->
                    invertUnitInterval (invertUnitInterval i) `shouldBe` i

            it "intervalValue i + intervalValue (invertUnitInterval i) == 1" $
                property $ \i ->
                    SL.unboundRational i + SL.unboundRational (invertUnitInterval i)
                        `shouldBe` 1

            it "invertUnitInterval interval0 == interval1" $
                invertUnitInterval interval0 `shouldBe` interval1

            it "invertUnitInterval interval1 == interval0" $
                invertUnitInterval interval1 `shouldBe` interval0

            it "invertUnitInterval half == half" $
                let half = unsafeBoundRational (1 % 2) in
                invertUnitInterval half `shouldBe` half

    describe "InspectAddr" $ do
        -- Cases below are taken straight from cardano-addresses. We don't go in
        -- depth with testing here because this is already tested on
        -- cardano-addresses.
        let matrix =
                [ ( "Byron (1)"
                  , "37btjrVyb4KEgoGCHJ7XFaJRLBRiVuvcrQWPpp4HeaxdTxhKwQjXHNKL43\
                    \NhXaQNa862BmxSFXZFKqPqbxRc3kCUeTRMwjJevFeCKokBG7A7num5Wh"
                  , isRight
                  )
                , ( "Byron (2)"
                  , "DdzFFzCqrht5csm2GKhnVrjzKpVHHQFNXUDhAFDyLWVY5w8ZsJRP2uhwZ\
                    \q2CEAVzDZXYXa4GvggqYEegQsdKAKikFfrrCoHheLH2Jskr"
                  , isRight
                  )
                , ( "Icarus"
                  , "Ae2tdPwUPEYz6ExfbWubiXPB6daUuhJxikMEb4eXRp5oKZBKZwrbJ2k7EZe"
                  , isRight
                  )
                , ( "Shelley (base)"
                  , "addr1vpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg0yu80w"
                  , isRight
                  )
                , ( "Shelley (stake by value)"
                  , "addr1qdu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5ew\
                    \vxwdrt70qlcpeeagscasafhffqsxy36t90ldv06wqrk2q5ggg4z"
                  , isRight
                  )
                , ( "Shelley (stake by pointer)"
                  , "addr1gw2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer5ph3wczvf2x4v58t"
                  , isRight
                  )
                , ( "Shelley (reward by key)"
                  , "stake1upshvetj09hxjcm9v9jxgunjv4ehxmr0d3hkcmmvdakx7mqcjv83c"
                  , isRight
                  )
                , ( "Shelley (reward by script)"
                  , "stake17pshvetj09hxjcm9v9jxgunjv4ehxmr0d3hkcmmvdakx7mq36s8xc"
                  , isRight
                  )
                , ( "Shelley (testnet 1)"
                  , "addr_test1qpwr8l57ceql23ylyprl6qgct239lxph8clwxy5w8r4qdz8ct9uut5a\
                    \hmxqkgwy9ecn5carsv39frsgsq09u70wmqwhqjqcjqs"
                  , isRight
                  )
                , ( "Shelley (testnet 2)"
                  , "stake_test1uru9j7w96wmanqty8zzuuf6vw3cxgj53cygq8j708hds8tsntl0j7"
                  , isRight
                  )
                , ( "Malformed (1)"
                  , "💩"
                  , isLeft
                  )
                , ( "Malformed (2)"
                  , "79467c69a9ac66280174d09d62575ba955748b21dec3b483a9469a65"
                  , isLeft
                  )
                ]

        forM_ matrix $ \(title, addr, predicate) ->
            it title $ inspectAddress addr `shouldSatisfy` predicate

    describe "golden tests for script hashes for different versions" $ do
        testScriptsAllLangs Cardano.SimpleScriptV1
        testScriptsAllLangs Cardano.SimpleScriptV2
        testScriptsTimelockLang

    describe "golden tests for script preimages for different versions" $ do
        testScriptPreimages Cardano.SimpleScriptV1
        testScriptPreimages Cardano.SimpleScriptV2
        testTimelockScriptImagesLang

--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

prop_unsafeIntToWord :: TrickyInt Integer Word16 -> Property
prop_unsafeIntToWord (TrickyInt n wrong) = monadicIO $ do
    res <- run $ tryInternalError $ unsafeIntToWord @Integer @Word16 n
    monitor (counterexample ("res = " ++ show res))
    assert $ case res of
        Right correct -> fromIntegral correct == n
        Left _ -> fromIntegral wrong /= n

data TrickyInt n w = TrickyInt n w deriving (Show, Eq)

instance (Arbitrary n, Integral n, Num w) => Arbitrary (TrickyInt n w) where
    arbitrary = do
        d <- arbitrary
        x <- getSmall . getNonNegative <$> arbitrary :: Gen Int
        s <- frequency [(20, pure 1), (5, pure (-1)), (1, pure 0)]
        let n = s * ((2 ^ x) + d)
        pure $ TrickyInt n (fromIntegral n)

--------------------------------------------------------------------------------
-- Assessing the sizes of token bundles
--------------------------------------------------------------------------------

-- Enlarging a token bundle that is over the size limit should yield a token
-- bundle that is still over the size limit.
--
prop_assessTokenBundleSize_enlarge
    :: Blind (VariableSize1024 TokenBundle)
    -> Blind (VariableSize16 TokenBundle)
    -> Property
prop_assessTokenBundleSize_enlarge b1' b2' =
    assess b1 == TokenBundleSizeExceedsLimit ==> conjoin
        [ assess (b1 `TokenBundle.add` b2)
            === TokenBundleSizeExceedsLimit
        , assess (b1 `TokenBundle.setCoin` txOutMaxCoin)
            === TokenBundleSizeExceedsLimit
        ]
  where
    assess = assessTokenBundleSize
        $ tokenBundleSizeAssessor maryTokenBundleMaxSize
    b1 = unVariableSize1024 $ getBlind b1'
    b2 = unVariableSize16 $ getBlind b2'

-- Shrinking a token bundle that is within the size limit should yield a token
-- bundle that is still within the size limit.
--
prop_assessTokenBundleSize_shrink
    :: Blind (VariableSize1024 TokenBundle)
    -> Blind (VariableSize16 TokenBundle)
    -> TokenBundleMaxSize
    -> Property
prop_assessTokenBundleSize_shrink b1' b2' maxSize =
    assess b1 == TokenBundleSizeWithinLimit ==> conjoin
        [ assess (b1 `TokenBundle.difference` b2)
            === TokenBundleSizeWithinLimit
        , assess (b1 `TokenBundle.setCoin` txOutMinCoin)
            === TokenBundleSizeWithinLimit
        ]
  where
    assess = assessTokenBundleSize (tokenBundleSizeAssessor maxSize)
    b1 = unVariableSize1024 $ getBlind b1'
    b2 = unVariableSize16 $ getBlind b2'

-- | Creates a test to assess the size of a token bundle with a fixed number of
--   assets, where the expected result is a constant.
--
-- Policy identifiers, asset names, token quantities are all allowed to vary.
--
unit_assessTokenBundleSize_fixedSizeBundle
    :: TokenBundle
    -- ^ Fixed size bundle
    -> TokenBundleSizeAssessment
    -- ^ Expected size assessment
    -> TokenBundleMaxSize
    -- ^ TokenBundle assessor function
    -> TxSize
    -- ^ Expected min length (bytes)
    -> TxSize
    -- ^ Expected max length (bytes)
    -> Property
unit_assessTokenBundleSize_fixedSizeBundle
    bundle
    expectedAssessment
    maxSize
    expectedMinLengthBytes
    expectedMaxLengthBytes =
        withMaxSuccess 100 $
        counterexample counterexampleText $
        conjoin . fmap property $
            [ actualAssessment  == expectedAssessment
            , actualLengthBytes >= expectedMinLengthBytes
            , actualLengthBytes <= expectedMaxLengthBytes
            ]
  where
    actualAssessment = assessTokenBundleSize
        (tokenBundleSizeAssessor maxSize)
        bundle
    actualLengthBytes = computeTokenBundleSerializedLengthBytes bundle
    counterexampleText = unlines
        [ "Expected min length bytes:"
        , show expectedMinLengthBytes
        , "Expected max length bytes:"
        , show expectedMaxLengthBytes
        , "Actual length bytes:"
        , show actualLengthBytes
        , "Expected assessment:"
        , show expectedAssessment
        , "Actual assessment:"
        , show actualAssessment
        ]

unit_assessTokenBundleSize_fixedSizeBundle_32
    :: Blind (FixedSize32 TokenBundle) -> Property
unit_assessTokenBundleSize_fixedSizeBundle_32 (Blind (FixedSize32 b)) =
    unit_assessTokenBundleSize_fixedSizeBundle b
        TokenBundleSizeWithinLimit
        maryTokenBundleMaxSize
        (TxSize 2116) (TxSize 2380)

unit_assessTokenBundleSize_fixedSizeBundle_48
    :: Blind (FixedSize48 TokenBundle) -> Property
unit_assessTokenBundleSize_fixedSizeBundle_48 (Blind (FixedSize48 b)) =
    unit_assessTokenBundleSize_fixedSizeBundle b
        TokenBundleSizeWithinLimit
        maryTokenBundleMaxSize
        (TxSize 3172) (TxSize 3564)

unit_assessTokenBundleSize_fixedSizeBundle_64
    :: Blind (FixedSize64 TokenBundle) -> Property
unit_assessTokenBundleSize_fixedSizeBundle_64 (Blind (FixedSize64 b)) =
    unit_assessTokenBundleSize_fixedSizeBundle b
        TokenBundleSizeExceedsLimit
        maryTokenBundleMaxSize
        (TxSize 4228) (TxSize 4748)

unit_assessTokenBundleSize_fixedSizeBundle_128
    :: Blind (FixedSize128 TokenBundle) -> Property
unit_assessTokenBundleSize_fixedSizeBundle_128 (Blind (FixedSize128 b)) =
    unit_assessTokenBundleSize_fixedSizeBundle b
        TokenBundleSizeExceedsLimit
        maryTokenBundleMaxSize
        (TxSize 8452) (TxSize 9484)

toKeyHash :: Text -> Script KeyHash
toKeyHash txt = case fromBase16 (T.encodeUtf8 txt) of
    Right bs -> case keyHashFromBytes (Payment, bs) of
        Just kh -> RequireSignatureOf kh
        Nothing -> error "Hash key not valid"
    Left _ -> error "Hash key not valid"

toPaymentHash :: Text -> Cardano.SimpleScript lang
toPaymentHash txt =
    case Cardano.deserialiseFromRawBytesHex (Cardano.AsHash Cardano.AsPaymentKey) (T.encodeUtf8 txt) of
        Right payKeyHash -> Cardano.RequireSignature payKeyHash
        Left err -> error $ "toPaymentHash: " <> show err

checkScriptHashes
    :: String
    -> Script KeyHash
    -> Cardano.Script lang
    -> SpecWith ()
checkScriptHashes title adrestiaScript nodeScript = it title $
    unScriptHash (toScriptHash adrestiaScript) `shouldBe`
    Cardano.serialiseToRawBytes (Cardano.hashScript nodeScript)

checkScriptPreimage
    :: Cardano.SerialiseAsCBOR (Cardano.Script lang)
    => String
    -> Script KeyHash
    -> Cardano.Script lang
    -> SpecWith ()
checkScriptPreimage title adrestiaScript nodeScript = it title $
    (serializeScript adrestiaScript) `shouldBe`
    BS.append "\00" (Cardano.serialiseToCBOR nodeScript)

scriptMatrix
    :: Cardano.SimpleScriptVersion lang
    -> [(String, Script KeyHash, Cardano.Script lang)]
scriptMatrix version =
    [ ( show version <> " RequireSignatureOf"
      , toKeyHash hashKeyTxt1
      , toSimpleScript $ toPaymentHash hashKeyTxt1
      )
    , ( show version <> " RequireSignatureOf"
      , toKeyHash hashKeyTxt2
      , toSimpleScript $ toPaymentHash hashKeyTxt2
      )
    , ( show version <> " RequireSignatureOf"
      , toKeyHash hashKeyTxt3
      , toSimpleScript $ toPaymentHash hashKeyTxt3
      )
    , ( show version <> " RequireSignatureOf"
      , toKeyHash hashKeyTxt4
      , toSimpleScript $ toPaymentHash hashKeyTxt4
      )
    , ( show version <> " RequireAllOf"
      , RequireAllOf [toKeyHash hashKeyTxt1, toKeyHash hashKeyTxt2]
      , toSimpleScript $
          Cardano.RequireAllOf [toPaymentHash hashKeyTxt1, toPaymentHash hashKeyTxt2]
      )
    , ( show version <> " RequireAllOf"
      , RequireAllOf [toKeyHash hashKeyTxt1, toKeyHash hashKeyTxt2, toKeyHash hashKeyTxt3]
      , toSimpleScript $
          Cardano.RequireAllOf [toPaymentHash hashKeyTxt1, toPaymentHash hashKeyTxt2, toPaymentHash hashKeyTxt3]
      )
    , ( show version <> " RequireAnyOf"
      , RequireAnyOf [toKeyHash hashKeyTxt1, toKeyHash hashKeyTxt2]
      , toSimpleScript $
          Cardano.RequireAnyOf [toPaymentHash hashKeyTxt1, toPaymentHash hashKeyTxt2]
      )
    , ( show version <> " RequireAnyOf"
      , RequireAnyOf [toKeyHash hashKeyTxt1, toKeyHash hashKeyTxt2, toKeyHash hashKeyTxt3]
      , toSimpleScript $
          Cardano.RequireAnyOf [toPaymentHash hashKeyTxt1, toPaymentHash hashKeyTxt2, toPaymentHash hashKeyTxt3]
      )
    , ( show version <> " RequireSomeOf"
      , RequireSomeOf 2 [toKeyHash hashKeyTxt1, toKeyHash hashKeyTxt2, toKeyHash hashKeyTxt3]
      , toSimpleScript $
          Cardano.RequireMOf 2 [toPaymentHash hashKeyTxt1, toPaymentHash hashKeyTxt2, toPaymentHash hashKeyTxt3]
      )
    , ( show version <> " RequireSomeOf"
      , RequireSomeOf 2 [toKeyHash hashKeyTxt1, toKeyHash hashKeyTxt2, toKeyHash hashKeyTxt3, toKeyHash hashKeyTxt4]
      , toSimpleScript $
          Cardano.RequireMOf 2 [toPaymentHash hashKeyTxt1, toPaymentHash hashKeyTxt2, toPaymentHash hashKeyTxt3, toPaymentHash hashKeyTxt4]
      )
    , ( show version <> " nested 1"
      , RequireSomeOf 2 [ toKeyHash hashKeyTxt1, toKeyHash hashKeyTxt2
                        , RequireAllOf [toKeyHash hashKeyTxt3, toKeyHash hashKeyTxt4]
                        ]
      , toSimpleScript $
          Cardano.RequireMOf 2 [ toPaymentHash hashKeyTxt1, toPaymentHash hashKeyTxt2
                               , Cardano.RequireAllOf [toPaymentHash hashKeyTxt3, toPaymentHash hashKeyTxt4]
                                       ]
      )
    , ( show version <> " nested 2"
      , RequireAllOf [ toKeyHash hashKeyTxt1
                     , RequireAnyOf [toKeyHash hashKeyTxt2, toKeyHash hashKeyTxt3, toKeyHash hashKeyTxt4]
                     ]
      , toSimpleScript $
          Cardano.RequireAllOf [ toPaymentHash hashKeyTxt1
                               , Cardano.RequireAnyOf [toPaymentHash hashKeyTxt2, toPaymentHash hashKeyTxt3, toPaymentHash hashKeyTxt4]
                               ]
      )
    , ( show version <> " nested 3"
      , RequireSomeOf 1 [ toKeyHash hashKeyTxt1
                        , RequireAllOf [ toKeyHash hashKeyTxt2
                                       , RequireAnyOf [toKeyHash hashKeyTxt3, toKeyHash hashKeyTxt4 ]
                                       ]
                        ]
      , toSimpleScript $
          Cardano.RequireMOf 1 [ toPaymentHash hashKeyTxt1
                               , Cardano.RequireAllOf [ toPaymentHash hashKeyTxt2
                                                      , Cardano.RequireAnyOf [toPaymentHash hashKeyTxt3, toPaymentHash hashKeyTxt4]
                                                      ]
                               ]
      )
    ]
  where
    toSimpleScript = Cardano.SimpleScript version
    hashKeyTxt1 = "deeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e"
    hashKeyTxt2 = "60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f"
    hashKeyTxt3 = "ffcbb72393215007d9a0aa02b7430080409cd8c053fd4f5b4d905053"
    hashKeyTxt4 = "96834025cdca063ce9c32dfae6bc6a3e47f8da07ee4fb8e1a3901559"

testScriptsAllLangs
    :: forall lang . Cardano.SimpleScriptVersion lang
    -> Spec
testScriptsAllLangs version = describe (show version) $ do
    forM_ (scriptMatrix version) $ \(title, adrestiaScript, nodeScript) ->
        checkScriptHashes title adrestiaScript nodeScript

testScriptPreimages
    :: forall lang . Cardano.IsScriptLanguage lang
    => Cardano.SimpleScriptVersion lang
    -> Spec
testScriptPreimages version = describe (show version) $ do
    forM_ (scriptMatrix version) $ \(title, adrestiaScript, nodeScript) ->
        checkScriptPreimage title adrestiaScript nodeScript

timelockScriptMatrix
    :: [(String, Script KeyHash, Cardano.Script Cardano.SimpleScriptV2)]
timelockScriptMatrix =
    [ ( "SimpleScriptV2 ActiveFromSlot"
      , RequireAllOf [toKeyHash hashKeyTxt1, ActiveFromSlot 120]
      , toSimpleScript $
          Cardano.RequireAllOf [toPaymentHash hashKeyTxt1, Cardano.RequireTimeAfter Cardano.TimeLocksInSimpleScriptV2 (SlotNo 120)]
      )
    , ( "SimpleScriptV2 ActiveUntilSlot"
      , RequireAllOf [toKeyHash hashKeyTxt1, ActiveUntilSlot 120]
      , toSimpleScript $
          Cardano.RequireAllOf [toPaymentHash hashKeyTxt1, Cardano.RequireTimeBefore Cardano.TimeLocksInSimpleScriptV2 (SlotNo 120)]
      )
    , ( "SimpleScriptV2 ActiveFromSlot and ActiveUntilSlot"
      , RequireAllOf [ActiveFromSlot 120, ActiveUntilSlot 150, RequireAnyOf [toKeyHash hashKeyTxt1, toKeyHash hashKeyTxt2]]
      , toSimpleScript $
          Cardano.RequireAllOf
          [ Cardano.RequireTimeAfter Cardano.TimeLocksInSimpleScriptV2 (SlotNo 120)
          , Cardano.RequireTimeBefore Cardano.TimeLocksInSimpleScriptV2 (SlotNo 150)
          , Cardano.RequireAnyOf [toPaymentHash hashKeyTxt1, toPaymentHash hashKeyTxt2 ]
          ]
      )
    ]
  where
    hashKeyTxt1 = "deeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e"
    hashKeyTxt2 = "60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f"
    toSimpleScript = Cardano.SimpleScript Cardano.SimpleScriptV2

testScriptsTimelockLang :: Spec
testScriptsTimelockLang =
    forM_ timelockScriptMatrix $ \(title, adrestiaScript, nodeScript) ->
        checkScriptHashes title adrestiaScript nodeScript

testTimelockScriptImagesLang :: Spec
testTimelockScriptImagesLang =
    forM_ timelockScriptMatrix $ \(title, adrestiaScript, nodeScript) ->
        checkScriptPreimage title adrestiaScript nodeScript

instance Arbitrary (Hash "Genesis") where
    arbitrary = Hash . BS.pack <$> vector 32

instance Arbitrary (Hash "BlockHeader") where
    arbitrary = Hash . BS.pack <$> vector 32

instance Arbitrary RewardAccount where
    arbitrary = RewardAccount . BS.pack <$> vector 28

instance Arbitrary (Tip (CardanoBlock StandardCrypto)) where
    arbitrary = frequency
        [ (10, return TipGenesis)
        , (90, arbitraryTip)
        ]
      where
        arbitraryTip = do
            n <- choose (0, 100)
            hash <- toCardanoHash
                . Hash
                . digest (Proxy @(HASH StandardCrypto))
                . BS.pack <$> vector 5
            return $ Tip (SlotNo n) hash (BlockNo n)

instance Arbitrary SL.UnitInterval where
    arbitrary = oneof
        [ pure interval0
        , pure interval1
        , pure $ unsafeBoundRational (1 % 2)
        , unsafeBoundRational . (% 1000) <$> choose (0, 1000)
        ]
    shrink = map unsafeBoundRational . shrink . SL.unboundRational

instance Arbitrary SlotId where
    arbitrary = SlotId
        <$> (W.EpochNo . fromIntegral <$> choose (0, 10 :: Word32))
        <*> (W.SlotInEpoch <$> choose (0, 10))

instance Arbitrary (ShelleyKey 'CredFromKeyK XPrv) where
    shrink _ = []
    arbitrary = do
        mnemonic <- arbitrary
        return $ Shelley.unsafeGenerateKeyFromSeed mnemonic mempty

instance Arbitrary (ByronKey 'CredFromKeyK XPrv) where
    shrink _ = []
    arbitrary = do
        mnemonic <- arbitrary
        acctIx <- arbitrary
        addrIx <- arbitrary
        pure $ Byron.unsafeGenerateKeyFromSeed (acctIx, addrIx) mnemonic mempty

instance
    ( Enum (AddressDerivation.Index derivationType depth)
    , Bounded (AddressDerivation.Index derivationType depth)
    ) =>
    Arbitrary (AddressDerivation.Index derivationType depth) where
    arbitrary = toEnum <$> chooseInt (0, maxIndex)
      where
        maxIndex = fromIntegral . getIndex $
            maxBound @(AddressDerivation.Index derivationType depth)

instance (WalletKey k, Arbitrary (k 'CredFromKeyK XPrv)) =>
    Arbitrary (k 'CredFromKeyK XPub)
  where
    shrink _ = []
    arbitrary = publicKey <$> arbitrary

instance Arbitrary SomeMnemonic where
    arbitrary = SomeMnemonic <$> genMnemonic @12

genMnemonic
    :: forall mw ent csz.
     ( ConsistentEntropy ent mw csz
     , EntropySize mw ~ ent
     )
    => Gen (Mnemonic mw)
genMnemonic = do
        let n = fromIntegral (natVal $ Proxy @(EntropySize mw)) `div` 8
        bytes <- BS.pack <$> vector n
        let ent = unsafeMkEntropy @(EntropySize mw) bytes
        return $ entropyToMnemonic ent

instance Show XPrv where
    show _ = "<xprv>"


instance Arbitrary TokenBundle.TokenBundle where
    arbitrary = genTokenBundleSmallRange
    shrink = shrinkTokenBundleSmallRange

newtype FixedSize32 a = FixedSize32 { unFixedSize32 :: a }
    deriving (Eq, Show)

newtype FixedSize48 a = FixedSize48 { unFixedSize48 :: a }
    deriving (Eq, Show)

newtype FixedSize64 a = FixedSize64 { unFixedSize64 :: a }
    deriving (Eq, Show)

newtype FixedSize128 a = FixedSize128 { unFixedSize128 :: a }
    deriving (Eq, Show)

newtype VariableSize16 a = VariableSize16 { unVariableSize16 :: a}
    deriving (Eq, Show)

newtype VariableSize1024 a = VariableSize1024 { unVariableSize1024 :: a}
    deriving (Eq, Show)

instance Arbitrary (FixedSize32 TokenBundle) where
    arbitrary = FixedSize32 <$> genTxOutTokenBundle 32
    -- No shrinking

instance Arbitrary (FixedSize48 TokenBundle) where
    arbitrary = FixedSize48 <$> genTxOutTokenBundle 48
    -- No shrinking

instance Arbitrary (FixedSize64 TokenBundle) where
    arbitrary = FixedSize64 <$> genTxOutTokenBundle 64
    -- No shrinking

instance Arbitrary (FixedSize128 TokenBundle) where
    arbitrary = FixedSize128 <$> genTxOutTokenBundle 128
    -- No shrinking

instance Arbitrary (VariableSize16 TokenBundle) where
    arbitrary = VariableSize16 <$> resize 16 genTokenBundle
    -- No shrinking

instance Arbitrary (VariableSize1024 TokenBundle) where
    arbitrary = VariableSize1024 <$> resize 1024 genTokenBundle
    -- No shrinking

--
-- Helpers
--
--

bech32 :: ByteString -> Text
bech32 = Bech32.encodeLenient hrp . Bech32.dataPartFromBytes
  where hrp = [humanReadablePart|addr|]

-- Expected bech32 encoding for testnets
-- https://github.com/cardano-foundation/CIPs/tree/master/CIP5
bech32testnet :: ByteString -> Text
bech32testnet = Bech32.encodeLenient hrp . Bech32.dataPartFromBytes
  where hrp = [humanReadablePart|addr_test|]

base58 :: ByteString -> Text
base58 = T.decodeUtf8 . encodeBase58 bitcoinAlphabet

unsafeBoundRational :: Rational -> SL.UnitInterval
unsafeBoundRational =
    fromMaybe (error "unsafeBoundRational: the impossible happened")
    . SL.boundRational
