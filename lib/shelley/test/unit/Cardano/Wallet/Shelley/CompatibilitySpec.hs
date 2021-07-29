{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
    ( ApiBalanceTransactionPostData
    , DecodeAddress (..)
    , DecodeStakeAddress (..)
    , EncodeStakeAddress (..)
    )
import Cardano.Wallet.Byron.Compatibility
    ( maryTokenBundleMaxSize )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , NetworkDiscriminant (..)
    , PaymentAddress (..)
    , WalletKey
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..) )
import Cardano.Wallet.Primitive.Types
    ( DecentralizationLevel (..), SlotId (..), TokenBundleMaxSize (..) )
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
    ( genFixedSizeTokenBundle
    , genTokenBundle
    , genTokenBundleSmallRange
    , shrinkTokenBundleSmallRange
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( TokenBundleSizeAssessment (..)
    , TokenBundleSizeAssessor (..)
    , TxSize (..)
    )
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
    , toPoint
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
import Data.Aeson
    ( eitherDecode )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase, convertToBase )
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
    ( BlockNo (..), Point, SlotNo (..), Tip (..), getTipPoint )
import System.FilePath
    ( (</>) )
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
import Test.Utils.Paths
    ( getTestData )

import qualified Cardano.Api as Cardano
import qualified Cardano.Ledger.Address as SL
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Shelley as SL
import qualified Cardano.Ledger.Shelley as SLAPI
import qualified Cardano.Wallet.Primitive.AddressDerivation.Byron as Byron
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shelley as Shelley
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T
import qualified Shelley.Spec.Ledger.PParams as SL

spec :: Spec
spec = do
    describe "Conversions" $ do
        it "toPoint' . fromTip' == getTipPoint" $ property $ \gh tip -> do
            let fromTip' = fromTip gh
            let toPoint' = toPoint gh :: W.BlockHeader -> Point (CardanoBlock StandardCrypto)
            toPoint' (fromTip' tip) === (getTipPoint tip)

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
            let Address addr = paymentAddress @'Mainnet @ShelleyKey k
            case SL.deserialiseAddr @StandardCrypto addr of
                Just _ -> property True
                Nothing -> property False

        prop "Shelley addresses from base16, bech32 and base58" $ \k -> do
            let addr@(Address bytes) = paymentAddress @'Mainnet @ShelleyKey k
            conjoin
                [ decodeAddress @'Mainnet (base16 bytes) === Right addr
                    & counterexample (show $ base16 bytes)
                , decodeAddress @'Mainnet (bech32 bytes) === Right addr
                    & counterexample (show $ bech32 bytes)
                , decodeAddress @'Mainnet (base58 bytes) === Right addr
                    & counterexample (show $ base58 bytes)
                ]

        prop "Shelley addresses from bech32 - testnet" $ \k ->
            let addr@(Address raw) = paymentAddress @('Testnet 0) @ShelleyKey k
            in decodeAddress @('Testnet 0) (bech32testnet raw) === Right addr
                   & counterexample (show $ bech32testnet raw)

        prop "Byron addresses from base16, bech32 and base58" $ \k -> do
            let addr@(Address bytes) = paymentAddress @'Mainnet @ByronKey k
            conjoin
                [ decodeAddress @'Mainnet (base16 bytes) === Right addr
                    & counterexample (show $ base16 bytes)
                , decodeAddress @'Mainnet (bech32 bytes) === Right addr
                    & counterexample (show $ bech32 bytes)
                , decodeAddress @'Mainnet (base58 bytes) === Right addr
                    & counterexample (show $ base58 bytes)
                ]

    describe "decentralizationLevelFromPParams" $ do

        let mkDecentralizationParam :: SL.UnitInterval -> SLAPI.PParams (SL.ShelleyEra StandardCrypto)
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
                    & unDecentralizationLevel
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

    describe "deserialize cborHash" $ do
        let matrix =
                [ ( "multiple-output tx with no inputs missing"
                  , "84a600818258200eaa33be8780935ca5a7c1e628a2d54402446f96236c\
                    \a8f1770e07fa22ba8648000d80018482583901a65f0e7aea387adbc109\
                    \123a571cfd8d0d139739d359caaf966aa5b9a062de6ec013404d4f9909\
                    \877d452fc57dfe4f8b67f94e0ea1e8a0ba1a000f422a82583901ac9a56\
                    \280ec283eb7e12146726bfe68dcd69c7a85123ce2f7a10e7afa062de6e\
                    \c013404d4f9909877d452fc57dfe4f8b67f94e0ea1e8a0ba1a000f422a\
                    \825839011a2f2f103b895dbe7388acc9cc10f90dc4ada53f46c841d2ac\
                    \44630789fc61d21ddfcbd4d43652bf05c40c346fa794871423b65052d7\
                    \614c1b0000000ba42b176a82583901c59701fee28ad31559870ecd6ea9\
                    \2b143b1ce1b68ccb62f8e8437b3089fc61d21ddfcbd4d43652bf05c40c\
                    \346fa794871423b65052d7614c1b0000000ba42b176a021a000234d803\
                    \198ceb0e80a0f5f6"
                  )
                , ( "single-output tx with inputs missing"
                  , "84a600818258200eaa33be8780935ca5a7c1e628a2d54402446f96236c\
                    \a8f1770e07fa22ba86480d0d800182825839010acce4f85ade867308f0\
                    \48fe4516c0383b38cc04602ea6f7a6a1e75f29450899547b0e4bb19413\
                    \2452d45fea30212aebeafc69bca8744ea61a002dc67e8258390110a9b4\
                    \666ba80e4878491d1ac20465c9893a8df5581dc705770626203d4d23fe\
                    \6a7acdda5a1b41f56100f02bfa270a3c560c4e55cf8312331b00000017\
                    \484721ca021a0001ffb803198d280e80a0f5f6"
                  )
                , ( "pubkey-2.json from plutus"
                  , "84a60081825820888963613d2bb4c5c55cee335f724624cbc54b185eca\
                    \a2fb1eb07545ed5db421010d80018002000e800b58201dd28c3485f707\
                    \dd5fb5db3ef96c5ed723c05ce5ef5439b0cc291f8a8ac6531ca3038159\
                    \1b6f591b6c010000332332233322232323232332232333222333222333\
                    \3333322222222332233333222223332223333222233223322332233223\
                    \3223332223322332233223322323232323232323232323232323232323\
                    \2323232323232323232323232323232323232323232323232323232323\
                    \2323232323232323232323232323232323232323232323232300112001\
                    \23355002307812001307812001112222223335306433300433503a0060\
                    \0333503a00500230070012088012350810149821c048c8c8c8c8c8c8cc\
                    \cd5ca99b8935573e600a24002900011801090009180389000843809198\
                    \26180109000980189000918039aba230031200123043357446ae8cc008\
                    \480048d55d018010900091ba900323507b4988c8c8c8c8c8c8c8c8c8c8\
                    \c8c8c8c8c8c8c8c8c8c8c8c8cccd5ca99b8935573e602a240029000118\
                    \01090009180b8900084b009199999999982e9801090009801890009802\
                    \0900098028900098030900098038900098040900098048900098050900\
                    \0980589000919a822980b890009aba23013120012335044301e1200135\
                    \744602224002466a086604e240026ae88c03c480048cd4108c09848004\
                    \d5d1180689000919a8209817890009aba2300b12001233504033550413\
                    \0321200130311200135744601224002466a072606c240026ae88c01c48\
                    \0048cd40f8c0f448004d5d1180289000919a81e99aa81f182289000982\
                    \4090009aba23003120012304f357446ae8cc008480048d5d1980109000\
                    \91aba330021200123574660042400246ae8cc008480048d5d198010900\
                    \091aba330021200123574660042400246ae8cc008480048d55d0180109\
                    \00091ba900323507a4988ccd41d800c0180088c8c8c8c8c8c8cccd5ca9\
                    \9b8935573e600a24002900011801090009180389000842009198269801\
                    \09000980189000918229aba23003120012300d357446ae8cc008480048\
                    \d55d018010900091ba90032350784988d4c11400488cdd22400066aed0\
                    \c010008cd5da18068009bb24988d4c18400488cdd22400066aed0c0100\
                    \08cd5da1ba7001376493119ba448000cd5da1ba800137649311999999a\
                    \baf2323232323232323333572a66e24d55cf980389000a400046004240\
                    \024a100021040246666aaed494200048c00c48004c01848004208048cc\
                    \cd55da9283f91801890009802090008408091999aabb52300312001250\
                    \7e35746600624002100024666a6a0f6600e6ae88c00c4800488ccd4d41\
                    \f4c048d5d1180209000911a84080998260020011283f840809283e83f9\
                    \1aba3300212001235574060042400246ea400c941e0941e0941e0941e0\
                    \0041e88ccccccd5d79191919191919191999ab95337126aae7cc01c480\
                    \05200023002120012507f08101233335576a4a0fe4600624002600c240\
                    \021020246666aaed4941f88c00c48004c01048004200048cccd55da918\
                    \01890009283e9aba330031200107f233353507a3007357446006240024\
                    \4666a6a0f860526ae88c0104800488d420004cc1a0010008941f820004\
                    \941f01f88d5d198010900091aaba0300212001237520064a0ee4a0ee4a\
                    \0ee4a0ee0020f246666666aebc8c8c8c8c8c8cccd5ca99b8935573e600\
                    \a24002900011801090009283e03f11999aabb52507c230031200130041\
                    \200107e233335576a46006240024a0f66ae8cc00c480041f48ccd4d41e\
                    \0c0d8d5d1180109000911a83e0011283d03e11aaba0300212001237520\
                    \064a0ec4a0ec4a0ec4a0ec0020f04666a0dc00a0100044646464646464\
                    \64646666ae54cdc49aab9f300712001480008c008480048c024480041f\
                    \88ccc124c00848004c00c48004c010480048c024d5d1180289000919a8\
                    \161808090009aba23003120012335008303312001357446ae8cc008480\
                    \048d5d198010900091aaba03002120012375200646a0e0931191919191\
                    \9191999ab95337126aae7cc0144800520002300212001230071200107b\
                    \23304d3002120013003120012303b35744600624002466a00c60362400\
                    \26ae88d5d198010900091aaba03002120012375200646a0de930911919\
                    \191919191999ab953371260082400290001180109000918020900083d9\
                    \1a83c980109000919a8168039aba235574060082400246666ae54cdc49\
                    \80109000a40044a0ee4600a240020f246aae7cc008480048dd480191a8\
                    \37a4c46a607a00244466e912000335768600a00666aed0cd403cc02848\
                    \004008cd5da19a8031817090008009bb24988d4c10c00488cdd2240006\
                    \6aed0c060008cd5da19a802180b090008009bb2498488ccd4d41b80048\
                    \8cdd22400066aed0cd40a4010008dd924c466e91200237649303911999\
                    \999abaf23232323232323232323333572a66e24d55cf980489000a4000\
                    \46004240024a0f00f446666aaed4941e08c00c48004c020480041e88cc\
                    \cd55da9283b918018900098030900083c91999aabb5250762300312001\
                    \300412001078233335576a46006240024a0ea6ae8cc00c480041dc8ccd\
                    \4d41c8c020d5d118020900091199a9a83a19a8099806090009aba23005\
                    \120012233353507633500d30321200135744600c24002446a0f466608e\
                    \00c0080044a0f00f44a0ec0f04a0e80ec46ae8cc008480048d5d198010\
                    \900091aaba0300212001237520064a0dc4a0dc4a0dc4a0dc0020e04666\
                    \6666aebc8c8c8c8c8c8c8c8cccd5ca99b8935573e600e2400290001180\
                    \1090009283a83b91999aabb52507523003120013006120010772333355\
                    \76a4a0e846006240026008240020ec46666aaed48c00c48004941ccd5d\
                    \198018900083a9199a9a838180d1aba230031200122333535072335009\
                    \30181200135744600824002446a0ec660940080044a0e80ec4a0e40e84\
                    \6ae8cc008480048d55d018010900091ba90032506d2506d2506d2506d0\
                    \0106f1223333333575e46464646464646464646666ae54cdc498028900\
                    \0a400046004240024600a240020f246666aaed4941dc8c00c48004c01c\
                    \480041e48cccd55da91801890009283b1aba3300612001078233353507\
                    \333502e00a35744600a24002446a0ee6a0ee0044a0ea0ee46666ae54cd\
                    \c4980109000a400446008240024a0e80ec46aae7cc010480048cccd55d\
                    \a91802890009283918010900083a11aaba03002120012375200846a0de\
                    \a0dc4a0da4a0da4a0da4a0da0020de466aa03c60142400260042400246\
                    \6aa006600424002601a240024666a0c404804aeb44488ccd4188cd5400\
                    \c008004cd54014008004cd5401c0080044488d400ccd5406c008004488\
                    \c8c8dd318008019a80090008918009aa83591199a9a81c00091bb24988\
                    \88cd5da19a812004001980280103608911a80199aa80c8010008911919\
                    \1999999abaf25067250672300237560084a0ce4a0ce0060d26a0024002\
                    \2460026aa0d244646666aaed48c008480048ccd4d41a0cd408c01cd5d1\
                    \00191199a9a83518031aba3005223506e33503d0040022506c06e2506a\
                    \06c00206b235069503911223501633550170020012333505b01d01e75a\
                    \4666a0b4004006044466666666a60920024466e912002335768600e004\
                    \6ec92622233748900219abb430080033357686ea0008dd924c4466e912\
                    \000335768600e0046ec92623374890051bb24988cdd2240186ec926222\
                    \33748900319abb4375000666aed0dd40011bb2498888cdd22401066aed\
                    \0dd400199abb4374e0046ec92606223333333575e46464646464646464\
                    \64646464646464646464646464646464646464646464646464646666ae\
                    \54cdc4980e89000a4018460042400246008240021080246666aaed48c0\
                    \0c480049420804c08448004210048d4204041b48cccd5ca99b89301b12\
                    \001480288c00c480048c01448004208048cccd55da91801890009283f9\
                    \80f090008408091a83f03591999ab95337126030240029004118018900\
                    \0918040900083f91999aabb52507c2300312001301b1200107e2333355\
                    \76a4a0f646006240026008240020fa46666aaed48c00c48004941e8d5d\
                    \198018900083e1199a9a83b981a9aba230181200122333535079302635\
                    \744600824002446a0fa660ce0080044a0f60fa4a0f20f646ae8cc05c48\
                    \0048cccd5ca99b89301212001480188c00c480048c020480041e48cccd\
                    \55da9283b1180189000980a8900083c11999aabb525075230031200130\
                    \0412001077233335576a46006240024a0e86ae8cc00c480041d88ccd4d\
                    \41c4c0bcd5d118090900091199a9a83998189aba230041200122350773\
                    \3062004002250750772507307523574660222400246666ae54cdc49806\
                    \09000a4008460062400246010240020e646666aaed4941c08c00c48004\
                    \c03c480041c88cccd55da92837918018900098020900083891999aabb5\
                    \23003120012506e357466006240020e04666a6a0d660286ae88c030480\
                    \0488ccd4d41b4c0acd5d1180209000911a838998300020011283783892\
                    \83683791aba3300b1200123333572a66e24c0184800520022300312001\
                    \230061200106d233335576a4a0d446006240026012240020d846666aae\
                    \d48c00c48004941a4d5d19804090008359199a9a83318079aba2300712\
                    \001223506a305a0022506806a23333572a66e24c008480052000230041\
                    \200125067069235573e600c2400246666aaed4941948c00c48004c0104\
                    \800419c8cccd55da9180189000928321aba33003120010662333535061\
                    \300a35744600424002446a0ca60a60044a0c60ca46aae80c008480048d\
                    \d48019282f9282f9282f9282f8008309199a82b804004bac2333505600\
                    \200401f2335304d001233748900019abb4300300137649311119ba4480\
                    \08cd5da1ba70033357686e9c008cd5da1ba7001376493119a982780091\
                    \9ba448000cd5da1ba8001376493119ba448008cd5da1ba800137649311\
                    \999999abaf232323232323232323232323232323333572a66e24c02448\
                    \00520022300212001230091200106b233335576a4a0d24600624002601\
                    \a240020d646666aaed4941a08c00c48004c018480041a88cccd55da928\
                    \33918018900098020900083491999aabb5230031200125066357466006\
                    \240020d04666a6a0c660206ae88c0244800488ccd4d4194c048d5d1180\
                    \28900091199a9a833980a1aba2300612001223506b33305d0060040022\
                    \506906b250670692506506723574660042400246ae8cc01c480048cccd\
                    \5ca99b89300212001480008c01048004941881908d55cf980309000919\
                    \99aabb5250602300312001300412001062233335576a46006240024a0b\
                    \e6ae8cc00c480041848ccd4d4170c018d5d1180109000911a830182980\
                    \11282f03011aaba0300212001237520064a0b44a0b44a0b44a0b40020b\
                    \846666666aebc8c8c8c8c8c8c8c8c8c8c8cccd5ca99b89300512001480\
                    \088c008480048c014480041988cccd55da928321180189000980489000\
                    \83311999aabb5230031200125063357466010240020ca4666a6a0c0603\
                    \c6ae88c01c4800488d4190c164008941881908cccd5ca99b8930021200\
                    \1480008c010480049418418c8d55cf98030900091999aabb52505f2300\
                    \312001300412001061233335576a46006240024a0bc6ae8cc00c480041\
                    \808ccd4d416cc064d5d1180109000911a82f982a8011282e82f91aaba0\
                    \300212001237520064a0b24a0b24a0b24a0b20020b64666a0a2004006e\
                    \b08dd380091999999abaf25056250562505623505737580044a0ac0020\
                    \b02446464646464646666ae54cdc49aab9f300512001480008c0084800\
                    \48c01c480041788cd40c8c00848004c00c480048cd402001cd5d118018\
                    \9000919a8040031aba23574660042400246aae80c008480048dd480191\
                    \a82924c2446464646464646666ae54cdc49aab9f300512001480008c00\
                    \8480048c01c480041748cd40d4c00848004c00c480048cd402401cd5d1\
                    \180189000918049aba23574660042400246aae80c008480048dd480191\
                    \a828a4c2446464646464646666ae54cdc49aab9f300512001480008c00\
                    \8480048c01c480041708cd40c8c00848004c00c480048cd402001cd5d1\
                    \180189000918041aba23574660042400246aae80c008480048dd480191\
                    \a82824c244646464646464646666ae54cdc4980289000a40084a06e460\
                    \04240020b846666ae54cdc4980289000a400446006240024600a240020\
                    \b846a06e600424002466a01a00e6ae88d55d018020900091999ab95337\
                    \1260042400290001281a918028900082c91aab9f300212001237520064\
                    \6a09e9311919191999ab95337126004240029001101b118010900082b1\
                    \1999ab95337126004240029000101a918020900082b11aab9f37520064\
                    \6a09a931199a8248058063ad1223232300137560066a00240022460026\
                    \aa0a8446666aaed4940908cd408ccd4024018d5d100118019aba300200\
                    \1055112233350483355005002001335500700200133550030020011122\
                    \23232323232323333572a66e24d55cf980289000a40004600424002460\
                    \0e240020ae466aa04e600424002600624002466a0120106ae88c00c480\
                    \048cd4020018d5d11aba3300212001235574060042400246ea400c8d41\
                    \2d26123535044001222001112223535501e0012233748900019abb4335\
                    \00600500233576866a00c0080026ec9261235350420012220031122233\
                    \33333575e4646464646464646666ae54cdc49aab9f300712001480008c\
                    \008480049414c1548cccd55da92829918018900098030900082a91999a\
                    \abb5250522300312001300412001054233335576a46006240024a0a26a\
                    \e8cc00c4800414c8ccd4d4138cd4024020d5d118018900091199a9a828\
                    \19a8058049aba230041200122350543355026004002250520542505005\
                    \223574660042400246aae80c008480048dd48019282592825928259282\
                    \5800826891a9a8200009110011199a820001001bad2375000246666666\
                    \aebc941149411494114941148d4118dd68010008239199a81ea8012822\
                    \001890009000919191919191919191919191919191999ab95337126014\
                    \240029003118010900091802090008289181a980109000918079aba235\
                    \574060182400246666ae54cdc4980409000a400846006240024600a240\
                    \0209e460606004240024601a6ae88d55d018048900091999ab95337126\
                    \00a2400290011180189000918028900082611816180109000918061aba\
                    \2355740600c2400246666ae54cdc4980109000a400046008240024600e\
                    \2400209246aae7cc010480048c0a8c008480048dd69aba235574060042\
                    \400246ea400c8d40f52623232323232323232323232323232323232323\
                    \23232323232323333572a66e24c05448005200c2046230021200105b23\
                    \333572a66e24c05448005200a2047230031200105b23333572a66e24c0\
                    \504800520082300312001230071200105a233041300212001300312001\
                    \2375a6ae88c00c480048dd61aba23574660042400246aae80c04c48004\
                    \8cccd5ca99b89300f12001480188c00c480048c01c480041548cc0f4c0\
                    \0848004c00c480048dd69aba23003120012375a6ae88d5d19801090009\
                    \1aaba0300e1200123333572a66e24c0284800520042300312001230071\
                    \200105023303c3002120013003120012300e3574460062400246eb4d5d\
                    \11aba3300212001235574060122400246666ae54cdc4980289000a4004\
                    \46006240024600a2400209646070600424002460126ae88d55d0180309\
                    \00091999ab953371260042400290001180209000918038900082411aab\
                    \9f3004120012303230021200123005357446aae80c008480048dd48019\
                    \1a81e24c46464646464646464646464646666ae54cdc4980409000a400\
                    \44600424002460102400209a466607a600424002600624002600824002\
                    \46eb0d5d118028900091bac3574460062400246eb0d5d11aba33002120\
                    \0123574660042400246aae80c018480048cccd5ca99b89300212001480\
                    \008c010480048c01c4800411c8d55cf9802090009181b1801090009180\
                    \29aba235574060042400246ea400c8d40ed26232323232323232323333\
                    \572a66e24c010480052002230021200123004120010482303b30021200\
                    \12375a6ae88d55d018030900091999ab95337126004240029000118020\
                    \9000918038900082311aab9f300412001230383002120012375a6ae88d\
                    \55d018010900091ba900323503a4988c8c8c8c8c8c8cccd5ca99b89355\
                    \73e600a240029000118010900091803890008229198159801090009801\
                    \89000918039aba2300312001237586ae88d5d198010900091aaba03002\
                    \120012375200646a0729311919191999ab95337126aae7cc0084800520\
                    \00230021200123004120010412375a6ae88d55d018010900091ba90032\
                    \350384988848cc00400c0088004888888888848cccccccccc00402c028\
                    \02402001c01801401000c00880048848cc00400c008800488848ccc004\
                    \01000c00880044488008488488cc00401000c48004448848cc00400c00\
                    \84480048848cc00400c008800448848cc00400c0084800448848cc0040\
                    \0c0084800448848cc00400c00848004484888c00c01044888008448880\
                    \04480044880084880048004848888c010014848888c00c014848888c00\
                    \8014848888c00401480048848cc00400c0088004848888888c01c02088\
                    \48888888cc018024020848888888c014020488888880104888888800c8\
                    \848888888cc0080240208848888888cc00402402080048488c00800c88\
                    \8488ccc00401401000c80048488c00800c8488c00400c80048ccd400c0\
                    \1801c010488848ccc00401000c008480048c8cccd5ca99b8935573e6ea\
                    \40092000200b230021200100a2350034984988d4c018004cdd2240006e\
                    \c92623333333575e46464646666ae54cdc49aab9f300212001480008c0\
                    \0848004940200288cccd55da9180209000928041aaba030021200100a2\
                    \375200846a00c0104a0084a0084a0084a00800200c2424460040062244\
                    \0022400224002400222464600200244660066460020020040026646664\
                    \4466644466666666444444446644666664444466644466664444664466\
                    \4466446664446644664466644466446644664466446466446464664464\
                    \646464646400244446600a6020002008446a60200044444444444666a6\
                    \a04e666aa601e24002a01a4666a606a666ae54cdc800600081b81b11a8\
                    \15801128150098019101c101b009099a804a800a80d891199a9a80d801\
                    \1100210010031a80090008918009aa81e910891911199a9a80b80091a8\
                    \05802911199803090009802801999aa98048900080380280100491a804\
                    \801090009000891a9a80180091000891a9a80100091001091091980080\
                    \18010900091a9801000910011109198008018011000911111111109199\
                    \9999999800805805004804003803002802001801100091091980080180\
                    \1100091109199800802001801100088910010910911980080200189000\
                    \8891091980080180108900091091980080180110008909118010018891\
                    \0008900089109198008018010900089109198008018010900089109198\
                    \0080180109000890911180180208911001089110008900089100109100\
                    \0900090911118020029091111801802909111180100290911118008029\
                    \0009109198008018011000909111111180380411091111111980300480\
                    \4109111111180280409111111002091111110019109111111198010048\
                    \0411091111111980080480410009091180100191109119980080280200\
                    \1900090911801001909118008019000889191800800911980199180080\
                    \0801000a451c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1c\
                    \f2d40e61a300010481d879800581840000d8798082191a221a019d6038\
                    \f5f6"
                  )
                ]

        forM_ matrix $ \(title, bs) -> do
            let (Right txFromCBOR) = Cardano.deserialiseFromCBOR (Cardano.AsTx Cardano.AsAlonzoEra) (unsafeFromBase16 bs)
            let envelope = Cardano.serialiseToTextEnvelope Nothing txFromCBOR
            let (Right txFromTextEnvelope) = Cardano.deserialiseFromTextEnvelope (Cardano.proxyToAsType Proxy) envelope
            it title $ txFromCBOR `shouldBe` txFromTextEnvelope

    -- ADP-656 idea is here to decode every plutus example and try to Server.balanceTransaction
    -- with wallets having enough funds, wallets not having enough funds.
    -- It would be better to test it here rather than in integration testing.
    -- Moreover, we can test some properties about contents of SealedTx like
    -- it only gets bigger, fee only increase when adding new inputs, etc.
    -- for now preparing infrastructure fo that
    describe "decode plutus jsons and coin select for different wallets" $ do
        let testPlutusDir = $(getTestData) </> "plutus"
        let matrix =
                [ "auction_1-2.json"
                , "crowdfunding-success-4.json"
                , "currency-2.json"
                , "escrow-redeem_1-3.json"
                , "escrow-redeem_2-4.json"
                , "escrow-refund-2.json"
                , "future-increase-margin-2.json"
                , "future-increase-margin-5.json"
                , "future-increase-margin-6.json"
                , "future-increase-margin-7.json"
                , "future-pay-out-2.json"
                , "future-pay-out-5.json"
                , "future-pay-out-6.json"
                , "future-settle-early-2.json"
                , "future-settle-early-5.json"
                , "future-settle-early-6.json"
                , "game-sm-success-2.json"
                , "game-sm-success-4.json"
                , "game-sm-success_2-2.json"
                , "game-sm-success_2-4.json"
                , "game-sm-success_2-6.json"
                , "multisig-failure-2.json"
                , "multisig-sm-10.json"
                , "multisig-sm-11.json"
                , "multisig-sm-2.json"
                , "multisig-sm-3.json"
                , "multisig-sm-4.json"
                , "multisig-sm-5.json"
                , "multisig-sm-6.json"
                , "multisig-sm-7.json"
                , "multisig-sm-8.json"
                , "multisig-sm-9.json"
                , "multisig-success-2.json"
                , "ping-pong-2.json"
                , "ping-pong-3.json"
                , "ping-pong_2-2.json"
                --, "prism-3.json" -- Error in $[0]: there should be one 'lovelace' in 'value'
                , "pubkey-2.json"
                --, "stablecoin_1-2.json" -- Error in $[0]: Value should not be empty
                , "stablecoin_1-3.json"
                , "stablecoin_1-4.json"
                --, "stablecoin_2-2.json" --Error in $[0]: Value should not be empty
                , "stablecoin_2-3.json"
                , "token-account-2.json"
                , "token-account-5.json"
                , "uniswap-10.json"
                , "uniswap-2.json"
                , "uniswap-7.json"
                --, "uniswap-9.json" -- Error in $[0]: there should be one 'lovelace' in 'value'
                , "vesting-2.json"
                ]
        forM_ matrix $ \json -> do
            let testFile = testPlutusDir </> json
            it json $ property $ \(_thereWillBeWalletsHere :: Int) -> monadicIO $ do
                bs <- run $ BL.readFile testFile
                let decodeResult = eitherDecode @(ApiBalanceTransactionPostData 'Mainnet) bs
                assert (isRight decodeResult)

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
    :: Blind (VariableSize128 TokenBundle)
    -> Blind (VariableSize16 TokenBundle)
    -> Property
prop_assessTokenBundleSize_enlarge b1' b2' =
    assess b1 == OutputTokenBundleSizeExceedsLimit ==> conjoin
        [ assess (b1 `TokenBundle.add` b2)
            === OutputTokenBundleSizeExceedsLimit
        , assess (b1 `TokenBundle.setCoin` maxBound)
            === OutputTokenBundleSizeExceedsLimit
        ]
  where
    assess = assessTokenBundleSize
        $ tokenBundleSizeAssessor maryTokenBundleMaxSize
    b1 = unVariableSize128 $ getBlind b1'
    b2 = unVariableSize16 $ getBlind b2'

-- Shrinking a token bundle that is within the size limit should yield a token
-- bundle that is still within the size limit.
--
prop_assessTokenBundleSize_shrink
    :: Blind (VariableSize128 TokenBundle)
    -> Blind (VariableSize16 TokenBundle)
    -> TokenBundleMaxSize
    -> Property
prop_assessTokenBundleSize_shrink b1' b2' maxSize =
    assess b1 == TokenBundleSizeWithinLimit ==> conjoin
        [ assess (b1 `TokenBundle.difference` b2)
            === TokenBundleSizeWithinLimit
        , assess (b1 `TokenBundle.setCoin` minBound)
            === TokenBundleSizeWithinLimit
        ]
  where
    assess = assessTokenBundleSize (tokenBundleSizeAssessor maxSize)
    b1 = unVariableSize128 $ getBlind b1'
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
        OutputTokenBundleSizeExceedsLimit
        maryTokenBundleMaxSize
        (TxSize 4228) (TxSize 4748)

unit_assessTokenBundleSize_fixedSizeBundle_128
    :: Blind (FixedSize128 TokenBundle) -> Property
unit_assessTokenBundleSize_fixedSizeBundle_128 (Blind (FixedSize128 b)) =
    unit_assessTokenBundleSize_fixedSizeBundle b
        OutputTokenBundleSizeExceedsLimit
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
        Just payKeyHash -> Cardano.RequireSignature payKeyHash
        Nothing -> error "Hash key not valid"

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

instance Arbitrary (ShelleyKey 'AddressK XPrv) where
    shrink _ = []
    arbitrary = do
        mnemonic <- arbitrary
        return $ Shelley.unsafeGenerateKeyFromSeed mnemonic mempty

instance Arbitrary (ByronKey 'AddressK XPrv) where
    shrink _ = []
    arbitrary = do
        mnemonic <- arbitrary
        acctIx <- toEnum <$> arbitrary
        addrIx <- toEnum <$> arbitrary
        return $ Byron.unsafeGenerateKeyFromSeed (acctIx, addrIx) mnemonic mempty

instance (WalletKey k, Arbitrary (k 'AddressK XPrv)) => Arbitrary (k 'AddressK XPub)
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

newtype VariableSize128 a = VariableSize128 { unVariableSize128 :: a}
    deriving (Eq, Show)

instance Arbitrary (FixedSize32 TokenBundle) where
    arbitrary = FixedSize32 <$> genFixedSizeTokenBundle 32
    -- No shrinking

instance Arbitrary (FixedSize48 TokenBundle) where
    arbitrary = FixedSize48 <$> genFixedSizeTokenBundle 48
    -- No shrinking

instance Arbitrary (FixedSize64 TokenBundle) where
    arbitrary = FixedSize64 <$> genFixedSizeTokenBundle 64
    -- No shrinking

instance Arbitrary (FixedSize128 TokenBundle) where
    arbitrary = FixedSize128 <$> genFixedSizeTokenBundle 128
    -- No shrinking

instance Arbitrary (VariableSize16 TokenBundle) where
    arbitrary = VariableSize16 <$> resize 16 genTokenBundle
    -- No shrinking

instance Arbitrary (VariableSize128 TokenBundle) where
    arbitrary = VariableSize128 <$> resize 128 genTokenBundle
    -- No shrinking

--
-- Helpers
--
--

base16 :: ByteString -> Text
base16 = T.decodeUtf8 . convertToBase Base16

unsafeFromBase16 :: Text -> ByteString
unsafeFromBase16 txt =
    let (Right bs) = convertFromBase Base16 $ T.encodeUtf8 txt
    in bs

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
