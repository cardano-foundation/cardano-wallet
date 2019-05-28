{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.HttpBridge.TransactionSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.HttpBridge.Binary
    ( encodeSignedTx )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge )
import Cardano.Wallet.HttpBridge.Environment
    ( Network (..), network )
import Cardano.Wallet.HttpBridge.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , Key
    , Passphrase (..)
    , XPrv
    , keyToAddress
    , publicKey
    , unsafeGenerateKeyFromSeed
    )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.CoinSelection.LargestFirst
    ( largestFirst )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Hash (..)
    , ShowFmt (..)
    , Tx (..)
    , TxIn (..)
    , TxOut (..)
    , TxWitness (..)
    , UTxO (..)
    )
import Cardano.Wallet.Transaction
    ( ErrMkStdTx (..), TransactionLayer (..) )
import Control.Arrow
    ( first )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Digest.CRC32
    ( crc32 )
import Data.Functor.Identity
    ( Identity (runIdentity) )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word32 )
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldBe, xit )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , InfiniteList (..)
    , Property
    , choose
    , property
    , scale
    , vectorOf
    , withMaxSuccess
    , (===)
    )

import qualified Cardano.Wallet.Primitive.CoinSelection as CS
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "estimateSize" $ do
        it "Arbitrary CoinSelection" $ property $ \(ShowFmt cs) ->
            property $ isValidSelection cs
        it "Estimated size is the same as taken by encodeSignedTx"
            (withMaxSuccess 2500 $ property propSizeEstimation)

    describe "mkStdTx" $ do
        it "Unknown input address yields an error" $ do
            let addr = keyToAddress @HttpBridge $ publicKey $ xprv "addr"
            let res = mkStdTx tl keyFrom inps outs
                  where
                    tl = newTransactionLayer
                    keyFrom = const Nothing
                    inps =
                        [ ( TxIn (Hash "arbitrary") 0
                          , TxOut addr (Coin 0)
                          )
                        ]
                    outs = []
            res `shouldBe` Left (ErrKeyNotFoundForAddress addr)

    describe "Golden Tests - Cardano-SL - signed tx" $ case network of
        Mainnet -> do
            goldenTestSignedTx 1
                [ (xprv "addr-0", Coin 42) ]
                "82839f8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace\
                \32d79336579a1939b3aad1c0b700ff9f8282d818582183581c8db0f00fabc0\
                \ff30e85ee171cefb4970c91d6b21a11038d7bbb581b3a0001ae7a1791b182a\
                \ffa0818200d81858858258403d4a81396e88155da40ec9e9807b77e4e5272c\
                \fd76a11f2dba6b6bd0a35195e720a2fd86f6692378c1f994c8a7af5e0805b9\
                \e945c091e2e7f7d987bf4f4561df5840a21fd4bfe3f3ae4f33bec35ff86ac2\
                \8a5de741739c408dad9c8287a19cde4f1361045ef86dc1f68b717e1318506a\
                \e3c69235b0f6e09a2225fd2d2856fb57c309"

            goldenTestSignedTx 2
                [ (xprv "addr-0", Coin 42)
                , (xprv "addr-1", Coin 14) ]
                "82839f8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace\
                \32d79336579a1939b3aad1c0b7008200d81858248258203b40265111d8bb3c\
                \3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b701ff9f8282d818\
                \582183581c8db0f00fabc0ff30e85ee171cefb4970c91d6b21a11038d7bbb5\
                \81b3a0001ae7a1791b182a8282d818582183581c1f0552afe4a22caa375ce1\
                \aa0f504227e9edccf99d13736540621cfda0001a9b5806490effa0828200d8\
                \1858858258403d4a81396e88155da40ec9e9807b77e4e5272cfd76a11f2dba\
                \6b6bd0a35195e720a2fd86f6692378c1f994c8a7af5e0805b9e945c091e2e7\
                \f7d987bf4f4561df58403ef8b25640d6aa2600a27908fed82994202780ab51\
                \e2596eacdb4e01025b0624b95d703d9e0654a9a282a63716a9f1e8bf5385b0\
                \9bc3e392b829aadfd94475078200d81858858258409b032aadfbb1fb8cddf2\
                \cf358420b2157dcf7e6057096dcbf40ba63306bfdc0ef3004c0ef2bc9faea3\
                \79a5c89ca98552b212e22d589541cc49beb508092fab9b5840e657a5572ca3\
                \4ae025a254b65a395f5cda37201949badd18ada0f1b3d32ddbb6f65213bb46\
                \c4b2e381455f7a36394e321d12600df44f2e41e3eb1e8f84eec90c"

            goldenTestSignedTx 25
                [ (xprv "addr-0", Coin 14) ]
                "82839f8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace\
                \32d79336579a1939b3aad1c0b700ff9f8282d818582183581c8db0f00fabc0\
                \ff30e85ee171cefb4970c91d6b21a11038d7bbb581b3a0001ae7a1791b0e82\
                \82d818582183581c8db0f00fabc0ff30e85ee171cefb4970c91d6b21a11038\
                \d7bbb581b3a0001ae7a1791b0e8282d818582183581c8db0f00fabc0ff30e8\
                \5ee171cefb4970c91d6b21a11038d7bbb581b3a0001ae7a1791b0e8282d818\
                \582183581c8db0f00fabc0ff30e85ee171cefb4970c91d6b21a11038d7bbb5\
                \81b3a0001ae7a1791b0e8282d818582183581c8db0f00fabc0ff30e85ee171\
                \cefb4970c91d6b21a11038d7bbb581b3a0001ae7a1791b0e8282d818582183\
                \581c8db0f00fabc0ff30e85ee171cefb4970c91d6b21a11038d7bbb581b3a0\
                \001ae7a1791b0e8282d818582183581c8db0f00fabc0ff30e85ee171cefb49\
                \70c91d6b21a11038d7bbb581b3a0001ae7a1791b0e8282d818582183581c8d\
                \b0f00fabc0ff30e85ee171cefb4970c91d6b21a11038d7bbb581b3a0001ae7\
                \a1791b0e8282d818582183581c8db0f00fabc0ff30e85ee171cefb4970c91d\
                \6b21a11038d7bbb581b3a0001ae7a1791b0e8282d818582183581c8db0f00f\
                \abc0ff30e85ee171cefb4970c91d6b21a11038d7bbb581b3a0001ae7a1791b\
                \0e8282d818582183581c8db0f00fabc0ff30e85ee171cefb4970c91d6b21a1\
                \1038d7bbb581b3a0001ae7a1791b0e8282d818582183581c8db0f00fabc0ff\
                \30e85ee171cefb4970c91d6b21a11038d7bbb581b3a0001ae7a1791b0e8282\
                \d818582183581c8db0f00fabc0ff30e85ee171cefb4970c91d6b21a11038d7\
                \bbb581b3a0001ae7a1791b0e8282d818582183581c8db0f00fabc0ff30e85e\
                \e171cefb4970c91d6b21a11038d7bbb581b3a0001ae7a1791b0e8282d81858\
                \2183581c8db0f00fabc0ff30e85ee171cefb4970c91d6b21a11038d7bbb581\
                \b3a0001ae7a1791b0e8282d818582183581c8db0f00fabc0ff30e85ee171ce\
                \fb4970c91d6b21a11038d7bbb581b3a0001ae7a1791b0e8282d81858218358\
                \1c8db0f00fabc0ff30e85ee171cefb4970c91d6b21a11038d7bbb581b3a000\
                \1ae7a1791b0e8282d818582183581c8db0f00fabc0ff30e85ee171cefb4970\
                \c91d6b21a11038d7bbb581b3a0001ae7a1791b0e8282d818582183581c8db0\
                \f00fabc0ff30e85ee171cefb4970c91d6b21a11038d7bbb581b3a0001ae7a1\
                \791b0e8282d818582183581c8db0f00fabc0ff30e85ee171cefb4970c91d6b\
                \21a11038d7bbb581b3a0001ae7a1791b0e8282d818582183581c8db0f00fab\
                \c0ff30e85ee171cefb4970c91d6b21a11038d7bbb581b3a0001ae7a1791b0e\
                \8282d818582183581c8db0f00fabc0ff30e85ee171cefb4970c91d6b21a110\
                \38d7bbb581b3a0001ae7a1791b0e8282d818582183581c8db0f00fabc0ff30\
                \e85ee171cefb4970c91d6b21a11038d7bbb581b3a0001ae7a1791b0e8282d8\
                \18582183581c8db0f00fabc0ff30e85ee171cefb4970c91d6b21a11038d7bb\
                \b581b3a0001ae7a1791b0e8282d818582183581c8db0f00fabc0ff30e85ee1\
                \71cefb4970c91d6b21a11038d7bbb581b3a0001ae7a1791b0effa0818200d8\
                \1858858258403d4a81396e88155da40ec9e9807b77e4e5272cfd76a11f2dba\
                \6b6bd0a35195e720a2fd86f6692378c1f994c8a7af5e0805b9e945c091e2e7\
                \f7d987bf4f4561df5840ad46c5de99d525589644554d3b482f334a27cf9c76\
                \84ace4543615452347f40e844e4540763aa6d9813535cf093c2c57a878a5f5\
                \0ccbf6a926dc070966a92508"

            goldenTestSignedTx 1
                [ (xprv "addr-0", Coin 14)
                , (xprv "addr-1", Coin 42)
                , (xprv "addr-2", Coin 287)
                , (xprv "addr-3", Coin 647)
                , (xprv "addr-4", Coin 1145)
                , (xprv "addr-5", Coin 2178)
                , (xprv "addr-6", Coin 6874)
                , (xprv "addr-7", Coin 9177)
                , (xprv "addr-8", Coin 21412)
                , (xprv "addr-9", Coin 35787)
                , (xprv "addr-10", Coin 66745)
                , (xprv "addr-11", Coin 142141)
                , (xprv "addr-12", Coin 314142)
                , (xprv "addr-13", Coin 666666)
                , (xprv "addr-14", Coin 1389571)
                , (xprv "addr-15", Coin 8589934592)
                , (xprv "addr-16", Coin 1)
                , (xprv "addr-17", Coin 1)
                , (xprv "addr-18", Coin 1)
                , (xprv "addr-19", Coin 1)
                , (xprv "addr-20", Coin 1)
                , (xprv "addr-21", Coin 1)
                , (xprv "addr-22", Coin 1)
                , (xprv "addr-23", Coin 1)
                , (xprv "addr-24", Coin 1)
                , (xprv "addr-25", Coin 1) ]
                "82839f8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace\
                \32d79336579a1939b3aad1c0b7008200d81858248258203b40265111d8bb3c\
                \3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b7018200d8185824\
                \8258203b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3\
                \aad1c0b7028200d81858248258203b40265111d8bb3c3c608d95b3a0bf8346\
                \1ace32d79336579a1939b3aad1c0b7038200d81858248258203b40265111d8\
                \bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b7048200d818\
                \58248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a19\
                \39b3aad1c0b7058200d81858248258203b40265111d8bb3c3c608d95b3a0bf\
                \83461ace32d79336579a1939b3aad1c0b7068200d81858248258203b402651\
                \11d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b7078200\
                \d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d7933657\
                \9a1939b3aad1c0b7088200d81858248258203b40265111d8bb3c3c608d95b3\
                \a0bf83461ace32d79336579a1939b3aad1c0b7098200d81858248258203b40\
                \265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b70a\
                \8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d793\
                \36579a1939b3aad1c0b70b8200d81858248258203b40265111d8bb3c3c608d\
                \95b3a0bf83461ace32d79336579a1939b3aad1c0b70c8200d8185824825820\
                \3b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0\
                \b70d8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32\
                \d79336579a1939b3aad1c0b70e8200d81858248258203b40265111d8bb3c3c\
                \608d95b3a0bf83461ace32d79336579a1939b3aad1c0b70f8200d818582482\
                \58203b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aa\
                \d1c0b7108200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461a\
                \ce32d79336579a1939b3aad1c0b7118200d81858248258203b40265111d8bb\
                \3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b7128200d81858\
                \248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939\
                \b3aad1c0b7138200d81858248258203b40265111d8bb3c3c608d95b3a0bf83\
                \461ace32d79336579a1939b3aad1c0b7148200d81858248258203b40265111\
                \d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b7158200d8\
                \1858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a\
                \1939b3aad1c0b7168200d81858248258203b40265111d8bb3c3c608d95b3a0\
                \bf83461ace32d79336579a1939b3aad1c0b7178200d81858258258203b4026\
                \5111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b71818\
                \8200d81858258258203b40265111d8bb3c3c608d95b3a0bf83461ace32d793\
                \36579a1939b3aad1c0b71819ff9f8282d818582183581c8db0f00fabc0ff30\
                \e85ee171cefb4970c91d6b21a11038d7bbb581b3a0001ae7a1791b0effa098\
                \1a8200d81858858258403d4a81396e88155da40ec9e9807b77e4e5272cfd76\
                \a11f2dba6b6bd0a35195e720a2fd86f6692378c1f994c8a7af5e0805b9e945\
                \c091e2e7f7d987bf4f4561df58402c4df370def4c85c0e5f177b823de1c3b7\
                \63eaf9555b85e1d9131f0f83853387d4f0d486eeca4b8f73377970b122eda8\
                \98946e99debabc0434922ece21f882078200d81858858258409b032aadfbb1\
                \fb8cddf2cf358420b2157dcf7e6057096dcbf40ba63306bfdc0ef3004c0ef2\
                \bc9faea379a5c89ca98552b212e22d589541cc49beb508092fab9b5840e3b7\
                \fd779ed34e7852abe8c8168401e5235074a5a4db9b7bd041655d94cb2d91a3\
                \dda141fc3fdcb1dbb307416c4b8eac04fed4b3a586ac1762974a3fe642bc08\
                \8200d8185885825840d757cd2f67d3d869115225bee5b3fdac2c730cc84593\
                \8b92bb0ba34fbc9f9d0bd30912fc69ab51bde67b0bb621473995cc16ded6d4\
                \f6bee0304f5a35e7bddae758409e17dc84e745af4ad72ac9596998cbe39bd8\
                \5d046d69526a74597564818723a585f2ab14e722bacebef3a2f24e8a6de291\
                \20d06a2f2418fd664fa99d2fbe75018200d8185885825840f7dd1457faa724\
                \e01abb6176e329f27868e9f14d5ca1a6dda565c91a09bb7cd3722c44ea449c\
                \29764d1ac1ae3501f5f61d181e1ec3787753255955c546867fc85840ce6c98\
                \512ab67e546a3ba8c4561b81a70c932b3bc274a4bfccaf8c5681a1dd9ca85e\
                \67dc97ce908f58bb2001be042d4be9c7946ac77ff780b4e342a25d5afb0782\
                \00d8185885825840d285277fc178f515a706ebe0aa92c28de04e21b0d00a7d\
                \786064157581ea41b4a00bd6c2cc4dd211112b2c6042ec8ffcb5f31c5b7f6e\
                \4707a0d490978d0aaba45840b0f88ee3dcaf59600f3130a34f1fcb28a1cfe5\
                \96f003191f4a7a452d78847f0faab46062d214dc9b6bdd938e47c2a17e4cfe\
                \0403766509583575f283681d980f8200d8185885825840b8e531b691a75ae4\
                \819973b9144cd5d92115259249b305d25c6a224738fe96adb3b106cd49cacd\
                \e92dc9cb95758db750ba87cbebd738b0624a402e46d146bb87584089611dd2\
                \1b1dd07bbf9b15a601a61321c3caf865f3c697a2ece4db5336d4c37eea81a1\
                \cc3c81ee7f3a8a33c5d4d5d19f7e0f73d7c7171b2b9016da6331a47e078200\
                \d818588582584063bf5ac584557788c2b77875e85e6f005f10386761d88d74\
                \c16670974abeb0ecbf4b250452fe234978b7aa0616d82b55af840d7d6aaabb\
                \4baca7532675ffb5935840958ed1e6e32d7048dff4070b29868a3e209ad34e\
                \4dc1eda97386f0cba20f3bedaa0d17e15e03cdef6d44ef624ba80ac9887858\
                \978f16f365978a6d28c83ec90e8200d8185885825840d13f79af5feee88a21\
                \cd25f7cc1e7599790959d401129153b6fd41472b4132759475a2910fe03146\
                \7621b479af0debd2e0f277e6a102507681ddb884028876a758407bb77c9af7\
                \9a00dc4d8cb3c3a7dda8fc21d8ec0701f304ed5befd94496975ea652966c2e\
                \5ccf31b150091810174e3ec887050c5dd6469167e4124d94af407b0b8200d8\
                \185885825840f9cf3b52ff3405f804be1c2ced5ec1058f76dddca9abaa6d9d\
                \fac9579880c85cde4d58cc819a315e762f39400bd347983694a8c6273da3d3\
                \a4b0041de45104245840ce44073fe192d9eb6185c03e493905c77604a58bc7\
                \2819b5559b77abef10f7ae5637d702a6174116fcae480c5fcd53698242bfc9\
                \1f908c041c790a953cc381058200d8185885825840f60e928e7780ceb1ceec\
                \7d804169e0f36c9a96c1acd9f1df36905e19d786e6f2746cb6451e1debcda7\
                \8a354bf596e5b5f5c986de1484f62f3388811e993c260658400ae8013ffcc7\
                \35d75c65575a9effbf204b17b4ac59146a27b6cd1588934e2a2644a9c7ed8b\
                \16fc6102ed7bbfe54ee1dcb510e22ce77d77ba3c6217aadc6dc4028200d818\
                \5885825840dc553209c3067843c48591b063dcc61b66d5fd136c5a167a2f9a\
                \074834bc9267fd4ac962bd360ddb953c14bf460c8e98d8836b2afa050c4f21\
                \bd0620beae4e1e58403221897d13bf51bc51f828312e6f4275d1682bf3e272\
                \d44c5995d397659308c06f4e435d5293daa2ac07f1eead83579ede690f3577\
                \8635605b75a001ee435c008200d81858858258402ccc5d35d817a66c44355b\
                \af5712fccdde5516be7a566c5e812773bd85cd1c0e7868f7acaa34a06d28b4\
                \6e5dd3499cfee657d1fd19d41f042153aecce0f0d41c58409f7e1b986ed2b4\
                \342f26cec97f7d5ed9d87855271330b32bf2378770a8baa11d1ff66e1acd96\
                \126e9f2ac8db8d0fadcf3adf454448f18dd52a1fcaebb9880b0f8200d81858\
                \858258405dc7a2a167c969d388ee3bb2a89bd3139844179fcb9d45a75e7b0c\
                \83df7a125f36dc89b2f7694a163f04509cff162fbd2f3a0e8c061ac7793e94\
                \5807107b5f2058401be0a47afcd9f8e3dceabdc044e735fcb5da4799879e56\
                \e59bb4a4d1bdc0168ba1b95384ec5e28c6a4929dd435fb18bf4014300eddee\
                \58031ce5e91cda96d3078200d818588582584007fb27974a79487549638b26\
                \e375bb57a62225edc1247f6ed30db72a8733119e9b6e2b15fa6adc9f998fa0\
                \b9a68f20b79eabe828f185bec7353a65a96d5893165840f910aaf5d4f4ce21\
                \74d67d1a426f154419d3b05b8a6bb69312060fb9575ed7380510e13bff7ad2\
                \f58b7d5c84f4ef7839abffc6abb37a65ebb82b1c7b62eb92028200d8185885\
                \82584057f0f86d688c7ffec02dfc0f4c304b60a27c53458dd7a7c576d10cd6\
                \0678580d4e82cb5d5f6610c5040b722b218679866b56743fffd80a64ecebb7\
                \4e50dfb85a58409f2d394b320fb0ccb818e3180df5511da8b58529f33cacd5\
                \259b6e7a5d1e7125258d4ff67d7af0823b62fab4e7565300ffbf71549b1a42\
                \8fdfd670b319a6bf028200d8185885825840f9fd608c4d215a8c54e36dbbde\
                \b376c5b53d3db6d4d635c9d4ffa98c2d7238c681f051d0d42fbb0245aa4e03\
                \ea6f54b234ecf1097866dbf91e03656239757dd35840e7f9cb966d8e48adb0\
                \ca8248b220f542dddf2763666f35fdaedff8ad4c7ed4b8b8c8ec0bd1b12eba\
                \e287b6685b0a05ba514674b7b883023ed7de944f593095008200d818588582\
                \584034b7fd1d8c882784e126565309e02b5dd9cc294287ee511d752b8e1000\
                \1cccf155f877983377de7f27e69cb91019159c8340fee027bae59a417a7fd5\
                \278900aa58403516f12ba83fc92bc3fa26c3d7b67f82fe6c21d0b4e7b77a35\
                \5d9a1e6661585b43004c6b45b083cf64d4b580ac922ab3755678cb0735c723\
                \466e0bbc767c270e8200d81858858258403ad21f0e1536775fe5489daf1cdd\
                \db4279593acf512872947e677d6a3a4ad71fbe762554121d5a82bc301ebec8\
                \d3de7f3ad35c074f3294c20f748cf3edf15a66584022bf557383f116b52d67\
                \cbbdc9c5cd2f2ae759951831f4e3a4a4988e1f939e8bf6a834cd2aa7cd4b4a\
                \468fc227c8d57747d9632bdbc10be5bde968bcf6edbe0b8200d81858858258\
                \40ec3737625da3e701a0ec0ab2e25df6558778f710e79fe204249110edcb05\
                \fbb630edaabfde2b68facb237473c28ce5c4f64f366be7fff55de37c0157be\
                \f6197b58402afb15b67c3d1f9e72dd5c70b2490193a06409b822a69b159476\
                \95cc4d03cf905c34a9829fff871a8251ba7d6ce954b2011dd6a7decb02d46a\
                \b8943085380f078200d81858858258401329b23ef943eb8b84a10f07f00789\
                \fbed07629a83ac046933497dad783cb2674091298c533033f846adc7999e37\
                \adc1a952ae90cb6ce481da54b85743cc3b645840987c7fce7d87775b5cdcfc\
                \6a7a14038426e6cb11d498649fbbe87ce69cab06bb204e1ddd3a9f6946c1b8\
                \ee0c2d198a4d4c11528f07de69641df8bc01f6a36c0c8200d8185885825840\
                \569f127d19ab6aad3d76983283de77f039695764ac2a2f1de0cb4f864ec49b\
                \3f1b80576b1fab2229ccfde51c503a2aaa7975dea1b8f3aa4e08c71ca0356e\
                \1d3e58408b4b94ee16c54fa54f1de3da08e15da5095c2828345b9a6a09ab06\
                \2bc680580a75433a73eb6291f736699536804222c201438e4f960cd678d9ad\
                \e5aa1490ec098200d81858858258409e53c51cfa3586073551ebadb3535d81\
                \10ba9ec0c3556e43130dfb44e74ba9865f9f3ebfb32f26b62064dabcdcfc55\
                \84c463657621a418615ec3eb5b6c124b0b5840c4d575fe6804957775aa106b\
                \325430939d42de77e027fd053d6432c5cee0cb357c160011c4ca09adfa3dfd\
                \a21a691265c2dabcf4069fe664a17eefca4e2ce80c8200d81858858258400e\
                \1fa5dd57f932e617ed9fbab26d20072400addb945794ff4f41ea2160b83a3f\
                \917a3096d89dc36dd075da6dca20266aa626251c259695ea75614ed7ed4f92\
                \595840371ceccc982e8bca6f4ecbbcac114602d0fb21c493496864bd9a7f4a\
                \152927507a1f2d39017bfd16923ddb51e6ed120180b84f9627e6e064d8572a\
                \e147a1dd098200d8185885825840186aa698d7c5862f9d909fa494973855a6\
                \0ab7ad062ae0cdfe328a8af8862d4c54588e6db988b4ef3eacfff1f0bdc7a9\
                \b3316b73195d466537453cb1ac9b0a1058402f3dd58ecc27969dd161f573b5\
                \4807be6fc8bc289f8f60305facb1c6daed69808ba3ab57beb380a0d47cbc75\
                \8428f3cf79eef6fa8bd910bcf5b862d2a14f210b8200d8185885825840698f\
                \66a96842a31898bc171492fea18d123ec852fce79bb00a9f9ec28fcfa35a2c\
                \7756bf21a0c314d7e3c69bba6560e919b02219a7b89f42c38db10fe1581957\
                \5840d53d26ac05f006d9dd253e9de24c74c890574a9db5584d64e60fb48a08\
                \c4df23634595fba0bb0217a680f9c7d8ec79ec7a4a7821bd05610056c6efd4\
                \4a51910a8200d8185885825840bbd293f8b9294bf5c3fa4592e54b44a10b76\
                \a54246579f143e86a750eb350288856d5325ae47e1b01e7cdfcf175aca4524\
                \68015df225b0431931ae15a21afb0f584021071c930e1f128f4086ef7d1daa\
                \e8e05d14c50d7fcb85a0a2af4cd48ceecb6dad5611f95b57a7c76aaec208f1\
                \fe0f5bd642e6bdbfb1f88759eb5142c9dd420a"

        Testnet -> do
            goldenTestSignedTx 1
                [(xprv "addr-0", Coin 42)]
                "82839f8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace\
                \32d79336579a1939b3aad1c0b700ff9f8282d818582883581c72dc02f53b5c\
                \84217630f8d25ba629c4a4e4575f1c21402e8028667ba102451a4170cb1700\
                \1aaf875866182affa0818200d81858858258403d4a81396e88155da40ec9e9\
                \807b77e4e5272cfd76a11f2dba6b6bd0a35195e720a2fd86f6692378c1f994\
                \c8a7af5e0805b9e945c091e2e7f7d987bf4f4561df5840334b1d54f35ce51f\
                \23e16d8541d8ebbbdcf3fa56f1fdcdaa427600510266ec90d0b1fba4176d61\
                \ef70bb54fde7c6f82ed089fe152d6ad4bdaab2e798156ab709"

            goldenTestSignedTx 2
                [ (xprv "addr-0", Coin 42)
                , (xprv "addr-1", Coin 14) ]
                "82839f8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace\
                \32d79336579a1939b3aad1c0b7008200d81858248258203b40265111d8bb3c\
                \3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b701ff9f8282d818\
                \582883581c72dc02f53b5c84217630f8d25ba629c4a4e4575f1c21402e8028\
                \667ba102451a4170cb17001aaf875866182a8282d818582883581c034a8cb8\
                \485f32812679e93b7b3e84c846234f76f0624d468dc3b0cda102451a4170cb\
                \17001a3702311c0effa0828200d81858858258403d4a81396e88155da40ec9\
                \e9807b77e4e5272cfd76a11f2dba6b6bd0a35195e720a2fd86f6692378c1f9\
                \94c8a7af5e0805b9e945c091e2e7f7d987bf4f4561df5840c4938016010175\
                \644acbc79c3b2b45422070d2517c011480e6551d8926f546c2ad790a915f53\
                \2cbec2ad141fa26fa346cb1847c14d4a09598cb1893953b906058200d81858\
                \858258409b032aadfbb1fb8cddf2cf358420b2157dcf7e6057096dcbf40ba6\
                \3306bfdc0ef3004c0ef2bc9faea379a5c89ca98552b212e22d589541cc49be\
                \b508092fab9b58403cc07c01f818c8ba974e49cbe4aea4514342756939e26a\
                \77e54d699dbc524a172a5686c5b006d0c049f66e0278fcdc984c3fcaf6861e\
                \11e8739e0600e3f3a50a"

            goldenTestSignedTx 25
                [ (xprv "addr-0", Coin 14) ]
                "82839f8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace\
                \32d79336579a1939b3aad1c0b700ff9f8282d818582883581c72dc02f53b5c\
                \84217630f8d25ba629c4a4e4575f1c21402e8028667ba102451a4170cb1700\
                \1aaf8758660e8282d818582883581c72dc02f53b5c84217630f8d25ba629c4\
                \a4e4575f1c21402e8028667ba102451a4170cb17001aaf8758660e8282d818\
                \582883581c72dc02f53b5c84217630f8d25ba629c4a4e4575f1c21402e8028\
                \667ba102451a4170cb17001aaf8758660e8282d818582883581c72dc02f53b\
                \5c84217630f8d25ba629c4a4e4575f1c21402e8028667ba102451a4170cb17\
                \001aaf8758660e8282d818582883581c72dc02f53b5c84217630f8d25ba629\
                \c4a4e4575f1c21402e8028667ba102451a4170cb17001aaf8758660e8282d8\
                \18582883581c72dc02f53b5c84217630f8d25ba629c4a4e4575f1c21402e80\
                \28667ba102451a4170cb17001aaf8758660e8282d818582883581c72dc02f5\
                \3b5c84217630f8d25ba629c4a4e4575f1c21402e8028667ba102451a4170cb\
                \17001aaf8758660e8282d818582883581c72dc02f53b5c84217630f8d25ba6\
                \29c4a4e4575f1c21402e8028667ba102451a4170cb17001aaf8758660e8282\
                \d818582883581c72dc02f53b5c84217630f8d25ba629c4a4e4575f1c21402e\
                \8028667ba102451a4170cb17001aaf8758660e8282d818582883581c72dc02\
                \f53b5c84217630f8d25ba629c4a4e4575f1c21402e8028667ba102451a4170\
                \cb17001aaf8758660e8282d818582883581c72dc02f53b5c84217630f8d25b\
                \a629c4a4e4575f1c21402e8028667ba102451a4170cb17001aaf8758660e82\
                \82d818582883581c72dc02f53b5c84217630f8d25ba629c4a4e4575f1c2140\
                \2e8028667ba102451a4170cb17001aaf8758660e8282d818582883581c72dc\
                \02f53b5c84217630f8d25ba629c4a4e4575f1c21402e8028667ba102451a41\
                \70cb17001aaf8758660e8282d818582883581c72dc02f53b5c84217630f8d2\
                \5ba629c4a4e4575f1c21402e8028667ba102451a4170cb17001aaf8758660e\
                \8282d818582883581c72dc02f53b5c84217630f8d25ba629c4a4e4575f1c21\
                \402e8028667ba102451a4170cb17001aaf8758660e8282d818582883581c72\
                \dc02f53b5c84217630f8d25ba629c4a4e4575f1c21402e8028667ba102451a\
                \4170cb17001aaf8758660e8282d818582883581c72dc02f53b5c84217630f8\
                \d25ba629c4a4e4575f1c21402e8028667ba102451a4170cb17001aaf875866\
                \0e8282d818582883581c72dc02f53b5c84217630f8d25ba629c4a4e4575f1c\
                \21402e8028667ba102451a4170cb17001aaf8758660e8282d818582883581c\
                \72dc02f53b5c84217630f8d25ba629c4a4e4575f1c21402e8028667ba10245\
                \1a4170cb17001aaf8758660e8282d818582883581c72dc02f53b5c84217630\
                \f8d25ba629c4a4e4575f1c21402e8028667ba102451a4170cb17001aaf8758\
                \660e8282d818582883581c72dc02f53b5c84217630f8d25ba629c4a4e4575f\
                \1c21402e8028667ba102451a4170cb17001aaf8758660e8282d81858288358\
                \1c72dc02f53b5c84217630f8d25ba629c4a4e4575f1c21402e8028667ba102\
                \451a4170cb17001aaf8758660e8282d818582883581c72dc02f53b5c842176\
                \30f8d25ba629c4a4e4575f1c21402e8028667ba102451a4170cb17001aaf87\
                \58660e8282d818582883581c72dc02f53b5c84217630f8d25ba629c4a4e457\
                \5f1c21402e8028667ba102451a4170cb17001aaf8758660e8282d818582883\
                \581c72dc02f53b5c84217630f8d25ba629c4a4e4575f1c21402e8028667ba1\
                \02451a4170cb17001aaf8758660effa0818200d81858858258403d4a81396e\
                \88155da40ec9e9807b77e4e5272cfd76a11f2dba6b6bd0a35195e720a2fd86\
                \f6692378c1f994c8a7af5e0805b9e945c091e2e7f7d987bf4f4561df58407a\
                \f54dd083930c2807f98b23cb7cce7162cd722422a6e97e22d2cdee40ce28ab\
                \ed6e28b6ff0b170104e633c80905ae3da9982a95c9cca73f5f997d6b5cd24a\
                \0e"

            goldenTestSignedTx 1
                [ (xprv "addr-0", Coin 14)
                , (xprv "addr-1", Coin 42)
                , (xprv "addr-2", Coin 287)
                , (xprv "addr-3", Coin 647)
                , (xprv "addr-4", Coin 1145)
                , (xprv "addr-5", Coin 2178)
                , (xprv "addr-6", Coin 6874)
                , (xprv "addr-7", Coin 9177)
                , (xprv "addr-8", Coin 21412)
                , (xprv "addr-9", Coin 35787)
                , (xprv "addr-10", Coin 66745)
                , (xprv "addr-11", Coin 142141)
                , (xprv "addr-12", Coin 314142)
                , (xprv "addr-13", Coin 666666)
                , (xprv "addr-14", Coin 1389571)
                , (xprv "addr-15", Coin 8589934592)
                , (xprv "addr-16", Coin 1)
                , (xprv "addr-17", Coin 1)
                , (xprv "addr-18", Coin 1)
                , (xprv "addr-19", Coin 1)
                , (xprv "addr-20", Coin 1)
                , (xprv "addr-21", Coin 1)
                , (xprv "addr-22", Coin 1)
                , (xprv "addr-23", Coin 1)
                , (xprv "addr-24", Coin 1)
                , (xprv "addr-25", Coin 1) ]
                "82839f8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace\
                \32d79336579a1939b3aad1c0b7008200d81858248258203b40265111d8bb3c\
                \3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b7018200d8185824\
                \8258203b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3\
                \aad1c0b7028200d81858248258203b40265111d8bb3c3c608d95b3a0bf8346\
                \1ace32d79336579a1939b3aad1c0b7038200d81858248258203b40265111d8\
                \bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b7048200d818\
                \58248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a19\
                \39b3aad1c0b7058200d81858248258203b40265111d8bb3c3c608d95b3a0bf\
                \83461ace32d79336579a1939b3aad1c0b7068200d81858248258203b402651\
                \11d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b7078200\
                \d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d7933657\
                \9a1939b3aad1c0b7088200d81858248258203b40265111d8bb3c3c608d95b3\
                \a0bf83461ace32d79336579a1939b3aad1c0b7098200d81858248258203b40\
                \265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b70a\
                \8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d793\
                \36579a1939b3aad1c0b70b8200d81858248258203b40265111d8bb3c3c608d\
                \95b3a0bf83461ace32d79336579a1939b3aad1c0b70c8200d8185824825820\
                \3b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0\
                \b70d8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32\
                \d79336579a1939b3aad1c0b70e8200d81858248258203b40265111d8bb3c3c\
                \608d95b3a0bf83461ace32d79336579a1939b3aad1c0b70f8200d818582482\
                \58203b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aa\
                \d1c0b7108200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461a\
                \ce32d79336579a1939b3aad1c0b7118200d81858248258203b40265111d8bb\
                \3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b7128200d81858\
                \248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939\
                \b3aad1c0b7138200d81858248258203b40265111d8bb3c3c608d95b3a0bf83\
                \461ace32d79336579a1939b3aad1c0b7148200d81858248258203b40265111\
                \d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b7158200d8\
                \1858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a\
                \1939b3aad1c0b7168200d81858248258203b40265111d8bb3c3c608d95b3a0\
                \bf83461ace32d79336579a1939b3aad1c0b7178200d81858258258203b4026\
                \5111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b71818\
                \8200d81858258258203b40265111d8bb3c3c608d95b3a0bf83461ace32d793\
                \36579a1939b3aad1c0b71819ff9f8282d818582883581c72dc02f53b5c8421\
                \7630f8d25ba629c4a4e4575f1c21402e8028667ba102451a4170cb17001aaf\
                \8758660effa0981a8200d81858858258403d4a81396e88155da40ec9e9807b\
                \77e4e5272cfd76a11f2dba6b6bd0a35195e720a2fd86f6692378c1f994c8a7\
                \af5e0805b9e945c091e2e7f7d987bf4f4561df5840bf09225c423d1f69dc24\
                \98f6d9874294e20932e194377cfc7b227243471c5ca5f4485e08c3e0a7b851\
                \df9d9fff2881ab2348885a5a37c188620445a77e7dc9098200d81858858258\
                \409b032aadfbb1fb8cddf2cf358420b2157dcf7e6057096dcbf40ba63306bf\
                \dc0ef3004c0ef2bc9faea379a5c89ca98552b212e22d589541cc49beb50809\
                \2fab9b5840713f1766cdf5dacb155066f1bbd5dc0f9903f359132facc9e7b6\
                \d8109eea1a71e38a76c466d3176c61eda81ebae8f5eda9692264c9c87c72f5\
                \d56809e301d20c8200d8185885825840d757cd2f67d3d869115225bee5b3fd\
                \ac2c730cc845938b92bb0ba34fbc9f9d0bd30912fc69ab51bde67b0bb62147\
                \3995cc16ded6d4f6bee0304f5a35e7bddae75840bef92a85f0c52e92ebe798\
                \016651497708a305df56747e65f9837ae7a1e6a0c0acadf5aacfce7014d679\
                \afc0c6080da7e47b8e6d2745102759041b2bb01c78058200d8185885825840\
                \f7dd1457faa724e01abb6176e329f27868e9f14d5ca1a6dda565c91a09bb7c\
                \d3722c44ea449c29764d1ac1ae3501f5f61d181e1ec3787753255955c54686\
                \7fc85840b950433b463f505ae571e9cabc51d265da156d0242898a09a06706\
                \d2adaa284fc2770525dff5ccea3bca37398102f20d1cf706d3500636589ecc\
                \c350e8b841058200d8185885825840d285277fc178f515a706ebe0aa92c28d\
                \e04e21b0d00a7d786064157581ea41b4a00bd6c2cc4dd211112b2c6042ec8f\
                \fcb5f31c5b7f6e4707a0d490978d0aaba45840eaa4499f4d15d1b2ea6784d0\
                \1e8d4e54d34b66f0d9e3d524323edb809995d85c0913152d4c556c68ddf8ab\
                \5ed541010f4e8b28d8705c4ed8fa97895b126a80038200d8185885825840b8\
                \e531b691a75ae4819973b9144cd5d92115259249b305d25c6a224738fe96ad\
                \b3b106cd49cacde92dc9cb95758db750ba87cbebd738b0624a402e46d146bb\
                \87584087f2e2eaca5e20c166f0cd5d509cdfde2eecc55e4af6dbd796cba6aa\
                \70c5f044c38e9bc88ea1d92b78418fd1fb21783d095faf7b0e8ecc1a5d0c2f\
                \5ebf57f1038200d818588582584063bf5ac584557788c2b77875e85e6f005f\
                \10386761d88d74c16670974abeb0ecbf4b250452fe234978b7aa0616d82b55\
                \af840d7d6aaabb4baca7532675ffb59358400499f954c0c7c2b0f842e1078d\
                \87e80a5ca74a77d0ae02b30fca9ae86a5949070157a03557302beee571b007\
                \9b7ecdac372d729817b1f8848042bf939e71af0b8200d8185885825840d13f\
                \79af5feee88a21cd25f7cc1e7599790959d401129153b6fd41472b41327594\
                \75a2910fe031467621b479af0debd2e0f277e6a102507681ddb884028876a7\
                \584001051c68ac4e9b48464621f1702324a88f9ed2a69a61961ba79abd39d6\
                \0e4478133d04d6175a0c9485583e995465eee225e54f208241769d6c0350e2\
                \a1a7980b8200d8185885825840f9cf3b52ff3405f804be1c2ced5ec1058f76\
                \dddca9abaa6d9dfac9579880c85cde4d58cc819a315e762f39400bd3479836\
                \94a8c6273da3d3a4b0041de45104245840e074c5bea945147c92cdf630cd16\
                \3f18a081c942374c79a6036d9e5fe7be2f4285d4c7ae8af9eb176d99364459\
                \cb05b3a2995d714b1cf3db6dcf0591b415ff038200d8185885825840f60e92\
                \8e7780ceb1ceec7d804169e0f36c9a96c1acd9f1df36905e19d786e6f2746c\
                \b6451e1debcda78a354bf596e5b5f5c986de1484f62f3388811e993c260658\
                \40a8088d46cc66fc230ffbec3c88bd028ad415ee54f51466d9db563a985886\
                \bd3d9de9c947cd57f66ff799fc60d471c8f3210cd67a74211406f8bfe1b33a\
                \6fa1088200d8185885825840dc553209c3067843c48591b063dcc61b66d5fd\
                \136c5a167a2f9a074834bc9267fd4ac962bd360ddb953c14bf460c8e98d883\
                \6b2afa050c4f21bd0620beae4e1e584009761da6c93a7068033e851f16fcba\
                \da65a346868d3a95fc0f625f26feac9befa29250e399160d52672e32f44722\
                \cddc0b9d31973ccbd580259f9da04bf8390d8200d81858858258402ccc5d35\
                \d817a66c44355baf5712fccdde5516be7a566c5e812773bd85cd1c0e7868f7\
                \acaa34a06d28b46e5dd3499cfee657d1fd19d41f042153aecce0f0d41c5840\
                \fe9b996d8ae52b7c6c2500479b9def258e9f8fbd3258724b44a6dd58b67aac\
                \f28ab803579eec2cbfa53c098e2e8ff6ebc66a5f1d46e0fdee0ca0f803dc87\
                \02038200d81858858258405dc7a2a167c969d388ee3bb2a89bd3139844179f\
                \cb9d45a75e7b0c83df7a125f36dc89b2f7694a163f04509cff162fbd2f3a0e\
                \8c061ac7793e945807107b5f205840d08afee45c6903439a0ed7b0c034ec66\
                \02f08d0a78569e2b1994d70ba87efa73dba90e7759df75a2ca53381ed460d3\
                \674d6859598f8fc48d77f0c0b5ad2d46038200d818588582584007fb27974a\
                \79487549638b26e375bb57a62225edc1247f6ed30db72a8733119e9b6e2b15\
                \fa6adc9f998fa0b9a68f20b79eabe828f185bec7353a65a96d58931658403f\
                \08fe42feb33a5a8e917cc4c1dc1c66040b71aad935b935534676630ee5c22d\
                \ded7742f0cb9f260a2931d309d84d52e99532cfaf81659f9dead895864a386\
                \0f8200d818588582584057f0f86d688c7ffec02dfc0f4c304b60a27c53458d\
                \d7a7c576d10cd60678580d4e82cb5d5f6610c5040b722b218679866b56743f\
                \ffd80a64ecebb74e50dfb85a58408613b359e366868f5b546ccf60b58cfd28\
                \10c38570c12099074f9edffcef00e2104ec645d32559b7c9f0692d40111e50\
                \21f55bdadf1bac98c095be2a80f9d20d8200d8185885825840f9fd608c4d21\
                \5a8c54e36dbbdeb376c5b53d3db6d4d635c9d4ffa98c2d7238c681f051d0d4\
                \2fbb0245aa4e03ea6f54b234ecf1097866dbf91e03656239757dd358404f0e\
                \bb98b666fb5aff2e47ec136afb02de6e694edf0bcdedbb4b84c5ccce06d24a\
                \5d9dbb923199b5061469764cd3ab28ff2ebefb3232ea14d99842c7cc4bfc06\
                \8200d818588582584034b7fd1d8c882784e126565309e02b5dd9cc294287ee\
                \511d752b8e10001cccf155f877983377de7f27e69cb91019159c8340fee027\
                \bae59a417a7fd5278900aa584012af97a82ff28b25a8e347afa1caaf0c12cd\
                \f8284a94ff47573f59c2d6cbbeae22a82d98e6fb357b9988cd5302f1931558\
                \b61efe3c0202c22a653c9bc2a106038200d81858858258403ad21f0e153677\
                \5fe5489daf1cdddb4279593acf512872947e677d6a3a4ad71fbe762554121d\
                \5a82bc301ebec8d3de7f3ad35c074f3294c20f748cf3edf15a665840a00287\
                \8d5f3674696430529d37ec1370db97458bf8d14b8b7a5f6b84924880524db0\
                \0577706138b628a1f229867dfcd3eed3c50e35da35361cb4df82cf13e20d82\
                \00d8185885825840ec3737625da3e701a0ec0ab2e25df6558778f710e79fe2\
                \04249110edcb05fbb630edaabfde2b68facb237473c28ce5c4f64f366be7ff\
                \f55de37c0157bef6197b5840df0c361396e4a7adf91edc73dad2321232fb69\
                \fbd7fce88614fb13180c24c7c7650bfaeed8e3af5c16e19d21920a99bd1d70\
                \231e70fe8ed13bc86f6a3cc66a028200d81858858258401329b23ef943eb8b\
                \84a10f07f00789fbed07629a83ac046933497dad783cb2674091298c533033\
                \f846adc7999e37adc1a952ae90cb6ce481da54b85743cc3b645840668340d9\
                \a2ce3ca01488a38a7822a82ffcf79c3b01f83d63c5047631cdd2dc17693bc4\
                \da3440f125448c700d37048d1fd2e873f0f4c7d93d86c03cd4a9e57d008200\
                \d8185885825840569f127d19ab6aad3d76983283de77f039695764ac2a2f1d\
                \e0cb4f864ec49b3f1b80576b1fab2229ccfde51c503a2aaa7975dea1b8f3aa\
                \4e08c71ca0356e1d3e5840af7044872986f3700feb090e3b807d459b9be7a5\
                \9c7891fcb88c9e0e45babfd8c604771b659d6bd0b9914530bf2211d408ddd6\
                \7a065d8aabe1d9b6c8efaf6d0b8200d81858858258409e53c51cfa35860735\
                \51ebadb3535d8110ba9ec0c3556e43130dfb44e74ba9865f9f3ebfb32f26b6\
                \2064dabcdcfc5584c463657621a418615ec3eb5b6c124b0b58402fae055514\
                \9b74f71b33af0d8cdcdd3da590881908c7ac23421533111d6f24665ce42f5a\
                \79366adb3c27134e20417a9cdaf982fc9acf56badb5719e79597a3008200d8\
                \1858858258400e1fa5dd57f932e617ed9fbab26d20072400addb945794ff4f\
                \41ea2160b83a3f917a3096d89dc36dd075da6dca20266aa626251c259695ea\
                \75614ed7ed4f92595840d8032db604059dfd6d30e00311b883dbd1e018ad06\
                \af9809157818bcc4bb02275aff0f73c1ffa12dcc4985c9cc8010fec34be045\
                \e770fccea675b51ad9d3b4078200d8185885825840186aa698d7c5862f9d90\
                \9fa494973855a60ab7ad062ae0cdfe328a8af8862d4c54588e6db988b4ef3e\
                \acfff1f0bdc7a9b3316b73195d466537453cb1ac9b0a105840a31dec3b7c7d\
                \17c8dec542122134a698534b2bc23b1075915472a9634f621f471489a0e2f3\
                \00e90cbac28a5d8901389c4d3cb21f96b8fcaa180acbcf623449038200d818\
                \5885825840698f66a96842a31898bc171492fea18d123ec852fce79bb00a9f\
                \9ec28fcfa35a2c7756bf21a0c314d7e3c69bba6560e919b02219a7b89f42c3\
                \8db10fe15819575840c63a9dcd436b4d52fadc40f147e52983d55932ca422c\
                \acec35b97bd72eb9fe47c7eb15ce2351bc4463d509500da2cf571837a276c8\
                \4d7d9889f1aea61c56fc058200d8185885825840bbd293f8b9294bf5c3fa45\
                \92e54b44a10b76a54246579f143e86a750eb350288856d5325ae47e1b01e7c\
                \dfcf175aca452468015df225b0431931ae15a21afb0f5840776e533138a861\
                \61cc5fbfc0e4503b9d3f54ab80c328c0fc8c00b1488bb90fdc9eded69567f2\
                \ee5af248a6060c758176885604c4c36b076bdd0061ce638b0d01"

        Staging ->
            xit "No golden tests for 'Staging' network" False

{-------------------------------------------------------------------------------
                                Size Estimation
-------------------------------------------------------------------------------}

propSizeEstimation
    :: (ShowFmt CoinSelection, InfiniteList (Network -> Address))
    -> Property
propSizeEstimation (ShowFmt sel, InfiniteList chngAddrs _) =
    let
        calcSize = estimateSize newTransactionLayer sel
        tx = fromCoinSelection sel
        encodedTx = CBOR.toLazyByteString $ encodeSignedTx tx
        size = fromIntegral $ BL.length encodedTx
        -- We always go for the higher bound for change address payload's size,
        -- so, we may end up with up to 4 extra bytes per change address in our
        -- estimation.
        margin = 4 * fromIntegral (length $ CS.change sel)
        realSizeSup = Quantity (size + margin)
        realSizeInf = Quantity size
    in
        (calcSize >= realSizeInf && calcSize <= realSizeSup, encodedTx)
            === (True, encodedTx)
  where
    dummyWitness = PublicKeyWitness
        "\226E\220\252\DLE\170\216\210\164\155\182mm$ePG\252\186\195\225_\b=\v\241=\255 \208\147[\239\RS\170|\214\202\247\169\229\205\187O_)\221\175\155?e\198\248\170\157-K\155\169z\144\174\ENQh" (Hash "\193\151*,\NULz\205\234\&1tL@\211\&2\165\129S\STXP\164C\176 Xvf\160|;\CANs{\SYN\204<N\207\154\130\225\229\t\172mbC\139\US\159\246\168x\163Mq\248\145)\160|\139\207-\SI")
    dummyInput = Hash
        "`\219\178g\158\233 T\f\CAN\EMZ=\146\238\155\229\n\238n\213\248\145\217-Q\219\138v\176,\210"
    fromCoinSelection (CoinSelection inps outs chngs) =
        let
            txIns = zipWith TxIn
                (replicate (length inps) dummyInput)
                [0..]
            txChngs = zipWith TxOut
                (take (length chngs) (chngAddrs <*> pure network))
                chngs
            wits = replicate (length inps) dummyWitness
        in
            (Tx txIns (outs <> txChngs), wits)

-- Check whether a selection is valid
isValidSelection :: CoinSelection -> Bool
isValidSelection (CoinSelection i o c) =
    let
        oAmt = sum $ map (fromIntegral . getCoin . coin) o
        cAmt = sum $ map (fromIntegral . getCoin) c
        iAmt = sum $ map (fromIntegral . getCoin . coin . snd) i
    in
        (iAmt :: Integer) >= (oAmt + cAmt)

-- | Generate a valid address with a payload of the given size. As pointers,
-- the sizes of payloads are as follows:
--
--     | Network | Scheme     | Size (bytes)   |
--     | ---     | ---        | ---            |
--     | Mainnet | Random     | 72,73,74 or 76 |
--     | Mainnet | Sequential | 39,40,41 or 43 |
--     | Testnet | Random     | 79,80,81 or 83 |
--     | Testnet | Sequential | 46,47,48 or 50 |
--
-- The address format on 'Staging' is the same as 'Mainnet'.
genAddress :: (Int, Int) -> Gen Address
genAddress range = do
    n <- choose range
    let prefix = BS.pack
            [ 130       -- Array(2)
            , 216, 24   -- Tag 24
            , 88, fromIntegral n -- Bytes(n), n > 23 && n < 256
            ]
    payload <- BS.pack <$> vectorOf n arbitrary
    let crc = CBOR.toStrictByteString (CBOR.encodeWord32 $ crc32 payload)
    return $ Address (prefix <> payload <> crc)

genUTxO :: [Coin] -> Gen UTxO
genUTxO coins = do
    let n = length coins
    inps <- vectorOf n arbitrary
    outs <- genTxOut coins
    return $ UTxO $ Map.fromList $ zip inps outs

genTxOut :: [Coin] -> Gen [TxOut]
genTxOut coins = do
    let n = length coins
    outs <- vectorOf n arbitrary
    return $ zipWith TxOut outs coins

genSelection :: NonEmpty TxOut -> Gen CoinSelection
genSelection outs = do
    let opts = CS.CoinSelectionOptions 100
    utxo <- vectorOf (NE.length outs * 3) arbitrary >>= genUTxO
    case runIdentity $ runExceptT $ largestFirst opts outs utxo of
        Left _ -> genSelection outs
        Right (s,_) -> return s

instance Show (Network -> Address) where
    show _ = "<Change Address Generator>"

deriving newtype instance Arbitrary a => Arbitrary (ShowFmt a)

instance Arbitrary Coin where
    shrink (Coin c) = Coin <$> shrink (fromIntegral c)
    arbitrary = Coin <$> choose (1, 200000)

instance Arbitrary (Hash "Tx") where
    shrink _ = []
    arbitrary = do
        bytes <- BS.pack <$> vectorOf 32 arbitrary
        pure $ Hash bytes

instance Arbitrary Address where
    shrink _ = []
    arbitrary = genAddress (30, 100)

-- | Generate change addresses for the given network. We consider that change
-- addresses are always following a sequential scheme.
instance {-# OVERLAPS #-} Arbitrary (Network -> Address) where
    shrink _ = []
    arbitrary = do
        mainnetA <- genAddress (33, 33)
        testnetA <- genAddress (40, 40)
        return $ \case
            Mainnet -> mainnetA
            Staging -> mainnetA
            Testnet -> testnetA

instance Arbitrary CoinSelection where
    shrink sel@(CoinSelection inps outs chgs) = case (inps, outs, chgs) of
        ([_], [_], []) ->
            []
        _ ->
            let
                inps' = take (max 1 (length inps `div` 2)) inps
                outs' = take (max 1 (length outs `div` 2)) outs
                chgs' = take (length chgs `div` 2) chgs
                inps'' = if length inps > 1 then drop 1 inps else inps
                outs'' = if length outs > 1 then drop 1 outs else outs
                chgs'' = drop 1 chgs
            in
                filter (\s -> s /= sel && isValidSelection s)
                    [ CoinSelection inps' outs' chgs'
                    , CoinSelection inps' outs chgs
                    , CoinSelection inps outs chgs'
                    , CoinSelection inps outs' chgs
                    , CoinSelection inps'' outs'' chgs''
                    , CoinSelection inps'' outs chgs
                    , CoinSelection inps outs'' chgs
                    , CoinSelection inps outs chgs''
                    ]
    arbitrary = do
        outs <- choose (1, 10)
            >>= \n -> vectorOf n arbitrary
            >>= genTxOut
        genSelection (NE.fromList outs)

instance Arbitrary TxIn where
    shrink _ = []
    arbitrary = TxIn
        <$> arbitrary
        <*> scale (`mod` 3) arbitrary -- No need for a high indexes

{-------------------------------------------------------------------------------
                                Golden Tests
-------------------------------------------------------------------------------}

xprv :: ByteString -> Key depth XPrv
xprv seed =
    unsafeGenerateKeyFromSeed (Passphrase (BA.convert seed), mempty) mempty

goldenTestSignedTx
    :: Int
        -- ^ Number of outputs
    -> [(Key 'AddressK XPrv, Coin)]
        -- ^ (Address Private Keys, Output value)
    -> ByteString
        -- ^ Expected result, in Base16
    -> SpecWith ()
goldenTestSignedTx nOuts xprvs expected = it title $ do
    let addrs = first (keyToAddress @HttpBridge . publicKey) <$> xprvs
    let s = Map.fromList (zip (fst <$> addrs) (fst <$> xprvs))
    let keyFrom a = (,mempty) <$> Map.lookup a s
    let inps = mkInput <$> zip addrs [0..]
    let outs = take nOuts $ mkOutput <$> cycle addrs
    let res = mkStdTx newTransactionLayer keyFrom inps outs
    case res of
        Left e -> fail (show e)
        Right tx -> do
            let bytes = convertToBase Base16
                    $ CBOR.toStrictByteString
                    $ encodeSignedTx
                    tx
            bytes `shouldBe` expected
  where
    title :: String
    title = mempty
        <> show (length xprvs) <> " inputs & "
        <> show nOuts <> " outputs"

    -- | Inputs are constructed from _resolved inputs_, i.e., inputs for which
    -- we have the associated address; This address is used for signing as we
    -- have, in order to submit a valid transaction, to provide evidence for
    -- ownership of these addresses
    mkInput :: ((Address, Coin), Word32) -> (TxIn, TxOut)
    mkInput (out, ix) =
        ( TxIn faucetTx ix
        , mkOutput out
        )

    -- | Arbitrary output too, we do re-use the same addresses here but this is
    -- purely arbitrary.
    mkOutput :: (Address, Coin) -> TxOut
    mkOutput =
        uncurry TxOut

    -- | An arbitrary source transaction for inputs (see 'TxIn'). This could be
    -- anything in practice and isn't relevant to our signing code.
    faucetTx :: Hash "Tx"
    faucetTx = either (\e -> error $ "faucetTx: " <> e) Hash $ convertFromBase
        @ByteString Base16 "3B40265111D8BB3C3C608D95B3A0BF83461ACE32D79336579A1939B3AAD1C0B7"

{- NOTE: The above golden tests were obtained from 'cardano-sl@3.0.1', using the
    following code:

module Main where

--  build-depends:
--      base
--      bytestring
--      cardano-crypto
--      cardano-sl
--      cardano-sl-binary
--      cardano-sl-chain
--      cardano-sl-core
--      cardano-sl-crypto
--      cardano-wallet
--      containers
--      memory
--      universum

import           Universum

import           Cardano.Wallet.Kernel.Transactions (mkStdTx)

import           Pos.Binary.Class
import           Pos.Chain.Txp
import           Pos.Core.Common
import           Pos.Core.NetworkMagic
import           Pos.Crypto.Configuration
import           Pos.Crypto.Hashing
import           Pos.Crypto.Signing

import           System.IO.Unsafe (unsafePerformIO)

import qualified Cardano.Crypto.Wallet as CC
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
    genGoldenTest mainnet 1
        [ (genESK "addr-0", Coin 42) ]

    -- and so forth ...

-- | Generate an encrypted key from a seed, with an empty passphrase
genESK
    :: ByteString
    -> EncryptedSecretKey
genESK seed = unsafePerformIO $
    mkEncSecretUnsafe mempty (CC.generateNew seed pwd pwd)
  where
    pwd :: ByteString
    pwd = mempty

-- | Generate a golden test containing a signed transaction
genGoldenTest
    :: (NetworkMagic, ProtocolMagic)
        -- ^ Protocol parameters
    -> Int
        -- ^ Number of outputs
    -> [(EncryptedSecretKey, Coin)]
        -- ^ (Address Private Keys, Output value)
    -> IO ()
genGoldenTest (nm, pm) nOuts xprvs = do
    let addrs = first (makePubKeyAddressBoot nm . encToPublic) <$> xprvs
    let res = mkStdTx pm shuffler signer inps outs []
          where
            shuffler = return
            signer addr = maybe
                (Left ()) (\(esk,_) -> Right $ SafeSigner esk mempty) (Map.lookup addr m)
              where
                m = Map.fromList (zip (fst <$> addrs) xprvs)
            inps = NE.fromList $ mkInput <$> zip [0..] addrs
            outs = NE.fromList $ take nOuts $ mkOutput <$> (cycle addrs)
    case res of
        Left _ -> fail $ "genGoldenTest: failed to sign tx"
        Right tx -> do
            let bytes = CBOR.toLazyByteString $ encode tx
            B8.putStrLn $
                B8.pack (show nm)
                <> " " <> show nOuts <> " ouputs"
                <> " " <> show (length xprvs) <> " inputs"
            print $ (B8.unpack . addrToBase58 . fst) <$> addrs
            -- NOTE Dropping first 4 bytes of 'TxAux' wrapper, not actually
            -- present on chain.
            B8.putStrLn $ BS.drop 4 $ BA.convertToBase BA.Base16 $ BL.toStrict bytes
            B8.putStrLn ""
  where
    mkInput (ix, addr) =
        let
            txId = unsafeHash @String "arbitrary"
        in
            ( TxInUtxo txId ix
            , mkOutput addr
            )
    mkOutput (addr, c) =
        TxOutAux $ TxOut addr c
-}
