{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.API.Byron.Transactions
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiAddress
    , ApiAsset (..)
    , ApiByronWallet
    , ApiFee (..)
    , ApiT (..)
    , ApiTransaction
    , ApiTxId (..)
    , ApiWallet
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( PaymentAddress )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( mkTokenFingerprint )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (..), TxStatus (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex, unsafeFromText )
import Control.Monad
    ( forM_, void )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Resource
    ( runResourceT )
import Data.Bifunctor
    ( bimap )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( fromText )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( ActionWith, SpecWith, describe, pendingWith )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldNotBe )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , between
    , emptyByronWalletFromXPrvWith
    , emptyByronWalletWith
    , emptyIcarusWallet
    , emptyRandomWallet
    , emptyWallet
    , eventually
    , expectErrorMessage
    , expectField
    , expectListField
    , expectListSize
    , expectListSizeSatisfy
    , expectResponseCode
    , expectSuccess
    , faucetAmt
    , faucetUtxoAmt
    , fixtureIcarusWallet
    , fixtureMultiAssetIcarusWallet
    , fixtureMultiAssetRandomWallet
    , fixturePassphrase
    , fixtureRandomWallet
    , fixtureWallet
    , getFromResponse
    , json
    , listAddresses
    , minUTxOValue
    , mkTxPayloadMA
    , pickAnAsset
    , postByronWallet
    , postTx
    , request
    , toQueryString
    , verify
    , waitForTxImmutability
    , walletId
    , (.>=)
    )
import Test.Integration.Framework.Request
    ( RequestException, unsafeRequest )
import Test.Integration.Framework.TestData
    ( errMsg400StartTimeLaterThanEndTime
    , errMsg404NoAsset
    , errMsg404NoWallet
    , steveToken
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.TokenPolicy as TokenPolicy
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types.Status as HTTP

data TestCase a = TestCase
    { query :: T.Text
    , assertions :: [(HTTP.Status, Either RequestException a) -> IO ()]
    }

spec :: forall n.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    , PaymentAddress n ByronKey
    , PaymentAddress n IcarusKey
    ) => SpecWith Context
spec = describe "BYRON_TRANSACTIONS" $ do

    describe "BYRON_TRANS_ASSETS_CREATE_01 - Multi-asset transaction with ADA" $
        forM_ [ (fixtureMultiAssetRandomWallet @n, "Byron wallet")
              , (fixtureMultiAssetIcarusWallet @n, "Icarus wallet")] $
              \(srcFixture, name) -> it name $ \ctx -> runResourceT $ do

        wSrc <- srcFixture ctx
        wDest <- emptyWallet ctx

        -- pick out an asset to send
        let assetsSrc = wSrc ^. #assets . #total . #getApiT
        assetsSrc `shouldNotBe` mempty
        let minUTxOValue' = minUTxOValue (_mainEra ctx)
        let val = minUTxOValue' <$ pickAnAsset assetsSrc

        addrs <- listAddresses @n ctx wDest
        let destination = (addrs !! 1) ^. #id
        payload <- mkTxPayloadMA @n destination (minUTxOValue' * 2) [val] fixturePassphrase

        rtx <- request @(ApiTransaction n) ctx
            (Link.createTransactionOld @'Byron wSrc) Default payload
        expectResponseCode HTTP.status202 rtx

        eventually "Payee wallet balance is as expected" $ do
            rb <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            verify rb
                [ expectField (#assets . #available . #getApiT)
                    (`shouldNotBe` TokenMap.empty)
                , expectField (#assets . #total . #getApiT)
                    (`shouldNotBe` TokenMap.empty)
                ]

    describe "BYRON_TRANS_ASSETS_CREATE_02 - Multi-asset transaction with too little ADA" $
        forM_ [ (fixtureMultiAssetRandomWallet @n, "Byron wallet")
              , (fixtureMultiAssetIcarusWallet @n, "Icarus wallet")] $
              \(srcFixture, name) -> it name $ \ctx -> runResourceT $ do

        wSrc <- srcFixture ctx
        wDest <- emptyWallet ctx

        -- pick out an asset to send
        let assetsSrc = wSrc ^. #assets . #total . #getApiT
        assetsSrc `shouldNotBe` mempty
        let minUTxOValue' = minUTxOValue (_mainEra ctx)
        let val = minUTxOValue' <$ pickAnAsset assetsSrc

        addrs <- listAddresses @n ctx wDest
        let destination = (addrs !! 1) ^. #id
        payload <- mkTxPayloadMA @n destination minUTxOValue' [val] fixturePassphrase

        rtx <- request @(ApiTransaction n) ctx
            (Link.createTransactionOld @'Byron wSrc) Default payload
        expectResponseCode HTTP.status403 rtx
        expectErrorMessage "Some outputs have ada values that are too small." rtx

    describe "BYRON_TRANS_ASSETS_CREATE_02a - Multi-asset transaction with no ADA" $
        forM_ [ (fixtureMultiAssetRandomWallet @n, "Byron wallet")
              , (fixtureMultiAssetIcarusWallet @n, "Icarus wallet")] $
              \(srcFixture, name) -> it name $ \ctx -> runResourceT $ do

        wSrc <- srcFixture ctx
        wDest <- emptyWallet ctx

        -- pick out an asset to send
        let assetsSrc = wSrc ^. #assets . #total . #getApiT
        assetsSrc `shouldNotBe` mempty
        let val = minUTxOValue (_mainEra ctx) <$ pickAnAsset assetsSrc

        addrs <- listAddresses @n ctx wDest
        let destination = (addrs !! 1) ^. #id
        payload <- mkTxPayloadMA @n destination 0 [val] fixturePassphrase

        rtx <- request @(ApiTransaction n) ctx
            (Link.createTransactionOld @'Byron wSrc) Default payload
        expectResponseCode HTTP.status202 rtx

        eventually "Payee wallet balance is as expected" $ do
            rb <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            verify rb
                [ expectField (#assets . #available . #getApiT)
                    (`shouldNotBe` TokenMap.empty)
                , expectField (#assets . #total . #getApiT)
                    (`shouldNotBe` TokenMap.empty)
                ]

    describe "BYRON_TRANS_ASSETS_LIST_01 - Asset list present" $
        forM_ [ (fixtureMultiAssetRandomWallet @n, "Byron wallet")
              , (fixtureMultiAssetIcarusWallet @n, "Icarus wallet")] $
              \(srcFixture, name) -> it name $ \ctx -> runResourceT $ do

        wal <- srcFixture ctx

        let assetsSrc = wal ^. (#assets . #total . #getApiT)
        assetsSrc `shouldNotBe` mempty
        let (polId, assName) = bimap unsafeFromText unsafeFromText $ fst $
                pickAnAsset assetsSrc
        let tokenFingerprint = mkTokenFingerprint polId assName

        r <- request @([ApiAsset]) ctx (Link.listByronAssets wal) Default Empty
        verify r
            [ expectSuccess
            , expectListSizeSatisfy ( > 0)
            , expectListField 0 #policyId (`shouldBe` ApiT polId)
            , expectListField 0 #assetName (`shouldBe` ApiT assName)
            , expectListField 0 (#fingerprint . #getApiT) (`shouldBe` tokenFingerprint)
            , expectListField 0 #metadata (`shouldBe` Just steveToken)
            ]

    describe "BYRON_TRANS_ASSETS_LIST_02 - Asset list present when not used" $
        forM_ [ (fixtureRandomWallet, "Byron fixture wallet")
              , (fixtureIcarusWallet, "Icarus fixture wallet")
              , (emptyRandomWallet, "Byron empty wallet")
              , (emptyIcarusWallet, "Icarus empty wallet")] $
              \(srcFixture, name) -> it name $ \ctx -> runResourceT $ do

        wal <- srcFixture ctx
        r <- request @([ApiAsset]) ctx (Link.listByronAssets wal) Default Empty
        verify r
            [ expectSuccess
            , expectListSize 0
            ]

    describe "BYRON_TRANS_ASSETS_GET_01 - Asset list present" $
        forM_ [ (fixtureMultiAssetRandomWallet @n, "Byron wallet")
              , (fixtureMultiAssetIcarusWallet @n, "Icarus wallet")] $
              \(srcFixture, name) -> it name $ \ctx -> runResourceT $ do

        wal <- srcFixture ctx

        -- pick an asset from the fixture wallet
        let assetsSrc = wal ^. (#assets . #total . #getApiT)
        assetsSrc `shouldNotBe` mempty
        let (polId, assName) = bimap unsafeFromText unsafeFromText $ fst $
                pickAnAsset assetsSrc
        let tokenFingerprint = mkTokenFingerprint polId assName
        let ep = Link.getByronAsset wal polId assName
        r <- request @(ApiAsset) ctx ep Default Empty
        verify r
            [ expectSuccess
            , expectField #policyId (`shouldBe` ApiT polId)
            , expectField #assetName (`shouldBe` ApiT assName)
            , expectField (#fingerprint . #getApiT) (`shouldBe` tokenFingerprint)
            , expectField #metadata (`shouldBe` Just steveToken)
            ]

    describe "BYRON_TRANS_ASSETS_GET_02 - Asset not present when isn't associated" $
        forM_ [ (fixtureMultiAssetRandomWallet @n, "Byron wallet")
              , (fixtureMultiAssetIcarusWallet @n, "Icarus wallet")] $
              \(srcFixture, name) -> it name $ \ctx -> runResourceT $ do

        wal <- srcFixture ctx
        let polId = TokenPolicy.UnsafeTokenPolicyId $ Hash $ BS.replicate 28 0
        let assName = TokenPolicy.UnsafeTokenName $ B8.replicate 4 'x'
        let ep = Link.getByronAsset wal polId assName
        r <- request @(ApiAsset) ctx ep Default Empty
        expectResponseCode HTTP.status404 r
        expectErrorMessage errMsg404NoAsset r

    describe "BYRON_TRANS_ASSETS_GET_02a - Asset not present when isn't associated" $
        forM_ [ (fixtureMultiAssetRandomWallet @n, "Byron wallet")
              , (fixtureMultiAssetIcarusWallet @n, "Icarus wallet")] $
              \(srcFixture, name) -> it name $ \ctx -> runResourceT $ do

        wal <- srcFixture ctx
        let polId = TokenPolicy.UnsafeTokenPolicyId $ Hash $ BS.replicate 28 0
        let ep = Link.getByronAsset wal polId TokenPolicy.nullTokenName
        r <- request @(ApiAsset) ctx ep Default Empty
        expectResponseCode HTTP.status404 r
        expectErrorMessage errMsg404NoAsset r

    describe "BYRON_TRANS_CREATE_01 - Single Output Transaction with non-Shelley witnesses" $
        forM_ [(fixtureRandomWallet, "Byron wallet"), (fixtureIcarusWallet, "Icarus wallet")] $
        \(srcFixture,name) -> it name $ \ctx -> runResourceT $ do

        (wByron, wShelley) <- (,) <$> srcFixture ctx <*> fixtureWallet ctx
        addrs <- listAddresses @n ctx wShelley

        let amt = minUTxOValue (_mainEra ctx) :: Natural
        let destination = (addrs !! 1) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }]
            }|]

        rFeeEst <- request @ApiFee ctx
            (Link.getTransactionFeeOld @'Byron wByron) Default payload
        verify rFeeEst
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]
        let (Quantity feeEstMin) = getFromResponse #estimatedMin rFeeEst
        let (Quantity feeEstMax) = getFromResponse #estimatedMax rFeeEst

        r <- postTx @n ctx
            (wByron, Link.createTransactionOld @'Byron, fixturePassphrase)
            wShelley
            amt
        verify r
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#amount . #getQuantity) $
                between (feeEstMin + amt, feeEstMax + amt)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            ]

        ra <- request @ApiByronWallet ctx (Link.getWallet @'Byron wByron) Default Empty
        verify ra
            [ expectSuccess
            , expectField (#balance . #total) $
                between
                    ( Quantity (faucetAmt - feeEstMax - amt)
                    , Quantity (faucetAmt - feeEstMin - amt)
                    )
            , expectField
                    (#balance . #available)
                    (.>= Quantity (faucetAmt - faucetUtxoAmt))
            ]

        eventually "wa and wb balances are as expected" $ do
            rb <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wShelley) Default Empty
            expectField
                (#balance . #available)
                (`shouldBe` Quantity (faucetAmt + amt)) rb

            ra2 <- request @ApiByronWallet ctx
                (Link.getWallet @'Byron wByron) Default Empty
            expectField
                (#balance . #available)
                (`shouldBe` Quantity (faucetAmt - feeEstMax - amt)) ra2

    it "BYRON_TRANS_CREATE_02 -\
        \ Cannot create tx on Byron wallet using shelley ep" $ \ctx -> runResourceT $ do
            w <- emptyRandomWallet ctx
            let wid = w ^. walletId
            wDest <- emptyWallet ctx
            addr:_ <- listAddresses @n ctx wDest
            let destination = addr ^. #id
            let payload = Json [json|{
                    "payments": [{
                        "address": #{destination},
                        "amount": {
                            "quantity": #{minUTxOValue (_mainEra ctx)},
                            "unit": "lovelace"
                        }
                    }],
                    "passphrase": "cardano-wallet"
                }|]
            let endpoint = "v2/wallets/" <> wid <> "/transactions"
            r <- request @(ApiTransaction n) ctx ("POST", endpoint) Default payload
            expectResponseCode HTTP.status404 r
            expectErrorMessage (errMsg404NoWallet wid) r

    it "BYRON_TRANS_DELETE -\
        \ Cannot delete tx on Byron wallet using shelley ep" $ \ctx -> runResourceT $ do
            w <- emptyRandomWallet ctx
            let wid = w ^. walletId
            let txid = "3e6ec12da4414aa0781ff8afa9717ae53ee8cb4aa55d622f65bc62619a4f7b12"
            let endpoint = "v2/wallets/" <> wid <> "/transactions/" <> txid
            r <- request @ApiTxId ctx ("DELETE", endpoint) Default Empty
            expectResponseCode HTTP.status404 r
            expectErrorMessage (errMsg404NoWallet wid) r

    it "BYRON_TRANS_ESTIMATE -\
        \ Cannot estimate tx on Byron wallet using shelley ep" $ \ctx -> runResourceT $ do
            w <- emptyRandomWallet ctx
            let wid = w ^. walletId
            wDest <- emptyWallet ctx
            addr:_ <- listAddresses @n ctx wDest
            let destination = addr ^. #id
            let payload = Json [json|{
                    "payments": [{
                        "address": #{destination},
                        "amount": {
                            "quantity": #{minUTxOValue (_mainEra ctx)},
                            "unit": "lovelace"
                        }
                    }]
                }|]
            let endpoint = "v2/wallets/" <> wid <> "/payment-fees"
            r <- request @ApiFee ctx ("POST", endpoint) Default payload
            expectResponseCode HTTP.status404 r
            expectErrorMessage (errMsg404NoWallet wid) r

    it "BYRON_TX_LIST_02 -\
        \ Byron endpoint does not list Shelley wallet transactions" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        let wid = w ^. walletId
        let ep = ("GET", "v2/byron-wallets/" <> wid <> "/transactions")
        r <- request @([ApiTransaction n]) ctx ep Default Empty
        verify r
            [ expectResponseCode HTTP.status404
            , expectErrorMessage (errMsg404NoWallet wid)
            ]

    it "BYRON_TX_LIST_03 -\
        \ Shelley endpoint does not list Byron wallet transactions" $ \ctx -> runResourceT $ do
        w <- emptyRandomWallet ctx
        let wid = w ^. walletId
        let ep = ("GET", "v2/wallets/" <> wid <> "/transactions")
        r <- request @([ApiTransaction n]) ctx ep Default Empty
        verify r
            [ expectResponseCode HTTP.status404
            , expectErrorMessage (errMsg404NoWallet wid)
            ]

    it "BYRON_RESTORE_09 - Ledger wallet" $ \ctx -> runResourceT $ do
        -- NOTE
        -- Special legacy wallets where addresses have been generated from a
        -- seed derived using the auxiliary method used by Ledger.
        let mnemonics =
                [ "vague" , "wrist" , "poet" , "crazy" , "danger" , "dinner"
                , "grace" , "home" , "naive" , "unfold" , "april" , "exile"
                , "relief" , "rifle" , "ranch" , "tone" , "betray" , "wrong"
                ] :: [T.Text]
        let payload = Json [json| {
                    "name": "Ledger Wallet",
                    "mnemonic_sentence": #{mnemonics},
                    "passphrase": #{fixturePassphrase},
                    "style": "ledger"
                    } |]

        r <- postByronWallet ctx payload
        verify r
            [ expectResponseCode HTTP.status201
            , expectField (#balance . #available) (`shouldBe` Quantity faucetAmt)
            ]

    it "BYRON_TX_LIST_01 - 0 txs on empty Byron wallet"
        $ \ctx -> runResourceT @IO $ forM_ [emptyRandomWallet, emptyIcarusWallet] $ \emptyByronWallet -> do
            w <- emptyByronWallet ctx
            let link = Link.listTransactions @'Byron w
            r <- request @([ApiTransaction n]) ctx link Default Empty
            verify r
                [ expectResponseCode HTTP.status200
                , expectListSize 0
                ]

    it "BYRON_TX_LIST_01 - Can list transactions on Byron Wallet" $ \ctx -> runResourceT @IO $ do
        w <- fixtureRandomWallet ctx
        let link = Link.listTransactions @'Byron w
        r <- request @([ApiTransaction n]) ctx link Default Empty
        verify r
            [ expectResponseCode HTTP.status200
            , expectListSize 10
            ]

    it "BYRON_TX_LIST_01 - Can list transactions on Icarus Wallet" $ \ctx -> runResourceT @IO $ do
        w <- fixtureIcarusWallet ctx
        let link = Link.listTransactions @'Byron w
        r <- request @([ApiTransaction n]) ctx link Default Empty
        verify r
            [ expectResponseCode HTTP.status200
            , expectListSize 1 -- Now funded through a tx in the cluster setup
            ]

    describe "BYRON_TX_LIST_01 - Faulty start, end, order values" $ do
        let orderErr = "Please specify one of the following values:\
            \ ascending, descending."
        let startEndErr = "Expecting ISO 8601 date-and-time format\
            \ (basic or extended), e.g. 2012-09-25T10:15:00Z."
        let queries :: [TestCase [ApiTransaction n]] =
                [
                  TestCase
                    { query = toQueryString [ ("start", "2009") ]
                    , assertions =
                             [ expectResponseCode HTTP.status400
                             , expectErrorMessage startEndErr
                             ]

                    }
                 , TestCase
                     { query = toQueryString
                             [ ("start", "2012-09-25T10:15:00Z")
                             , ("end", "2016-11-21")
                             ]
                     , assertions =
                             [ expectResponseCode HTTP.status400
                             , expectErrorMessage startEndErr
                             ]

                     }
                 , TestCase
                     { query = toQueryString
                             [ ("start", "2012-09-25")
                             , ("end", "2016-11-21T10:15:00Z")
                             ]
                     , assertions =
                             [ expectResponseCode HTTP.status400
                             , expectErrorMessage startEndErr
                             ]

                     }
                 , TestCase
                     { query = toQueryString
                             [ ("end", "2012-09-25T10:15:00Z")
                             , ("start", "2016-11-21")
                             ]
                     , assertions =
                             [ expectResponseCode HTTP.status400
                             , expectErrorMessage startEndErr
                             ]

                     }
                 , TestCase
                     { query = toQueryString [ ("order", "scending") ]
                     , assertions =
                            [ expectResponseCode HTTP.status400
                            , expectErrorMessage orderErr
                            ]

                     }
                 , TestCase
                     { query = toQueryString
                             [ ("start", "2012-09-25T10:15:00Z")
                             , ("order", "asc")
                             ]
                     , assertions =
                             [ expectResponseCode HTTP.status400
                             , expectErrorMessage orderErr
                             ]
                     }
                ]

        let withQuery q (method, link) = (method, link <> q)

        forM_ queries $ \tc -> it (T.unpack $ query tc) $ \ctx -> runResourceT @IO $ do
            w <- emptyRandomWallet ctx
            let link = withQuery (query tc) $ Link.listTransactions @'Byron w
            r <- request @([ApiTransaction n]) ctx link Default Empty
            liftIO $ verify r (assertions tc)

    it "BYRON_TX_LIST_01 - Start time shouldn't be later than end time" $
        \ctx -> runResourceT @IO $ do
            w <- emptyRandomWallet ctx
            let startTime = "2009-09-09T09:09:09Z"
            let endTime = "2001-01-01T01:01:01Z"
            let link = Link.listTransactions' @'Byron w
                    Nothing
                    (either (const Nothing) Just $ fromText $ T.pack startTime)
                    (either (const Nothing) Just $ fromText $ T.pack endTime)
                    Nothing
            r <- request @([ApiTransaction n]) ctx link Default Empty
            expectResponseCode HTTP.status400 r
            expectErrorMessage
                (errMsg400StartTimeLaterThanEndTime startTime endTime) r

    it "BYRON_TX_LIST_04 - Deleted wallet" $ \ctx -> runResourceT @IO $ do
        w <- emptyRandomWallet ctx
        _ <- request @ApiByronWallet ctx
            (Link.deleteWallet @'Byron w) Default Empty
        let link = Link.listTransactions @'Byron w
        r <- request @([ApiTransaction n]) ctx link Default Empty
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    -- Golden from https://github.com/input-output-hk/cardano-sl/pull/4278#issuecomment-600553878
    it "BYRON_SCRYPT_TRANS_CREATE_01 - wallet with password" $ \ctx -> do
        flip runByronScryptGolden ctx $ ByronScryptGolden
            { mnemonic = T.words
                "inhale arm pilot fitness ceiling october donate \
                \between language all limit taxi"
            , encryptedRootXPrv =
                "f6e79f49b8999a39d7e970e42d0a91224ecacefc3aa1edb342f34eb8bc6c2f\
                \c63e743b862b312a6f92ba0161d4d53c3ee5a2bd8085476d9575765c49dcee\
                \cbe54b34ec47daf9b7ebc6bdb706622616451c000e85ba81c7449ae436a8cb\
                \bf3aab98e5cc704977bd11bb0ba8d5b5571a705704cb9334d27a048532eab4\
                \9a698c2d"
            , passwordHash =
                "31347c387c317c5743413633702f6a487a5777575278756756344e31685479\
                \3470646c6d4f76665177653863775a575472784f79773d3d7c796341722f61\
                \326f4f777a736e4e746f4e655049416e4f6b7978426549494a6b59623039574\
                \b564a7159493d"
            , password = T.pack . B8.unpack $ unsafeFromHex
                "00000000000000000000000000000000000000\
                \50415441544520504154415445"
            }
    -- Golden from https://github.com/input-output-hk/cardano-sl/pull/4278#issuecomment-600553878
    it "BYRON_SCRYPT_TRANS_CREATE_02 - wallet without password" $
        runByronScryptGolden ByronScryptGolden
            { mnemonic = T.words
                "marriage blouse orbit quarter treat series release sing lava \
                \spice surface rule"
            , encryptedRootXPrv =
                "38e8de9c583441213fe34eecc4e28265267466877ba4048e3ab1fa99563669\
                \47aefaf5ba9779db67eead7fc9cd1354b994a5d8d9cd40ab874bfeb1b33649\
                \280cd33651377731e0e59e0233425a55257782c5adaa768da0567f43c1c6c0\
                \c18766ed0a547bb34eb472c120b170a8640279832ddf18002887f03c15dea5\
                \9705422d"
            , passwordHash =
                "31347c387c317c574342652b796362417576356c2b4258676a344a314c6343\
                \675375414c2f5653393661364e576a2b7550766655513d3d7c6f7846366549\
                \39734151444e6f38395147747366324e653937426338372b484b6b41377567\
                \72752f5970673d"
            , password = ""
            }
  where
    runByronScryptGolden :: ByronScryptGolden -> ActionWith Context
    runByronScryptGolden golden ctx = runResourceT $ do
        wFaucet <- fixtureRandomWallet ctx

        wMnemonic <- emptyByronWalletWith ctx "random"
            ("Random Wallet", mnemonic golden, fixturePassphrase)

        let pay amt pwd src dest = do
                addr <- view #id . snd
                    <$> unsafeRequest @(ApiAddress n) ctx
                        (Link.postRandomAddress dest)
                        (Json [json| { "passphrase": #{fixturePassphrase} }|])

                payload <- mkTxPayloadMA @n addr amt [] pwd
                rtx <- request @(ApiTransaction n) ctx
                    (Link.createTransactionOld @'Byron src) Default payload
                expectResponseCode HTTP.status202 rtx

        let ada = 1_000_000

        pay (100 * ada) fixturePassphrase wFaucet wMnemonic

        void $ request @() ctx
            (Link.deleteWallet @'Byron wMnemonic) Default Empty

        wKey <- emptyByronWalletFromXPrvWith ctx "random"
            ("Random Wallet", encryptedRootXPrv golden, passwordHash golden)

        liftIO $ waitForTxImmutability ctx

        pay (99 * ada) (password golden) wKey wFaucet

data ByronScryptGolden = ByronScryptGolden
    { encryptedRootXPrv :: Text
      -- ^ Encrypted XPrv extracted from cardano-sl
    , passwordHash :: Text
      -- ^ Password hash extracted from cardano-sl
    , password :: Text
      -- ^ Password used when spending funds.
    , mnemonic :: [Text]
      -- ^ Corresponding mnemonic for reference
    }

