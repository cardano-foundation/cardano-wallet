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
    , ApiWalletDiscovery (..)
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Api.Types.Transaction
    ( ApiLimit (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( mkTokenFingerprint )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (..), TxStatus (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromText )
import Control.Monad
    ( forM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Resource
    ( runResourceT )
import Data.Bifunctor
    ( bimap )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( fromText )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith, describe )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldNotBe )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , between
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
    , walletId
    , (.>=)
    )
import Test.Integration.Framework.Request
    ( RequestException )
import Test.Integration.Framework.TestData
    ( errMsg400StartTimeLaterThanEndTime
    , errMsg403MinUTxOValue
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
import qualified Network.HTTP.Types.Status as HTTP

data TestCase a = TestCase
    { query :: T.Text
    , assertions :: [(HTTP.Status, Either RequestException a) -> IO ()]
    }

spec :: forall n.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
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
        expectErrorMessage errMsg403MinUTxOValue rtx

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

    describe "BYRON_TRANS_CREATE_01 - Single Output Transaction Byron -> Shelley" $
        forM_ [(fixtureRandomWallet, "Byron wallet"), (fixtureIcarusWallet, "Icarus wallet")] $
        \(srcFixture,name) -> it name $ \ctx -> runResourceT $ do

        (wByron, wShelley) <- (,) <$> srcFixture ctx <*> emptyWallet ctx
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

        -- TODO [ADP-2268] This padding should be droppable once postTransaction
        -- also relies on balanceTx.
        --
        -- We need it in the meantime as the new implementation is using a lower
        -- upper bound for the length of Icarus change addresses.
        let tmpIcarusPadding =
                if name == "Icarus wallet"
                then 3_500 -- 35 bytes * 100 lovelace/byte
                else 0
        let feeEstMin = getFromResponse (#estimatedMin . #getQuantity) rFeeEst
        let feeEstMax = getFromResponse (#estimatedMax . #getQuantity) rFeeEst
                + tmpIcarusPadding

        r <- postTx @n ctx
            (wByron, Link.createTransactionOld @'Byron, fixturePassphrase)
            wShelley
            amt
        verify r
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField (#amount . #getQuantity) $
                between (feeEstMin + amt, feeEstMax + amt)
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

        let txId = getFromResponse (#id) r
        eventually "wa and wb balances are as expected as well as fees on txs" $ do
            -- check that tx and balance on source Byron wallet is as expected
            rTxSrc <- request @(ApiTransaction n) ctx
                (Link.getTransaction @'Byron wByron (ApiTxId txId)) Default Empty
            verify rTxSrc
                [ expectSuccess
                , expectField (#amount . #getQuantity) $
                    between (amt + feeEstMin, amt + feeEstMax)
                , expectField (#fee . #getQuantity) $
                    between (feeEstMin, feeEstMax)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                ]
            rSrc <- request @ApiByronWallet ctx
                (Link.getWallet @'Byron wByron) Default Empty
            verify rSrc
                [ expectSuccess
                , expectField (#balance . #total . #getQuantity) $
                    between
                        ( faucetAmt - feeEstMax - amt
                        , faucetAmt - feeEstMin - amt
                        )
                ]

            -- check that tx and balance on destination Shelley wallet is as expected
            rTxDest <- request @(ApiTransaction n) ctx
                (Link.getTransaction @'Shelley wShelley (ApiTxId txId)) Default Empty
            verify rTxDest
                [ expectSuccess
                , expectField (#amount . #getQuantity) (`shouldBe` amt)
                , expectField (#fee . #getQuantity) $
                    between (feeEstMin, feeEstMax)
                , expectField (#direction . #getApiT) (`shouldBe` Incoming)
                ]
            rTx <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wShelley) Default Empty
            verify rTx
                [ expectSuccess
                , expectField
                        (#balance . #available)
                        (`shouldBe` Quantity amt)
                ]

    describe "BYRON_TRANS_CREATE_01a - Single Output Transaction Byron -> Byron" $
        forM_ [(emptyRandomWallet, "Byron wallet"), (emptyIcarusWallet, "Icarus wallet")] $
        \(emptyByronWallet,name) -> it name $ \ctx -> runResourceT $ do

        (wByron, wDestByron) <- (,) <$> fixtureRandomWallet ctx <*> emptyByronWallet ctx
        ra <- request @ApiByronWallet ctx (Link.getWallet @'Byron wDestByron) Default Empty
        let walType = getFromResponse #discovery ra

        destination <- case walType of
            DiscoveryRandom -> do
                let payloadAddr = Json [json| { "passphrase": #{fixturePassphrase} }|]
                rA <- request @(ApiAddress n) ctx (Link.postRandomAddress wDestByron) Default payloadAddr
                pure $ getFromResponse #id rA
            DiscoverySequential -> do
                let link = Link.listAddresses @'Byron wDestByron
                rA2 <- request @[ApiAddress n] ctx link Default Empty
                let addrs = getFromResponse Prelude.id rA2
                pure $ (addrs !! 1) ^. #id

        let amt = 2 * minUTxOValue (_mainEra ctx) :: Natural
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

        -- make transaction from Byron wallet to Byron wallet
        let payloadTx = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": #{fixturePassphrase}
            }|]
        rTx <- request @(ApiTransaction n) ctx
            (Link.createTransactionOld @'Byron wByron) Default payloadTx
        verify rTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#amount . #getQuantity) $
                between (feeEstMin + amt, feeEstMax + amt)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField (#fee . #getQuantity) $
                between (feeEstMin, feeEstMax)
            ]
        let txId = getFromResponse (#id) rTx

        eventually "wa and wb balances and fees are as expected" $ do
            -- check that tx and balance on destination Byron wallet is as expected
            rTxDest <- request @(ApiTransaction n) ctx
                (Link.getTransaction @'Byron wDestByron (ApiTxId txId)) Default Empty
            verify rTxDest
                [ expectSuccess
                , expectField (#amount . #getQuantity) (`shouldBe` amt)
                , expectField (#fee . #getQuantity) $
                    between (feeEstMin, feeEstMax)
                , expectField (#direction . #getApiT) (`shouldBe` Incoming)
                ]
            rDest <- request @ApiByronWallet ctx
                (Link.getWallet @'Byron wDestByron) Default Empty
            verify rDest
                [ expectSuccess
                , expectField
                        (#balance . #available)
                        (`shouldBe` Quantity amt)
                ]

            -- check that tx and balance on Byron source wallet is as expected
            rTxSrc <- request @(ApiTransaction n) ctx
                (Link.getTransaction @'Byron wByron (ApiTxId txId)) Default Empty
            verify rTxSrc
                [ expectSuccess
                , expectField (#amount . #getQuantity) $
                    between (feeEstMin + amt, feeEstMax + amt)
                , expectField (#fee . #getQuantity) $
                    between (feeEstMin, feeEstMax)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                ]
            rSrc <- request @ApiByronWallet ctx
                (Link.getWallet @'Byron wByron) Default Empty
            verify rSrc
                [ expectSuccess
                , expectField (#balance . #total . #getQuantity) $
                    between
                        ( faucetAmt - feeEstMax - amt
                        , faucetAmt - feeEstMin - amt
                        )
                ]

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
            , expectListSize 10
            ]

    describe "BYRON_TX_LIST_LIMIT - Transactions can be limited" $
        forM_ [(fixtureRandomWallet, "Byron wallet"), (fixtureIcarusWallet, "Icarus wallet")] $
            \(srcFixture,name) -> it name $ \ctx -> runResourceT $ do
                w <- srcFixture ctx
                let link = Link.listTransactions' @'Byron w
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        (Just $ ApiLimit 9)
                r <- request @([ApiTransaction n]) ctx link Default Empty
                verify r
                    [ expectResponseCode HTTP.status200
                    , expectListSize 9
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
