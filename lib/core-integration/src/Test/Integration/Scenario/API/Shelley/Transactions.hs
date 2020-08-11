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

{- HLINT ignore "Use head" -}

module Test.Integration.Scenario.API.Shelley.Transactions
    ( spec
    ) where

import Prelude

import Cardano.Mnemonic
    ( entropyToMnemonic, genEntropy, mnemonicToText )
import Cardano.Wallet.Api.Types
    ( ApiByronWallet
    , ApiFee (..)
    , ApiT (..)
    , ApiTransaction
    , ApiTxId (..)
    , ApiWallet
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress (..)
    , WalletStyle (..)
    , insertedAt
    , pendingSince
    , time
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( PaymentAddress )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.Types
    ( Direction (..)
    , Hash (..)
    , SortOrder (..)
    , TxMetadata (..)
    , TxMetadataValue (..)
    , TxStatus (..)
    , WalletId
    )
import Control.Monad
    ( forM_ )
import Data.Aeson
    ( (.=) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Generics.Product.Typed
    ( HasType )
import Data.Maybe
    ( isJust )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), ToText (..) )
import Data.Time.Clock
    ( UTCTime, addUTCTime )
import Data.Time.Utils
    ( utcTimePred, utcTimeSucc )
import Network.HTTP.Types.Method
    ( Method )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith, describe )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldSatisfy )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , Payload (..)
    , between
    , emptyRandomWallet
    , emptyWallet
    , eventually
    , expectErrorMessage
    , expectField
    , expectListField
    , expectListSize
    , expectResponseCode
    , expectSuccess
    , faucetAmt
    , faucetUtxoAmt
    , fixtureIcarusWallet
    , fixtureIcarusWalletAddrs
    , fixturePassphrase
    , fixtureRandomWallet
    , fixtureWallet
    , fixtureWalletWith
    , getFromResponse
    , json
    , listAddresses
    , listAllTransactions
    , listTransactions
    , minUTxOValue
    , request
    , rewardWallet
    , toQueryString
    , unsafeRequest
    , utcIso8601ToText
    , verify
    , walletId
    , (.<)
    , (.<=)
    , (.>)
    , (.>=)
    )
import Test.Integration.Framework.Request
    ( RequestException )
import Test.Integration.Framework.TestData
    ( errMsg400MinWithdrawalWrong
    , errMsg400StartTimeLaterThanEndTime
    , errMsg400TxMetadataStringTooLong
    , errMsg400TxTooLarge
    , errMsg403Fee
    , errMsg403InputsDepleted
    , errMsg403NoPendingAnymore
    , errMsg403NotAShelleyWallet
    , errMsg403NotEnoughMoney
    , errMsg403NotEnoughMoney_
    , errMsg403WithdrawalNotWorth
    , errMsg403WrongPass
    , errMsg404CannotFindTx
    , errMsg404MinUTxOValue
    , errMsg404NoWallet
    , mnemonics15
    , mnemonics18
    )
import Web.HttpApiData
    ( ToHttpApiData (..) )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

data TestCase a = TestCase
    { query :: T.Text
    , assertions :: [(HTTP.Status, Either RequestException a) -> IO ()]
    }

spec :: forall n t.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    , PaymentAddress n IcarusKey
    ) => SpecWith (Context t)
spec = describe "SHELLEY_TRANSACTIONS" $ do
    it "TRANS_MIN_UTXO_01 - I cannot spend less than minUTxOValue" $ \ctx -> do
      wSrc <- fixtureWallet ctx
      wDest <- emptyWallet ctx

      let amt = minUTxOValue  - 1
      addrs <- listAddresses @n ctx wDest
      let destination = (addrs !! 1) ^. #id
      let payload = Json [json|{
              "payments": [{
                  "address": #{destination},
                  "amount": {
                      "quantity": #{amt},
                      "unit": "lovelace"
                  }
              }],
              "passphrase": #{fixturePassphrase}
          }|]

      let ep = Link.createTransaction @'Shelley
      r <- request @(ApiTransaction n) ctx (ep wSrc) Default payload
      expectResponseCode HTTP.status403 r
      expectErrorMessage (errMsg404MinUTxOValue minUTxOValue) r

    it "Regression #1004 -\
        \ Transaction to self shows only fees as a tx amount\
        \ while both, pending and in_ledger" $ \ctx -> do
        wSrc <- fixtureWallet ctx

        payload <- mkTxPayload ctx wSrc minUTxOValue fixturePassphrase

        (_, ApiFee (Quantity feeMin) (Quantity feeMax)) <- unsafeRequest ctx
            (Link.getTransactionFee @'Shelley wSrc) payload

        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload

        verify r
            [ expectSuccess
            , expectResponseCode HTTP.status202
            -- tx amount includes only fees because it is tx to self address
            -- when tx is pending
            , expectField (#amount . #getQuantity) $ between (feeMin, feeMax)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            ]

        eventually "Tx is in ledger" $ do
            rt <- request @([ApiTransaction n]) ctx
                (Link.listTransactions @'Shelley wSrc) Default Empty
            verify rt
                [ expectSuccess
                , expectResponseCode HTTP.status200
                -- tx amount includes only fees because it is tx to self address
                -- also when tx is already in ledger
                , expectListField 0 (#amount . #getQuantity) $ between (feeMin, feeMax)
                , expectListField 0 (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 0 (#status . #getApiT) (`shouldBe` InLedger)
                ]

    it "Regression #935 -\
        \ Pending tx should have pendingSince in the list tx response" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx

        eventually "Pending tx has pendingSince field" $ do
            -- Post Tx
            let amt = (minUTxOValue :: Natural)
            r <- postTx ctx
                (wSrc, Link.createTransaction @'Shelley,fixturePassphrase)
                wDest
                amt
            let tx = getFromResponse Prelude.id r
            tx ^. (#status . #getApiT) `shouldBe` Pending
            insertedAt tx `shouldBe` Nothing
            pendingSince tx `shouldSatisfy` isJust

            -- Verify Tx
            let link = Link.listTransactions' @'Shelley wSrc
                    Nothing
                    Nothing
                    Nothing
                    (Just Descending)
            (_, txs) <- unsafeRequest @([ApiTransaction n]) ctx link Empty
            case filter ((== Pending) . view (#status . #getApiT)) txs of
                [] ->
                    fail "Tx no longer pending, need to retry scenario."
                tx':_ -> do
                    tx' ^. (#direction . #getApiT) `shouldBe` Outgoing
                    tx' ^. (#status . #getApiT) `shouldBe` Pending
                    insertedAt tx' `shouldBe` Nothing
                    pendingSince tx' `shouldBe` pendingSince tx

    it "TRANS_CREATE_01 - Single Output Transaction" $ \ctx -> do
        (wa, wb) <- (,) <$> fixtureWallet ctx <*> fixtureWallet ctx
        let amt = (minUTxOValue :: Natural)

        payload <- mkTxPayload ctx wb amt fixturePassphrase

        (_, ApiFee (Quantity feeMin) (Quantity feeMax)) <- unsafeRequest ctx
            (Link.getTransactionFee @'Shelley wa) payload

        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wa) Default payload

        verify r
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#amount . #getQuantity) $
                between (feeMin + amt, feeMax + amt)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#metadata . #getApiTxMetadata) (`shouldBe` Nothing)
            ]

        ra <- request @ApiWallet ctx (Link.getWallet @'Shelley wa) Default Empty
        verify ra
            [ expectSuccess
            , expectField (#balance . #getApiT . #total) $
                between
                    ( Quantity (faucetAmt - feeMax - amt)
                    , Quantity (faucetAmt - feeMin - amt)
                    )
            , expectField
                    (#balance . #getApiT . #available)
                    (.>= Quantity (faucetAmt - faucetUtxoAmt))
            ]

        eventually "wa and wb balances are as expected" $ do
            rb <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wb) Default Empty
            expectField
                (#balance . #getApiT . #available)
                (`shouldBe` Quantity (faucetAmt + amt)) rb

            ra2 <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wa) Default Empty
            expectField
                (#balance . #getApiT . #available)
                (`shouldBe` Quantity (faucetAmt - feeMax - amt)) ra2

    it "TRANS_CREATE_02 - Multiple Output Tx to single wallet" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addrs <- listAddresses @n ctx wDest

        let amt = minUTxOValue :: Natural
        let destination1 = (addrs !! 1) ^. #id
        let destination2 = (addrs !! 2) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination1},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                },
                {
                    "address": #{destination2},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]

        (_, ApiFee (Quantity feeMin) (Quantity feeMax)) <- unsafeRequest ctx
            (Link.getTransactionFee @'Shelley wSrc) payload

        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload

        ra <- request @ApiWallet ctx (Link.getWallet @'Shelley wSrc) Default Empty
        verify r
            [ expectResponseCode HTTP.status202
            , expectField (#amount . #getQuantity) $
                between (feeMin + (2*amt), feeMax + (2*amt))
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            ]
        verify ra
            [ expectField (#balance . #getApiT . #total) $
                between
                    ( Quantity (faucetAmt - feeMax - (2*amt))
                    , Quantity (faucetAmt - feeMin - (2*amt))
                    )
            , expectField
                    (#balance . #getApiT . #available)
                    (.>= Quantity (faucetAmt - 2 * faucetUtxoAmt))
            ]
        eventually "wDest balance is as expected" $ do
            rd <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            verify rd
                [ expectField
                        (#balance . #getApiT . #available)
                        (`shouldBe` Quantity (2*amt))
                , expectField
                        (#balance . #getApiT . #total)
                        (`shouldBe` Quantity (2*amt))
                ]

    it "TRANS_CREATE_03 - 0 balance after transaction" $ \ctx -> do
        let amt = minUTxOValue

        wDest <- fixtureWalletWith @n ctx [amt]
        payload <- mkTxPayload ctx wDest amt fixturePassphrase

        (_, ApiFee (Quantity feeMin) _) <- unsafeRequest ctx
            (Link.getTransactionFee @'Shelley wDest) payload

        -- NOTE It's a little tricky to estimate the fee needed for a
        -- transaction with no change output, because in order to know the right
        -- amount of fees, we need to create a transaction spends precisely this
        -- amount.
        --
        -- Said differently, in order to know what amount we need, we need to
        -- know what the amount is... ¯\_(ツ)_/¯ ... So, we use a little
        -- hard-wired margin, which works with the current fee settings. If we
        -- ever change that, this test will fail.
        let margin = 400
        wSrc <- fixtureWalletWith @n ctx [feeMin+amt+margin]

        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status202
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            ]

        ra <- request @ApiWallet ctx (Link.getWallet @'Shelley wSrc) Default Empty
        verify ra
            [ expectField (#balance . #getApiT . #total) (`shouldBe` Quantity 0)
            , expectField (#balance . #getApiT . #available) (`shouldBe` Quantity 0)
            ]

        eventually "Wallet balance is as expected" $ do
            rd <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            verify rd
                [ expectField
                        (#balance . #getApiT . #available)
                        (`shouldBe` Quantity amt)
                , expectField
                        (#balance . #getApiT . #total)
                        (`shouldBe` Quantity amt)
                ]

        ra2 <- request @ApiWallet ctx (Link.getWallet @'Shelley wSrc) Default Empty
        verify ra2
            [ expectField (#balance . #getApiT . #total) (`shouldBe` Quantity 0)
            , expectField (#balance . #getApiT . #available) (`shouldBe` Quantity 0)
            ]

    it "TRANS_CREATE_04 - Can't cover fee" $ \ctx -> do
        wDest <- fixtureWallet ctx

        payload <- mkTxPayload ctx wDest minUTxOValue fixturePassphrase
        (_, ApiFee (Quantity feeMin) _) <- unsafeRequest ctx
            (Link.getTransactionFee @'Shelley wDest) payload

        wSrc <- fixtureWalletWith @n ctx [minUTxOValue + (feeMin `div` 2)]

        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403Fee
            ]

    it "TRANS_CREATE_04 - Not enough money" $ \ctx -> do
        let (srcAmt, reqAmt) = (minUTxOValue, 2 * minUTxOValue)
        wSrc <- fixtureWalletWith @n ctx [srcAmt]
        wDest <- emptyWallet ctx
        payload <- mkTxPayload ctx wDest reqAmt fixturePassphrase
        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage $ errMsg403NotEnoughMoney srcAmt reqAmt
            ]

    it "TRANS_CREATE_04 - Wrong password" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addr:_ <- listAddresses @n ctx wDest

        let destination = addr ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{minUTxOValue},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "This password is wrong"
            }|]
        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403WrongPass
            ]

    it "TRANS_CREATE_07 - Deleted wallet" $ \ctx -> do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
        wDest <- emptyWallet ctx
        addr:_ <- listAddresses @n ctx wDest
        let destination = addr ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{minUTxOValue},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]
        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley w) Default payload
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    describe "TRANS_CREATE_08 - Bad payload" $ do
        let matrix =
                [ ( "empty payload", NonJson "" )
                , ( "{} payload", NonJson "{}" )
                , ( "non-json valid payload"
                  , NonJson
                        "{ payments: [{\
                         \\"address\": 12312323,\
                         \\"amount: {\
                         \\"quantity\": 1,\
                         \\"unit\": \"lovelace\"} }],\
                         \\"passphrase\": \"cardano-wallet\" }"
                  )
                ]

        forM_ matrix $ \(name, nonJson) -> it name $ \ctx -> do
            w <- emptyWallet ctx
            let payload = nonJson
            r <- request @(ApiTransaction n) ctx
                (Link.createTransaction @'Shelley w) Default payload
            expectResponseCode @IO HTTP.status400 r

    describe "TRANS_CREATE_09 - Single Output Transaction with non-Shelley witnesses" $
        forM_ [(fixtureRandomWallet, "Byron wallet"), (fixtureIcarusWallet, "Icarus wallet")] $
        \(srcFixture,name) -> it name $ \ctx -> do

        (wByron, wShelley) <- (,) <$> srcFixture ctx <*> fixtureWallet ctx
        addrs <- listAddresses @n ctx wShelley

        let amt = minUTxOValue :: Natural
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
            (Link.getTransactionFee @'Byron wByron) Default payload
        verify rFeeEst
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]
        let (Quantity feeEstMin) = getFromResponse #estimatedMin rFeeEst
        let (Quantity feeEstMax) = getFromResponse #estimatedMax rFeeEst

        r <- postTx ctx
            (wByron, Link.createTransaction @'Byron, fixturePassphrase)
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
                (#balance . #getApiT . #available)
                (`shouldBe` Quantity (faucetAmt + amt)) rb

            ra2 <- request @ApiByronWallet ctx
                (Link.getWallet @'Byron wByron) Default Empty
            expectField
                (#balance . #available)
                (`shouldBe` Quantity (faucetAmt - feeEstMax - amt)) ra2

    it "TRANSMETA_CREATE_01 - Transaction with metadata" $ \ctx -> do
        (wa, wb) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
        let amt = (minUTxOValue :: Natural)

        basePayload <- mkTxPayload ctx wb amt fixturePassphrase

        let txMeta = [json|{ "1": { "string": "hello" } }|]
        let expected = TxMetadata $ Map.singleton 1 $ TxMetaText "hello"
        let payload = addTxMetadata txMeta basePayload

        ra <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wa) Default payload

        verify ra
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField
                (#metadata . #getApiTxMetadata)
                (`shouldBe` Just (ApiT expected))
            ]

        eventually "metadata is confirmed in transaction list" $ do
            -- on src wallet
            let linkSrcList = Link.listTransactions @'Shelley wa
            rla <- request @([ApiTransaction n]) ctx linkSrcList Default Empty
            verify rla
                [ expectResponseCode HTTP.status200
                , expectListField 0 (#status . #getApiT) (`shouldBe` InLedger)
                , expectListField 0 (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 0
                    (#metadata . #getApiTxMetadata)
                    (`shouldBe` Just (ApiT expected))
                ]
            -- on dst wallet
            let linkDstList = Link.listTransactions @'Shelley wb
            rlb <- request @([ApiTransaction n]) ctx linkDstList Default Empty
            verify rlb
                [ expectResponseCode HTTP.status200
                , expectListField 0 (#status . #getApiT) (`shouldBe` InLedger)
                , expectListField 0 (#direction . #getApiT) (`shouldBe` Incoming)
                , expectListField 0
                    (#metadata . #getApiTxMetadata)
                    (`shouldBe` Just (ApiT expected))
                ]

        let txid = getFromResponse #id ra
        eventually "metadata is confirmed in transaction get" $ do
          -- on src wallet
            let linkSrc = Link.getTransaction @'Shelley wa (ApiTxId txid)
            rg1 <- request @(ApiTransaction n) ctx linkSrc Default Empty
            verify rg1
                [ expectResponseCode HTTP.status200
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField
                    (#metadata . #getApiTxMetadata)
                    (`shouldBe` Just (ApiT expected))
                ]
          -- on dst wallet
            let linkDst = Link.getTransaction @'Shelley wb (ApiTxId txid)
            rg2 <- request @(ApiTransaction n) ctx linkDst Default Empty
            verify rg2
                [ expectResponseCode HTTP.status200
                , expectField (#direction . #getApiT) (`shouldBe` Incoming)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField
                    (#metadata . #getApiTxMetadata)
                    (`shouldBe` Just (ApiT expected))
                ]

    it "TRANSMETA_CREATE_02 - Transaction with invalid metadata" $ \ctx -> do
        (wa, wb) <- (,) <$> fixtureWallet ctx <*> fixtureWallet ctx
        let amt = (minUTxOValue :: Natural)

        basePayload <- mkTxPayload ctx wb amt fixturePassphrase

        let txMeta = [json|{ "1": { "string": #{T.replicate 65 "a"} } }|]
        let payload = addTxMetadata txMeta basePayload

        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wa) Default payload

        expectResponseCode @IO HTTP.status400 r
        expectErrorMessage errMsg400TxMetadataStringTooLong r

    it "TRANSMETA_CREATE_03 - Transaction with too much metadata" $ \ctx -> do
        (wa, wb) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
        let amt = (minUTxOValue :: Natural)

        basePayload <- mkTxPayload ctx wb amt fixturePassphrase

        -- This will encode to at least 8k of CBOR. The max tx size for the
        -- integration tests cluster is 4k.
        let txMeta = Aeson.object
                [ (toText @Int i, bytes)
                | i <- [0..127] ]
            bytes = [json|{ "bytes": #{T.replicate 64 "a"} }|]
        let payload = addTxMetadata txMeta basePayload

        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wa) Default payload

        expectResponseCode @IO HTTP.status400 r
        expectErrorMessage errMsg400TxTooLarge r

    it "TRANSMETA_ESTIMATE_01 - fee estimation includes metadata" $ \ctx -> do
        (wa, wb) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
        let amt = (minUTxOValue :: Natural)

        payload <- mkTxPayload ctx wb amt fixturePassphrase

        let txMeta = [json|{ "1": { "string": "hello" } }|]
        let payloadWithMetadata = addTxMetadata txMeta payload

        ra <- request @ApiFee ctx
            (Link.getTransactionFee @'Shelley wa) Default payloadWithMetadata
        verify ra
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]
        let (Quantity feeEstMin) = getFromResponse #estimatedMin ra
        let (Quantity feeEstMax) = getFromResponse #estimatedMax ra

        -- check that it's estimated to have less fees for transactions without
        -- metadata.
        rb <- request @ApiFee ctx
            (Link.getTransactionFee @'Shelley wa) Default payload
        verify rb
            [ expectResponseCode HTTP.status202
            , expectField (#estimatedMin . #getQuantity) (.< feeEstMin)
            , expectField (#estimatedMax . #getQuantity) (.< feeEstMax)
            ]

    it "TRANSMETA_ESTIMATE_02 - fee estimation with invalid metadata" $ \ctx -> do
        (wa, wb) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
        let amt = (minUTxOValue :: Natural)

        basePayload <- mkTxPayload ctx wb amt fixturePassphrase

        let txMeta = [json|{ "1": { "string": #{T.replicate 65 "a"} } }|]
        let payload = addTxMetadata txMeta basePayload

        r <- request @ApiFee ctx
            (Link.getTransactionFee @'Shelley wa) Default payload

        expectResponseCode @IO HTTP.status400 r
        expectErrorMessage errMsg400TxMetadataStringTooLong r

    it "TRANSMETA_ESTIMATE_03 - fee estimation with too much metadata" $ \ctx -> do
        (wa, wb) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
        let amt = (minUTxOValue :: Natural)

        basePayload <- mkTxPayload ctx wb amt fixturePassphrase

        -- This will encode to at least 8k of CBOR. The max tx size for the
        -- integration tests cluster is 4k.
        let txMeta = Aeson.object
                [ (toText @Int i, bytes)
                | i <- [0..127] ]
            bytes = [json|{ "bytes": #{T.replicate 64 "a"} }|]
        let payload = addTxMetadata txMeta basePayload
        print payload
        r <- request @ApiFee ctx
            (Link.getTransactionFee @'Shelley wa) Default payload

        expectResponseCode @IO HTTP.status400 r
        expectErrorMessage errMsg400TxTooLarge r

    it "TRANS_EXTERNAL_01 - Single Output Transaction" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        let amt = (10_000_000 :: Natural)

        let walletPostData = Json [json| {
                "name": "empty wallet",
                "mnemonic_sentence": #{mnemonics15},
                "passphrase": #{fixturePassphrase}
                } |]
        r1 <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default walletPostData
        verify r1
            [ expectSuccess
            , expectResponseCode HTTP.status201
            , expectField
                (#balance . #getApiT . #available) (`shouldBe` Quantity 0)
            ]
        let (_, Right wDest) = r1

        payload1 <- mkTxPayload ctx wDest amt fixturePassphrase

        (_, ApiFee (Quantity feeMin) (Quantity feeMax)) <- unsafeRequest ctx
            (Link.getTransactionFee @'Shelley wSrc) payload1

        r2 <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload1

        verify r2
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#amount . #getQuantity) $
                between (feeMin + amt, feeMax + amt)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            ]

        r3 <- request @ApiWallet ctx (Link.getWallet @'Shelley wSrc) Default Empty
        verify r3
            [ expectSuccess
            , expectField (#balance . #getApiT . #total) $
                between
                    ( Quantity (faucetAmt - feeMax - amt)
                    , Quantity (faucetAmt - feeMin - amt)
                    )
            , expectField
                    (#balance . #getApiT . #available)
                    (.>= Quantity (faucetAmt - faucetUtxoAmt))
            ]

        eventually "wSrc and wDest balances are as expected" $ do
            r4 <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            expectField
                (#balance . #getApiT . #available)
                (`shouldBe` Quantity amt) r4

            r5 <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wSrc) Default Empty
            expectField
                (#balance . #getApiT . #available)
                (`shouldBe` Quantity (faucetAmt - feeMax - amt)) r5

        let amt1 = (2_000_000 :: Natural)

        let walletPostData1 = Json [json| {
                "name": "empty wallet",
                "mnemonic_sentence": #{mnemonics18},
                "passphrase": #{fixturePassphrase}
                } |]
        r11 <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default walletPostData1
        verify r11
            [ expectSuccess
            , expectResponseCode HTTP.status201
            , expectField
                (#balance . #getApiT . #available) (`shouldBe` Quantity 0)
            ]
        let (_, Right wDest1) = r11

        --here will come external
        ------ beginning
        payload2 <- mkTxPayload ctx wDest1 amt1 fixturePassphrase

        (_, ApiFee (Quantity feeMin1) (Quantity feeMax1)) <- unsafeRequest ctx
            (Link.getTransactionFee @'Shelley wDest) payload2

        r6 <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wDest) Default payload2

        verify r6
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#amount . #getQuantity) $
                between (feeMin1 + amt1, feeMax1 + amt1)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            ]
        --- end
        r7 <- request @ApiWallet ctx (Link.getWallet @'Shelley wDest) Default Empty
        verify r7
            [ expectSuccess
            , expectField (#balance . #getApiT . #total) $
                between
                    ( Quantity (amt - feeMax1 - amt1)
                    , Quantity (amt - feeMin1 - amt1)
                    )
            ]

        eventually "wDest1 and wDest balances are as expected" $ do
            r8 <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest1) Default Empty
            expectField
                (#balance . #getApiT . #available)
                (`shouldBe` Quantity amt1) r8

            r9 <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            expectField
                (#balance . #getApiT . #available)
                (`shouldBe` Quantity (amt - feeMax1 - amt1)) r9
>>>>>>> 22b9072ea... add temporary test which part will be replaced with externalTx

    describe "TRANS_ESTIMATE_08 - Bad payload" $ do
        let matrix =
                [ ( "empty payload", NonJson "" )
                , ( "{} payload", NonJson "{}" )
                , ( "non-json valid payload"
                  , NonJson
                        "{ payments: [{\
                         \\"address\": 12312323,\
                         \\"amount: {\
                         \\"quantity\": 1,\
                         \\"unit\": \"lovelace\"} }]\
                         \ }"
                  )
                ]

        forM_ matrix $ \(name, nonJson) -> it name $ \ctx -> do
            w <- emptyWallet ctx
            let payload = nonJson
            r <- request @ApiFee ctx
                (Link.getTransactionFee @'Shelley w) Default payload
            expectResponseCode @IO HTTP.status400 r

    it "TRANS_ESTIMATE_03a - we see result when we can't cover fee" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        payload <- mkTxPayload ctx wSrc faucetAmt fixturePassphrase
        r <- request @ApiFee ctx
            (Link.getTransactionFee @'Shelley wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status202
            , expectField (#estimatedMin . #getQuantity) (.>= 0)
            , expectField (#estimatedMax . #getQuantity) (.<= oneAda)
            ]

    it "TRANS_ESTIMATE_03b - we see result when we can't cover fee (with withdrawal)" $ \ctx -> do
        (wSrc, _) <- rewardWallet ctx
        addr:_ <- fmap (view #id) <$> listAddresses @n ctx wSrc
        let totalBalance = wSrc ^. #balance . #getApiT . #total
        let payload = Json [json|{
                "withdrawal": "self",
                "payments": [{
                    "address": #{addr},
                    "amount": #{totalBalance}
                }],
                "passphrase": #{fixturePassphrase}
            }|]
        r <- request @ApiFee ctx
            (Link.getTransactionFee @'Shelley wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status202
            , expectField (#estimatedMin . #getQuantity) (.>= 0)
            , expectField (#estimatedMax . #getQuantity) (.<= oneAda)
            ]

    it "TRANS_ESTIMATE_04 - Not enough money" $ \ctx -> do
        let (srcAmt, reqAmt) = (minUTxOValue, 2 * minUTxOValue)
        wSrc <- fixtureWalletWith @n ctx [srcAmt]
        wDest <- emptyWallet ctx
        payload <- mkTxPayload ctx wDest reqAmt fixturePassphrase
        r <- request @ApiFee ctx
            (Link.getTransactionFee @'Shelley wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage $
                errMsg403NotEnoughMoney srcAmt reqAmt
            ]

    it "TRANS_ESTIMATE_07 - Deleted wallet" $ \ctx -> do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
        wDest <- emptyWallet ctx
        payload <- mkTxPayload ctx wDest minUTxOValue fixturePassphrase
        r <- request @ApiFee ctx
            (Link.getTransactionFee @'Shelley w) Default payload
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    it "TRANS_LIST_01 - Can list Incoming and Outgoing transactions" $ \ctx -> do
        -- Make tx from fixtureWallet
        (wSrc, wDest) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
        addrs <- listAddresses @n ctx wDest

        let amt = minUTxOValue :: Natural
        let destination = (addrs !! 1) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]

        tx <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload
        expectResponseCode HTTP.status202 tx
        eventually "Wallet balance is as expected" $ do
            rGet <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            verify rGet
                [ expectField
                        (#balance . #getApiT . #total) (`shouldBe` Quantity amt)
                , expectField
                        (#balance . #getApiT . #available) (`shouldBe` Quantity amt)
                ]

        -- Verify Tx list contains Incoming and Outgoing
        let link = Link.listTransactions @'Shelley wSrc
        r <- request @([ApiTransaction n]) ctx link Default Empty
        expectResponseCode @IO HTTP.status200 r

        verify r
            [ expectListField 0 (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectListField 1 (#direction . #getApiT) (`shouldBe` Incoming)
            ]

    -- This scenario covers the following matrix of cases. Cases were generated
    -- using one of pairwise test cases generation tools available online.
    -- +---+----------+----------+------------+--------------+
    --     |  start   |   end    |   order    |    result    |
    -- +---+----------+----------+------------+--------------+
    --   1 | edge     | edge     | ascending  | 2 ascending  |
    --   2 | edge     | edge + 1 | descending | 2 descending |
    --   3 | edge     | edge - 1 | empty      | 1st one      |
    --   4 | edge     | empty    | empty      | 2 descending |
    --   5 | edge + 1 | edge + 1 | empty      | 2nd one      |
    --   6 | edge + 1 | edge - 1 | empty      | none         |
    --   7 | edge + 1 | empty    | ascending  | 2nd one      |
    --   8 | edge + 1 | edge     | descending | 2nd one      |
    --   9 | edge - 1 | edge - 1 | ascending  | 1st one      |
    --  10 | edge - 1 | empty    | descending | 2 descending |
    --  11 | edge - 1 | edge     | empty      | 2 descending |
    --  12 | edge - 1 | edge + 1 | empty      | 2 descending |
    --  13 | empty    | empty    | empty      | 2 descending |
    --  14 | empty    | edge     | empty      | 2 descending |
    --  15 | empty    | edge + 1 | ascending  | 2 ascending  |
    --  16 | empty    | edge - 1 | descending | 1st one      |
    --  17 | t1       | t1       | empty      | 1st one      |
    --  18 | t2       | t2       | descending | 2nd one      |
    -- +---+----------+----------+------------+--------------+
    it "TRANS_LIST_02,03x - Can limit/order results with start, end and order"
        $ \ctx -> do
        let a1 = Quantity $ sum $ replicate 10 minUTxOValue
        let a2 = Quantity $ sum $ replicate 10 (2 * minUTxOValue)
        w <- fixtureWalletWith @n ctx $ mconcat
                [ replicate 10 minUTxOValue
                , replicate 10 (2 * minUTxOValue)
                ]
        txs <- listAllTransactions @n ctx w
        let [Just t2, Just t1] = fmap (fmap time . insertedAt) txs
        let matrix :: [TestCase [ApiTransaction n]] =
                [ TestCase -- 1
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t1)
                        , ("end", utcIso8601ToText t2)
                        , ("order", "ascending")
                        ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` a1)
                        , expectListField 1 #amount (`shouldBe` a2)
                        ]
                    }
                , TestCase -- 2
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t1)
                        , ("end", utcIso8601ToText $ plusDelta t2)
                        , ("order", "descending")
                        ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` a2)
                        , expectListField 1 #amount (`shouldBe` a1)
                        ]
                    }
                , TestCase -- 3
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t1)
                        , ("end", utcIso8601ToText $ minusDelta t2)
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` a1)
                        ]
                    }
                , TestCase -- 4
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t1) ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` a2)
                        , expectListField 1 #amount (`shouldBe` a1)
                        ]
                    }
                , TestCase --5
                    { query = toQueryString
                        [ ("start", utcIso8601ToText $ plusDelta t1)
                        , ("end", utcIso8601ToText $ plusDelta t2)
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` a2)
                        ]
                    }
                , TestCase -- 6
                    { query = toQueryString
                        [ ("start", utcIso8601ToText $ plusDelta t1)
                        , ("end", utcIso8601ToText $ minusDelta t2)
                        ]
                    , assertions =
                        [ expectListSize 0 ]
                    }
                , TestCase -- 7
                    { query = toQueryString
                        [ ("start", utcIso8601ToText $ plusDelta t1)
                        , ("order", "ascending")
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` a2)
                        ]
                    }
                , TestCase -- 8
                    { query = toQueryString
                        [ ("order", "descending")
                        , ("start", utcIso8601ToText $ plusDelta t1)
                        , ("end", utcIso8601ToText t2)
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` a2)
                        ]
                    }
                , TestCase -- 9
                    { query = toQueryString
                        [ ("order", "ascending")
                        , ("start", utcIso8601ToText $ minusDelta t1)
                        , ("end", utcIso8601ToText $ minusDelta t2)
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` a1)
                        ]
                    }
                , TestCase -- 10
                    { query = toQueryString
                        [ ("order", "descending")
                        , ("start", utcIso8601ToText $ minusDelta t1)
                        ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` a2)
                        , expectListField 1 #amount (`shouldBe` a1)
                        ]
                    }
                , TestCase -- 11
                    { query = toQueryString
                        [ ("start", utcIso8601ToText $ minusDelta t1)
                        , ("end", utcIso8601ToText t2)
                        ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` a2)
                        , expectListField 1 #amount (`shouldBe` a1)
                        ]
                    }
                , TestCase -- 12
                    { query = toQueryString
                        [ ("start", utcIso8601ToText $ minusDelta t1)
                        , ("end", utcIso8601ToText $ plusDelta t2)
                        ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` a2)
                        , expectListField 1 #amount (`shouldBe` a1)
                        ]
                    }
                , TestCase -- 13
                    { query = mempty
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` a2)
                        , expectListField 1 #amount (`shouldBe` a1)
                        ]
                    }
                , TestCase -- 14
                    { query = toQueryString
                        [ ("end", utcIso8601ToText t2) ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` a2)
                        , expectListField 1 #amount (`shouldBe` a1)
                        ]
                    }
                , TestCase -- 15
                    { query = toQueryString
                        [ ("end", utcIso8601ToText $ plusDelta t2) ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` a2)
                        , expectListField 1 #amount (`shouldBe` a1)
                        ]
                    }
                , TestCase -- 16
                    { query = toQueryString
                        [ ("end", utcIso8601ToText $ minusDelta t2) ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` a1)
                        ]
                    }
                , TestCase -- 17
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t1)
                        , ("end", utcIso8601ToText t1)
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` a1)
                        ]
                    }
                , TestCase -- 18
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t2)
                        , ("end", utcIso8601ToText t2)
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` a2)
                        ]
                    }
                ]

        let withQuery q (method, link) = (method, link <> q)

        forM_ matrix $ \tc -> do
            let link = withQuery (query tc) $ Link.listTransactions @'Shelley w
            rf <- request @([ApiTransaction n]) ctx link Default Empty
            verify rf (assertions tc)

    describe "TRANS_LIST_02,03 - Faulty start, end, order values" $ do
        let orderErr = "Please specify one of the following values:\
            \ ascending, descending."
        let startEndErr = "Expecting ISO 8601 date-and-time format\
            \ (basic or extended), e.g. 2012-09-25T10:15:00Z."
        let queries :: [TestCase [ApiTransaction n]] =
                [
                  TestCase
                    { query = toQueryString [ ("start", "2009") ]
                    , assertions =
                             [ expectResponseCode @IO HTTP.status400
                             , expectErrorMessage startEndErr
                             ]

                    }
                 , TestCase
                     { query = toQueryString
                             [ ("start", "2012-09-25T10:15:00Z")
                             , ("end", "2016-11-21")
                             ]
                     , assertions =
                             [ expectResponseCode @IO HTTP.status400
                             , expectErrorMessage startEndErr
                             ]

                     }
                 , TestCase
                     { query = toQueryString
                             [ ("start", "2012-09-25")
                             , ("end", "2016-11-21T10:15:00Z")
                             ]
                     , assertions =
                             [ expectResponseCode @IO HTTP.status400
                             , expectErrorMessage startEndErr
                             ]

                     }
                 , TestCase
                     { query = toQueryString
                             [ ("end", "2012-09-25T10:15:00Z")
                             , ("start", "2016-11-21")
                             ]
                     , assertions =
                             [ expectResponseCode @IO HTTP.status400
                             , expectErrorMessage startEndErr
                             ]

                     }
                 , TestCase
                     { query = toQueryString [ ("order", "scending") ]
                     , assertions =
                            [ expectResponseCode @IO HTTP.status400
                            , expectErrorMessage orderErr
                            ]

                     }
                 , TestCase
                     { query = toQueryString
                             [ ("start", "2012-09-25T10:15:00Z")
                             , ("order", "asc")
                             ]
                     , assertions =
                             [ expectResponseCode @IO HTTP.status400
                             , expectErrorMessage orderErr
                             ]
                     }
                ]

        let withQuery q (method, link) = (method, link <> q)

        forM_ queries $ \tc -> it (T.unpack $ query tc) $ \ctx -> do
            w <- emptyWallet ctx
            let link = withQuery (query tc) $ Link.listTransactions @'Shelley w
            r <- request @([ApiTransaction n]) ctx link Default Empty
            verify r (assertions tc)

    it "TRANS_LIST_02 - Start time shouldn't be later than end time" $
        \ctx -> do
            w <- emptyWallet ctx
            let startTime = "2009-09-09T09:09:09Z"
            let endTime = "2001-01-01T01:01:01Z"
            let link = Link.listTransactions' @'Shelley w
                    Nothing
                    (either (const Nothing) Just $ fromText $ T.pack startTime)
                    (either (const Nothing) Just $ fromText $ T.pack endTime)
                    Nothing
            r <- request @([ApiTransaction n]) ctx link Default Empty
            expectResponseCode @IO HTTP.status400 r
            expectErrorMessage
                (errMsg400StartTimeLaterThanEndTime startTime endTime) r
            pure ()

    it "TRANS_LIST_03 - Minimum withdrawal shouldn't be 0" $
        \ctx -> do
            w <- emptyWallet ctx
            let link = Link.listTransactions' @'Shelley w
                    (Just 0)
                    Nothing
                    Nothing
                    Nothing
            r <- request @([ApiTransaction n]) ctx link Default Empty
            expectResponseCode @IO HTTP.status400 r
            expectErrorMessage errMsg400MinWithdrawalWrong r
            pure ()

    it "TRANS_LIST_03 - Minimum withdrawal can be 1, shows empty when no withdrawals" $
        \ctx -> do
            w <- emptyWallet ctx
            let link = Link.listTransactions' @'Shelley w
                    (Just 1)
                    Nothing
                    Nothing
                    Nothing
            r <- request @([ApiTransaction n]) ctx link Default Empty
            expectResponseCode @IO HTTP.status200 r
            let txs = getFromResponse Prelude.id r
            txs `shouldBe` []

    it "TRANS_LIST_04 - Deleted wallet" $ \ctx -> do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
        r <- request @([ApiTransaction n]) ctx (Link.listTransactions @'Shelley w)
            Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    it "TRANS_LIST_RANGE_01 - \
       \Transaction at time t is SELECTED by small ranges that cover it" $
          \ctx -> do
              w <- fixtureWalletWith @n ctx [minUTxOValue]
              t <- unsafeGetTransactionTime <$> listAllTransactions ctx w
              let (te, tl) = (utcTimePred t, utcTimeSucc t)
              txs1 <- listTransactions @n ctx w (Just t ) (Just t ) Nothing
              txs2 <- listTransactions @n ctx w (Just te) (Just t ) Nothing
              txs3 <- listTransactions @n ctx w (Just t ) (Just tl) Nothing
              txs4 <- listTransactions @n ctx w (Just te) (Just tl) Nothing
              length <$> [txs1, txs2, txs3, txs4] `shouldSatisfy` all (== 1)

    it "TRANS_LIST_RANGE_02 - \
       \Transaction at time t is NOT selected by range (t + 𝛿t, ...)" $
          \ctx -> do
              w <- fixtureWalletWith @n ctx [minUTxOValue]
              t <- unsafeGetTransactionTime <$> listAllTransactions ctx w
              let tl = utcTimeSucc t
              txs1 <- listTransactions @n ctx w (Just tl) (Nothing) Nothing
              txs2 <- listTransactions @n ctx w (Just tl) (Just tl) Nothing
              length <$> [txs1, txs2] `shouldSatisfy` all (== 0)

    it "TRANS_LIST_RANGE_03 - \
       \Transaction at time t is NOT selected by range (..., t - 𝛿t)" $
          \ctx -> do
              w <- fixtureWalletWith @n ctx [minUTxOValue]
              t <- unsafeGetTransactionTime <$> listAllTransactions ctx w
              let te = utcTimePred t
              txs1 <- listTransactions @n ctx w (Nothing) (Just te) Nothing
              txs2 <- listTransactions @n ctx w (Just te) (Just te) Nothing
              length <$> [txs1, txs2] `shouldSatisfy` all (== 0)

    it "TRANS_GET_01 - Can get Incoming and Outgoing transaction" $ \ctx -> do
        (wSrc, wDest) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
        -- post tx
        let amt = (minUTxOValue :: Natural)
        rMkTx <- postTx ctx
            (wSrc, Link.createTransaction @'Shelley, "cardano-wallet")
            wDest
            amt
        let txid = getFromResponse #id rMkTx
        verify rMkTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            ]

        eventually "Wallet balance is as expected" $ do
            rGet <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            verify rGet
                [ expectField
                        (#balance . #getApiT . #total) (`shouldBe` Quantity amt)
                , expectField
                        (#balance . #getApiT . #available) (`shouldBe` Quantity amt)
                ]

        eventually "Transactions are available and in ledger" $ do
            -- Verify Tx in source wallet is Outgoing and InLedger
            let linkSrc = Link.getTransaction @'Shelley wSrc (ApiTxId txid)
            r1 <- request @(ApiTransaction n) ctx linkSrc Default Empty
            verify r1
                [ expectResponseCode HTTP.status200
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                ]

            -- Verify Tx in destination wallet is Incoming and InLedger
            let linkDest = Link.getTransaction @'Shelley wDest (ApiTxId txid)
            r2 <- request @(ApiTransaction n) ctx linkDest Default Empty
            verify r2
                [ expectResponseCode HTTP.status200
                , expectField (#direction . #getApiT) (`shouldBe` Incoming)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                ]

    it "TRANS_GET_02 - Deleted wallet" $ \ctx -> do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
        let txid = ApiT $ Hash $ BS.pack $ replicate 32 1
        let link = Link.getTransaction @'Shelley w (ApiTxId txid)
        r <- request @(ApiTransaction n) ctx link Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    it "TRANS_GET_03 - Using wrong transaction id" $ \ctx -> do
        (wSrc, wDest) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
        -- post tx
        let amt = (minUTxOValue :: Natural)
        rMkTx <- postTx ctx
            (wSrc, Link.createTransaction @'Shelley, "cardano-wallet")
            wDest
            amt
        verify rMkTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            ]

        let txid =  Hash $ BS.pack $ replicate 32 1
        let link = Link.getTransaction @'Shelley wSrc (ApiTxId $ ApiT txid)
        r <- request @(ApiTransaction n) ctx link Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404CannotFindTx $ toText txid) r


    it "TRANS_DELETE_01 -\
        \ Shelley: Can forget pending transaction" $ \ctx -> do
        (wSrc, wDest) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
        -- post tx
        let amt = (minUTxOValue :: Natural)
        rMkTx <- postTx ctx
            (wSrc, Link.createTransaction @'Shelley, "cardano-wallet")
            wDest
            amt
        let txid = getFromResponse #id rMkTx
        verify rMkTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            ]

        -- verify balance on src wallet
        request @ApiWallet ctx (Link.getWallet @'Shelley wSrc) Default Empty >>= flip verify
            [ expectSuccess
            , expectField
                    (#balance . #getApiT . #available)
                    (`shouldBe` Quantity (faucetAmt - faucetUtxoAmt))
            ]

        -- forget transaction
        request @ApiTxId ctx (Link.deleteTransaction @'Shelley wSrc (ApiTxId txid)) Default Empty
            >>= expectResponseCode @IO HTTP.status204

        -- verify again balance on src wallet
        request @ApiWallet ctx (Link.getWallet @'Shelley wSrc) Default Empty >>= flip verify
            [ expectSuccess
            , expectField
                    (#balance . #getApiT . #total)
                    (`shouldBe` Quantity faucetAmt)
            , expectField
                    (#balance . #getApiT . #available)
                    (`shouldBe` Quantity faucetAmt)
            ]

        eventually "transaction eventually is in source wallet" $ do
            let ep = Link.listTransactions @'Shelley wSrc
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListField 0
                    (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 0
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]

        eventually "transaction eventually is in target wallet" $ do
            let ep = Link.listTransactions @'Shelley wDest
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListField 0
                    (#direction . #getApiT) (`shouldBe` Incoming)
                , expectListField 0
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]

    it "TRANS_DELETE_02 -\
        \ Shelley: Cannot forget tx that is already in ledger" $ \ctx -> do
        (wSrc, wDest) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx

        -- post transaction
        rTx <-
            postTx ctx
            (wSrc, Link.createTransaction @'Shelley, "cardano-wallet")
            wDest
            (minUTxOValue :: Natural)
        let txid = getFromResponse #id rTx

        eventually "Transaction is accepted" $ do
            let ep = Link.listTransactions @'Shelley wSrc
            request @([ApiTransaction n]) ctx ep Default Empty >>= flip verify
                [ expectListField 0
                    (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 0
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]

        -- Try Forget transaction once it's no longer pending
        let ep = Link.deleteTransaction @'Shelley wSrc (ApiTxId txid)
        rDel <- request @ApiTxId ctx ep Default Empty
        expectResponseCode @IO HTTP.status403 rDel
        let err = errMsg403NoPendingAnymore (toUrlPiece (ApiTxId txid))
        expectErrorMessage err rDel

    describe "TRANS_DELETE_03 - checking no transaction id error for " $ do
        txDeleteNotExistsingTxIdTest emptyWallet "wallets"
        txDeleteNotExistsingTxIdTest emptyRandomWallet "byron-wallets"

    describe "TRANS_DELETE_06 -\
        \ Cannot forget tx that is performed from different wallet" $ do
        txDeleteFromDifferentWalletTest emptyWallet "wallets"
        txDeleteFromDifferentWalletTest emptyRandomWallet "byron-wallets"

    it "BYRON_TRANS_DELETE -\
        \ Cannot delete tx on Byron wallet using shelley ep" $ \ctx -> do
            w <- emptyRandomWallet ctx
            let wid = w ^. walletId
            let txid = "3e6ec12da4414aa0781ff8afa9717ae53ee8cb4aa55d622f65bc62619a4f7b12"
            let endpoint = "v2/wallets/" <> wid <> "/transactions/" <> txid
            r <- request @ApiTxId @IO ctx ("DELETE", endpoint) Default Empty
            expectResponseCode HTTP.status404 r
            expectErrorMessage (errMsg404NoWallet wid) r

    it "BYRON_TRANS_ESTIMATE -\
        \ Cannot estimate tx on Byron wallet using shelley ep" $ \ctx -> do
            w <- emptyRandomWallet ctx
            let wid = w ^. walletId
            wDest <- emptyWallet ctx
            addr:_ <- listAddresses @n ctx wDest
            let destination = addr ^. #id
            let payload = Json [json|{
                    "payments": [{
                        "address": #{destination},
                        "amount": {
                            "quantity": #{minUTxOValue},
                            "unit": "lovelace"
                        }
                    }]
                }|]
            let endpoint = "v2/wallets/" <> wid <> "/payment-fees"
            r <- request @ApiFee ctx ("POST", endpoint) Default payload
            expectResponseCode @IO HTTP.status404 r
            expectErrorMessage (errMsg404NoWallet wid) r

    it "BYRON_TRANS_CREATE -\
        \ Cannot create tx on Byron wallet using shelley ep" $ \ctx -> do
            w <- emptyRandomWallet ctx
            let wid = w ^. walletId
            wDest <- emptyWallet ctx
            addr:_ <- listAddresses @n ctx wDest
            let destination = addr ^. #id
            let payload = Json [json|{
                    "payments": [{
                        "address": #{destination},
                        "amount": {
                            "quantity": #{minUTxOValue},
                            "unit": "lovelace"
                        }
                    }],
                    "passphrase": "cardano-wallet"
                }|]
            let endpoint = "v2/wallets/" <> wid <> "/transactions"
            r <- request @(ApiTransaction n) ctx ("POST", endpoint) Default payload
            expectResponseCode @IO HTTP.status404 r
            expectErrorMessage (errMsg404NoWallet wid) r

    it "BYRON_TX_LIST_02 -\
        \ Byron endpoint does not list Shelley wallet transactions" $ \ctx -> do
        w <- emptyWallet ctx
        let wid = w ^. walletId
        let ep = ("GET", "v2/byron-wallets/" <> wid <> "/transactions")
        r <- request @([ApiTransaction n]) ctx ep Default Empty
        verify r
            [ expectResponseCode @IO HTTP.status404
            , expectErrorMessage (errMsg404NoWallet wid)
            ]

    it "BYRON_TX_LIST_03 -\
        \ Shelley endpoint does not list Byron wallet transactions" $ \ctx -> do
        w <- emptyRandomWallet ctx
        let wid = w ^. walletId
        let ep = ("GET", "v2/wallets/" <> wid <> "/transactions")
        r <- request @([ApiTransaction n]) ctx ep Default Empty
        verify r
            [ expectResponseCode @IO HTTP.status404
            , expectErrorMessage (errMsg404NoWallet wid)
            ]

    it "SHELLEY_TX_REDEEM_01 - Can redeem rewards from self" $ \ctx -> do
        (wSrc,_) <- rewardWallet ctx
        addr:_ <- fmap (view #id) <$> listAddresses @n ctx wSrc

        let payload = Json [json|{
                "withdrawal": "self",
                "payments": [{
                    "address": #{addr},
                    "amount": { "quantity": #{minUTxOValue}, "unit": "lovelace" }
                }],
                "passphrase": #{fixturePassphrase}
            }|]
        rTx <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload
        verify rTx
            [ expectResponseCode HTTP.status202
            , expectField #withdrawals
                (`shouldSatisfy` (not . null))
            ]

        eventually "rewards are transferred from self to self" $ do
            rW <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wSrc) Default payload
            print rW
            verify rW
                [ expectField (#balance . #getApiT . #available)
                    (.> (wSrc ^. #balance . #getApiT . #available))
                , expectField (#balance . #getApiT . #reward)
                    (`shouldBe` Quantity 0)
                ]

    it "SHELLEY_TX_REDEEM_02 - Can redeem rewards from other" $ \ctx -> do
        (wOther, mw) <- rewardWallet ctx
        wSelf  <- fixtureWallet ctx
        addr:_ <- fmap (view #id) <$> listAddresses @n ctx wSelf

        let payload = Json [json|{
                "withdrawal": #{mnemonicToText mw},
                "payments": [{
                    "address": #{addr},
                    "amount": { "quantity": #{minUTxOValue}, "unit": "lovelace" }
                }],
                "passphrase": #{fixturePassphrase}
            }|]
        (_, ApiFee (Quantity _) (Quantity fee)) <- unsafeRequest ctx
            (Link.getTransactionFee @'Shelley wSelf) payload

        rTx <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSelf) Default payload
        verify rTx
            [ expectResponseCode HTTP.status202
            , expectField #withdrawals
                (`shouldSatisfy` (not . null))
            , expectField (#direction . #getApiT)
                (`shouldBe` Incoming)
            , expectField (#amount . #getQuantity)
                (`shouldBe` (oneMillionAda - fee))
            ]
        let tid = getFromResponse Prelude.id rTx

        eventually "rewards disappear from other" $ do
            rWOther <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wOther) Default payload
            verify rWOther
                [ expectField (#balance . #getApiT . #reward)
                    (`shouldBe` Quantity 0)
                ]

        eventually "withdrawal transaction is listed on other" $ do
            rTxOther <- request @(ApiTransaction n) ctx
                (Link.getTransaction  @'Shelley wOther tid) Default payload
            verify rTxOther
                [ expectResponseCode
                    HTTP.status200
                , expectField #withdrawals
                    (`shouldSatisfy` (not . null))
                , expectField (#direction . #getApiT)
                    (`shouldBe` Outgoing)
                , expectField (#amount . #getQuantity)
                    (`shouldBe` oneMillionAda)
                ]

        eventually "rewards appear on self" $ do
            rWSelf <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wSelf) Default payload
            verify rWSelf
                [ expectField (#balance . #getApiT . #available)
                    (.> (wSelf ^. #balance . #getApiT . #available))
                ]

        eventually "withdrawal transaction is listed on self" $ do
            rTxSelf <- request @(ApiTransaction n) ctx
                (Link.getTransaction  @'Shelley wSelf tid) Default payload
            verify rTxSelf
                [ expectResponseCode
                    HTTP.status200
                , expectField #withdrawals
                    (`shouldSatisfy` (not . null))
                , expectField (#direction . #getApiT)
                    (`shouldBe` Incoming)
                , expectField (#amount . #getQuantity)
                    (`shouldBe` (oneMillionAda - fee))
                , expectField (#status . #getApiT)
                    (`shouldBe` InLedger)
                ]

    it "SHELLEY_TX_REDEEM_03 - Can't redeem rewards from other if none left" $ \ctx -> do
        (wOther, mw) <- rewardWallet ctx
        wSelf  <- fixtureWallet ctx
        addr:_ <- fmap (view #id) <$> listAddresses @n ctx wSelf

        let payload = Json [json|{
                "withdrawal": #{mnemonicToText mw},
                "payments": [{
                    "address": #{addr},
                    "amount": { "quantity": #{minUTxOValue}, "unit": "lovelace" }
                }],
                "passphrase": #{fixturePassphrase}
            }|]

        -- Withdraw rewards from the other wallet.
        _ <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSelf) Default payload
        eventually "rewards disappear from other" $ do
            rWOther <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wOther) Default payload
            verify rWOther
                [ expectField (#balance . #getApiT . #reward)
                    (`shouldBe` Quantity 0)
                ]

        -- Try withdrawing AGAIN, rewards that aren't there anymore.
        rTx <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSelf) Default payload
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403WithdrawalNotWorth
            ]

    it "SHELLEY_TX_REDEEM_04 - Can always ask for self redemption" $ \ctx -> do
        wSelf <- fixtureWallet ctx
        addr:_ <- fmap (view #id) <$> listAddresses @n ctx wSelf

        let payload = Json [json|{
                "withdrawal": "self",
                "payments": [{
                    "address": #{addr},
                    "amount": { "quantity": #{minUTxOValue}, "unit": "lovelace" }
                }],
                "passphrase": #{fixturePassphrase}
            }|]

        rTx <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSelf) Default payload
        verify rTx
            [ expectResponseCode HTTP.status202
            , expectField #withdrawals (`shouldSatisfy` null)
            ]

    it "SHELLEY_TX_REDEEM_05 - Can't redeem rewards from unknown key" $ \ctx -> do
        wSelf  <- fixtureWallet ctx
        addr:_ <- fmap (view #id) <$> listAddresses @n ctx wSelf

        mw <- entropyToMnemonic <$> genEntropy @160
        let payload = Json [json|{
                "withdrawal": #{mnemonicToText mw},
                "payments": [{
                    "address": #{addr},
                    "amount": { "quantity": #{minUTxOValue}, "unit": "lovelace" }
                }],
                "passphrase": #{fixturePassphrase}
            }|]

        rTx <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSelf) Default payload
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403WithdrawalNotWorth
            ]

    it "SHELLEY_TX_REDEEM_06 - Can't redeem rewards using byron wallet" $ \ctx -> do
        (wSelf, addrs) <- fixtureIcarusWalletAddrs @n ctx
        let addr = encodeAddress @n (head addrs)

        let payload = Json [json|{
                "withdrawal": "self",
                "payments": [{
                    "address": #{addr},
                    "amount": { "quantity": #{minUTxOValue}, "unit": "lovelace" }
                }],
                "passphrase": #{fixturePassphrase}
            }|]

        rTx <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Byron wSelf) Default payload
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403NotAShelleyWallet
            ]

    it "SHELLEY_TX_REDEEM_06a - Can't redeem rewards if utxo = 0 from other" $ \ctx -> do
        (_, mw) <- rewardWallet ctx
        wSelf  <- emptyWallet ctx
        addr:_ <- fmap (view #id) <$> listAddresses @n ctx wSelf

        let payload = Json [json|{
                "withdrawal": #{mnemonicToText mw},
                "payments": [{
                    "address": #{addr},
                    "amount": { "quantity": #{minUTxOValue}, "unit": "lovelace" }
                }],
                "passphrase": #{fixturePassphrase}
            }|]

        -- Try withdrawing when no UTxO on a wallet
        rTx <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSelf) Default payload
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403InputsDepleted
            ]

    it "SHELLEY_TX_REDEEM_06b - Can't redeem rewards if utxo = 0 from self" $ \ctx -> do
        (wRewards, mw) <- rewardWallet ctx
        wOther  <- emptyWallet ctx

        -- migrate all utxo from rewards wallet
        addr:_ <- fmap (view #id) <$> listAddresses @n ctx wOther
        let payloadMigr = Json [json|{
                "passphrase": #{fixturePassphrase},
                "addresses": [#{addr}]
            }|]

        let ep = Link.migrateWallet @'Shelley wRewards
        rM <- request @[ApiTransaction n] ctx ep Default payloadMigr
        expectResponseCode HTTP.status202 rM

        eventually "No UTxO is on rewards wallet" $ do
            rWOther <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wRewards) Default Empty
            verify rWOther
                [ expectField (#balance . #getApiT . #available)
                    (`shouldBe` Quantity 0)
                ]

        -- Try withdrawing when no UTxO on a wallet
        let payload = Json [json|{
                "withdrawal": #{mnemonicToText mw},
                "payments": [{
                    "address": #{addr},
                    "amount": { "quantity": #{oneAda}, "unit": "lovelace" }
                }],
                "passphrase": #{fixturePassphrase}
            }|]
        rTx <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wRewards) Default payload
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403InputsDepleted
            ]

    it "SHELLEY_TX_REDEEM_07a - Can't redeem rewards if cannot cover fee" $ \ctx -> do
        (_, mw) <- rewardWallet ctx
        wSelf  <- fixtureWalletWith @n ctx [oneThousandAda]
        addr:_ <- fmap (view #id) <$> listAddresses @n ctx wSelf
        let amt = oneThousandAda + oneMillionAda

        let payload = Json [json|{
                "withdrawal": #{mnemonicToText mw},
                "payments": [{
                    "address": #{addr},
                    "amount": { "quantity": #{amt}, "unit": "lovelace" }
                }],
                "passphrase": #{fixturePassphrase}
            }|]

        -- Try withdrawing when cannot cover fee
        rTx <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSelf) Default payload
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403Fee
            ]

    it "SHELLEY_TX_REDEEM_07b - Can't redeem rewards if not enough money" $ \ctx -> do
        (_, mw) <- rewardWallet ctx
        wSelf  <- fixtureWalletWith @n ctx [oneThousandAda]
        addr:_ <- fmap (view #id) <$> listAddresses @n ctx wSelf
        let amt = oneThousandAda + oneMillionAda + oneAda

        let payload = Json [json|{
                "withdrawal": #{mnemonicToText mw},
                "payments": [{
                    "address": #{addr},
                    "amount": { "quantity": #{amt}, "unit": "lovelace" }
                }],
                "passphrase": #{fixturePassphrase}
            }|]

        -- Try withdrawing when no not enough money
        rTx <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSelf) Default payload
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403NotEnoughMoney_
            ]
  where
    txDeleteNotExistsingTxIdTest eWallet resource =
        it resource $ \ctx -> do
            w <- eWallet ctx
            let walId = w ^. walletId
            let txid = "3e6ec12da4414aa0781ff8afa9717ae53ee8cb4aa55d622f65bc62619a4f7b12"
            let endpoint = "v2/" <> T.pack resource <> "/" <> walId <> "/transactions/" <> txid
            ra <- request @ApiTxId @IO ctx ("DELETE", endpoint) Default Empty
            expectResponseCode @IO HTTP.status404 ra
            expectErrorMessage (errMsg404CannotFindTx txid) ra

    txDeleteFromDifferentWalletTest
        :: (HasType (ApiT WalletId) wal)
        => (Context t -> IO wal)
        -> String
        -> SpecWith (Context t)
    txDeleteFromDifferentWalletTest eWallet resource =
        it resource $ \ctx -> do
            -- post tx
            (wSrc, wDest) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
            rMkTx <- postTx ctx
                (wSrc, Link.createTransaction @'Shelley, "cardano-wallet")
                wDest
                (minUTxOValue :: Natural)

            -- try to forget from different wallet
            wDifferent <- eWallet ctx
            let txid = toText $ getApiT $ getFromResponse #id rMkTx
            let endpoint = "v2/" <> T.pack resource <> "/"
                     <> wDifferent ^. walletId
                     <> "/transactions/"
                     <> txid
            ra <- request @ApiTxId @IO ctx ("DELETE", endpoint) Default Empty
            expectResponseCode @IO HTTP.status404 ra
            expectErrorMessage (errMsg404CannotFindTx txid) ra

    postTx
        :: Context t
        -> (wal, wal -> (Method, Text), Text)
        -> ApiWallet
        -> Natural
        -> IO (HTTP.Status, Either RequestException (ApiTransaction n))
    postTx ctx (wSrc, postTxEndp, pass) wDest amt = do
        addrs <- listAddresses @n ctx wDest
        let destination = (addrs !! 1) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": #{pass}
            }|]
        r <- request @(ApiTransaction n) ctx (postTxEndp wSrc) Default payload
        expectResponseCode HTTP.status202 r
        return r

    mkTxPayload
        :: Context t
        -> ApiWallet
        -> Natural
        -> Text
        -> IO Payload
    mkTxPayload ctx wDest amt passphrase = do
        addrs <- listAddresses @n ctx wDest
        let destination = (addrs !! 1) ^. #id
        return $ Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": #{passphrase}
            }|]

    addTxMetadata :: Aeson.Value -> Payload -> Payload
    addTxMetadata md (Json (Aeson.Object o)) =
        Json (Aeson.Object (o <> ("metadata" .= md)))
    addTxMetadata _ _ = error "can't do that"

    unsafeGetTransactionTime
        :: [ApiTransaction n]
        -> UTCTime
    unsafeGetTransactionTime txs =
        case fmap time . insertedAt <$> txs of
            (Just t):_ -> t
            _ -> error "Expected at least one transaction with a time."

    plusDelta, minusDelta :: UTCTime -> UTCTime
    plusDelta = addUTCTime (toEnum 1000000000)
    minusDelta = addUTCTime (toEnum (-1000000000))

    oneAda :: Natural
    oneAda = 1_000_000

    oneThousandAda :: Natural
    oneThousandAda = 1_000 * oneAda

    oneMillionAda :: Natural
    oneMillionAda = 1_000 * oneThousandAda
