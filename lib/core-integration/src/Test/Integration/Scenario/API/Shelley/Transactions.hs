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
    ( AddressAmount (..)
    , ApiAddress
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
    , insertedAt
    , pendingSince
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( PaymentAddress )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.Types
    ( Direction (..)
    , SortOrder (..)
    , TxMetadata (..)
    , TxMetadataValue (..)
    , TxStatus (..)
    , WalletId
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Control.Concurrent
    ( threadDelay )
import Control.Monad
    ( forM_ )
import Control.Monad.Catch
    ( MonadCatch )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Trans.Resource
    ( ResourceT, runResourceT )
import Data.Aeson
    ( (.=) )
import Data.ByteArray.Encoding
    ( Base (Base16, Base64), convertFromBase, convertToBase )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58 )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Generics.Product.Typed
    ( HasType )
import Data.Maybe
    ( fromJust, fromMaybe, isJust )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), ToText (..) )
import Data.Time.Clock
    ( NominalDiffTime, UTCTime, addUTCTime )
import Data.Time.Utils
    ( utcTimePred, utcTimeSucc )
import Data.Word
    ( Word32 )
import Network.HTTP.Types.Method
    ( Method )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith, describe, pendingWith )
import Test.Hspec.Expectations.Lifted
    ( expectationFailure, shouldBe, shouldNotBe, shouldSatisfy )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , Payload (..)
    , between
    , counterexample
    , defaultTxTTL
    , emptyByronWalletWith
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
    , getTTLSlots
    , json
    , listAddresses
    , listAllTransactions
    , listTransactions
    , minUTxOValue
    , oneSecond
    , postWallet
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
    , errMsg403Fee
    , errMsg403InputsDepleted
    , errMsg403NoPendingAnymore
    , errMsg403NotAShelleyWallet
    , errMsg403NotEnoughMoney
    , errMsg403NotEnoughMoney_
    , errMsg403TxTooLarge
    , errMsg403WithdrawalNotWorth
    , errMsg403WrongPass
    , errMsg404CannotFindTx
    , errMsg404MinUTxOValue
    , errMsg404NoWallet
    )
import Web.HttpApiData
    ( ToHttpApiData (..) )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.UTxO.Transaction as CardanoTransactions
import qualified Data.UTxO.Transaction.Cardano.Shelley as CardanoTransactions
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
    it "TRANS_MIN_UTXO_01 - I cannot spend less than minUTxOValue" $ \ctx -> runResourceT $ do
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
        \ while both, pending and in_ledger" $ \ctx -> runResourceT $ do
        wSrc <- fixtureWallet ctx

        payload <- liftIO $ mkTxPayload ctx wSrc minUTxOValue fixturePassphrase

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
        \ Pending tx should have pendingSince in the list tx response" $ \ctx -> runResourceT $ do
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

    it "TRANS_CREATE_01 - Single Output Transaction" $ \ctx -> runResourceT $ do
        (wa, wb) <- (,) <$> fixtureWallet ctx <*> fixtureWallet ctx
        let amt = (minUTxOValue :: Natural)

        payload <- liftIO $ mkTxPayload ctx wb amt fixturePassphrase

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

    it "TRANS_CREATE_02 - Multiple Output Tx to single wallet" $ \ctx -> runResourceT $ do
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

    it "TRANS_CREATE_03 - 0 balance after transaction" $ \ctx -> runResourceT $ do
        let amt = minUTxOValue

        wDest <- fixtureWalletWith @n ctx [amt]
        payload <- liftIO $ mkTxPayload ctx wDest amt fixturePassphrase

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
                        (`shouldBe` Quantity (2*amt))
                , expectField
                        (#balance . #getApiT . #total)
                        (`shouldBe` Quantity (2*amt))
                ]

        ra2 <- request @ApiWallet ctx (Link.getWallet @'Shelley wSrc) Default Empty
        verify ra2
            [ expectField (#balance . #getApiT . #total) (`shouldBe` Quantity 0)
            , expectField (#balance . #getApiT . #available) (`shouldBe` Quantity 0)
            ]

    it "TRANS_CREATE_04 - Can't cover fee" $ \ctx -> runResourceT $ do
        wDest <- fixtureWallet ctx

        payload <- liftIO $ mkTxPayload ctx wDest minUTxOValue fixturePassphrase
        (_, ApiFee (Quantity feeMin) _) <- unsafeRequest ctx
            (Link.getTransactionFee @'Shelley wDest) payload

        wSrc <- fixtureWalletWith @n ctx [minUTxOValue + (feeMin `div` 2)]

        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403Fee
            ]

    it "TRANS_CREATE_04 - Not enough money" $ \ctx -> runResourceT $ do
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

    it "TRANS_CREATE_04 - Wrong password" $ \ctx -> runResourceT $ do
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

    it "TRANS_CREATE_07 - Deleted wallet" $ \ctx -> runResourceT $ do
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
        expectResponseCode HTTP.status404 r
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

        forM_ matrix $ \(name, nonJson) -> it name $ \ctx -> runResourceT $ do
            w <- emptyWallet ctx
            let payload = nonJson
            r <- request @(ApiTransaction n) ctx
                (Link.createTransaction @'Shelley w) Default payload
            expectResponseCode HTTP.status400 r

    describe "TRANS_CREATE_09 - Single Output Transaction with non-Shelley witnesses" $
        forM_ [(fixtureRandomWallet, "Byron wallet"), (fixtureIcarusWallet, "Icarus wallet")] $
        \(srcFixture,name) -> it name $ \ctx -> runResourceT $ do

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

    let absSlotB = view (#absoluteSlotNumber . #getApiT)
    let absSlotS = view (#absoluteSlotNumber . #getApiT)
    let slotDiff a b = if a > b then a - b else b - a

    it "TRANS_TTL_01 - Pending transaction expiry" $ \ctx -> runResourceT $ do
        (wa, wb) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
        let amt = minUTxOValue :: Natural

        payload <- mkTxPayload ctx wb amt fixturePassphrase

        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wa) Default payload

        verify r
            [ expectSuccess
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField #expiresAt (`shouldSatisfy` isJust)
            ]

        -- This stuff would be easier with Control.Lens...

        -- Get insertion slot and out of response.
        let (_, Right apiTx) = r
        let Just sl = absSlotB <$> apiTx ^. #pendingSince

        -- The expected expiry slot (adds the hardcoded default ttl)
        ttl <- liftIO $ getTTLSlots ctx defaultTxTTL
        let txExpectedExp = sl + ttl

        -- The actual expiry slot
        let Just txActualExp = absSlotS <$> apiTx ^. #expiresAt

        -- Expected and actual are fairly close
        (slotDiff txExpectedExp txActualExp `shouldSatisfy` (< 50))
            & counterexample ("expected expiry: " <> show txExpectedExp)
            & counterexample ("actual expiry: " <> show txActualExp)

    it "TRANS_TTL_02 - Custom transaction expiry" $ \ctx -> runResourceT $ do
        (wa, wb) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
        let amt = minUTxOValue :: Natural
        let testTTL = 42 :: NominalDiffTime

        basePayload <- mkTxPayload ctx wb amt fixturePassphrase
        let payload = addTxTTL (realToFrac testTTL) basePayload

        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wa) Default payload

        verify r
            [ expectSuccess
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField #expiresAt (`shouldSatisfy` isJust)
            ]

        -- Get insertion slot and out of response.
        let (_, Right apiTx) = r
        let Just sl = absSlotB <$> apiTx ^. #pendingSince

        -- The expected expiry slot (adds the hardcoded default ttl)
        ttl <- liftIO $ getTTLSlots ctx testTTL
        let txExpectedExp = sl + ttl

        -- The actual expiry slot
        let Just txActualExp = absSlotS <$> apiTx ^. #expiresAt

        -- Expected and actual are fairly close. Any difference should only be
        -- due to slot rounding.
        (slotDiff txExpectedExp txActualExp `shouldSatisfy` (< 50))
            & counterexample ("expected expiry: " <> show txExpectedExp)
            & counterexample ("actual expiry: " <> show txActualExp)

    it "TRANS_TTL_03 - Expired transactions" $ \ctx -> runResourceT $ do
        liftIO $ pendingWith "#1840 this is flaky -- need a better approach"

        (wa, wb) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
        let amt = minUTxOValue :: Natural

        basePayload <- mkTxPayload ctx wb amt fixturePassphrase
        -- Set a transaction TTL that is going to expire really soon.
        --
        -- The TTL wants to be small enough that it expires before it can get
        -- into a block, but large enough that the node allows it into its
        -- mempool.
        --
        -- This is probably impossible to do reliably, so this test is pending.
        --
        -- Perhaps we could disconnect the test cluster relay node from its
        -- peers temporarily while letting the tx expire.
        let payload = addTxTTL 0.1 basePayload

        ra <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wa) Default payload

        verify ra
            [ expectSuccess
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField #expiresAt (`shouldSatisfy` isJust)
            ]

        let txid = getFromResponse #id ra
        let linkSrc = Link.getTransaction @'Shelley wa (ApiTxId txid)

        rb <- eventually "transaction is no longer pending" $ do
            rr <- request @(ApiTransaction n) ctx linkSrc Default Empty
            verify rr
                [ expectSuccess
                , expectField (#status . #getApiT) (`shouldNotBe` Pending)
                ]
            pure rr

        verify rb
            [ expectField (#status . #getApiT) (`shouldBe` Expired)
            , expectField #expiresAt (`shouldSatisfy` isJust)
            ]

    it "TRANS_TTL_04 - Large TTL" $ \ctx -> runResourceT $ do
        (wa, wb) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
        let amt = minUTxOValue :: Natural
        let hugeTTL = 1e9 :: NominalDiffTime

        basePayload <- mkTxPayload ctx wb amt fixturePassphrase
        let payload = addTxTTL (realToFrac hugeTTL) basePayload

        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wa) Default payload

        -- If another HFC Era is added, then this payment request will fail
        -- because the expiry would be past the slotting horizon.
        verify r
            [ expectSuccess
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField #expiresAt (`shouldSatisfy` isJust)
            ]

    it "TRANSMETA_CREATE_01 - Transaction with metadata" $ \ctx -> runResourceT $ do
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

    it "TRANSMETA_CREATE_02 - Transaction with invalid metadata" $ \ctx -> runResourceT $ do
        (wa, wb) <- (,) <$> fixtureWallet ctx <*> fixtureWallet ctx
        let amt = (minUTxOValue :: Natural)

        basePayload <- mkTxPayload ctx wb amt fixturePassphrase

        let txMeta = [json|{ "1": { "string": #{T.replicate 65 "a"} } }|]
        let payload = addTxMetadata txMeta basePayload

        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wa) Default payload

        expectResponseCode HTTP.status400 r
        expectErrorMessage errMsg400TxMetadataStringTooLong r

    it "TRANSMETA_CREATE_03 - Transaction with too much metadata" $ \ctx -> runResourceT $ do
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

        expectResponseCode HTTP.status403 r
        expectErrorMessage errMsg403TxTooLarge r

    it "TRANSMETA_ESTIMATE_01 - fee estimation includes metadata" $ \ctx -> runResourceT $ do
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

    it "TRANSMETA_ESTIMATE_02 - fee estimation with invalid metadata" $ \ctx -> runResourceT $ do
        (wa, wb) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
        let amt = (minUTxOValue :: Natural)

        basePayload <- mkTxPayload ctx wb amt fixturePassphrase

        let txMeta = [json|{ "1": { "string": #{T.replicate 65 "a"} } }|]
        let payload = addTxMetadata txMeta basePayload

        r <- request @ApiFee ctx
            (Link.getTransactionFee @'Shelley wa) Default payload

        expectResponseCode HTTP.status400 r
        expectErrorMessage errMsg400TxMetadataStringTooLong r

    it "TRANSMETA_ESTIMATE_03 - fee estimation with too much metadata" $ \ctx -> runResourceT $ do
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
        r <- request @ApiFee ctx
            (Link.getTransactionFee @'Shelley wa) Default payload

        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403TxTooLarge
            ]

    it "TRANS_EXTERNAL_01 - Single Output Transaction - Shelley witnesses" $ \ctx -> runResourceT $ do
        wFaucet <- fixtureWallet ctx
        let amtSrc = (10_000_000 :: Natural)

        let mnemonicsSrc =
                [ "nothing", "heart", "matrix", "fly", "sleep", "slogan", "tomato"
                , "pulse", "what", "roof", "rail", "since", "plastic", "false", "enlist"
                ] :: [Text]
        let walletPostData = Json [json| {
                "name": "empty wallet",
                "mnemonic_sentence": #{mnemonicsSrc},
                "passphrase": #{fixturePassphrase}
                } |]
        r1 <- postWallet ctx walletPostData
        verify r1
            [ expectResponseCode HTTP.status201
            , expectField
                (#balance . #getApiT . #available) (`shouldBe` Quantity 0)
            ]
        let wSrc = getFromResponse Prelude.id r1

        -- 1. recovery-phrase-src.prv
        -- --> nothing heart matrix fly sleep slogan tomato pulse what roof rail since plastic false enlist
        -- 2. corresponding root key:
        --- $ cat recovery-phrase-src.prv | cardano-address key from-recovery-phrase Shelley > root-src.prv
        -- --> xprv1apjwjs3ksgm5mnnk0cc5v5emgv0hmafmmy8tffay5s2ffk69830whwznr46672ruucdzwwtv9upv72e4ylrypyz5m6cyh0p00t7n3u3agt20lv32j4kxcqlkzu78nzjx0ysxxlc2ghfz9prxfmrds802xsh67k7t
        -- 3. staking private key:
        --- $ cat root-src.prv | cardano-address key child 1852H/1815H/0H/2/0 > stake-src.prv
        -- --> xprv1vqxfvfacglus6perg957zahktdch6lg6w4fhpwptvsgmr669830d08h0kk6lyfhlq34r2982jfypmkyc57uguztx7mvguvn8wuc8zjj7etm3ej2h5d52k6ssld7cchcmehgx45h5km3mukfyc5h04urr9qpzq79h
        -- 4. delegation address to which we send 10 ada (index 1 and for network tag 1):
        --- $ cat root-src.prv \
        --- | cardano-address key child 1852H/1815H/0H/0/1 \
        --- | cardano-address key public \
        --- | cardano-address address payment --network-tag 1 \
        --- | cardano-address address delegation $(cat stake-src.prv | cardano-address key public)
        --- --> addr1qx2l6wzfqtswyeqe2auxtj5742r5kcl23d66q2ukpgv0j6tv465tn73z5d4m4t3fptal7lnnqsdngmu93e68vxavl8zqef9maq
        payload1 <- mkTxPayload ctx wSrc amtSrc fixturePassphrase

        r2 <- request @ApiFee ctx
               (Link.getTransactionFee @'Shelley wFaucet) Default payload1
        let (Quantity feeMin) = getFromResponse #estimatedMin r2
        let (Quantity feeMax) = getFromResponse #estimatedMax r2

        -- Workaround for flaky #2287. Seems we cannot count on the fee
        -- estimation.
        let wFaucetBalRange = ( Quantity (faucetAmt - 2*feeMax - amtSrc)
                              , Quantity (faucetAmt - 0*feeMin - amtSrc)
                              )

        r3 <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wFaucet) Default payload1
        expectResponseCode HTTP.status202 r3

        let (Hash txid) = getApiT $ getFromResponse #id r3
        let txix = case getFromResponse #outputs r3 of
                [(AddressAmount _ (Quantity out1)), (AddressAmount _ (Quantity out2))]
                    | out1 == amtSrc -> 0
                    | out2 == amtSrc -> 1
                    | otherwise -> error "this should not happen"
                _ -> error "this should not happen"

        eventually "wFaucet and wSrc balances are as expected" $ do
            r' <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wSrc) Default Empty
            expectField
                (#balance . #getApiT . #available)
                (`shouldBe` Quantity amtSrc) r'

            r'' <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wFaucet) Default Empty
            expectField
                (#balance . #getApiT . #available)
                (between wFaucetBalRange) r''

        -- #2238 quick fix to reduce likelihood of rollback.
        liftIO $ threadDelay $ 10 * oneSecond

        let amtDest = (2_000_000 :: Natural)

        let mnemonicsDest =
                [ "lens", "cable", "debate", "child", "gaze", "air", "gadget"
                , "vocal", "urge", "agree", "ivory", "milk", "midnight", "equal"
                , "meadow", "odor", "fancy", "rain"
                ] :: [Text]
        let walletPostData1 = Json [json| {
                "name": "empty wallet",
                "mnemonic_sentence": #{mnemonicsDest},
                "passphrase": #{fixturePassphrase}
                } |]
        r4 <- postWallet ctx walletPostData1
        verify r4
            [ expectSuccess
            , expectResponseCode HTTP.status201
            , expectField
                (#balance . #getApiT . #available) (`shouldBe` Quantity 0)
            ]
        let wDest = getFromResponse Prelude.id r4

        -- The construction of external tx
        -- (a) fee which was estimated using cardano-wallet endpoint
        payload2 <- mkTxPayload ctx wDest amtDest fixturePassphrase
        r5 <- request @ApiFee ctx
               (Link.getTransactionFee @'Shelley wSrc) Default payload2
        let (Quantity feeMin1) = getFromResponse #estimatedMin r5

        -- (b) the change address of wSrc
        --- $ cat root-src.prv \
        ---  > | cardano-address key child 1852H/1815H/0H/1/0 \
        ---  > | cardano-address key public \
        ---  > | cardano-address address payment --network-tag 1 \
        ---  > | cardano-address address delegation $(cat stake-src.prv | cardano-address key public)
        let addrChange =
               "addr1qyfcdmnncvnrnax8jlly3t25uwhp3tfhn658wq03xmmcz0tv465tn73z5d\
               \4m4t3fptal7lnnqsdngmu93e68vxavl8zq8v804u"
        let outChange = amtSrc - feeMin1 - amtDest
        -- (c) the output of wDest wallet, it is expected to receive amtDest
        -- 1. recovery-phrase-dest.prv
        -- --> lens cable debate child gaze air gadget vocal urge agree ivory milk midnight equal meadow odor fancy rain
        -- 2. corresponding root key:
        --- $ cat recovery-phrase-dest.prv | cardano-address key from-recovery-phrase Shelley > root-dest.prv
        -- --> xprv18rm0qqpnhjff0zlxfxvewxlqecj2p9kumjnj8v3srwje5al5udwkeq2w09nemnc76s2h42ynv7k44epe6n2e5p5wz3pu0lcxy338gcv2nvvlaszcpqa5al94ummslwv7muzmxnxml4l5xzfx3vgff297nqssus24
        -- 3. staking private key:
        --- $ cat root-dest.prv | cardano-address key child 1852H/1815H/0H/2/0 > stake-dest.prv
        -- --> xprv1lzgm04cw604c48lws825squexqy6800ldhm627p3vgelfz05udwhwrm8cs6ky86mw94jzvf5k5qz0l3ll0cxdwrfyks70t69nfqymm85q6e9dx4d7mdn0ywj80cmgd7hn5p8f4cxtvl8x5j69cpsuja02yskqld4
        -- 4. delegation address (index 1 and for network tag 1):
        --- $ cat root-dest.prv \
        --- | cardano-address key child 1852H/1815H/0H/0/1 \
        --- | cardano-address key public \
        --- | cardano-address address payment --network-tag 1 \
        --- | cardano-address address delegation $(cat stake-dest.prv | cardano-address key public)
        let addrDest =
                "addr1q9msdjwd4g76graphf58aq9jzs5m554ehu56rtgultej6luvqzvd0c9np\
                \7kn7nwjde979jfhjvxlhcrqtee8g6cc49gsn08662"

        -- Produce corresponding private key (witness) for input address of wSrc
        -- selected in the faucet->src wallet transaction (see above):
        --- cat root-src.prv | cardano-address key child 1852H/1815H/0H/0/1 --base16
        let wit =
                "a8440eef40cf0923d831ea2832095afb81c3c3961d6091b6d3874eedec453c\
                \5ea05908aa8997a7518fb14d6c4e11127323c5e08b88f215284243aa7591f4\
                \df956f7f9864ef1a6c79727346fc7913f78bd8c1e0ae3d25f06334fd68dec4\
                \a34aac"
        let (Right blob) = constructTxFromCardanoTransactions
                (fromIntegral feeMin1) txid txix outChange addrChange amtDest addrDest wit
        let baseOk = Base64
        let encodedSignedTx = T.decodeUtf8 $ convertToBase baseOk blob
        let payloadExt = NonJson . BL.fromStrict . toRawBytes baseOk
        let headers = Headers [ ("Content-Type", "application/octet-stream") ]
        r6 <- request
            @ApiTxId ctx Link.postExternalTransaction headers (payloadExt encodedSignedTx)
        expectResponseCode HTTP.status202 r6

        eventually "wDest and wSrc balances are as expected" $ do
            r' <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            expectField
                (#balance . #getApiT . #available)
                (`shouldBe` Quantity amtDest) r'

            r'' <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wSrc) Default Empty
            expectField
                (#balance . #getApiT . #available)
                (`shouldBe` Quantity outChange) r''

    it "TRANS_EXTERNAL_02 - Multiple Outputs Transaction - Shelley witnesses" $ \ctx -> runResourceT $ do
        wFaucet <- fixtureWallet ctx
        let amt1 = (4_000_000 :: Natural)
        let amt2 = (6_000_000 :: Natural)
        let amtSrc = amt1 + amt2

        let mnemonicsSrc =
                [ "swim", "surprise", "bar", "spike", "thrive", "uncle", "narrow"
                , "quote", "now", "hundred", "target", "common", "silly", "chunk"
                , "lake", "tornado", "hunt", "captain"
                ] :: [Text]
        let walletPostData = Json [json| {
                "name": "empty shelley wallet",
                "mnemonic_sentence": #{mnemonicsSrc},
                "passphrase": #{fixturePassphrase}
                } |]
        r1 <- postWallet ctx walletPostData
        verify r1
            [ expectResponseCode HTTP.status201
            , expectField
                (#balance . #getApiT . #available) (`shouldBe` Quantity 0)
            ]
        let wSrc = getFromResponse Prelude.id r1

        -- 1. wSrc: recovery-phrase-src.prv
        -- --> swim surprise bar spike thrive uncle narrow quote now hundred target common silly chunk lake tornado hunt captain
        -- 2. corresponding root key:
        --- $ cat recovery-phrase-src.prv | cardano-address key from-recovery-phrase Shelley > root-src.prv
        -- --> xprv15rluw4e9jq3khcpdn0k8lj67l5r72am3uxhkmvmzfeqhmalx6ad393jg2l40lk2y248ulk4ptcc3fdu8cvzlvq55kdm7ageklvqjuv8yjl2urt5j7yvn55602vhe3clfhf2tszeljygu70zqgdt068hgevwxdvth
        -- 3. staking private key:
        --- $ cat root-src.prv | cardano-address key child 1852H/1815H/0H/2/0 > stake-src.prv
        -- --> xprv1wpq5lv869qwyx2ndhfst04mmuj958zn99vd7qqcl8t67yzh86adesvkd065sh64xwg03w3n63mp20037mtl6a0jk92s0rzdvz2g5nduyycs2u286pqq6pe5485h8nc8acyg5ecutfzyjn045plprtpf4y5veh36j
        -- 4. delegation address to which we send 3 ada (index 1 and for network tag 1):
        --- $ cat root-src.prv \
        --- | cardano-address key child 1852H/1815H/0H/0/1 \
        --- | cardano-address key public \
        --- | cardano-address address payment --network-tag 1 \
        --- | cardano-address address delegation $(cat stake-src.prv | cardano-address key public)
        --- --> addr1q9gz079rdhel79rqrad7rwfmgy4yp29x7s5h8cej87p4w0ctp3s99reas2y8mmf2zz27q557mdkjlux8k8kzgrj526mqr6g7ny
        -- 5. delegation address to which we send 7 ada (index 2 and for network tag 1):
        --- $ cat root-src.prv \
        --- | cardano-address key child 1852H/1815H/0H/0/2 \
        --- | cardano-address key public \
        --- | cardano-address address payment --network-tag 1 \
        --- | cardano-address address delegation $(cat stake-src.prv | cardano-address key public)
        --- --> addr1q895m0p42rwsenhedkjnvnhmvp67p52yrjjc9xn799w0ksctp3s99reas2y8mmf2zz27q557mdkjlux8k8kzgrj526mqyca3zy
        payload1 <- liftIO $ mkMultipleTxPayload ctx wSrc amt1 amt2 fixturePassphrase
        r2 <- request @ApiFee ctx
               (Link.getTransactionFee @'Shelley wFaucet) Default payload1
        let (Quantity feeMin) = getFromResponse #estimatedMin r2
        let (Quantity feeMax) = getFromResponse #estimatedMax r2

        -- Workaround for flaky #2287. Seems we cannot count on the fee
        -- estimation.
        let wFaucetBalRange = ( Quantity (faucetAmt - 2*feeMax - amtSrc)
                              , Quantity (faucetAmt - 0*feeMin - amtSrc)
                              )

        r3 <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wFaucet) Default payload1
        expectResponseCode HTTP.status202 r3

        let (Hash txid) = getApiT $ getFromResponse #id r3
        let (txix1, txix2) = case getFromResponse #outputs r3 of
                [ (AddressAmount _ (Quantity out1)), (AddressAmount _ (Quantity out2)), (AddressAmount _ (Quantity out3)), (AddressAmount _ (Quantity out4))] ->
                    let pairs = [(out1, 0), (out2, 1), (out3, 2), (out4, 3)]
                        (Just ix1) = L.lookup amt1 pairs
                        (Just ix2) = L.lookup amt2 pairs
                    in (ix1, ix2)
                _ -> error "this should not happen"

        eventually "wFaucet and wSrc balances are as expected" $ do
            r' <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wSrc) Default Empty
            expectField
                (#balance . #getApiT . #available)
                (`shouldBe` Quantity amtSrc) r'

            r'' <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wFaucet) Default Empty
            txs <- listTransactions @n ctx wFaucet Nothing Nothing Nothing
            expectField
                (#balance . #getApiT . #available)
                (between wFaucetBalRange) r''
                & counterexample ("fee response: " <> show r2)
                & counterexample ("faucet txs: " <> show txs)

        -- #2238 quick fix to reduce likelihood of rollback.
        liftIO $ threadDelay $ 10 * oneSecond

        let amtDest = (7_000_000 :: Natural)

        let mnemonicsDest =
                [ "pelican", "file", "space", "basket", "fetch", "trade", "dragon"
                , "turn", "spawn", "uncle", "chapter", "jacket", "draft", "speak"
                , "lobster", "tumble", "skin", "parent"
                ] :: [Text]
        let walletPostData1 = Json [json| {
                "name": "empty wallet",
                "mnemonic_sentence": #{mnemonicsDest},
                "passphrase": #{fixturePassphrase}
                } |]
        r4 <- postWallet ctx walletPostData1
        verify r4
            [ expectResponseCode HTTP.status201
            , expectField
                (#balance . #getApiT . #available) (`shouldBe` Quantity 0)
            ]
        let wDest = getFromResponse Prelude.id r4

        -- The construction of external tx
        -- (a) fee which was estimated using cardano-wallet endpoint
        payload2 <- mkTxPayload ctx wDest amtDest fixturePassphrase
        r6 <- request @ApiFee ctx
               (Link.getTransactionFee @'Shelley wSrc) Default payload2
        let (Quantity feeMin1) = getFromResponse #estimatedMin r6

        -- (b) the change adddress of wSrc
        --- $ cat root-src.prv \
        ---  > | cardano-address key child 1852H/1815H/0H/1/0 \
        ---  > | cardano-address key public \
        ---  > | cardano-address address payment --network-tag 1 \
        ---  > | cardano-address address delegation $(cat stake-src.prv | cardano-address key public)
        let addrChange =
               "addr1q8fq675q3srjsfnlzgljy4d456tnegfw2hdqtnxfdd0yl8stp3s99reas2\
               \y8mmf2zz27q557mdkjlux8k8kzgrj526mqnqphyw"
        let outChange = amtSrc - feeMin1 - amtDest
        -- (c) the output of wDest wallet, it is expected to receive amt1
        -- 1. recovery-phrase-dest.prv
        -- --> pelican file space basket fetch trade dragon turn spawn uncle chapter jacket draft speak lobster tumble skin parent
        -- 2. corresponding root key:
        --- $ cat recovery-phrase-dest.prv | cardano-address key from-recovery-phrase Shelley > root-dest.prv
        -- --> xprv1cp5f5tulqkvqtawrh52ve2x7ukh2476zyfuwsqxgygdfxvrky42eplmltxsluhw7k0tcmmjy5nve7nu5uudf9c8xs994s9fladl5zvccls2g47x5kuvdjf9nsj2mqauxykg2j7yf5apw7p2uvzs00nmag53kn2ut
        -- 3. staking private key:
        --- $ cat root-dest.prv | cardano-address key child 1852H/1815H/0H/2/0 > stake-dest.prv
        -- --> xprv1dz3xdkyeptmhln76r2cpcluysdv3np3lfc5tn7xdq6r0ksrky42jm0hw438znprchxrc0303n9q2868ugp34j2q79jjped68yfdhz96phz3tm0zjgs4302x79tsm7yydhezxq6at0xsh4cjm4w5tx5lr0um0wvv3
        -- 4. delegation address (index 1 and for network tag 1):
        --- $ cat root-dest.prv \
        --- | cardano-address key child 1852H/1815H/0H/0/1 \
        --- | cardano-address key public \
        --- | cardano-address address payment --network-tag 1 \
        --- | cardano-address address delegation $(cat stake-dest.prv | cardano-address key public)
        let addrOut =
                "addr1qyz0222jj95arfm6hrvc60ltg4pzs47ljraju0mthzmp6j4u6mln6zdy7\
                \nzumpfez8jqpk2zam2y7ztdlg93gtfr2v8stngyhe"

        -- Produce corresponding private keys (witnesses) for input addresses
        -- selected in the faucet->src wallet transaction (see above):
        --- $ cat root-src.prv | cardano-address key child 1852H/1815H/0H/0/1 --base16
        let wit1 =
                "98aada1fddda4b3749199d885912a69a906351ea3644a995acdc4cd40de7d7\
                \5ba26191018737d043b751e5f612bccc1629a9b0885218fa0b2118f3ec7f62\
                \7c625ecf301989b44a0fe45e27c02b71d525f544ac6d785719eb19b421c7b3\
                \adbddd"

        --- $ cat root-src.prv | cardano-address key child 1852H/1815H/0H/0/2 --base16
        let wit2 =
                "b0c9ca18a1466939c0ae51485ee5c9646d542e37f779b490146fbeba0fe7d7\
                \5b7789004c30eab9cad37c45901359a7fce6bfa950721d9b17db76f4b1cb2e\
                \0ecca9e73da0af9110cd1c95e07794ebca0aa424bd8cb7250d3c46833cefa8\
                \739970"

        let (Right blob) = constructMultiTxFromCardanoTransactions
                (fromIntegral feeMin1) txid (txix1, txix2) outChange addrChange amtDest addrOut wit1 wit2
        let baseOk = Base64
        let encodedSignedTx = T.decodeUtf8 $ convertToBase baseOk blob
        let payloadExt = NonJson . BL.fromStrict . toRawBytes baseOk
        let headers = Headers [ ("Content-Type", "application/octet-stream") ]
        r7 <- request
            @ApiTxId ctx Link.postExternalTransaction headers (payloadExt encodedSignedTx)
        expectResponseCode HTTP.status202 r7

        eventually "wDest and wSrc balances are as expected" $ do
            r' <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            expectField
                (#balance . #getApiT . #available)
                (`shouldBe` Quantity amtDest) r'

            r'' <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wSrc) Default Empty
            expectField
                (#balance . #getApiT . #available)
                (`shouldBe` Quantity outChange) r''

    describe "TRANS_EXTERNAL_03 - Single Output Transaction with Byron witness" $ do
        it "Byron wallet" $ \ctx -> runResourceT $ do

            wFaucet <- fixtureRandomWallet ctx

            let byronMnemonics =
                   [ "ghost", "casino", "minor", "vast", "filter", "flip"
                   , "polar", "alarm", "purchase", "curtain", "dry", "wisdom"]
            wByron <- emptyByronWalletWith ctx "random"
                      ("Random Wallet", byronMnemonics, fixturePassphrase)

            -- 1. wByron: recovery-phrase-src.prv
            --- --> ghost casino minor vast filter flip polar alarm purchase curtain dry wisdom
            -- 2. prv root key:
            --- $ cat recovery-phrase-src.prv | cardano-address key from-recovery-phrase Byron > root-src.prv
            --- --> xprv1wqk3jtxymg2mjst56sede5a53n8wlpnjkmptywyhyxxycrv2letemsd6ulamrexmgq4427g2wl80exune2c035fa08hen44at825u9nrcyl27za26waa2vgztmvl39esu6ucsa9m8r742czjnqhgyzcd8cea3fw5
            -- 3. cat root-src.prv | cardano-address key public > root-src.pub
            --- --> xpub1alff7qkyshud2kstclddrnf582zags6hfc4wl5qpxn7gx6r65kuk8sf74u9645am65csyhkelztnpe4e3p6tkw8a24s99xpwsg9s60s36nlzc
            -- 4. create child address (for 2147483662-2^31=2147483662-2147483648=14)
            --- $ cat root-src.prv | cardano-address key child --legacy 0H/14H --base16
            let wit =
                    "c323bec83ccf2e39ee42d499acd5ee7ade10e822270b4479f8e98c8c8e8abf\
                    \0842dc7de2c0e69e52982743c73a75ea06ef87c80b94d380c83a9a99060488\
                    \438f06c552df3d16013201a44025f79b7eae1cd697b3fb173f31110b97935c\
                    \6ba600"

            -- 5. produce address
            --- $ cat root-src.prv | cardano-address key child --legacy 0H/14H \
            --- | cardano-address key public | cardano-address address bootstrap \
            --- | xpub1alff7qkyshud2kstclddrnf582zags6hfc4wl5qpxn7gx6r65kuk8sf74u9645am65csyhkelztnpe4e3p6tkw8a24s99xpwsg9s60s36nlzc \
            --- --path 0H/14H --network-tag 764824073
            let addrInp =
                    "DdzFFzCqrhsfRTAFKYtEMjB1vy5fTth3QEitVbMBuk5r9Um6Uf2bWYj8cSYfba\
                    \d9MLKokM2Y5FhybrJqCgUDVPNPkgG6oa33VLQ6jugc"

            -- 6. change address
            --- $ cat root-src.prv | cardano-address key child --legacy 0H/15H \
            --- | cardano-address key public | cardano-address address bootstrap \
            --- | xpub1alff7qkyshud2kstclddrnf582zags6hfc4wl5qpxn7gx6r65kuk8sf74u9645am65csyhkelztnpe4e3p6tkw8a24s99xpwsg9s60s36nlzc \
            --- --path 0H/15H --network-tag 764824073
            let addrChange =
                    "DdzFFzCqrht3xS8ySv9t4MVM1euWvcC6WbzPZKkr6WqE7zcW8nSS9A3eGvcrMK\
                    \vANGF6auayauFQjazrQtC7T8hx9CvXKq4U3qS2ApAC"

            let payload1 = Json [json|
                    { "passphrase": #{fixturePassphrase}
                    , "address_index": 2147483662
                    }|]
            r1 <- request @(ApiAddress n) ctx (Link.postRandomAddress wByron) Default payload1
            expectResponseCode HTTP.status201 r1
            let destination = getFromResponse #id r1
            let amtSrc = (10_000_000 :: Natural)
            let payload2 = Json [json|{
                    "payments": [{
                        "address": #{destination},
                        "amount": {
                            "quantity": #{amtSrc},
                            "unit": "lovelace"
                        }
                    }],
                    "passphrase": #{fixturePassphrase}
                }|]
            r2 <- request @(ApiTransaction n) ctx
                (Link.createTransaction @'Byron wFaucet) Default payload2
            expectResponseCode HTTP.status202 r2

            let (Hash txid) = getApiT $ getFromResponse #id r2
            let txix = case getFromResponse #outputs r2 of
                    [(AddressAmount _ (Quantity out1)), (AddressAmount _ (Quantity out2))]
                        | out1 == amtSrc -> 0
                        | out2 == amtSrc -> 1
                        | otherwise -> error "this should not happen"
                    _ -> error "this should not happen"

            liftIO $ eventually "wByron received money" $ do
                r' <- request @ApiByronWallet ctx
                    (Link.getWallet @'Byron wByron) Default Empty
                expectField
                    (#balance . #available)
                    (`shouldBe` Quantity amtSrc) r'

            -- #2232 quick fix to reduce likelihood of rollback.
            liftIO $ threadDelay $ 10 * oneSecond

            let shelleyMnemonics =
                  [ "broken", "pass", "shrug", "pause", "crush"
                  , "caught", "honey", "lonely", "dose", "rabbit"
                  , "olympic", "honey", "hair", "panther", "stage"] :: [Text]
            -- 1. recovery-phrase-dest.prv
            -- --> broken pass shrug pause crush caught honey lonely dose rabbit olympic honey hair panther stage
            -- 2. corresponding root key:
            --- $ cat recovery-phrase-dest.prv | cardano-address key from-recovery-phrase Shelley > root-dest.prv
            -- --> xprv1cqnvuzmz796zgfmuuu8fq9rdufqsv0ar0sr9x0nee932snhw7f9w7qfyxz525c2f8d5uzyv38hru6c5ad8hz38eltrseu8s3ela226745sge3xtgsfjkhyzrz0dkjelz8hksgjtk73fzkrp4jgjns0569q9pz6js
            -- 3. staking private key:
            --- $ cat root-dest.prv | cardano-address key child 1852H/1815H/0H/2/0 > stake-dest.prv
            -- --> xprv1pzgx4cds2qu6qcstnqx8srnfn99xu6356nktr4npdpjwse0w7f9y77kec8vc7sl8zvqh43lzq6e9rgudgh2rz87aupyx0ncrwfsnzredmntfg0u2v9v3dar75vsw0eyxphmz70fsx99l8r4586weg77xyuu8t0sg
            -- 4. delegation address (index 1 and for network tag 1):
            --- $ cat root-dest.prv \
            --- | cardano-address key child 1852H/1815H/0H/0/1 \
            --- | cardano-address key public \
            --- | cardano-address address payment --network-tag 1 \
            --- | cardano-address address delegation $(cat stake-dest.prv | cardano-address key public)
            let addrOut =
                    "addr1q8zrclx9djykejekxdr2yec20c0vn9gl2clykc9ktsegm9k89d93kzpx8\
                    \gf9y35hfmgkx68avmaucem9lzg0lucpex8qxqqqy8"

            let walletPostData = Json [json| {
                    "name": "empty Shelley wallet",
                    "mnemonic_sentence": #{shelleyMnemonics},
                    "passphrase": #{fixturePassphrase}
                    } |]
            r3 <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default walletPostData
            expectResponseCode HTTP.status201 r3
            let wShelley = getFromResponse Prelude.id r3

            addrs <- listAddresses @n ctx wShelley
            let amtDest = (1_000_000 :: Natural)
            let destination1 = (addrs !! 1) ^. #id
            let payload3 = Json [json|{
                    "payments": [{
                        "address": #{destination1},
                        "amount": {
                            "quantity": #{amtDest},
                            "unit": "lovelace"
                        }
                    }]
                }|]

            rFeeEst <- request @ApiFee ctx
                (Link.getTransactionFee @'Byron wByron) Default payload3
            expectResponseCode HTTP.status202 rFeeEst
            let (Quantity feeEstMin) = getFromResponse #estimatedMin rFeeEst

            let outChange = amtSrc - feeEstMin - amtDest

            let (Right blob) = constructTxByronWitsFromCardanoTransactions
                    (fromIntegral feeEstMin) txid txix outChange addrChange amtDest addrOut addrInp wit
            let baseOk = Base64
            let encodedSignedTx = T.decodeUtf8 $ convertToBase baseOk blob
            let payloadExt = NonJson . BL.fromStrict . toRawBytes baseOk
            let headers = Headers [ ("Content-Type", "application/octet-stream") ]
            r4 <- request
                @ApiTxId ctx Link.postExternalTransaction headers (payloadExt encodedSignedTx)
            expectResponseCode HTTP.status202 r4

            liftIO $ eventually "wByron and wShelley balances are as expected" $ do
                r' <- request @ApiWallet ctx
                    (Link.getWallet @'Shelley wShelley) Default Empty
                expectField
                    (#balance . #getApiT . #available)
                    (`shouldBe` Quantity amtDest) r'

                r'' <- request @ApiByronWallet ctx
                    (Link.getWallet @'Byron wByron) Default Empty
                expectField
                    (#balance . #available)
                    (`shouldBe` Quantity outChange) r''

        it "Icarus wallet" $ \ctx -> runResourceT $ do
            -- Prepare src wIcarus wallet for external transaction
            wFaucet <- fixtureWallet ctx

            let byronMnemonics =
                   ["theme", "book", "settle", "across", "rhythm", "year"
                   , "riot", "primary", "day", "sudden", "nest", "develop"
                   , "purpose", "check", "stove"]
            wIcarus <- emptyByronWalletWith ctx "icarus"
                      ("Icarus Wallet", byronMnemonics, fixturePassphrase)

            -- 1. wIcarus: recovery-phrase-src.prv
            --- --> theme book settle across rhythm year riot primary day sudden nest develop purpose check stove
            -- 2. prv root key:
            --  $ cat recovery-phrase-src.prv \
            --  | cardano-address key from-recovery-phrase Icarus > root-src.prv
            --- --> xprv1wrl6ezlqxfhpwtg0dxwszhtkaew3k8vvnd5g0nrth7cjwjsy39xgn550wugnpl5pxyp22h6ncslnyhcnsjn9n6rzjvz9hjr4pl7azvztutj034ugd4cwu39gpn428etwrhha2pewqvjs9l3xrsgupz4u5gek8cr5
            -- 3. pub root key:
            --  $ cat root-src.prv \
            --  | cardano-address key public > root-src.pub
            --- --> xpub1hhdt9d94ea4hl2h7r4mszcpuxgwqqs5um6fm0zjl7qauf89j8m0yhchylrtcsmtsaez2sr8250jku80065rjuqe9qtlzv8q3cz9tegskgq8hf
            -- 4. create address
            --- $ cat root-src.prv | cardano-address key child 44H/1815H/0H/0/0 --base16
            let wit =
                    "981b4c73ed9b2d75a54271e8e166ac053fe9cfb49bf250ea288beadb54\
                    \04894cc2139a332c8b8141fb14acd3bfcfd4c0af55abfbf12b83a11f24\
                    \2a71d5a4a9a010723b6c2ab4b18feffe6931140d1d2ecfeb6a28ea6f0e\
                    \c9fe6a5e9ac4838fd0"

            let link = Link.listAddresses @'Byron wIcarus
            (_, addrs) <- unsafeRequest @[ApiAddress n] ctx link Empty

            -- 5. produce address
            --  $ cat root-src.prv \
            --  | cardano-address key child 44H/1815H/0H/0/0 \
            --  | cardano-address key public \
            --  | cardano-address address bootstrap --network-tag mainnet
            --  Ae2tdPwUPEZBruS2sSxNUsH3pL6K2PDFJhocE7PswF2pno7JUcirBNakCTA
            let addrInp = encodeAddress @n (getApiT $ fst $ addrs !! 0 ^. #id)

            -- 6. change address
            --  $ cat root-src.prv \
            --  | cardano-address key child 44H/1815H/0H/0/1 \
            --  | cardano-address key public \
            --  | cardano-address address bootstrap --network-tag mainnet
            -- Ae2tdPwUPEZHC3SC8Vbss8AXg7TNKBK1BfAbJRXDFfkgivH8RawguHBoebZ
            let addrChange = encodeAddress @n (getApiT $ fst $ addrs !! 1 ^. #id)

            -- send inital transaction to wIcarus src wallet from faucet wallet
            let amtSrc = (10_000_000 :: Natural)
            let payload = Json [json|{
                    "payments": [{
                        "address": #{addrInp},
                        "amount": {
                            "quantity": #{amtSrc},
                            "unit": "lovelace"
                        }
                    }],
                    "passphrase": #{fixturePassphrase}
                }|]
            r2 <- request @(ApiTransaction n) ctx
                (Link.createTransaction @'Shelley wFaucet) Default payload
            expectResponseCode HTTP.status202 r2

            let (Hash txid) = getApiT $ getFromResponse #id r2
            let txix = case getFromResponse #outputs r2 of
                    [(AddressAmount _ (Quantity out1)), (AddressAmount _ (Quantity out2))]
                        | out1 == amtSrc -> 0
                        | out2 == amtSrc -> 1
                        | otherwise -> error "this should not happen"
                    _ -> error "this should not happen"

            liftIO $ eventually "wIcarus received money" $ do
                r' <- request @ApiByronWallet ctx
                    (Link.getWallet @'Byron wIcarus) Default Empty
                expectField
                    (#balance . #available)
                    (`shouldBe` Quantity amtSrc) r'

            -- #2232 quick fix to reduce likelihood of rollback.
            liftIO $ threadDelay $ 10 * oneSecond

            -- Create Shelley destination wallet for external tx
            wShelley <- emptyWallet ctx

            -- Estimate fee for external transaction
            let amtDest = (1_000_000 :: Natural)
            addr <- listAddresses @n ctx wShelley
            let addrDest = encodeAddress @n (getApiT $ fst $ addr !! 1 ^. #id)
            let payload3 = Json [json|{
                    "payments": [{
                        "address": #{addrDest},
                        "amount": {
                            "quantity": #{amtDest},
                            "unit": "lovelace"
                        }
                    }]
                }|]

            rFeeEst <- request @ApiFee ctx
                (Link.getTransactionFee @'Byron wIcarus) Default payload3
            expectResponseCode HTTP.status202 rFeeEst
            let (Quantity feeEstMin) = getFromResponse #estimatedMin rFeeEst
            let outChange = amtSrc - feeEstMin - amtDest

            -- Construct tx blob and post external tx from icarus to shelley wallet
            let (Right blob) = constructTxByronWitsFromCardanoTransactions
                    (fromIntegral feeEstMin) txid txix outChange addrChange amtDest addrDest addrInp wit
            let baseOk = Base64
            let encodedSignedTx = T.decodeUtf8 $ convertToBase baseOk blob
            let payloadExt = NonJson . BL.fromStrict . toRawBytes baseOk
            let headers = Headers [ ("Content-Type", "application/octet-stream") ]
            r4 <- request
                @ApiTxId ctx Link.postExternalTransaction headers (payloadExt encodedSignedTx)
            expectResponseCode HTTP.status202 r4

            eventually "wIcarus and wShelley balances are as expected" $ do
                r' <- request @ApiWallet ctx
                    (Link.getWallet @'Shelley wShelley) Default Empty
                expectField
                    (#balance . #getApiT . #available)
                    (`shouldBe` Quantity amtDest) r'

                r'' <- request @ApiByronWallet ctx
                    (Link.getWallet @'Byron wIcarus) Default Empty
                expectField
                    (#balance . #available)
                    (`shouldBe` Quantity outChange) r''

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

        forM_ matrix $ \(name, nonJson) -> it name $ \ctx -> runResourceT $ do
            w <- emptyWallet ctx
            let payload = nonJson
            r <- request @ApiFee ctx
                (Link.getTransactionFee @'Shelley w) Default payload
            expectResponseCode HTTP.status400 r

    it "TRANS_ESTIMATE_03a - we see result when we can't cover fee" $ \ctx -> runResourceT $ do
        wSrc <- fixtureWallet ctx
        payload <- mkTxPayload ctx wSrc faucetAmt fixturePassphrase
        r <- request @ApiFee ctx
            (Link.getTransactionFee @'Shelley wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status202
            , expectField (#estimatedMin . #getQuantity) (.>= 0)
            , expectField (#estimatedMax . #getQuantity) (.<= oneAda)
            ]

    it "TRANS_ESTIMATE_03b - we see result when we can't cover fee (with withdrawal)" $ \ctx -> runResourceT $ do
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

    it "TRANS_ESTIMATE_04 - Not enough money" $ \ctx -> runResourceT $ do
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

    it "TRANS_ESTIMATE_07 - Deleted wallet" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
        wDest <- emptyWallet ctx
        payload <- mkTxPayload ctx wDest minUTxOValue fixturePassphrase
        r <- request @ApiFee ctx
            (Link.getTransactionFee @'Shelley w) Default payload
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    it "TRANS_LIST_01 - Can list Incoming and Outgoing transactions" $ \ctx -> runResourceT $ do
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
        expectResponseCode HTTP.status200 r

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
        $ \ctx -> runResourceT $ do
        let a1 = Quantity $ sum $ replicate 10 minUTxOValue
        let a2 = Quantity $ sum $ replicate 10 (2 * minUTxOValue)
        w <- fixtureWalletWith @n ctx $ mconcat
                [ replicate 10 minUTxOValue
                , replicate 10 (2 * minUTxOValue)
                ]
        txs <- listAllTransactions @n ctx w
        let [Just t2, Just t1] = fmap (fmap (view #time) . insertedAt) txs
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

        liftIO $ forM_ matrix $ \tc -> do
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

        forM_ queries $ \tc -> it (T.unpack $ query tc) $ \ctx -> runResourceT $ do
            w <- emptyWallet ctx
            let link = withQuery (query tc) $ Link.listTransactions @'Shelley w
            r <- request @([ApiTransaction n]) ctx link Default Empty
            liftIO $ verify r (assertions tc)

    it "TRANS_LIST_02 - Start time shouldn't be later than end time" $
        \ctx -> runResourceT $ do
            w <- emptyWallet ctx
            let startTime = "2009-09-09T09:09:09Z"
            let endTime = "2001-01-01T01:01:01Z"
            let link = Link.listTransactions' @'Shelley w
                    Nothing
                    (either (const Nothing) Just $ fromText $ T.pack startTime)
                    (either (const Nothing) Just $ fromText $ T.pack endTime)
                    Nothing
            r <- request @([ApiTransaction n]) ctx link Default Empty
            expectResponseCode HTTP.status400 r
            expectErrorMessage
                (errMsg400StartTimeLaterThanEndTime startTime endTime) r
            pure ()

    it "TRANS_LIST_03 - Minimum withdrawal shouldn't be 0" $
        \ctx -> runResourceT $ do
            w <- emptyWallet ctx
            let link = Link.listTransactions' @'Shelley w
                    (Just 0)
                    Nothing
                    Nothing
                    Nothing
            r <- request @([ApiTransaction n]) ctx link Default Empty
            expectResponseCode HTTP.status400 r
            expectErrorMessage errMsg400MinWithdrawalWrong r
            pure ()

    it "TRANS_LIST_03 - Minimum withdrawal can be 1, shows empty when no withdrawals" $
        \ctx -> runResourceT $ do
            w <- emptyWallet ctx
            let link = Link.listTransactions' @'Shelley w
                    (Just 1)
                    Nothing
                    Nothing
                    Nothing
            r <- request @([ApiTransaction n]) ctx link Default Empty
            expectResponseCode HTTP.status200 r
            let txs = getFromResponse Prelude.id r
            txs `shouldBe` []

    it "TRANS_LIST_04 - Deleted wallet" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
        r <- request @([ApiTransaction n]) ctx (Link.listTransactions @'Shelley w)
            Default Empty
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    it "TRANS_LIST_RANGE_01 - \
       \Transaction at time t is SELECTED by small ranges that cover it" $
          \ctx -> runResourceT $ do
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
          \ctx -> runResourceT $ do
              w <- fixtureWalletWith @n ctx [minUTxOValue]
              t <- unsafeGetTransactionTime <$> listAllTransactions ctx w
              let tl = utcTimeSucc t
              txs1 <- listTransactions @n ctx w (Just tl) (Nothing) Nothing
              txs2 <- listTransactions @n ctx w (Just tl) (Just tl) Nothing
              length <$> [txs1, txs2] `shouldSatisfy` all (== 0)

    it "TRANS_LIST_RANGE_03 - \
       \Transaction at time t is NOT selected by range (..., t - 𝛿t)" $
          \ctx -> runResourceT $ do
              w <- fixtureWalletWith @n ctx [minUTxOValue]
              t <- unsafeGetTransactionTime <$> listAllTransactions ctx w
              let te = utcTimePred t
              txs1 <- listTransactions @n ctx w (Nothing) (Just te) Nothing
              txs2 <- listTransactions @n ctx w (Just te) (Just te) Nothing
              length <$> [txs1, txs2] `shouldSatisfy` all (== 0)

    it "TRANS_GET_01 - Can get Incoming and Outgoing transaction" $ \ctx -> runResourceT $ do
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

    it "TRANS_GET_02 - Deleted wallet" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
        let txid = ApiT $ Hash $ BS.pack $ replicate 32 1
        let link = Link.getTransaction @'Shelley w (ApiTxId txid)
        r <- request @(ApiTransaction n) ctx link Default Empty
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    it "TRANS_GET_03 - Using wrong transaction id" $ \ctx -> runResourceT $ do
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
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404CannotFindTx $ toText txid) r


    it "TRANS_DELETE_01 -\
        \ Shelley: Can forget pending transaction" $ \ctx -> runResourceT $ do
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

        -- forget transaction
        (statusDelete, _) <- request @ApiTxId ctx
            (Link.deleteTransaction @'Shelley wSrc (ApiTxId txid)) Default Empty
        rBalance <- getFromResponse (#balance . #getApiT . #total)
            <$> request @ApiWallet ctx (Link.getWallet @'Shelley wSrc) Default Empty

        let assertSourceTx = do
                let ep = Link.listTransactions @'Shelley wSrc
                request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                    [ expectListField 0
                        (#direction . #getApiT) (`shouldBe` Outgoing)
                    , expectListField 0
                        (#status . #getApiT) (`shouldBe` InLedger)
                    ]

        -- We cannot guarantee that we're able to forget the tx before it is
        -- accepted. The slot length is really fast in the integration tests.
        --
        -- As a workaround we also pass if the tx is already accepted.
        case (statusDelete, rBalance) of
            (s, balance) | s == HTTP.status204 && balance == Quantity faucetAmt ->
                eventually "transaction eventually is in source wallet"
                    assertSourceTx

            (s, balance) | s == HTTP.status204 && balance < Quantity faucetAmt ->
                liftIO assertSourceTx

            (s, balance) | s == HTTP.status403 -> liftIO $ do
                assertSourceTx
                balance .< Quantity faucetAmt

            _ ->
                expectationFailure $ "invalid combination of results: "
                    <> show statusDelete
                    <> ", wallet balance="
                    <> show rBalance

        eventually "transaction eventually is in target wallet" $ do
            let ep = Link.listTransactions @'Shelley wDest
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListField 0
                    (#direction . #getApiT) (`shouldBe` Incoming)
                , expectListField 0
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]

    it "TRANS_DELETE_02 -\
        \ Shelley: Cannot forget tx that is already in ledger" $ \ctx -> runResourceT $ do
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
        expectResponseCode HTTP.status403 rDel
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
                            "quantity": #{minUTxOValue},
                            "unit": "lovelace"
                        }
                    }]
                }|]
            let endpoint = "v2/wallets/" <> wid <> "/payment-fees"
            r <- request @ApiFee ctx ("POST", endpoint) Default payload
            expectResponseCode HTTP.status404 r
            expectErrorMessage (errMsg404NoWallet wid) r

    it "BYRON_TRANS_CREATE -\
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
                            "quantity": #{minUTxOValue},
                            "unit": "lovelace"
                        }
                    }],
                    "passphrase": "cardano-wallet"
                }|]
            let endpoint = "v2/wallets/" <> wid <> "/transactions"
            r <- request @(ApiTransaction n) ctx ("POST", endpoint) Default payload
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

    it "SHELLEY_TX_REDEEM_01 - Can redeem rewards from self" $ \ctx -> runResourceT $ do
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
            verify rW
                [ expectField (#balance . #getApiT . #available)
                    (.> (wSrc ^. #balance . #getApiT . #available))
                , expectField (#balance . #getApiT . #reward)
                    (`shouldBe` Quantity 0)
                ]

    it "SHELLEY_TX_REDEEM_02 - Can redeem rewards from other" $ \ctx -> runResourceT $ do
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

    it "SHELLEY_TX_REDEEM_03 - Can't redeem rewards from other if none left" $ \ctx -> runResourceT $ do
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

    it "SHELLEY_TX_REDEEM_04 - Can always ask for self redemption" $ \ctx -> runResourceT $ do
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

    it "SHELLEY_TX_REDEEM_05 - Can't redeem rewards from unknown key" $ \ctx -> runResourceT $ do
        wSelf  <- fixtureWallet ctx
        addr:_ <- fmap (view #id) <$> listAddresses @n ctx wSelf

        mw <- liftIO $ entropyToMnemonic <$> genEntropy @160
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

    it "SHELLEY_TX_REDEEM_06 - Can't redeem rewards using byron wallet" $ \ctx -> runResourceT $ do
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

    it "SHELLEY_TX_REDEEM_06a - Can't redeem rewards if utxo = 0 from other" $ \ctx -> runResourceT $ do
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

    it "SHELLEY_TX_REDEEM_06b - Can't redeem rewards if utxo = 0 from self" $ \ctx -> runResourceT $ do
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

    it "SHELLEY_TX_REDEEM_07a - Can't redeem rewards if cannot cover fee" $ \ctx -> runResourceT $ do
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

    it "SHELLEY_TX_REDEEM_07b - Can't redeem rewards if not enough money" $ \ctx -> runResourceT $ do
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
        it resource $ \ctx -> runResourceT $ do
            w <- eWallet ctx
            let walId = w ^. walletId
            let txid = "3e6ec12da4414aa0781ff8afa9717ae53ee8cb4aa55d622f65bc62619a4f7b12"
            let endpoint = "v2/" <> T.pack resource <> "/" <> walId <> "/transactions/" <> txid
            ra <- request @ApiTxId ctx ("DELETE", endpoint) Default Empty
            expectResponseCode HTTP.status404 ra
            expectErrorMessage (errMsg404CannotFindTx txid) ra

    txDeleteFromDifferentWalletTest
        :: (HasType (ApiT WalletId) wal)
        => (Context t -> ResourceT IO wal)
        -> String
        -> SpecWith (Context t)
    txDeleteFromDifferentWalletTest eWallet resource =
        it resource $ \ctx -> runResourceT $ do
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
            ra <- request @ApiTxId ctx ("DELETE", endpoint) Default Empty
            expectResponseCode HTTP.status404 ra
            expectErrorMessage (errMsg404CannotFindTx txid) ra

    postTx
        :: (MonadIO m, MonadCatch m)
        => Context t
        -> (wal, wal -> (Method, Text), Text)
        -> ApiWallet
        -> Natural
        -> m (HTTP.Status, Either RequestException (ApiTransaction n))
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
        :: (MonadIO m, MonadCatch m)
        => Context t
        -> ApiWallet
        -> Natural
        -> Text
        -> m Payload
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

    addTxTTL :: Double -> Payload -> Payload
    addTxTTL t (Json (Aeson.Object o)) = Json (Aeson.Object (o <> ttl))
      where
        ttl = "time_to_live" .= [json|{ "quantity": #{t}, "unit": "second"}|]
    addTxTTL _ _ = error "can't do that"

    addTxMetadata :: Aeson.Value -> Payload -> Payload
    addTxMetadata md (Json (Aeson.Object o)) =
        Json (Aeson.Object (o <> ("metadata" .= md)))
    addTxMetadata _ _ = error "can't do that"

    mkMultipleTxPayload
        :: Context t
        -> ApiWallet
        -> Natural
        -> Natural
        -> Text
        -> IO Payload
    mkMultipleTxPayload ctx wDest amt1 amt2 passphrase = do
        addrs <- listAddresses @n ctx wDest
        let destination1 = (addrs !! 1) ^. #id
        let destination2 = (addrs !! 2) ^. #id
        return $ Json [json|{
                "payments": [{
                    "address": #{destination1},
                    "amount": {
                        "quantity": #{amt1},
                        "unit": "lovelace"
                    }
                },
                {
                    "address": #{destination2},
                    "amount": {
                        "quantity": #{amt2},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": #{passphrase}
            }|]

    unsafeGetTransactionTime
        :: [ApiTransaction n]
        -> UTCTime
    unsafeGetTransactionTime txs =
        case fmap (view #time) . insertedAt <$> txs of
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

    toRawBytes base bs = case convertFromBase base (T.encodeUtf8 bs) of
        Left err -> error err
        Right res -> res

    initTx = CardanoTransactions.mkInit CardanoTransactions.Mainnet 7750

    unsafeMkInput
        :: Word32
        -> BS.ByteString
        -> CardanoTransactions.Input CardanoTransactions.Shelley
    unsafeMkInput ix bs = fromJust $ CardanoTransactions.mkInput ix bs

    unsafeMkOutput
        :: Natural
        -> Text
        -> CardanoTransactions.Output CardanoTransactions.Shelley
    unsafeMkOutput n str =
        fromJust $ CardanoTransactions.mkOutput n (unsafeBech32 str)

    unsafeMkOutput'
        :: Natural
        -> Text
        -> CardanoTransactions.Output CardanoTransactions.Shelley
    unsafeMkOutput' n str =
        fromJust $ CardanoTransactions.mkOutput n (unsafeBase58 str)

    fromBech32 :: Text -> Maybe BS.ByteString
    fromBech32 txt = do
        (_, dp) <- either (const Nothing) Just (Bech32.decodeLenient txt)
        Bech32.dataPartToBytes dp

    unsafeBech32 :: Text -> BS.ByteString
    unsafeBech32 = fromMaybe (error msg) . fromBech32
        where msg = "unable to decode bech32 string."

    fromBase16 :: Text -> Maybe BS.ByteString
    fromBase16 = eitherToMaybe . convertFromBase Base16 . T.encodeUtf8

    unsafeB16 :: Text -> BS.ByteString
    unsafeB16 = fromMaybe (error msg) . fromBase16
        where msg = "unable to decode base16 string."

    unsafeMkShelleySignKey
        :: Text
        -> CardanoTransactions.SignKey CardanoTransactions.Shelley
    unsafeMkShelleySignKey str =
        fromJust $ CardanoTransactions.mkShelleySignKey (unsafeB16 str)

    fromBase58 :: Text -> Maybe BS.ByteString
    fromBase58 = decodeBase58 bitcoinAlphabet . T.encodeUtf8

    unsafeBase58 :: Text -> BS.ByteString
    unsafeBase58 = fromMaybe (error msg) . fromBase58
        where msg = "unable to decode base58 string."

    unsafeMkByronSignKey
        :: Text
        -> Text
        -> CardanoTransactions.SignKey CardanoTransactions.Shelley
    unsafeMkByronSignKey str1 str2 =
        let (Just addr) = fromBase58 str1
            (Just addrAttr) = CardanoTransactions.mkAddrAttributes addr
        in fromJust $ CardanoTransactions.mkByronSignKey addrAttr (unsafeB16 str2)

    constructTxFromCardanoTransactions fee txid txix out1 addr1 out2 addr2 wit =
        CardanoTransactions.empty (initTx fee)
        & CardanoTransactions.addInput (unsafeMkInput txix txid)
        & CardanoTransactions.addOutput (unsafeMkOutput out1 addr1)
        & CardanoTransactions.addOutput (unsafeMkOutput out2 addr2)
        & CardanoTransactions.lock
        & CardanoTransactions.signWith (unsafeMkShelleySignKey wit)
        & CardanoTransactions.serialize

    constructMultiTxFromCardanoTransactions fee txid (txix1, txix2) out1 addr1 out2 addr2 wit1 wit2 =
        CardanoTransactions.empty (initTx fee)
        & CardanoTransactions.addInput (unsafeMkInput txix1 txid)
        & CardanoTransactions.addInput (unsafeMkInput txix2 txid)
        & CardanoTransactions.addOutput (unsafeMkOutput out1 addr1)
        & CardanoTransactions.addOutput (unsafeMkOutput out2 addr2)
        & CardanoTransactions.lock
        & CardanoTransactions.signWith (unsafeMkShelleySignKey wit1)
        & CardanoTransactions.signWith (unsafeMkShelleySignKey wit2)
        & CardanoTransactions.serialize

    constructTxByronWitsFromCardanoTransactions fee txid txix out1 addr1 out2 addr2 addr3 wit =
        CardanoTransactions.empty (initTx fee)
        & CardanoTransactions.addInput (unsafeMkInput txix txid)
        & CardanoTransactions.addOutput (unsafeMkOutput' out1 addr1)
        & CardanoTransactions.addOutput (unsafeMkOutput out2 addr2)
        & CardanoTransactions.lock
        & CardanoTransactions.signWith (unsafeMkByronSignKey addr3 wit)
        & CardanoTransactions.serialize
