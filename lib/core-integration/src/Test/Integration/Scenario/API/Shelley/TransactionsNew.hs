{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- HLINT ignore "Use head" -}

module Test.Integration.Scenario.API.Shelley.TransactionsNew
    ( spec
    ) where

import Prelude

import Cardano.Mnemonic
    ( mnemonicToText )
import Cardano.Wallet.Api.Types
    ( ApiCoinSelectionInput (..)
    , ApiConstructTransaction
    , ApiFee (..)
    , ApiT (..)
    , ApiWallet
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( PaymentAddress )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Control.Monad.IO.Unlift
    ( MonadIO (..), MonadUnliftIO (..), liftIO )
import Control.Monad.Trans.Resource
    ( runResourceT )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Maybe
    ( isJust )
import Data.Proxy
    ( Proxy )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith, describe, pendingWith )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldNotBe, shouldSatisfy )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , Payload (..)
    , emptyWallet
    , expectErrorMessage
    , expectField
    , expectResponseCode
    , expectSuccess
    , fixtureMultiAssetWallet
    , fixtureWallet
    , fixtureWalletWith
    , getFromResponse
    , json
    , listAddresses
    , minUTxOValue
    , pickAnAsset
    , request
    , rewardWallet
    , unsafeRequest
    , verify
    )
import Test.Integration.Framework.TestData
    ( errMsg403Fee
    , errMsg403InvalidConstructTx
    , errMsg403MinUTxOValue
    , errMsg403NotEnoughMoney
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.List.NonEmpty as NE
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall n.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    , PaymentAddress n IcarusKey
    ) => SpecWith Context
spec = describe "NEW_SHELLEY_TRANSACTIONS" $ do
    it "TRANS_NEW_CREATE_01a - Empty payload is not allowed" $ \ctx -> runResourceT $ do
        wa <- fixtureWallet ctx
        let emptyPayload = Json [json|{}|]

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default emptyPayload
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403InvalidConstructTx
            ]

    it "TRANS_NEW_CREATE_01b - Validity interval only is not allowed" $ \ctx -> runResourceT $ do
        wa <- fixtureWallet ctx
        let validityInterval = Json [json|{
                "validity_interval": {
                    "invalid_before": {
                      "quantity": 10,
                      "unit": "second"
                    },
                    "invalid_hereafter": {
                      "quantity": 50,
                      "unit": "second"
                    }
                  }
                }|]

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default validityInterval
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403InvalidConstructTx
            ]

    it "TRANS_NEW_CREATE_01c - No payload is bad request" $ \ctx -> runResourceT $ do
        wa <- fixtureWallet ctx

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default Empty
        verify rTx
            [ expectResponseCode HTTP.status400
            ]

    it "TRANS_NEW_CREATE_02 - Only metadata" $ \ctx -> runResourceT $ do
        wa <- fixtureWallet ctx
        let metadata = Json [json|{ "metadata": { "1": { "string": "hello" } } }|]
        let expectedFee = 1129000

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default metadata
        verify rTx
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #metadata) (`shouldSatisfy` isJust)
            , expectField (#fee . #getQuantity) (`shouldBe` expectedFee)
            ]
        -- TODO: sign and submit tx,
        --       check metadata presence on submitted tx,
        --       make sure only fee is deducted from fixtureWallet

    it "TRANS_NEW_CREATE_03a - Withdrawal from self" $ \ctx -> runResourceT $ do
        (wa, _) <- rewardWallet ctx
        let withdrawal = Json [json|{ "withdrawal": "self" }|]
        let expectedFee = 139000
        -- let withdrawalAmt = 1000000000000

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default withdrawal
        verify rTx
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #metadata) (`shouldBe` Nothing)
            , expectField (#fee . #getQuantity) (`shouldBe` expectedFee)
            , expectField (#coinSelection . #withdrawals) (`shouldSatisfy` (not . null))
            ]
        -- TODO: sign and submit tx,
        --       check that reward account is 0,
        --       make sure wallet balance is increased by withdrawalAmt - fee

    it "TRANS_NEW_CREATE_03b - Withdrawal from external wallet" $ \ctx -> runResourceT $ do
        (_, mw) <- rewardWallet ctx
        wa <- fixtureWallet ctx
        let withdrawal = Json [json|{ "withdrawal": #{mnemonicToText mw} }|]
        let expectedFee = 1139000
        -- let withdrawalAmt = 1000000000000

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default withdrawal
        verify rTx
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #metadata) (`shouldBe` Nothing)
            , expectField (#fee . #getQuantity) (`shouldBe` expectedFee)
            , expectField (#coinSelection . #withdrawals) (`shouldSatisfy` (not . null))
            ]
        -- TODO: sign and submit tx,
        --       check that reward account is 0 on external rewardWallet,
        --       make sure wa wallet balance is increased by withdrawalAmt - fee

    it "TRANS_NEW_CREATE_04a - Single Output Transaction" $ \ctx -> runResourceT $ do

        liftIO $ pendingWith "Missing outputs on response - to be fixed in ADP-985"

        let initialAmt = 3*minUTxOValue
        wa <- fixtureWalletWith @n ctx [initialAmt]
        wb <- emptyWallet ctx
        let amt = (minUTxOValue :: Natural)

        payload <- liftIO $ mkTxPayload ctx wb amt

        (_, ApiFee (Quantity feeMin) _ _ _) <- unsafeRequest ctx
            (Link.getTransactionFee @'Shelley wa) payload
        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#coinSelection . #inputs) (`shouldSatisfy` (not . null))
            , expectField (#coinSelection . #outputs) (`shouldSatisfy` (not . null))
            , expectField (#coinSelection . #change) (`shouldSatisfy` (not . null))
            , expectField (#fee . #getQuantity) (`shouldBe` feeMin)
            ]

        let filterInitialAmt =
                filter (\(ApiCoinSelectionInput _ _ _ _ amt' _) -> amt' == Quantity initialAmt)
        let coinSelInputs = filterInitialAmt $ NE.toList $
                getFromResponse (#coinSelection . #inputs) rTx
        length coinSelInputs `shouldBe` 1

        -- TODO: now we should sign it and send it in two steps
        --       make sure it is delivered
        --       make sure balance is updated accordingly on src and dst wallets

    it "TRANS_NEW_CREATE_04b - Cannot spend less than minUTxOValue" $ \ctx -> runResourceT $ do
        wa <- fixtureWallet ctx
        wb <- emptyWallet ctx
        let amt = minUTxOValue - 1

        payload <- liftIO $ mkTxPayload ctx wb amt

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403MinUTxOValue
            ]

    it "TRANS_NEW_CREATE_04c - Can't cover fee" $ \ctx -> runResourceT $ do
        wa <- fixtureWalletWith @n ctx [minUTxOValue + 1]
        wb <- emptyWallet ctx

        payload <- liftIO $ mkTxPayload ctx wb minUTxOValue

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403Fee
            ]

    it "TRANS_NEW_CREATE_04d - Not enough money" $ \ctx -> runResourceT $ do
        let (srcAmt, reqAmt) = (minUTxOValue, 2 * minUTxOValue)
        wa <- fixtureWalletWith @n ctx [srcAmt]
        wb <- emptyWallet ctx

        payload <- liftIO $ mkTxPayload ctx wb reqAmt

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403NotEnoughMoney
            ]

    it "TRANS_NEW_CREATE_04e- Multiple Output Tx to single wallet" $ \ctx -> runResourceT $ do

        liftIO $ pendingWith "Missing outputs on response - to be fixed in ADP-985"

        wa <- fixtureWallet ctx
        wb <- emptyWallet ctx
        addrs <- listAddresses @n ctx wb

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
                }]
            }|]

        (_, ApiFee (Quantity feeMin) _ _ _) <- unsafeRequest ctx
            (Link.getTransactionFee @'Shelley wa) payload
        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#coinSelection . #inputs) (`shouldSatisfy` (not . null))
            , expectField (#coinSelection . #outputs) (`shouldSatisfy` (not . null))
            , expectField (#coinSelection . #change) (`shouldSatisfy` (not . null))
            , expectField (#fee . #getQuantity) (`shouldBe` feeMin)
            ]
        -- TODO: now we should sign it and send it in two steps,
        --       make sure it is delivered
        --       make sure balance is updated accordingly on src and dst wallets

    it "TRANS_NEW_ASSETS_CREATE_01a - Multi-asset tx with Ada" $ \ctx -> runResourceT $ do

        liftIO $ pendingWith "Missing outputs on response - to be fixed in ADP-985"

        wa <- fixtureMultiAssetWallet ctx
        wb <- emptyWallet ctx
        ra <- request @ApiWallet ctx (Link.getWallet @'Shelley wa) Default Empty
        let (_, Right wal) = ra

        -- pick out an asset to send
        let assetsSrc = wal ^. #assets . #total . #getApiT
        assetsSrc `shouldNotBe` mempty
        let val = minUTxOValue <$ pickAnAsset assetsSrc

        -- create payload
        addrs <- listAddresses @n ctx wb
        let destination = (addrs !! 1) ^. #id
        let amt = 2 * minUTxOValue
        payload <- mkTxPayloadMA @n destination amt [val]

        --construct transaction
        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#coinSelection . #inputs) (`shouldSatisfy` (not . null))
            , expectField (#coinSelection . #outputs) (`shouldSatisfy` (not . null))
            , expectField (#coinSelection . #change) (`shouldSatisfy` (not . null))
            ]
        -- TODO: now we should sign it and send it in two steps
        --       make sure it is delivered
        --       make sure balance is updated accordingly on src and dst wallets

    it "TRANS_NEW_ASSETS_CREATE_01b - Multi-asset tx with not enough Ada" $ \ctx -> runResourceT $ do
        wa <- fixtureMultiAssetWallet ctx
        wb <- emptyWallet ctx
        ra <- request @ApiWallet ctx (Link.getWallet @'Shelley wa) Default Empty
        let (_, Right wal) = ra

        -- pick out an asset to send
        let assetsSrc = wal ^. #assets . #total . #getApiT
        assetsSrc `shouldNotBe` mempty
        let val = minUTxOValue <$ pickAnAsset assetsSrc

        -- create payload
        addrs <- listAddresses @n ctx wb
        let destination = (addrs !! 1) ^. #id
        let amt = minUTxOValue
        payload <- mkTxPayloadMA @n destination amt [val]

        --construct transaction
        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage "Some outputs have ada values that are too small."
            ]

    it "TRANS_NEW_ASSETS_CREATE_01c - Multi-asset tx without Ada" $ \ctx -> runResourceT $ do

        liftIO $ pendingWith "Missing outputs on response - to be fixed in ADP-985"

        wa <- fixtureMultiAssetWallet ctx
        wb <- emptyWallet ctx
        ra <- request @ApiWallet ctx (Link.getWallet @'Shelley wa) Default Empty
        let (_, Right wal) = ra

        -- pick out an asset to send
        let assetsSrc = wal ^. #assets . #total . #getApiT
        assetsSrc `shouldNotBe` mempty
        let val = minUTxOValue <$ pickAnAsset assetsSrc

        -- create payload
        addrs <- listAddresses @n ctx wb
        let destination = (addrs !! 1) ^. #id
        let amt = 0
        payload <- mkTxPayloadMA @n destination amt [val]

        --construct transaction
        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#coinSelection . #inputs) (`shouldSatisfy` (not . null))
            , expectField (#coinSelection . #outputs) (`shouldSatisfy` (not . null))
            , expectField (#coinSelection . #change) (`shouldSatisfy` (not . null))
            ]
        -- TODO: now we should sign it and send it in two steps
        --       make sure it is delivered
        --       make sure balance is updated accordingly on src and dst wallets

    it "TRANS_NEW_ASSETS_CREATE_01d - Multi-asset tx with not enough assets" $ \ctx -> runResourceT $ do
        wa <- fixtureMultiAssetWallet ctx
        wb <- emptyWallet ctx
        ra <- request @ApiWallet ctx (Link.getWallet @'Shelley wa) Default Empty
        let (_, Right wal) = ra

        -- pick out an asset to send
        let assetsSrc = wal ^. #assets . #total . #getApiT
        assetsSrc `shouldNotBe` mempty
        let val = (minUTxOValue * minUTxOValue) <$ pickAnAsset assetsSrc

        -- create payload
        addrs <- listAddresses @n ctx wb
        let destination = (addrs !! 1) ^. #id
        let amt = 0
        payload <- mkTxPayloadMA @n destination amt [val]

        --construct transaction
        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403NotEnoughMoney
            ]


  -- TODO:
  -- join and quit pool
  -- mint
  -- validity interval
  -- everything (payment, delegation, mint, metadata, withdrawal)
  where
    -- Construct a JSON payment request for the given quantity of lovelace.
    mkTxPayload
        :: MonadUnliftIO m
        => Context
        -> ApiWallet
        -> Natural
        -> m Payload
    mkTxPayload ctx wDest amt = do
        addrs <- listAddresses @n ctx wDest
        let destination = (addrs !! 1) ^. #id
        return $ Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }]
            }|]

    -- Like mkTxPayload, except that assets are included in the payment.
    -- Asset amounts are specified by ((PolicyId Hex, AssetName Hex), amount).
    mkTxPayloadMA
        :: forall l m.
            ( DecodeAddress l
            , DecodeStakeAddress l
            , EncodeAddress l
            , MonadUnliftIO m
            )
        => (ApiT Address, Proxy l)
        -> Natural
        -> [((Text, Text), Natural)]
        -> m Payload
    mkTxPayloadMA destination coin val = do
        let assetJson ((pid, name), q) = [json|{
                    "policy_id": #{pid},
                    "asset_name": #{name},
                    "quantity": #{q}
                }|]
        return $ Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{coin},
                        "unit": "lovelace"
                    },
                    "assets": #{map assetJson val}
                }]
            }|]
