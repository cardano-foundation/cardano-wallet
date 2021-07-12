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

module Test.Integration.Scenario.API.Byron.TransactionsNew
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiByronWallet
    , ApiConstructTransaction
    , ApiT (..)
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
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Control.Monad
    ( forM_ )
import Control.Monad.IO.Unlift
    ( MonadUnliftIO (..), liftIO )
import Control.Monad.Trans.Resource
    ( runResourceT )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Proxy
    ( Proxy )
import Data.Text
    ( Text )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith, describe, pendingWith )
import Test.Hspec.Expectations.Lifted
    ( shouldNotBe, shouldSatisfy )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , Payload (..)
    , emptyIcarusWallet
    , emptyRandomWallet
    , emptyWallet
    , expectErrorMessage
    , expectField
    , expectResponseCode
    , expectSuccess
    , fixtureIcarusWallet
    , fixtureIcarusWalletWith
    , fixtureMultiAssetIcarusWallet
    , fixtureMultiAssetRandomWallet
    , fixtureRandomWallet
    , fixtureRandomWalletWith
    , json
    , listAddresses
    , minUTxOValue
    , pickAnAsset
    , request
    , verify
    )
import Test.Integration.Framework.TestData
    ( errMsg403Fee, errMsg403InvalidConstructTx, errMsg403NotEnoughMoney )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall n.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    , PaymentAddress n ByronKey
    , PaymentAddress n IcarusKey
    ) => SpecWith Context
spec = describe "NEW_BYRON_TRANSACTIONS" $ do

    describe "BYRON_TRANS_NEW_CREATE_01a - Empty payload is not allowed" $
        forM_ [ (fixtureRandomWallet, "Byron fixture wallet")
              , (fixtureIcarusWallet, "Icarus fixture wallet")] $
              \(srcFixture, name) -> it name $ \ctx -> runResourceT $ do

                liftIO $ pendingWith "Byron fixture wallet returns 500"

                wa <- srcFixture ctx
                let emptyPayload = Json [json|{}|]

                rTx <- request @(ApiConstructTransaction n) ctx
                    (Link.createUnsignedTransaction @'Byron wa)
                    Default emptyPayload
                verify rTx
                    [ expectResponseCode HTTP.status403
                    , expectErrorMessage errMsg403InvalidConstructTx
                    ]

    describe "BYRON_TRANS_NEW_CREATE_01c - No payload is bad request" $
        forM_ [ (fixtureRandomWallet, "Byron fixture wallet")
              , (fixtureIcarusWallet, "Icarus fixture wallet")] $
              \(srcFixture, name) -> it name $ \ctx -> runResourceT $ do

                wa <- srcFixture ctx

                rTx <- request @(ApiConstructTransaction n) ctx
                    (Link.createUnsignedTransaction @'Byron wa) Default Empty
                verify rTx
                    [ expectResponseCode HTTP.status400
                    ]

    describe "BYRON_TRANS_NEW_CREATE_04 - Single Output Transaction" $
        forM_ [(fixtureRandomWallet, "Byron wallet"),
              (fixtureIcarusWallet, "Icarus wallet")] $
              \(srcFixture,name) -> it name $ \ctx -> runResourceT $ do

            liftIO $ pendingWith "Byron wallet returns 500, Icarus wallet returns 403 (invalid_wallet_type)"


            (wByron, wShelley) <- (,) <$> srcFixture ctx <*> emptyWallet ctx
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

            rTx <- request @(ApiConstructTransaction n) ctx
                (Link.createUnsignedTransaction @'Byron wByron) Default payload
            verify rTx
                [ expectSuccess
                , expectResponseCode HTTP.status202
                , expectField (#coinSelection . #inputs) (`shouldSatisfy` (not . null))
                , expectField (#coinSelection . #outputs) (`shouldSatisfy` (not . null))
                , expectField (#coinSelection . #change) (`shouldSatisfy` (not . null))
                ]

    describe "BYRON_TRANS_NEW_CREATE_04c - Can't cover fee" $
        forM_ [(fixtureRandomWalletWith @n, "Byron wallet"),
               (fixtureIcarusWalletWith @n, "Icarus wallet")] $
               \(srcFixture,name) -> it name $ \ctx -> runResourceT $ do

            liftIO $ pendingWith "Byron wallet returns 500, Icarus wallet as expected"

            wa <- srcFixture ctx [minUTxOValue + 1]
            wb <- emptyWallet ctx

            payload <- liftIO $ mkTxPayload ctx wb minUTxOValue

            rTx <- request @(ApiConstructTransaction n) ctx
                (Link.createUnsignedTransaction @'Byron wa) Default payload
            verify rTx
                [ expectResponseCode HTTP.status403
                , expectErrorMessage errMsg403Fee
                ]

    describe "BYRON_TRANS_NEW_CREATE_04d - Not enough money" $
        forM_ [(fixtureRandomWalletWith @n, "Byron wallet"),
               (fixtureIcarusWalletWith @n, "Icarus wallet")] $
               \(srcFixture,name) -> it name $ \ctx -> runResourceT $ do

            liftIO $ pendingWith "Byron wallet returns 500, Icarus wallet as expected"

            let (srcAmt, reqAmt) = (minUTxOValue, 2 * minUTxOValue)
            wa <- srcFixture ctx [srcAmt]
            wb <- emptyWallet ctx

            payload <- liftIO $ mkTxPayload ctx wb reqAmt

            rTx <- request @(ApiConstructTransaction n) ctx
                (Link.createUnsignedTransaction @'Byron wa) Default payload
            verify rTx
                [ expectResponseCode HTTP.status403
                , expectErrorMessage errMsg403NotEnoughMoney
                ]

    describe "BYRON_TRANS_NEW_CREATE_04d - Not enough money emptyWallet" $
        forM_ [(emptyRandomWallet, "Empty Byron wallet"),
               (emptyIcarusWallet, "Empty Icarus wallet")] $
              \(emptySrcWallet,name) -> it name $ \ctx -> runResourceT $ do

            liftIO $ pendingWith "Byron wallet returns 500, Icarus wallet as expected"

            wa <- emptySrcWallet ctx
            wb <- emptyWallet ctx

            payload <- liftIO $ mkTxPayload ctx wb minUTxOValue

            rTx <- request @(ApiConstructTransaction n) ctx
                (Link.createUnsignedTransaction @'Byron wa) Default payload
            verify rTx
                [ expectResponseCode HTTP.status403
                , expectErrorMessage errMsg403NotEnoughMoney
                ]

    describe "BYRON_TRANS_NEW_CREATE_04e - Multiple Output Tx to single wallet" $
        forM_ [(fixtureRandomWallet, "Byron wallet"),
              (fixtureIcarusWallet, "Icarus wallet")] $
              \(srcFixture,name) -> it name $ \ctx -> runResourceT $ do

            liftIO $ pendingWith "Byron wallet returns 500, Icarus wallet returns 403 (invalid_wallet_type)"

            wa <- srcFixture ctx
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

            rTx <- request @(ApiConstructTransaction n) ctx
                (Link.createUnsignedTransaction @'Byron wa) Default payload
            verify rTx
                [ expectSuccess
                , expectResponseCode HTTP.status202
                , expectField (#coinSelection . #inputs) (`shouldSatisfy` (not . null))
                , expectField (#coinSelection . #outputs) (`shouldSatisfy` (not . null))
                , expectField (#coinSelection . #change) (`shouldSatisfy` (not . null))
                ]
            -- TODO: now we should sign it and send it in two steps,
            --       make sure it is delivered
            --       make sure balance is updated accordingly on src and dst wallets

    describe "BYRON_TRANS_NEW_ASSETS_CREATE_01a - Multi-asset tx with Ada" $
        forM_ [(fixtureMultiAssetRandomWallet @n, "Byron wallet"),
              (fixtureMultiAssetIcarusWallet @n, "Icarus wallet")] $
              \(srcFixture,name) -> it name $ \ctx -> runResourceT $ do

        liftIO $ pendingWith "Byron wallet returns 500, Icarus wallet returns 403 (invalid_wallet_type)"

        wa <- srcFixture ctx
        wb <- emptyWallet ctx
        ra <- request @ApiByronWallet ctx (Link.getWallet @'Byron wa) Default Empty
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
            (Link.createUnsignedTransaction @'Byron wa) Default payload
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

    describe "BYRON_TRANS_NEW_ASSETS_CREATE_01b - Multi-asset tx with not enough Ada" $
        forM_ [(fixtureMultiAssetRandomWallet @n, "Byron wallet"),
              (fixtureMultiAssetIcarusWallet @n, "Icarus wallet")] $
              \(srcFixture,name) -> it name $ \ctx -> runResourceT $ do

            liftIO $ pendingWith "Byron wallet returns 500, Icarus wallet as expected"

            wa <- srcFixture ctx
            wb <- emptyWallet ctx
            ra <- request @ApiByronWallet ctx (Link.getWallet @'Byron wa) Default Empty
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
                (Link.createUnsignedTransaction @'Byron wa) Default payload
            verify rTx
                [ expectResponseCode HTTP.status403
                , expectErrorMessage "Some outputs have ada values that are too small."
                ]

    describe "BYRON_TRANS_NEW_ASSETS_CREATE_01c - Multi-asset tx without Ada" $
        forM_ [(fixtureMultiAssetRandomWallet @n, "Byron wallet"),
              (fixtureMultiAssetIcarusWallet @n, "Icarus wallet")] $
              \(srcFixture,name) -> it name $ \ctx -> runResourceT $ do

            liftIO $ pendingWith "Byron wallet returns 500, Icarus wallet returns 403 (invalid_wallet_type)"

            wa <- srcFixture ctx
            wb <- emptyWallet ctx
            ra <- request @ApiByronWallet ctx (Link.getWallet @'Byron wa) Default Empty
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
                (Link.createUnsignedTransaction @'Byron wa) Default payload
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

    describe "BYRON_TRANS_NEW_ASSETS_CREATE_01d - Multi-asset tx with not enough assets" $
        forM_ [(fixtureMultiAssetRandomWallet @n, "Byron wallet"),
              (fixtureMultiAssetIcarusWallet @n, "Icarus wallet")] $
              \(srcFixture,name) -> it name $ \ctx -> runResourceT $ do
            
            liftIO $ pendingWith "Byron wallet returns 500, Icarus wallet as expected"

            wa <- srcFixture ctx
            wb <- emptyWallet ctx
            ra <- request @ApiByronWallet ctx (Link.getWallet @'Byron wa) Default Empty
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
                (Link.createUnsignedTransaction @'Byron wa) Default payload
            verify rTx
                [ expectResponseCode HTTP.status403
                , expectErrorMessage errMsg403NotEnoughMoney
                ]

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
