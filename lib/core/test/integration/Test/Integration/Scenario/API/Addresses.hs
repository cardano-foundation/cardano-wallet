{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.API.Addresses
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiAddress, ApiTransaction, ApiWallet )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( defaultAddressPoolGap, getAddressPoolGap )
import Cardano.Wallet.Primitive.Types
    ( AddressState (..), DecodeAddress (..), EncodeAddress (..) )
import Control.Monad
    ( forM_ )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , Payload (..)
    , balanceAvailable
    , deleteWalletEp
    , emptyWallet
    , emptyWalletWith
    , expectErrorMessage
    , expectEventually
    , expectListItemFieldEqual
    , expectListSizeEqual
    , expectResponseCode
    , fixtureWallet
    , getAddressesEp
    , getWalletEp
    , json
    , listAddresses
    , postTxEp
    , request
    , state
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( errMsg404NoEndpoint
    , errMsg404NoWallet
    , errMsg405
    , errMsg406
    , falseWalletIds
    )

import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall t. (DecodeAddress t, EncodeAddress t) => SpecWith (Context t)
spec = do
    it "ADDRESS_LIST_01 - Can list known addresses on a default wallet" $ \ctx -> do
        let g = fromIntegral $ getAddressPoolGap defaultAddressPoolGap
        w <- emptyWallet ctx
        r <- request @[ApiAddress t] ctx (getAddressesEp w "") Default Empty
        expectResponseCode @IO HTTP.status200 r
        expectListSizeEqual g r
        forM_ [0..(g-1)] $ \addrNum -> do
            expectListItemFieldEqual addrNum state Unused r

    it "ADDRESS_LIST_01 - Can list addresses with non-default pool gap" $ \ctx -> do
        let g = 15
        w <- emptyWalletWith ctx ("Wallet", "cardano-wallet", g)
        r <- request @[ApiAddress t] ctx (getAddressesEp w "") Default Empty
        expectResponseCode @IO HTTP.status200 r
        expectListSizeEqual g r
        forM_ [0..(g-1)] $ \addrNum -> do
            expectListItemFieldEqual addrNum state Unused r

    it "ADDRESS_LIST_02 - Can filter used and unused addresses" $ \ctx -> do
        let g = fromIntegral $ getAddressPoolGap defaultAddressPoolGap
        w <- fixtureWallet ctx
        rUsed <- request @[ApiAddress t] ctx (getAddressesEp w "?state=used")
                Default Empty
        expectResponseCode @IO HTTP.status200 rUsed
        expectListSizeEqual 10 rUsed
        forM_ [0..9] $ \addrNum -> do
            expectListItemFieldEqual addrNum state Used rUsed
        rUnused <- request @[ApiAddress t] ctx (getAddressesEp w "?state=unused")
                Default Empty
        expectResponseCode @IO HTTP.status200 rUnused
        expectListSizeEqual g rUnused
        forM_ [10..(g-1)] $ \addrNum -> do
            expectListItemFieldEqual addrNum state Unused rUnused

    it "ADDRESS_LIST_02 - Shows nothing when there are no used addresses"
        $ \ctx -> do
        w <- emptyWallet ctx
        rUsed <- request @[ApiAddress t] ctx (getAddressesEp w "?state=used")
                Default Empty
        rUnused <- request @[ApiAddress t] ctx (getAddressesEp w "?state=unused")
                Default Empty
        expectResponseCode @IO HTTP.status200 rUsed
        expectListSizeEqual 0 rUsed
        expectResponseCode @IO HTTP.status200 rUnused
        expectListSizeEqual 20 rUnused
        forM_ [0..19] $ \addrNum -> do
            expectListItemFieldEqual addrNum state Unused rUnused

    describe "ADDRESS_LIST_02 - Invalid filters are bad requests" $ do
        let filters =
                [ "usedd"
                , "uused"
                , "unusedd"
                , "uunused"
                , "USED"
                , "UNUSED"
                , "-1000"
                , "44444444"
                , "*"
                ]
        forM_ filters $ \fil -> it fil $ \ctx -> do
            let stateFilter = "?state=" <> T.pack fil
            w <- emptyWallet ctx
            r <- request @[ApiAddress t] ctx (getAddressesEp w stateFilter)
                    Default Empty
            verify r
                [ expectResponseCode @IO HTTP.status400
                , expectErrorMessage $
                    "Error parsing query parameter state failed: Unable to\
                    \ decode the given value: '" <> fil <> "'. Please specify\
                    \ one of the following values: used, unused."
                ]

    it "ADDRESS_LIST_03 - Generates new address pool gap" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWalletWith ctx ("Wallet", "cardano-wallet", 10)

        -- make sure all addresses in address_pool_gap are 'Unused'
        r <- request @[ApiAddress t] ctx (getAddressesEp wDest "") Default Empty
        verify r
            [ expectResponseCode @IO HTTP.status200
            , expectListSizeEqual 10
            ]
        forM_ [0..9] $ \addrNum -> do
            expectListItemFieldEqual addrNum state Unused r
        addrs <- listAddresses ctx wDest

        -- run 10 transactions to make all addresses `Used`
        forM_ [0..9] $ \addrNum -> do
            let destination = (addrs !! addrNum) ^. #id
            let payload = Json [json|{
                    "payments": [{
                        "address": #{destination},
                        "amount": {
                            "quantity": 1,
                            "unit": "lovelace"
                        }
                    }],
                    "passphrase": "cardano-wallet"
                }|]

            rTrans <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
            expectResponseCode @IO HTTP.status202 rTrans

        -- make sure all transactions are in ledger
        rb <- request @ApiWallet ctx (getWalletEp wDest) Default Empty
        expectEventually ctx balanceAvailable 10 rb

        -- verify new address_pool_gap has been created
        rAddr <- request @[ApiAddress t] ctx (getAddressesEp wDest "") Default Empty
        verify rAddr
            [ expectResponseCode @IO HTTP.status200
            , expectListSizeEqual 20
            ]
        forM_ [0..9] $ \addrNum -> do
            expectListItemFieldEqual addrNum state Used rAddr
        forM_ [10..19] $ \addrNum -> do
            expectListItemFieldEqual addrNum state Unused rAddr

    describe "ADDRESS_LIST_04 - False wallet ids" $ do
        forM_ falseWalletIds $ \(title, walId) -> it title $ \ctx -> do
            let endpoint = "v2/wallets/" <> walId <> "/addresses"
            r <- request @[ApiAddress t] ctx ("GET", T.pack endpoint) Default Empty
            expectResponseCode @IO HTTP.status404 r
            if (title == "40 chars hex") then
                expectErrorMessage (errMsg404NoWallet $ T.pack walId) r
            else
                expectErrorMessage errMsg404NoEndpoint r

    it "ADDRESS_LIST_04 - 'almost' valid walletId" $ \ctx -> do
        w <- emptyWallet ctx
        let endpoint =
                "v2/wallets" <> T.unpack (T.append (w ^. walletId) "0")
                <> "/addresses"
        r <- request @[ApiAddress t] ctx ("GET", T.pack endpoint) Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage errMsg404NoEndpoint r

    it "ADDRESS_LIST_04 - Deleted wallet" $ \ctx -> do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx (deleteWalletEp w) Default Empty
        r <- request @[ApiAddress t] ctx (getAddressesEp w "") Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    describe "ADDRESS_LIST_05 - v2/wallets/{id}/addresses - Methods Not Allowed" $ do
        let matrix = ["PUT", "DELETE", "CONNECT", "TRACE", "OPTIONS", "POST"]
        forM_ matrix $ \method -> it (show method) $ \ctx -> do
            w <- emptyWallet ctx
            let endpoint = "v2/wallets/" <> w ^. walletId <> "/addresses"
            r <- request @[ApiAddress t] ctx (method, endpoint) Default Empty
            expectResponseCode @IO HTTP.status405 r
            expectErrorMessage errMsg405 r


    describe "ADDRESS_LIST_05 - Request headers" $ do
        let headerCases =
                  [ ( "No HTTP headers -> 200", None
                    , [ expectResponseCode @IO HTTP.status200 ] )
                  , ( "Accept: text/plain -> 406"
                    , Headers
                          [ ("Content-Type", "application/json")
                          , ("Accept", "text/plain") ]
                    , [ expectResponseCode @IO HTTP.status406
                      , expectErrorMessage errMsg406 ]
                    )
                  , ( "No Accept -> 200"
                    , Headers [ ("Content-Type", "application/json") ]
                    , [ expectResponseCode @IO HTTP.status200 ]
                    )
                  , ( "No Content-Type -> 200"
                    , Headers [ ("Accept", "application/json") ]
                    , [ expectResponseCode @IO HTTP.status200 ]
                    )
                  , ( "Content-Type: text/plain -> 200"
                    , Headers [ ("Content-Type", "text/plain") ]
                    , [ expectResponseCode @IO HTTP.status200 ]
                    )
                  ]
        forM_ headerCases $ \(title, headers, expectations) -> it title $ \ctx -> do
            w <- emptyWallet ctx
            r <- request @[ApiAddress t] ctx (getAddressesEp w "") headers Empty
            verify r expectations
