{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.Addresses
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiAddress, ApiTransactions, ApiWallet )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge )
import Cardano.Wallet.Primitive.Types
    ( AddressState (..) )
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
    , deleteWallet
    , emptyWallet
    , emptyWalletWith
    , expectErrorMessage
    , expectEventually
    , expectListItemFieldEqual
    , expectListSizeEqual
    , expectResponseCode
    , fixtureWallet
    , getAddresses
    , getWallet
    , listAddresses
    , postTx
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
    , postTransPayload
    )

import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

spec :: SpecWith Context
spec = do
    it "ADDRESS_LIST_01 - Can list known addresses on a default wallet" $ \ctx -> do
        w <- fixtureWallet ctx
        r <- request @[ApiAddress HttpBridge] ctx (getAddresses w) Default Empty
        expectResponseCode @IO HTTP.status200 r
        expectListSizeEqual 21 r
        expectListItemFieldEqual 0 state Used r
        forM_ [1..20] $ \addrNum -> do
            expectListItemFieldEqual addrNum state Unused r

    it "ADDRESS_LIST_01 - Can list addresses with non-default pool gap"
        $ \ctx -> do
        w <- emptyWalletWith ctx ("Wallet", "cardano-wallet", 15)
        r <- request @[ApiAddress HttpBridge] ctx (getAddresses w) Default Empty
        expectResponseCode @IO HTTP.status200 r
        expectListSizeEqual 15 r
        forM_ [0..14] $ \addrNum -> do
            expectListItemFieldEqual addrNum state Unused r

    it "ADDRESS_LIST_01 - Can list addresses with default pool gap" $ \ctx -> do
        w <- emptyWallet ctx
        r <- request @[ApiAddress HttpBridge] ctx (getAddresses w) Default Empty
        expectResponseCode @IO HTTP.status200 r
        expectListSizeEqual 20 r
        forM_ [0..19] $ \addrNum -> do
            expectListItemFieldEqual addrNum state Unused r

    it "ADDRESS_LIST_03 - Generates new address pool gap" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWalletWith ctx ("Wallet", "cardano-wallet", 10)

        -- make sure all addresses in address_pool_gap are 'Unused'
        r <- request @[ApiAddress HttpBridge] ctx (getAddresses wDest) Default Empty
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
            let payload = postTransPayload 1 destination "cardano-wallet"
            rTrans <- request @(ApiTransaction HttpBridge) ctx (postTx wSrc) Default payload
            expectResponseCode @IO HTTP.status202 rTrans

        -- make sure all transactions are in ledger
        rb <- request @ApiWallet ctx (getWallet wDest) Default Empty
        expectEventually ctx balanceAvailable 10 rb

        -- verify new address_pool_gap has been created
        rAddr <- request @[ApiAddress HttpBridge] ctx (getAddresses wDest) Default Empty
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
            r <- request @[ApiAddress HttpBridge] ctx ("GET", T.pack endpoint) Default Empty
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
        r <- request @[ApiAddress HttpBridge] ctx ("GET", T.pack endpoint) Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage errMsg404NoEndpoint r

    it "ADDRESS_LIST_04 - Deleted wallet" $ \ctx -> do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx (deleteWallet w) Default Empty
        r <- request @[ApiAddress HttpBridge] ctx (getAddresses w) Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    describe "ADDRESS_LIST_05 - v2/wallets/{id}/addresses - Methods Not Allowed" $ do
        let matrix = ["PUT", "DELETE", "CONNECT", "TRACE", "OPTIONS", "POST"]
        forM_ matrix $ \method -> it (show method) $ \ctx -> do
            w <- emptyWallet ctx
            let endpoint = "v2/wallets/" <> w ^. walletId <> "/addresses"
            r <- request @[ApiAddress HttpBridge] ctx (method, endpoint) Default Empty
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
            r <- request @[ApiAddress HttpBridge] ctx (getAddresses w) headers Empty
            verify r expectations
