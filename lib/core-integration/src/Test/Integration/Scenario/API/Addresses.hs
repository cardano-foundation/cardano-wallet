{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.API.Addresses
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiAddress, ApiTransaction, ApiWallet, WalletStyle (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( defaultAddressPoolGap, getAddressPoolGap )
import Cardano.Wallet.Primitive.Types
    ( AddressState (..) )
import Control.Monad
    ( forM_ )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Quantity
    ( Quantity (..) )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , Payload (..)
    , emptyRandomWallet
    , emptyWallet
    , emptyWalletWith
    , eventually_
    , expectErrorMessage
    , expectFieldSatisfy
    , expectListItemFieldSatisfy
    , expectListSizeEqual
    , expectResponseCode
    , fixtureWallet
    , json
    , listAddresses
    , request
    , verify
    , walletId
    , withMethod
    , withPathParam
    )
import Test.Integration.Framework.TestData
    ( errMsg404NoEndpoint
    , errMsg404NoWallet
    , errMsg405
    , errMsg406
    , falseWalletIds
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall t n. (n ~ 'Testnet) => SpecWith (Context t)
spec = do
    it "BYRON_ADDRESS_LIST - Byron wallet on Shelley ep" $ \ctx -> do
        w <- emptyRandomWallet ctx
        let wid = w ^. walletId
        let ep = ("GET", "v2/wallets/" <> wid <> "/addresses")
        r <- request @[ApiAddress n] ctx ep Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet wid) r

    it "ADDRESS_LIST_01 - Can list known addresses on a default wallet" $ \ctx -> do
        let g = fromIntegral $ getAddressPoolGap defaultAddressPoolGap
        w <- emptyWallet ctx
        r <- request @[ApiAddress n] ctx
            (Link.listAddresses w) Default Empty
        expectResponseCode @IO HTTP.status200 r
        expectListSizeEqual g r
        forM_ [0..(g-1)] $ \addrNum -> do
            expectListItemFieldSatisfy addrNum (#state . #getApiT) (== Unused) r

    it "ADDRESS_LIST_01 - Can list addresses with non-default pool gap" $ \ctx -> do
        let g = 15
        w <- emptyWalletWith ctx ("Wallet", "cardano-wallet", g)
        r <- request @[ApiAddress n] ctx
            (Link.listAddresses w) Default Empty
        expectResponseCode @IO HTTP.status200 r
        expectListSizeEqual g r
        forM_ [0..(g-1)] $ \addrNum -> do
            expectListItemFieldSatisfy addrNum (#state . #getApiT) (== Unused) r

    it "ADDRESS_LIST_02 - Can filter used and unused addresses" $ \ctx -> do
        let g = fromIntegral $ getAddressPoolGap defaultAddressPoolGap
        w <- fixtureWallet ctx
        rUsed <- request @[ApiAddress n] ctx
            (Link.listAddresses' w (Just Used)) Default Empty
        expectResponseCode @IO HTTP.status200 rUsed
        expectListSizeEqual 10 rUsed
        forM_ [0..9] $ \addrNum -> do
            expectListItemFieldSatisfy
                addrNum (#state . #getApiT) (== Used) rUsed
        rUnused <- request @[ApiAddress n] ctx
            (Link.listAddresses' w (Just Unused)) Default Empty
        expectResponseCode @IO HTTP.status200 rUnused
        expectListSizeEqual g rUnused
        forM_ [10..(g-1)] $ \addrNum -> do
            expectListItemFieldSatisfy
                addrNum (#state . #getApiT) (== Unused) rUnused

    it "ADDRESS_LIST_02 - Shows nothing when there are no used addresses"
        $ \ctx -> do
        w <- emptyWallet ctx
        rUsed <- request @[ApiAddress n] ctx
            (Link.listAddresses' w (Just Used)) Default Empty
        rUnused <- request @[ApiAddress n] ctx
            (Link.listAddresses' w (Just Unused)) Default Empty
        expectResponseCode @IO HTTP.status200 rUsed
        expectListSizeEqual 0 rUsed
        expectResponseCode @IO HTTP.status200 rUnused
        expectListSizeEqual 20 rUnused
        forM_ [0..19] $ \addrNum -> do
            expectListItemFieldSatisfy
                addrNum (#state . #getApiT) (== Unused) rUnused

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

        let withQuery f (method, link) = (method, link <> "?state=" <> T.pack f)
        forM_ filters $ \fil -> it fil $ \ctx -> do
            w <- emptyWallet ctx
            let link = withQuery fil $ Link.listAddresses w
            r <- request @[ApiAddress n] ctx link Default Empty
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
        r <- request @[ApiAddress n] ctx
            (Link.listAddresses wDest) Default Empty
        verify r
            [ expectResponseCode @IO HTTP.status200
            , expectListSizeEqual 10
            ]
        forM_ [0..9] $ \addrNum -> do
            expectListItemFieldSatisfy addrNum (#state . #getApiT) (== Unused) r
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

            rTrans <- request @(ApiTransaction n) ctx
                (Link.createTransaction wSrc) Default payload
            expectResponseCode @IO HTTP.status202 rTrans

        -- make sure all transactions are in ledger
        eventually_ $ do
            rb <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            expectFieldSatisfy
                (#balance . #getApiT . #available) (== Quantity 10) rb

        -- verify new address_pool_gap has been created
        rAddr <- request @[ApiAddress n] ctx
            (Link.listAddresses wDest) Default Empty
        verify rAddr
            [ expectResponseCode @IO HTTP.status200
            , expectListSizeEqual 20
            ]
        forM_ [0..9] $ \addrNum -> do
            expectListItemFieldSatisfy
                addrNum (#state . #getApiT) (== Used) rAddr
        forM_ [10..19] $ \addrNum -> do
            expectListItemFieldSatisfy
                addrNum (#state . #getApiT) (== Unused) rAddr

    describe "ADDRESS_LIST_04 - False wallet ids" $ do
        forM_ falseWalletIds $ \(title, walId) -> it title $ \ctx -> do
            w <- emptyWallet ctx
            let endpoint = withPathParam 0 (const $ T.pack walId) $
                    Link.listAddresses w
            r <- request @[ApiAddress n] ctx endpoint Default Empty
            expectResponseCode @IO HTTP.status404 r
            if (title == "40 chars hex") then
                expectErrorMessage (errMsg404NoWallet $ T.pack walId) r
            else
                expectErrorMessage errMsg404NoEndpoint r

    it "ADDRESS_LIST_04 - 'almost' valid walletId" $ \ctx -> do
        w <- emptyWallet ctx
        let endpoint = withPathParam 0 (<> "0") $
                Link.listAddresses w
        r <- request @[ApiAddress n] ctx endpoint Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage errMsg404NoEndpoint r

    it "ADDRESS_LIST_04 - Deleted wallet" $ \ctx -> do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx
            (Link.deleteWallet @'Shelley w) Default Empty
        r <- request @[ApiAddress n] ctx
            (Link.listAddresses w) Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    describe "ADDRESS_LIST_05 - v2/wallets/{id}/addresses - Methods Not Allowed" $ do
        let matrix = ["PUT", "DELETE", "CONNECT", "TRACE", "OPTIONS", "POST"]
        forM_ matrix $ \method -> it (show method) $ \ctx -> do
            w <- emptyWallet ctx
            let link = withMethod method $ Link.listAddresses w
            r <- request @[ApiAddress n] ctx link Default Empty
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
            r <- request @[ApiAddress n] ctx
                (Link.listAddresses w) headers Empty
            verify r expectations
