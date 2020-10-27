{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.API.Shelley.Addresses
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( AnyAddress
    , ApiAddress
    , ApiTransaction
    , ApiWallet
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( defaultAddressPoolGap, getAddressPoolGap )
import Cardano.Wallet.Primitive.Types
    ( AddressState (..) )
import Control.Monad
    ( forM_ )
import Control.Monad.Trans.Resource
    ( runResourceT )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Quantity
    ( Quantity (..) )
import Test.Hspec
    ( SpecWith, describe, shouldBe, shouldNotSatisfy, shouldSatisfy )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , Payload (..)
    , emptyRandomWallet
    , emptyWallet
    , emptyWalletWith
    , eventually
    , expectErrorMessage
    , expectField
    , expectListField
    , expectListSize
    , expectResponseCode
    , fixtureWallet
    , json
    , listAddresses
    , minUTxOValue
    , request
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( errMsg404NoWallet )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Lens as Aeson
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall n t.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    ) => SpecWith (Context t)
spec = describe "SHELLEY_ADDRESSES" $ do
    it "BYRON_ADDRESS_LIST - Byron wallet on Shelley ep" $ \ctx -> runResourceT $ do
        w <- emptyRandomWallet ctx
        let wid = w ^. walletId
        let ep = ("GET", "v2/wallets/" <> wid <> "/addresses")
        r <- request @[ApiAddress n] ctx ep Default Empty
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet wid) r

    it "ADDRESS_LIST_01 - Can list known addresses on a default wallet" $ \ctx -> runResourceT $ do
        let g = fromIntegral $ getAddressPoolGap defaultAddressPoolGap
        w <- emptyWallet ctx
        r <- request @[ApiAddress n] ctx
            (Link.listAddresses @'Shelley w) Default Empty
        expectResponseCode HTTP.status200 r
        expectListSize g r
        forM_ [0..(g-1)] $ \addrNum -> do
            expectListField addrNum (#state . #getApiT) (`shouldBe` Unused) r

    it "ADDRESS_LIST_01 - Can list addresses with non-default pool gap" $ \ctx -> runResourceT $ do
        let g = 15
        w <- emptyWalletWith ctx ("Wallet", "cardano-wallet", g)
        r <- request @[ApiAddress n] ctx
            (Link.listAddresses @'Shelley w) Default Empty
        expectResponseCode HTTP.status200 r
        expectListSize g r
        forM_ [0..(g-1)] $ \addrNum -> do
            expectListField addrNum (#state . #getApiT) (`shouldBe` Unused) r

    it "ADDRESS_LIST_02 - Can filter used and unused addresses" $ \ctx -> runResourceT $ do
        let g = fromIntegral $ getAddressPoolGap defaultAddressPoolGap
        w <- fixtureWallet ctx
        rUsed <- request @[ApiAddress n] ctx
            (Link.listAddresses' @'Shelley w (Just Used)) Default Empty
        expectResponseCode HTTP.status200 rUsed
        expectListSize 10 rUsed
        forM_ [0..9] $ \addrNum -> do
            expectListField
                addrNum (#state . #getApiT) (`shouldBe` Used) rUsed
        rUnused <- request @[ApiAddress n] ctx
            (Link.listAddresses' @'Shelley w (Just Unused)) Default Empty
        expectResponseCode HTTP.status200 rUnused
        expectListSize g rUnused
        forM_ [10..(g-1)] $ \addrNum -> do
            expectListField
                addrNum (#state . #getApiT) (`shouldBe` Unused) rUnused

    it "ADDRESS_LIST_02 - Shows nothing when there are no used addresses"
        $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        rUsed <- request @[ApiAddress n] ctx
            (Link.listAddresses' @'Shelley w (Just Used)) Default Empty
        rUnused <- request @[ApiAddress n] ctx
            (Link.listAddresses' @'Shelley w (Just Unused)) Default Empty
        expectResponseCode HTTP.status200 rUsed
        expectListSize 0 rUsed
        expectResponseCode HTTP.status200 rUnused
        expectListSize 20 rUnused
        forM_ [0..19] $ \addrNum -> do
            expectListField
                addrNum (#state . #getApiT) (`shouldBe` Unused) rUnused

    -- TODO
    -- MOVE TO test/unit/Cardano/Wallet/ApiSpec.hs
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
        forM_ filters $ \fil -> it fil $ \ctx -> runResourceT $ do
            w <- emptyWallet ctx
            let link = withQuery fil $ Link.listAddresses @'Shelley w
            r <- request @[ApiAddress n] ctx link Default Empty
            verify r
                [ expectResponseCode HTTP.status400
                , expectErrorMessage
                    "Error parsing query parameter state failed: Unable to\
                    \ decode the given text value. Please specify\
                    \ one of the following values: used, unused."
                ]

    it "ADDRESS_LIST_03 - Generates new address pool gap" $ \ctx -> runResourceT $ do
        let initPoolGap = 10
        wSrc <- fixtureWallet ctx
        wDest <- emptyWalletWith ctx ("Wallet", "cardano-wallet", initPoolGap)

        -- make sure all addresses in address_pool_gap are 'Unused'
        r <- request @[ApiAddress n] ctx
            (Link.listAddresses @'Shelley wDest) Default Empty
        verify r
            [ expectResponseCode HTTP.status200
            , expectListSize initPoolGap
            ]
        forM_ [0..9] $ \addrNum -> do
            expectListField addrNum (#state . #getApiT) (`shouldBe` Unused) r
        addrs <- listAddresses @n ctx wDest

        -- run 10 transactions to make all addresses `Used`
        forM_ [0..9] $ \addrNum -> do
            let destination = (addrs !! addrNum) ^. #id
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

            rTrans <- request @(ApiTransaction n) ctx
                (Link.createTransaction @'Shelley wSrc) Default payload
            expectResponseCode HTTP.status202 rTrans

        -- make sure all transactions are in ledger
        eventually "Wallet balance = initPoolGap * minUTxOValue" $ do
            rb <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            expectField
                (#balance . #getApiT . #available)
                (`shouldBe` Quantity (10 * 1_000_000))
                rb

        -- verify new address_pool_gap has been created
        rAddr <- request @[ApiAddress n] ctx
            (Link.listAddresses @'Shelley wDest) Default Empty
        verify rAddr
            [ expectResponseCode HTTP.status200
            , expectListSize 20
            ]
        forM_ [0..9] $ \addrNum -> do
            expectListField
                addrNum (#state . #getApiT) (`shouldBe` Used) rAddr
        forM_ [10..19] $ \addrNum -> do
            expectListField
                addrNum (#state . #getApiT) (`shouldBe` Unused) rAddr

    it "ADDRESS_LIST_04 - Deleted wallet" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx
            (Link.deleteWallet @'Shelley w) Default Empty
        r <- request @[ApiAddress n] ctx
            (Link.listAddresses @'Shelley w) Default Empty
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    it "ADDRESS_LIST_05 - bech32 HRP is correct - mainnet" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        r <- request @[Aeson.Value] ctx
            (Link.listAddresses @'Shelley w) Default Empty
        verify r
            [ expectResponseCode HTTP.status200
            -- integration tests are configured for mainnet
            , expectListField 0 (Aeson.key "id" . Aeson._String)
                (`shouldSatisfy` T.isPrefixOf "addr")
            , expectListField 0 (Aeson.key "id" . Aeson._String)
                (`shouldNotSatisfy` T.isPrefixOf "addr_test")
            ]

    it "ADDRESS_INSPECT_01 - Address inspect OK" $ \ctx -> do
        let str = "Ae2tdPwUPEYz6ExfbWubiXPB6daUuhJxikMEb4eXRp5oKZBKZwrbJ2k7EZe"
        r <- request @Aeson.Value ctx (Link.inspectAddress str) Default Empty
        expectResponseCode HTTP.status200 r

    it "ADDRESS_INSPECT_02 - Address inspect KO" $ \ctx -> runResourceT $ do
        let str = "patate"
        r <- request @Aeson.Value ctx (Link.inspectAddress str) Default Empty
        expectResponseCode HTTP.status400 r

    it "ANY_ADDRESS_POST_01 - Golden tests for enterprise script address - signature" $ \ctx -> do
        let payload = Json [json|{
                "spending": {
                    "script": "script_vkh1yf07000d4ml3ywd3d439kmwp07xzgv6p35cwx8h605jfx0dtd4a"
                    }
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status201 r

    it "ANY_ADDRESS_POST_02 - Golden tests for enterprise script address - any" $ \ctx -> do
        let payload = Json [json|{
                "spending": {
                    "script": {
                        "any": [
                            "script_vkh1yf07000d4ml3ywd3d439kmwp07xzgv6p35cwx8h605jfx0dtd4a",
                            "script_vkh1mwlngj4fcwegw53tdmyemfupen2758xwvudmcz9ap8cnqk7jmh4"
                            ]
                        }
                    }
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status201 r

    it "ANY_ADDRESS_POST_03 - Golden tests for enterprise script address - all" $ \ctx -> do
        let payload = Json [json|{
                "spending": {
                    "script" : {
                        "all": [
                            "script_vkh1yf07000d4ml3ywd3d439kmwp07xzgv6p35cwx8h605jfx0dtd4a",
                            "script_vkh1mwlngj4fcwegw53tdmyemfupen2758xwvudmcz9ap8cnqk7jmh4"
                            ]
                        }
                    }
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status201 r

    it "ANY_ADDRESS_POST_04 - Golden tests for enterprise script address - some" $ \ctx -> do
        let payload = Json [json|{
                "spending": {
                    "script": {
                        "some": {
                            "from" : [
                                "script_vkh1yf07000d4ml3ywd3d439kmwp07xzgv6p35cwx8h605jfx0dtd4a",
                                "script_vkh1yf07000d4ml3ywd3d439kmwp07xzgv6p35cwx8h605jfx0dtd4a",
                                "script_vkh1qw4l62k4203dllrk3dk3sfjpnh3gufhtrtm4qvtrvn4xjp5x5rt"
                                ],
                             "at_least": 2
                             }
                        }
                    }
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status201 r
