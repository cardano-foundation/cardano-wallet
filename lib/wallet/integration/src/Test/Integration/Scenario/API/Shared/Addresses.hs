{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Integration.Scenario.API.Shared.Addresses
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Address.Derivation.SharedKey
    ( purposeCIP1854
    )
import Cardano.Wallet.Address.Discovery.Sequential
    ( defaultAddressPoolGap
    , getAddressPoolGap
    )
import Cardano.Wallet.Api.Types
    ( ApiAddressWithPath
    , ApiSharedWallet (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId
    )
import Cardano.Wallet.Primitive.Types.Address
    ( AddressState (..)
    )
import Control.Monad
    ( forM_
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Monad.Trans.Resource
    ( runResourceT
    )
import Data.Text
    ( Text
    )
import Test.Hspec
    ( SpecWith
    , describe
    , shouldBe
    , shouldSatisfy
    )
import Test.Hspec.Extra
    ( rit
    )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , expectListField
    , expectListSize
    , expectResponseCode
    , genXPubsBech32
    , getFromResponse
    , isValidDerivationPath
    , json
    , postSharedWallet
    , request
    , verify
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Network.HTTP.Types as HTTP

spec :: forall n. HasSNetworkId n => SpecWith Context
spec = describe "SHARED_ADDRESSES" $ do
    rit "SHARED_ADDRESSES_LIST_01 - Can list known addresses on a default wallet" $ \ctx -> runResourceT $ do
        let walName = "Shared Wallet" :: Text
        (_, payload) <- getAccountWallet walName
        rPost <- postSharedWallet ctx Default payload
        verify
            rPost
            [ expectResponseCode HTTP.status201
            ]
        let (ApiSharedWallet (Right wal)) = getFromResponse id rPost

        r <-
            request @[ApiAddressWithPath n]
                ctx
                (Link.listAddresses @'Shared wal)
                Default
                Empty
        expectResponseCode HTTP.status200 r
        let g = fromIntegral $ getAddressPoolGap defaultAddressPoolGap
        expectListSize g r
        forM_ [0 .. (g - 1)] $ \addrNum -> do
            expectListField addrNum (#state . #getApiT) (`shouldBe` Unused) r
            expectListField
                addrNum
                #derivationPath
                (`shouldSatisfy` (isValidDerivationPath purposeCIP1854))
                r

    rit "SHARED_ADDRESSES_LIST_02 - Can list known addresses on a pending wallet" $ \ctx -> runResourceT $ do
        (_, accXPubTxt) : _ <- liftIO $ genXPubsBech32 1
        let payload =
                Json
                    [json| {
                "name": "Shared Wallet",
                "account_public_key": #{accXPubTxt},
                "account_index": "10H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": #{accXPubTxt} },
                      "template":
                          { "all":
                             [ "cosigner#0",
                               "cosigner#1",
                               { "active_from": 120 }
                             ]
                          }
                    }
                } |]
        rPost <- postSharedWallet ctx Default payload
        verify
            rPost
            [ expectResponseCode HTTP.status201
            ]
        let (ApiSharedWallet (Left wal)) = getFromResponse id rPost

        r <-
            request @[ApiAddressWithPath n]
                ctx
                (Link.listAddresses @'Shared wal)
                Default
                Empty
        expectResponseCode HTTP.status200 r
        expectListSize 0 r
  where
    getAccountWallet name = do
        (_, accXPubTxt) : _ <- liftIO $ genXPubsBech32 1
        -- NOTE: A previous test had used "account_index": "30H",
        -- presumably to spice things up,
        -- but the `isValidDerivationPath` function expects that the
        -- account index is equal to "0H".
        let payload =
                Json
                    [json| {
                  "name": #{name},
                  "account_public_key": #{accXPubTxt},
                  "account_index": "0H",
                  "payment_script_template":
                      { "cosigners":
                          { "cosigner#0": #{accXPubTxt} },
                        "template":
                            { "all":
                               [ "cosigner#0",
                                 { "active_from": 120 }
                               ]
                            }
                      }
                  } |]
        return (accXPubTxt, payload)
