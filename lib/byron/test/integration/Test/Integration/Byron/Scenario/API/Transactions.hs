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

module Test.Integration.Byron.Scenario.API.Transactions
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiByronWallet
    , ApiT (..)
    , ApiTransaction
    , EncodeAddress (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( Address, Direction (..), TxStatus (..) )
import Control.Monad
    ( forM, forM_ )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Quantity
    ( Quantity (..) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith, describe, it, shouldBe )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , Payload (..)
    , TxDescription (..)
    , between
    , eventually
    , expectField
    , expectResponseCode
    , faucetAmt
    , fixtureIcarusWallet
    , fixtureIcarusWalletAddrs
    , fixturePassphrase
    , fixtureRandomWallet
    , fixtureRandomWalletAddrs
    , json
    , request
    , verify
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall t n. (n ~ 'Mainnet) => SpecWith (Context t)
spec = describe "BYRON_TXS" $ do
    -- Random → Random
    scenario_TRANS_CREATE_01_02 fixtureRandomWallet
        [ fixtureRandomWalletAddrs @n
        ]

    -- Random → [Random, Icarus]
    scenario_TRANS_CREATE_01_02 fixtureRandomWallet
        [ fixtureRandomWalletAddrs @n
        , fixtureIcarusWalletAddrs @n
        ]

    -- Icarus → Icarus
    scenario_TRANS_CREATE_01_02 fixtureIcarusWallet
        [ fixtureIcarusWalletAddrs @n
        ]

    -- Icarus → [Icarus, Random]
    scenario_TRANS_CREATE_01_02 fixtureRandomWallet
        [ fixtureIcarusWalletAddrs @n
        , fixtureRandomWalletAddrs @n
        ]


--
-- Scenarios
--

scenario_TRANS_CREATE_01_02
    :: forall t n.
        ( n ~ 'Mainnet
        )
    => (Context t -> IO ApiByronWallet)
    -> [Context t -> IO (ApiByronWallet, [Address])]
    -> SpecWith (Context t)
scenario_TRANS_CREATE_01_02 fixtureSource fixtures = it title $ \ctx -> do
    -- SETUP
    let amnt = 1 :: Natural
    wSrc <- fixtureSource ctx
    (recipients, payments) <- fmap unzip $ forM fixtures $ \fixtureTarget -> do
        (wDest, addrs) <- fixtureTarget ctx
        let addr = encodeAddress @n $ head addrs
        pure (wDest, [json|
            { "address": #{addr}
            , "amount":
                { "quantity": #{amnt}
                , "unit": "lovelace"
                }
            }|])


    -- ACTION
    let body = [json|
            { "payments": #{payments}
            , "passphrase": #{fixturePassphrase}
            }|]
    r <- request @(ApiTransaction n) ctx
        (Link.createTransaction @'Byron wSrc) Default (Json body)

    -- ASSERTIONS
    let (feeMin, feeMax) = ctx ^. #_feeEstimator $ PaymentDescription
            { nInputs  = length fixtures
            , nOutputs = length fixtures
            , nChanges = length fixtures
            }
    verify r
        [ expectResponseCode HTTP.status202
        , expectField #amount $ between
              ( Quantity (feeMin + amnt)
              , Quantity (feeMax + amnt)
              )
        , expectField #direction (`shouldBe` ApiT Outgoing)
        , expectField #status (`shouldBe` ApiT Pending)
        ]

    eventually "source balance decreases" $ do
        rSrc <- request @ApiByronWallet ctx
            (Link.getWallet @'Byron wSrc) Default Empty
        verify rSrc
            [ expectField (#balance . #available) $ between
                ( Quantity (faucetAmt - amnt - feeMax)
                , Quantity (faucetAmt - amnt - feeMin)
                )
            ]

    forM_ recipients $ \wDest -> do
        eventually "destination balance increases" $ do
            rDest <- request @ApiByronWallet ctx
                (Link.getWallet @'Byron wDest) Default Empty
            verify rDest
                [ expectField (#balance . #available)
                    (`shouldBe` Quantity (faucetAmt + amnt))
                ]
  where
    title =
        "TRANS_CREATE_01/02 - " ++ show (length fixtures) ++ " recipients"
