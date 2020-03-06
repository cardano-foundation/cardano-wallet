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

import Cardano.Wallet.Api.Link
    ( Discriminate )
import Cardano.Wallet.Api.Types
    ( ApiByronWallet
    , ApiT (..)
    , ApiTransaction
    , EncodeAddress (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Mnemonic
    ( Mnemonic )
import Cardano.Wallet.Primitive.Types
    ( Address, Direction (..), TxStatus (..), WalletId )
import Data.Aeson
    ( FromJSON )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Product.Typed
    ( HasType )
import Data.Quantity
    ( Quantity (..) )
import GHC.Generics
    ( Generic )
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
    , fixturePassphrase
    , fixtureRandomWallet
    , icarusAddresses
    , json
    , randomAddresses
    , request
    , verify
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall t n. (n ~ 'Mainnet) => SpecWith (Context t)
spec = describe "BYRON_TXS" $ do
    scenario_TRANS_CREATE_01 @'Byron fixtureRandomWallet (randomAddresses @n)
    scenario_TRANS_CREATE_01 @'Byron fixtureIcarusWallet (icarusAddresses @n)

--
-- Scenarios
--

scenario_TRANS_CREATE_01
    :: forall style t n mw wallet.
        ( n ~ 'Mainnet
        , Discriminate style
        , HasType (ApiT WalletId) wallet
        , FromJSON wallet
        , Generic wallet
        , Show wallet
        )
    => (Context t -> IO (wallet, Mnemonic mw))
    -> (Mnemonic mw -> [Address])
    -> SpecWith (Context t)
scenario_TRANS_CREATE_01 fixtureWallet fixtureAddresses =
  it "TRANS_CREATE_01 - Single Output Transaction" $ \ctx -> do
    -- SETUP
    (wSrc, _)   <- fixtureWallet ctx
    (wDest, mw) <- fixtureWallet ctx

    -- ACTION
    let addr = encodeAddress @n $ head $ fixtureAddresses mw
    let amnt = 1 :: Natural
    let body = [json|{
            "payments": [{
                "address": #{addr},
                "amount": {
                    "quantity": #{amnt},
                    "unit": "lovelace"
                }
            }],
            "passphrase": #{fixturePassphrase}
        }|]
    r <- request @(ApiTransaction n) ctx
        (Link.createTransaction @style wSrc) Default (Json body)

    -- ASSERTIONS
    let (feeMin, feeMax) = ctx ^. #_feeEstimator $ PaymentDescription
            { nInputs  = 1
            , nOutputs = 1
            , nChanges = 1
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

    eventually "destination balance increases" $ do
        rDest <- request @ApiByronWallet ctx
            (Link.getWallet @'Byron wDest) Default Empty
        verify rDest
            [ expectField (#balance . #available)
                (`shouldBe` Quantity (faucetAmt + amnt))
            ]
