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

module Test.Integration.Scenario.API.Byron.TransactionsShelley
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiByronWallet
    , ApiFee
    , ApiTransaction
    , ApiWallet
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.Types
    ( Direction (..), TxStatus (..) )
import Control.Monad
    ( forM_ )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Network.HTTP.Types.Method
    ( Method )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith, describe )
import Test.Hspec.Expectations.Lifted
    ( shouldBe )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , Payload (..)
    , between
    , eventually
    , expectField
    , expectResponseCode
    , expectSuccess
    , faucetAmt
    , faucetUtxoAmt
    , fixtureIcarusWallet
    , fixturePassphrase
    , fixtureRandomWallet
    , fixtureWallet
    , getFromResponse
    , json
    , listAddresses
    , request
    , verify
    , (.>=)
    )
import Test.Integration.Framework.Request
    ( RequestException )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall n t.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    ) => SpecWith (Context t)
spec = do
  describe "BYRON_TRANS_SHELLEY_01 - Single Output Transaction with non-Shelley witnesses" $
      forM_ [(fixtureRandomWallet, "Byron wallet"), (fixtureIcarusWallet, "Icarus wallet")] $
      \(srcFixture,name) -> it name $ \ctx -> do

      (wByron, wShelley) <- (,) <$> srcFixture ctx <*> fixtureWallet ctx
      addrs <- listAddresses @n ctx wShelley

      let amt = 1
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

      eventually "wByron and wShelley balances are as expected" $ do
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
  where

    postTx
        :: Context t
        -> (wal, wal -> (Method, Text), Text)
        -> ApiWallet
        -> Natural
        -> IO (HTTP.Status, Either RequestException (ApiTransaction n))
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
