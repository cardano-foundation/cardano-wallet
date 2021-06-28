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

import Cardano.Wallet.Api.Types
    ( ApiCoinSelectionInput (..)
    , ApiConstructTransaction
    , ApiFee (..)
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
import Control.Monad.IO.Unlift
    ( MonadIO (..), MonadUnliftIO (..), liftIO )
import Control.Monad.Trans.Resource
    ( runResourceT )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
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
    , emptyWallet
    , expectField
    , expectResponseCode
    , expectSuccess
    , fixturePassphrase
    , fixtureWalletWith
    , getFromResponse
    , json
    , listAddresses
    , minUTxOValue
    , request
    , unsafeRequest
    , verify
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
    it "TRANS_NEW_CREATE_01x - Single Output Transaction" $ \ctx -> runResourceT $ do
        let initialAmt = 3*minUTxOValue
        wa <- fixtureWalletWith @n ctx [initialAmt]
        wb <- emptyWallet ctx
        let amt = (minUTxOValue :: Natural)

        payload <- liftIO $ mkTxPayload ctx wb amt fixturePassphrase

        (_, ApiFee (Quantity feeMin) _ _ _) <- unsafeRequest ctx
            (Link.getTransactionFee @'Shelley wa) payload
        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#fee . #getQuantity) (`shouldBe` feeMin)
            ]

        let filterInitialAmt =
                filter (\(ApiCoinSelectionInput _ _ _ _ amt' _) -> amt' == Quantity initialAmt)
        let coinSelInputs = filterInitialAmt $ NE.toList $
                getFromResponse (#coinSelection . #inputs) rTx
        length coinSelInputs `shouldBe` 1

        -- now we should sign it and send it in two steps
  where
    -- Construct a JSON payment request for the given quantity of lovelace.
    mkTxPayload
        :: MonadUnliftIO m
        => Context
        -> ApiWallet
        -> Natural
        -> Text
        -> m Payload
    mkTxPayload ctx wDest amt passphrase = do
        addrs <- listAddresses @n ctx wDest
        let destination = (addrs !! 1) ^. #id
        return $ Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": #{passphrase}
            }|]
