{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.API.Shelley.CoinSelections
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( AddressAmount (..)
    , ApiCoinSelection
    , ApiCoinSelectionOutput (..)
    , ApiT (..)
    , ApiWallet
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DerivationIndex (..)
    , DerivationType (..)
    , Index (..)
    , PaymentAddress
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( coinTypeAda, purposeCIP1852 )
import Control.Monad
    ( forM_ )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.List
    ( isPrefixOf )
import Data.List.NonEmpty
    ( NonEmpty ((:|)) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( toText )
import Test.Hspec
    ( SpecWith, describe, shouldBe, shouldSatisfy )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , emptyWallet
    , eventually
    , expectErrorMessage
    , expectField
    , expectResponseCode
    , fixtureWallet
    , fixtureWalletWith
    , json
    , listAddresses
    , minUTxOValue
    , request
    , selectCoins
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( errMsg404NoWallet
    , errMsg406
    , errMsg415
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Network.HTTP.Types as HTTP

spec :: forall n t.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    , PaymentAddress n ShelleyKey
    , PaymentAddress n IcarusKey
    , PaymentAddress n ByronKey
    ) => SpecWith (Context t)
spec = describe "SHELLEY_COIN_SELECTION" $ do

    it "WALLETS_COIN_SELECTION_01 - \
        \A singleton payment is included in the coin selection output." $
        \ctx -> do
            source <- fixtureWallet ctx
            target <- emptyWallet ctx
            targetAddress : _ <- fmap (view #id) <$> listAddresses @n ctx target
            let amount = Quantity minUTxOValue
            let payment = AddressAmount targetAddress amount
            let output = ApiCoinSelectionOutput targetAddress amount
            let isValidDerivationPath path =
                    ( length path == 5 )
                    &&
                    ( [ ApiT $ DerivationIndex $ getIndex purposeCIP1852
                      , ApiT $ DerivationIndex $ getIndex coinTypeAda
                      , ApiT $ DerivationIndex $ getIndex @'Hardened minBound
                      ] `isPrefixOf` NE.toList path
                    )
            selectCoins @_ @'Shelley ctx source (payment :| []) >>= flip verify
                [ expectResponseCode HTTP.status200
                , expectField #inputs
                    (`shouldSatisfy` (not . null))
                , expectField #inputs
                    (`shouldSatisfy` all
                        (isValidDerivationPath . view #derivationPath))
                , expectField #change
                    (`shouldSatisfy` (not . null))
                , expectField #change
                    (`shouldSatisfy` all
                        (isValidDerivationPath . view #derivationPath))
                , expectField #outputs
                    (`shouldBe` [output])
                ]

    it "WALLETS_COIN_SELECTION_02 - \
        \Multiple payments are all included in the coin selection output." $
        \ctx -> do
            let paymentCount = 10
            source <- fixtureWallet ctx
            target <- emptyWallet ctx
            targetAddresses <- fmap (view #id) <$> listAddresses @n ctx target
            let amounts = Quantity <$> [minUTxOValue ..]
            let payments = NE.fromList
                    $ take paymentCount
                    $ zipWith AddressAmount targetAddresses amounts
            let outputs =
                    take paymentCount
                    $ zipWith ApiCoinSelectionOutput targetAddresses amounts
            selectCoins @_ @'Shelley ctx source payments >>= flip verify
                [ expectResponseCode HTTP.status200
                , expectField #inputs (`shouldSatisfy` (not . null))
                , expectField #change (`shouldSatisfy` (not . null))
                , expectField
                    #outputs (`shouldSatisfy` ((L.sort outputs ==) . L.sort))
                ]

    it "WALLETS_COIN_SELECTION_03 - \
        \Deleted wallet is not available for selection" $ \ctx -> do
        w <- emptyWallet ctx
        (addr:_) <- fmap (view #id) <$> listAddresses @n ctx w
        let payments = NE.fromList [ AddressAmount addr (Quantity minUTxOValue) ]
        _ <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
        selectCoins @_ @'Shelley ctx w payments >>= flip verify
            [ expectResponseCode @IO HTTP.status404
            , expectErrorMessage (errMsg404NoWallet $ w ^. walletId)
            ]

    it "WALLETS_COIN_SELECTION_03 - \
        \Wrong selection method (not 'random')" $ \ctx -> do
        w <- fixtureWallet ctx
        (addr:_) <- fmap (view #id) <$> listAddresses @n ctx w
        let payments = NE.fromList [ AddressAmount addr (Quantity minUTxOValue) ]
        let payload = Json [json| { "payments": #{payments} } |]
        let wid = toText $ getApiT $ w ^. #id
        let endpoints = ("POST",) . mconcat <$>
                [ [ "v2/wallets/", wid, "/coin-selections/largest-first" ]
                , [ "v2/wallets/", wid, "/coin-selections" ]
                ]
        forM_ endpoints $ \endpoint -> do
            r <- request @(ApiCoinSelection n) ctx endpoint Default payload
            verify r [ expectResponseCode @IO HTTP.status404 ]

    describe "WALLETS_COIN_SELECTION_04 - HTTP headers" $ do
        let matrix =
                [ ( "No HTTP headers -> 415"
                  , None
                  , [ expectResponseCode @IO HTTP.status415
                    , expectErrorMessage errMsg415
                    ]
                  )
                , ( "Accept: text/plain -> 406"
                  , Headers
                        [ ("Content-Type", "application/json")
                        , ("Accept", "text/plain")
                        ]
                  , [ expectResponseCode @IO HTTP.status406
                    , expectErrorMessage errMsg406
                    ]
                  )
                , ( "No Accept -> 200"
                  , Headers [ ("Content-Type", "application/json") ]
                  , [ expectResponseCode @IO HTTP.status200 ]
                  )
                , ( "No Content-Type -> 415"
                  , Headers [ ("Accept", "application/json") ]
                  , [ expectResponseCode @IO HTTP.status415
                    , expectErrorMessage errMsg415
                    ]
                  )
                , ( "Content-Type: text/plain -> 415"
                  , Headers [ ("Content-Type", "text/plain") ]
                  , [ expectResponseCode @IO HTTP.status415
                    , expectErrorMessage errMsg415
                    ]
                  )
                ]
        forM_ matrix $ \(title, headers, expectations) -> it title $ \ctx -> do
            w <- fixtureWallet ctx
            (addr:_) <- fmap (view #id) <$> listAddresses @n ctx w
            let payments = NE.fromList [ AddressAmount addr (Quantity minUTxOValue) ]
            let payload = Json [json| { "payments": #{payments} } |]
            r <- request @(ApiCoinSelection n) ctx
                (Link.selectCoins @'Shelley w) headers payload
            verify r expectations

    it "WALLETS_COIN_SELECTION_05 - \
        \No change when payment fee eats leftovers due to minUTxOValue" $
        \ctx -> do
            source  <- fixtureWalletWith @n ctx [minUTxOValue, minUTxOValue]
            eventually "Source wallet balance is as expected" $ do
                rGet <- request @ApiWallet ctx
                    (Link.getWallet @'Shelley source) Default Empty
                verify rGet
                    [ expectField
                            (#balance . #getApiT . #total)
                            (`shouldBe` Quantity (2 * minUTxOValue))
                    , expectField
                            (#balance . #getApiT . #available)
                            (`shouldBe` Quantity (2 * minUTxOValue))
                    ]
            target <- emptyWallet ctx

            targetAddress:_ <- fmap (view #id) <$> listAddresses @n ctx target
            let amount = Quantity minUTxOValue
            let payment = AddressAmount targetAddress amount
            let output = ApiCoinSelectionOutput targetAddress amount
            let isValidDerivationPath path =
                    ( length path == 5 )
                    &&
                    ( [ ApiT $ DerivationIndex $ getIndex purposeCIP1852
                      , ApiT $ DerivationIndex $ getIndex coinTypeAda
                      , ApiT $ DerivationIndex $ getIndex @'Hardened minBound
                      ] `isPrefixOf` NE.toList path
                    )
            selectCoins @_ @'Shelley ctx source (payment :| []) >>= flip verify
                [ expectResponseCode HTTP.status200
                , expectField #inputs
                    (`shouldSatisfy` (not . null))
                , expectField #inputs
                    (`shouldSatisfy` all
                        (isValidDerivationPath . view #derivationPath))
                , expectField #change
                    (`shouldSatisfy` null)
                , expectField #outputs
                    (`shouldBe` [output])
                ]
