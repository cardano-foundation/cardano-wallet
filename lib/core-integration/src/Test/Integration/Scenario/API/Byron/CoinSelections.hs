{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.API.Byron.CoinSelections
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( AddressAmount (..)
    , ApiByronWallet
    , ApiCoinSelectionOutput (..)
    , ApiT (..)
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DerivationIndex (..), DerivationType (..), Index (..), PaymentAddress )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( coinTypeAda, purposeBIP44 )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.List
    ( isPrefixOf )
import Data.List.NonEmpty
    ( NonEmpty ((:|)) )
import Data.Quantity
    ( Quantity (..) )
import Test.Hspec
    ( SpecWith, describe, shouldSatisfy )
import Test.Hspec.Expectations.Lifted
    ( shouldBe )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , emptyIcarusWallet
    , emptyRandomWallet
    , emptyWallet
    , expectErrorMessage
    , expectField
    , expectResponseCode
    , fixtureIcarusWallet
    , listAddresses
    , minUTxOValue
    , request
    , runResourceT
    , selectCoins
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( errMsg403NotAnIcarusWallet, errMsg404NoWallet )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.HashSet as Set
import qualified Data.List.NonEmpty as NE
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall n.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    , PaymentAddress n IcarusKey
    , PaymentAddress n ByronKey
    ) => SpecWith Context
spec = describe "BYRON_COIN_SELECTION" $ do

    it "BYRON_COIN_SELECTION_00 - \
        \No coin selection on Byron random" $ \ctx -> runResourceT $ do
        rnW <- emptyRandomWallet ctx
        shW <- emptyWallet ctx
        (addr:_) <- fmap (view (#address . #id)) <$> listAddresses @n ctx shW
        let payments = NE.fromList [ AddressAmount addr (Quantity minUTxOValue) mempty ]
        selectCoins @_ @'Byron ctx rnW payments >>= flip verify
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403NotAnIcarusWallet
            ]

    it "BYRON_COIN_SELECTION_01 - \
        \A singleton payment is included in the coin selection output." $
        \ctx -> runResourceT $ do
            source <- fixtureIcarusWallet ctx
            target <- emptyWallet ctx
            targetAddress : _ <- fmap (view (#address . #id)) <$> listAddresses @n ctx target
            let amt = Quantity minUTxOValue
            let payment = AddressAmount targetAddress amt mempty
            let output = ApiCoinSelectionOutput targetAddress amt mempty
            let isValidDerivationPath path =
                    ( length path == 5 )
                    &&
                    ( [ ApiT $ DerivationIndex $ getIndex purposeBIP44
                      , ApiT $ DerivationIndex $ getIndex coinTypeAda
                      , ApiT $ DerivationIndex $ getIndex @'Hardened minBound
                      ] `isPrefixOf` NE.toList path
                    )
            selectCoins @_ @'Byron ctx source (payment :| []) >>= flip verify
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

    it "BYRON_COIN_SELECTION_02 - \
        \Multiple payments are all included in the coin selection output." $
        \ctx -> runResourceT $ do
            let paymentCount = 10
            source <- fixtureIcarusWallet ctx
            target <- emptyWallet ctx
            targetAddresses <- fmap (view (#address . #id)) <$> listAddresses @n ctx target
            let amounts = Quantity <$> [minUTxOValue ..]
            let targetAssets = repeat mempty
            let payments = NE.fromList
                    $ take paymentCount
                    $ map ($ mempty)
                    $ zipWith AddressAmount targetAddresses amounts
            let outputs = take paymentCount $ zipWith3 ApiCoinSelectionOutput
                    targetAddresses amounts targetAssets
            selectCoins @_ @'Byron ctx source payments >>= flip verify
                [ expectResponseCode HTTP.status200
                , expectField #inputs (`shouldSatisfy` (not . null))
                , expectField #change (`shouldSatisfy` (not . null))
                , expectField #outputs
                    (`shouldSatisfy` ((Set.fromList outputs ==) . Set.fromList))
                ]

    it "BYRON_COIN_SELECTION_03 - \
        \Deleted wallet is not available for selection" $ \ctx -> runResourceT $ do
        icW <- emptyIcarusWallet ctx
        shW <- emptyWallet ctx
        (addr:_) <- fmap (view (#address . #id)) <$> listAddresses @n ctx shW
        let payments = NE.fromList [ AddressAmount addr (Quantity minUTxOValue) mempty ]
        _ <- request @ApiByronWallet ctx (Link.deleteWallet @'Byron icW) Default Empty
        selectCoins @_ @'Byron ctx icW payments >>= flip verify
            [ expectResponseCode HTTP.status404
            , expectErrorMessage (errMsg404NoWallet $ icW ^. walletId)
            ]
