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

import Cardano.Wallet.Address.Discovery.Sequential
    ( purposeBIP44
    )
import Cardano.Wallet.Api.Types
    ( AddressAmount (..)
    , ApiByronWallet
    , ApiCoinSelectionOutput (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Api.Types.Amount
    ( ApiAmount (ApiAmount)
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId
    )
import Data.Generics.Internal.VL.Lens
    ( view
    , (^.)
    )
import Data.List.NonEmpty
    ( NonEmpty ((:|))
    )
import Test.Hspec
    ( SpecWith
    , describe
    , shouldSatisfy
    )
import Test.Hspec.Expectations.Lifted
    ( shouldBe
    )
import Test.Hspec.Extra
    ( it
    )
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
    , isValidDerivationPath
    , listAddresses
    , minUTxOValue
    , request
    , runResourceT
    , selectCoins
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( errMsg403NotAnIcarusWallet
    , errMsg404NoWallet
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.HashSet as Set
import qualified Data.List.NonEmpty as NE
import qualified Network.HTTP.Types.Status as HTTP

spec
    :: forall n
     . HasSNetworkId n
    => SpecWith Context
spec = describe "BYRON_COIN_SELECTION" $ do

    it "BYRON_COIN_SELECTION_00 - \
        \No coin selection on Byron random" $ \ctx -> runResourceT $ do
        rnW <- emptyRandomWallet ctx
        shW <- emptyWallet ctx
        (addr:_) <- fmap (view #id) <$> listAddresses @n ctx shW
        let amt = ApiAmount . minUTxOValue . _mainEra $ ctx
        let payments = pure (AddressAmount addr amt mempty)
        selectCoins @_ @'Byron ctx rnW payments >>= flip verify
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403NotAnIcarusWallet
            ]

    it "BYRON_COIN_SELECTION_01 - \
        \A singleton payment is included in the coin selection output." $
        \ctx -> runResourceT $ do
            source <- fixtureIcarusWallet ctx
            target <- emptyWallet ctx
            targetAddress : _ <- fmap (view #id) <$> listAddresses @n ctx target
            let amt = ApiAmount . minUTxOValue $ _mainEra ctx
            let payment = AddressAmount targetAddress amt mempty
            let output = ApiCoinSelectionOutput targetAddress amt mempty
            selectCoins @_ @'Byron ctx source (payment :| []) >>= flip verify
                [ expectResponseCode HTTP.status200
                , expectField #inputs
                    (`shouldSatisfy` (not . null))
                , expectField #inputs
                    (`shouldSatisfy` all
                        (isValidDerivationPath purposeBIP44 . view #derivationPath))
                , expectField #change
                    (`shouldSatisfy` (not . null))
                , expectField #change
                    (`shouldSatisfy` all
                        (isValidDerivationPath purposeBIP44 . view #derivationPath))
                , expectField #outputs
                    (`shouldBe` [output])
                ]

    it "BYRON_COIN_SELECTION_02 - \
        \Multiple payments are all included in the coin selection output." $
        \ctx -> runResourceT $ do
            let paymentCount = 10
            source <- fixtureIcarusWallet ctx
            target <- emptyWallet ctx
            targetAddresses <- fmap (view #id) <$> listAddresses @n ctx target
            let amounts = ApiAmount <$> [minUTxOValue (_mainEra ctx) ..]
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
        (addr:_) <- fmap (view #id) <$> listAddresses @n ctx shW
        let minUTxOValue' = ApiAmount . minUTxOValue $ _mainEra ctx
        let payments = pure (AddressAmount addr minUTxOValue' mempty)
        _ <- request @ApiByronWallet ctx (Link.deleteWallet @'Byron icW) Default Empty
        selectCoins @_ @'Byron ctx icW payments >>= flip verify
            [ expectResponseCode HTTP.status404
            , expectErrorMessage (errMsg404NoWallet $ icW ^. walletId)
            ]
