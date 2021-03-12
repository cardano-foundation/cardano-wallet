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
{- HLINT ignore "Use camelCase" -}

module Test.Integration.Scenario.API.Shelley.CoinSelections
    ( spec
    ) where

import Prelude

import Cardano.Mnemonic
    ( mnemonicToText )
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
    ( Depth (..)
    , DerivationIndex (..)
    , DerivationType (..)
    , Index (..)
    , Index
    , PaymentAddress
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( coinTypeAda, purposeBIP44, purposeCIP1852 )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..), TokenPolicyId (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( txOutMaxTokenQuantity )
import Control.Monad
    ( forM_ )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.List
    ( isPrefixOf )
import Data.List.NonEmpty
    ( NonEmpty ((:|)) )
import Data.Maybe
    ( isJust )
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
    , addField
    , emptyWallet
    , expectErrorMessage
    , expectField
    , expectResponseCode
    , fixtureWallet
    , json
    , listAddresses
    , minUTxOValue
    , request
    , rewardWallet
    , runResourceT
    , selectCoins
    , selectCoinsWith
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( errMsg400TxMetadataStringTooLong
    , errMsg403TokenQuantityExceedsMaxBound
    , errMsg404NoWallet
    , errMsg406
    , errMsg415
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as TokenQuantity
import qualified Data.HashSet as Set
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Network.HTTP.Types as HTTP

spec :: forall n.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    , PaymentAddress n ShelleyKey
    , PaymentAddress n IcarusKey
    , PaymentAddress n ByronKey
    ) => SpecWith Context
spec = describe "SHELLEY_COIN_SELECTION" $ do

    it "WALLETS_COIN_SELECTION_01 - \
        \A singleton payment is included in the coin selection output." $
        \ctx -> runResourceT $ do
            source <- fixtureWallet ctx
            target <- emptyWallet ctx
            targetAddress : _ <- fmap (view #id) <$> listAddresses @n ctx target
            let amount = Quantity minUTxOValue
            let payment = AddressAmount targetAddress amount mempty
            let output = ApiCoinSelectionOutput targetAddress amount mempty
            selectCoins @_ @'Shelley ctx source (payment :| []) >>= flip verify
                [ expectResponseCode HTTP.status200
                , expectField #inputs
                    (`shouldSatisfy` (not . null))
                , expectField #inputs
                    (`shouldSatisfy` all
                        (isValidDerivationPath purposeCIP1852 . view #derivationPath))
                , expectField #change
                    (`shouldSatisfy` (not . null))
                , expectField #change
                    (`shouldSatisfy` all
                        (isValidDerivationPath purposeCIP1852 . view #derivationPath))
                , expectField #outputs
                    (`shouldBe` [output])
                , expectField #withdrawals
                    (`shouldSatisfy` null)
                , expectField #metadata
                    (`shouldBe` Nothing)
                ]

    it "WALLETS_COIN_SELECTION_02 - \
        \Multiple payments are all included in the coin selection output." $
        \ctx -> runResourceT $ do
            let paymentCount = 10
            source <- fixtureWallet ctx
            target <- emptyWallet ctx
            targetAddresses <- fmap (view #id) <$> listAddresses @n ctx target
            let amounts = Quantity <$> [minUTxOValue ..]
            let assets = repeat mempty
            let payments = NE.fromList
                    $ take paymentCount
                    $ map ($ mempty)
                    $ zipWith AddressAmount targetAddresses amounts
            let outputs = take paymentCount $ zipWith3 ApiCoinSelectionOutput
                    targetAddresses amounts assets
            selectCoins @_ @'Shelley ctx source payments >>= flip verify
                [ expectResponseCode HTTP.status200
                , expectField #inputs (`shouldSatisfy` (not . null))
                , expectField #change (`shouldSatisfy` (not . null))
                , expectField #outputs
                    (`shouldSatisfy` ((Set.fromList outputs ==) . Set.fromList))
                , expectField #withdrawals (`shouldSatisfy` null)
                , expectField #metadata (`shouldBe` Nothing)
                ]

    it "WALLETS_COIN_SELECTION_03 - \
        \Deleted wallet is not available for selection" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        (addr:_) <- fmap (view #id) <$> listAddresses @n ctx w
        let payments = NE.fromList [ AddressAmount addr (Quantity minUTxOValue) mempty ]
        _ <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
        selectCoins @_ @'Shelley ctx w payments >>= flip verify
            [ expectResponseCode HTTP.status404
            , expectErrorMessage (errMsg404NoWallet $ w ^. walletId)
            ]

    it "WALLETS_COIN_SELECTION_03 - \
        \Wrong selection method (not 'random')" $ \ctx -> runResourceT $ do
        w <- fixtureWallet ctx
        (addr:_) <- fmap (view #id) <$> listAddresses @n ctx w
        let payments = NE.fromList [ AddressAmount addr (Quantity minUTxOValue) mempty ]
        let payload = Json [json| { "payments": #{payments} } |]
        let wid = toText $ getApiT $ w ^. #id
        let endpoints = ("POST",) . mconcat <$>
                [ [ "v2/wallets/", wid, "/coin-selections/largest-first" ]
                , [ "v2/wallets/", wid, "/coin-selections" ]
                ]
        forM_ endpoints $ \endpoint -> do
            r <- request @(ApiCoinSelection n) ctx endpoint Default payload
            verify r [ expectResponseCode HTTP.status404 ]

    describe "WALLETS_COIN_SELECTION_04 - HTTP headers" $ do
        let matrix =
                [ ( "No HTTP headers -> 415"
                  , None
                  , [ expectResponseCode HTTP.status415
                    , expectErrorMessage errMsg415
                    ]
                  )
                , ( "Accept: text/plain -> 406"
                  , Headers
                        [ ("Content-Type", "application/json")
                        , ("Accept", "text/plain")
                        ]
                  , [ expectResponseCode HTTP.status406
                    , expectErrorMessage errMsg406
                    ]
                  )
                , ( "No Accept -> 200"
                  , Headers [ ("Content-Type", "application/json") ]
                  , [ expectResponseCode HTTP.status200 ]
                  )
                , ( "No Content-Type -> 415"
                  , Headers [ ("Accept", "application/json") ]
                  , [ expectResponseCode HTTP.status415
                    , expectErrorMessage errMsg415
                    ]
                  )
                , ( "Content-Type: text/plain -> 415"
                  , Headers [ ("Content-Type", "text/plain") ]
                  , [ expectResponseCode HTTP.status415
                    , expectErrorMessage errMsg415
                    ]
                  )
                ]
        forM_ matrix $ \(title, headers, expectations) -> it title $ \ctx -> runResourceT $ do
            w <- fixtureWallet ctx
            (addr:_) <- fmap (view #id) <$> listAddresses @n ctx w
            let payments = NE.fromList [ AddressAmount addr (Quantity minUTxOValue) mempty ]
            let payload = Json [json| { "payments": #{payments} } |]
            r <- request @(ApiCoinSelection n) ctx
                (Link.selectCoins @'Shelley w) headers payload
            verify r expectations

    it "WALLETS_COIN_SELECTION_05a - can include metadata" $ \ctx -> runResourceT $ do
        source <- fixtureWallet ctx
        addr:_ <- fmap (view #id) <$> listAddresses @n ctx source

        let amount = Quantity minUTxOValue
        let payment = AddressAmount addr amount mempty
        let transform = addField "metadata"
                [json|{ "1": { "string": "hello" } }|]

        selectCoinsWith @_ @'Shelley ctx source (payment :| []) transform >>= flip verify
            [ expectResponseCode HTTP.status200
            , expectField #metadata (`shouldSatisfy` isJust)
            ]

    it "WALLETS_COIN_SELECTION_05b - choke on invalid metadata" $ \ctx -> runResourceT $ do
        source <- fixtureWallet ctx
        addr:_ <- fmap (view #id) <$> listAddresses @n ctx source

        let amount = Quantity minUTxOValue
        let payment = AddressAmount addr amount mempty
        let transform = addField "metadata"
                [json|{ "1": { "string": #{T.replicate 65 "a"} } }|]

        selectCoinsWith @_ @'Shelley ctx source (payment :| []) transform >>= flip verify
            [ expectResponseCode HTTP.status400
            , expectErrorMessage errMsg400TxMetadataStringTooLong
            ]

    it "WALLETS_COIN_SELECTION_06a - can redeem rewards from self" $ \ctx -> runResourceT $ do
        (source,_) <- rewardWallet ctx
        addr:_ <- fmap (view #id) <$> listAddresses @n ctx source

        let amount = Quantity minUTxOValue
        let payment = AddressAmount addr amount mempty
        let transform = addField "withdrawal" ("self" :: String)

        selectCoinsWith @_ @'Shelley ctx source (payment :| []) transform >>= flip verify
            [ expectResponseCode HTTP.status200
            , expectField #withdrawals
                (`shouldSatisfy` ((== 1) . length))
            , expectField #withdrawals
                (`shouldSatisfy` all
                    (isValidDerivationPath purposeCIP1852 . view #derivationPath))
            ]

    it "WALLETS_COIN_SELECTION_06b - can redeem rewards from other" $ \ctx -> runResourceT $ do
        (_, mnemonic) <- rewardWallet ctx
        source <- fixtureWallet ctx
        addr:_ <- fmap (view #id) <$> listAddresses @n ctx source

        let amount = Quantity minUTxOValue
        let payment = AddressAmount addr amount mempty
        let transform = addField "withdrawal" (mnemonicToText mnemonic)

        selectCoinsWith @_ @'Shelley ctx source (payment :| []) transform >>= flip verify
            [ expectResponseCode HTTP.status200
            , expectField #withdrawals
                (`shouldSatisfy` ((== 1) . length))
            , expectField #withdrawals
                (`shouldSatisfy` all
                    (isValidDerivationPath purposeBIP44 . view #derivationPath))
            ]

    it "WALLETS_COIN_SELECTION_07 - \
        \Single output with excessively high token quantity." $
        wallets_coin_selection_07 @n

-- Attempt to create a coin selection with an output that has an excessively
-- high token quantity. (This should fail.)
--
wallets_coin_selection_07
    :: forall n. (DecodeAddress n, DecodeStakeAddress n, EncodeAddress n)
    => Context
    -> IO ()
wallets_coin_selection_07 ctx = runResourceT $ do
    sourceWallet <- fixtureWallet ctx
    targetWallet <- emptyWallet ctx
    targetAddress : _ <- fmap (view #id) <$> listAddresses @n ctx targetWallet
    let payment =
            AddressAmount targetAddress adaQuantity (ApiT nonAdaQuantities)
    let makeRequest = selectCoins @_ @'Shelley ctx sourceWallet (payment :| [])
    makeRequest >>= flip verify
        [ expectResponseCode HTTP.status403
        , expectErrorMessage $ errMsg403TokenQuantityExceedsMaxBound
            (getApiT $ fst targetAddress)
            (policyId)
            (assetName)
            (excessiveQuantity)
            (txOutMaxTokenQuantity)
        ]
  where
    adaQuantity = Quantity minUTxOValue
    assetId = AssetId policyId assetName
    assetName = UnsafeTokenName "1"
    excessiveQuantity = TokenQuantity.succ txOutMaxTokenQuantity
    nonAdaQuantities = TokenMap.singleton assetId excessiveQuantity
    policyId = UnsafeTokenPolicyId $ Hash "1234567890123456789012345678"

isValidDerivationPath
    :: Index 'Hardened 'PurposeK
    -> NonEmpty (ApiT DerivationIndex)
    -> Bool
isValidDerivationPath purpose path =
    ( length path == 5 )
    &&
    ( [ ApiT $ DerivationIndex $ getIndex purpose
      , ApiT $ DerivationIndex $ getIndex coinTypeAda
      , ApiT $ DerivationIndex $ getIndex @'Hardened minBound
      ] `isPrefixOf` NE.toList path
    )
