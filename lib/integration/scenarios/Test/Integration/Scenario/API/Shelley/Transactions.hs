{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{- HLINT ignore "Use head" -}
{- HLINT ignore "Use :" -}

module Test.Integration.Scenario.API.Shelley.Transactions
    ( spec
    ) where

import Prelude

import Cardano.Mnemonic
    ( SomeMnemonic (..)
    , entropyToMnemonic
    , genEntropy
    , mnemonicToText
    )
import Cardano.Wallet.Address.Encoding
    ( encodeAddress
    )
import Cardano.Wallet.Api.Types
    ( AddressAmount (..)
    , ApiAddress
    , ApiAsset (..)
    , ApiCoinSelectionOutput (..)
    , ApiEra (..)
    , ApiFee (..)
    , ApiT (..)
    , ApiTransaction
    , ApiTxId (..)
    , ApiTxInput (..)
    , ApiWallet
    , WalletStyle (..)
    , apiAddress
    , insertedAt
    , pendingSince
    )
import Cardano.Wallet.Api.Types.Amount
    ( ApiAmount (ApiAmount)
    )
import Cardano.Wallet.Api.Types.Error
    ( ApiErrorInfo (..)
    , ApiErrorTxOutputLovelaceInsufficient (ApiErrorTxOutputLovelaceInsufficient)
    )
import Cardano.Wallet.Api.Types.SchemaMetadata
    ( detailedMetadata
    )
import Cardano.Wallet.Faucet
    ( seaHorseAssetName
    , seaHorsePolicyId
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId (..)
    )
import Cardano.Wallet.Primitive.Types
    ( SortOrder (..)
    , WalletId
    )
import Cardano.Wallet.Primitive.Types.Address
    ( unAddress
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.TokenFingerprint
    ( mkTokenFingerprint
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxMetadata (..)
    , TxMetadataValue (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
    ( Direction (..)
    , TxStatus (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromText
    )
import Control.Monad
    ( forM
    , forM_
    , when
    )
import Control.Monad.IO.Unlift
    ( MonadIO (..)
    , MonadUnliftIO (..)
    , liftIO
    )
import Control.Monad.Trans.Resource
    ( ResourceT
    , runResourceT
    )
import Data.Aeson
    ( (.=)
    )
import Data.Bifunctor
    ( bimap
    )
import Data.Generics.Internal.VL.Lens
    ( view
    , (^.)
    )
import Data.Generics.Product.Typed
    ( HasType
    )
import Data.Maybe
    ( isJust
    , isNothing
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( FromText (..)
    , ToText (..)
    )
import Data.Time.Clock
    ( NominalDiffTime
    , UTCTime
    , addUTCTime
    , getCurrentTime
    )
import Data.Time.Utils
    ( utcTimePred
    , utcTimeSucc
    )
import Numeric.Natural
    ( Natural
    )
import Test.Hspec
    ( SpecWith
    , describe
    , pendingWith
    )
import Test.Hspec.Expectations.Lifted
    ( expectationFailure
    , shouldBe
    , shouldNotBe
    , shouldSatisfy
    )
import Test.Hspec.Extra
    ( it
    )
import Test.Integration.Framework.DSL
    ( Context (_mainEra, _mintSeaHorseAssets)
    , Headers (..)
    , Payload (..)
    , between
    , computeApiCoinSelectionFee
    , counterexample
    , decodeErrorInfo
    , emptyRandomWallet
    , emptyWallet
    , eventually
    , expectErrorInfo
    , expectErrorMessage
    , expectField
    , expectListField
    , expectListSize
    , expectListSizeSatisfy
    , expectResponseCode
    , expectSuccess
    , faucetAmt
    , faucetUtxoAmt
    , fixtureIcarusWalletAddrs
    , fixtureMultiAssetWallet
    , fixturePassphrase
    , fixtureWallet
    , fixtureWalletWith
    , getFromResponse
    , getWallet
    , json
    , listAddresses
    , listAllTransactions
    , listLimitedTransactions
    , listTransactions
    , minUTxOValue
    , mkTxPayloadMA
    , pickAnAsset
    , postTx
    , request
    , rewardWallet
    , selectCoinsWith
    , toQueryString
    , unsafeGetTransactionTime
    , unsafeRequest
    , utcIso8601ToText
    , verify
    , waitForTxImmutability
    , walletId
    , (.<)
    , (.<=)
    , (.>)
    , (.>=)
    )
import Test.Integration.Framework.Request
    ( RequestException
    )
import Test.Integration.Framework.TestData
    ( errMsg400StartTimeLaterThanEndTime
    , errMsg400TxMetadataStringTooLong
    , errMsg403AlreadyInLedger
    , errMsg403Fee
    , errMsg403WithdrawalNotBeneficial
    , errMsg403WrongPass
    , errMsg404CannotFindTx
    , errMsg404NoAsset
    , errMsg404NoWallet
    , steveToken
    , txMetadata_ADP_1005
    )
import Web.HttpApiData
    ( ToHttpApiData (..)
    )

import qualified Cardano.Address as CA
import qualified Cardano.Wallet.Api.Link as Link
import qualified Cardano.Wallet.Api.Types.Amount as ApiAmount
import qualified Cardano.Wallet.Primitive.Types.AssetName as AssetName
import qualified Cardano.Wallet.Primitive.Types.TokenPolicyId as TokenPolicy
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashSet as Set
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

data TestCase a = TestCase
    { query :: T.Text
    , assertions :: [(HTTP.Status, Either RequestException a) -> IO ()]
    }

spec
    :: forall n
     . HasSNetworkId n
    => SpecWith Context
spec = describe "SHELLEY_TRANSACTIONS" $ do
    it
        "TRANS_MIN_UTXO_01: \
        \Specifying a non-zero quantity of lovelace that is below the minimum \
        \required by the ledger results in an error, but specifying a quantity \
        \that is equal to the required minimum results in success."
        $ \ctx -> runResourceT $ do
            sourceWallet <- fixtureWallet ctx
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddress = (targetAddresses !! 1) ^. #id
            let endpoint = Link.createTransactionOld @'Shelley sourceWallet
            let mkRequest = request @(ApiTransaction n) ctx endpoint Default
            let mkPayload :: ApiAmount -> Payload
                mkPayload lovelaceRequested =
                    Json
                        [json|
                    { "payments":
                        [
                            { "address": #{targetAddress}
                            , "amount": #{lovelaceRequested}
                            }
                        ]
                    , "passphrase": #{fixturePassphrase}
                    }
                |]

            -- Attempt #1:
            --
            -- Attempt to create a transaction with a non-zero quantity of lovelace
            -- that is significantly below the minimum required by the ledger.
            --
            -- This attempt should fail with an error that states the minimum
            -- required amount.
            --
            e <- counterexample "Attempt #1" $ do
                let lovelaceRequested = ApiAmount 1
                response <- mkRequest (mkPayload lovelaceRequested)
                expectResponseCode HTTP.status403 response
                let UtxoTooSmall e = decodeErrorInfo response
                e
                    ^. #txOutputIndex
                        `shouldBe` 0
                e
                    ^. #txOutputLovelaceSpecified
                        `shouldBe` lovelaceRequested
                e
                    ^. #txOutputLovelaceRequiredMinimum
                        `shouldSatisfy` (> lovelaceRequested)
                pure e

            -- Attempt #2:
            --
            -- Attempt to create a transaction with a quantity of lovelace equal to
            -- the minimum required amount, as specified by the error returned in
            -- the previous attempt.
            --
            -- This attempt should succeed.
            --
            counterexample "Attempt #2" $ do
                let lovelaceRequested = e ^. #txOutputLovelaceRequiredMinimum
                response <- mkRequest (mkPayload lovelaceRequested)
                expectResponseCode HTTP.status202 response

    it "Regression ADP-626 - Filtering transactions between eras" $ do
        \ctx -> runResourceT $ do
            w <- fixtureWallet ctx
            let startTimeBeforeShelley = T.pack "2009-09-09T09:09:09Z"
            currTime <- liftIO getCurrentTime
            let endTimeAfterShelley = utcIso8601ToText currTime
            let link =
                    Link.listTransactions' @'Shelley
                        w
                        Nothing
                        ( either (const Nothing) Just
                            $ fromText startTimeBeforeShelley
                        )
                        ( either (const Nothing) Just
                            $ fromText endTimeAfterShelley
                        )
                        Nothing
                        Nothing
                        Nothing
            r <- request @([ApiTransaction n]) ctx link Default Empty
            expectResponseCode HTTP.status200 r
            expectListSize 10 r

    it
        "Regression #1004 -\
        \ Transaction to self shows only fees as a tx amount\
        \ while both, pending and in_ledger"
        $ \ctx -> runResourceT $ do
            wSrc <- fixtureWallet ctx

            payload <-
                liftIO
                    $ mkTxPayload ctx wSrc (minUTxOValue (_mainEra ctx)) fixturePassphrase

            (_, ApiFee (ApiAmount feeMin) (ApiAmount feeMax) _ _) <-
                unsafeRequest
                    ctx
                    (Link.getTransactionFeeOld @'Shelley wSrc)
                    payload

            r <-
                request @(ApiTransaction n)
                    ctx
                    (Link.createTransactionOld @'Shelley wSrc)
                    Default
                    payload

            verify
                r
                [ expectSuccess
                , expectResponseCode HTTP.status202
                , -- tx amount includes only fees because it is tx to self address
                  -- when tx is pending
                  expectField (#amount . #toNatural) $ between (feeMin, feeMax)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` Pending)
                ]

            eventually "Tx is in ledger" $ do
                rt <-
                    request @([ApiTransaction n])
                        ctx
                        (Link.listTransactions @'Shelley wSrc)
                        Default
                        Empty
                verify
                    rt
                    [ expectSuccess
                    , expectResponseCode HTTP.status200
                    , -- tx amount includes only fees because it is tx to self address
                      -- also when tx is already in ledger
                      expectListField 0 (#amount . #toNatural)
                        $ between (feeMin, feeMax)
                    , expectListField
                        0
                        (#direction . #getApiT)
                        (`shouldBe` Outgoing)
                    , expectListField
                        0
                        (#status . #getApiT)
                        (`shouldBe` InLedger)
                    ]

    it
        "Regression #935 -\
        \ Pending tx should have pendingSince in the list tx response"
        $ \ctx -> runResourceT $ do
            wSrc <- fixtureWallet ctx
            wDest <- emptyWallet ctx

            eventually "Pending tx has pendingSince field" $ do
                -- Post Tx
                let amt = (minUTxOValue (_mainEra ctx) :: Natural)
                r <-
                    postTx @n
                        ctx
                        (wSrc, Link.createTransactionOld @'Shelley, fixturePassphrase)
                        wDest
                        amt
                let tx = getFromResponse Prelude.id r
                tx ^. (#status . #getApiT) `shouldBe` Pending
                insertedAt tx `shouldBe` Nothing
                pendingSince tx `shouldSatisfy` isJust

                -- Verify Tx
                let link =
                        Link.listTransactions' @'Shelley
                            wSrc
                            Nothing
                            Nothing
                            Nothing
                            (Just Descending)
                            Nothing
                            Nothing
                (_, txs) <- unsafeRequest @([ApiTransaction n]) ctx link Empty
                case filter ((== Pending) . view (#status . #getApiT)) txs of
                    [] ->
                        fail "Tx no longer pending, need to retry scenario."
                    tx' : _ -> do
                        tx' ^. (#direction . #getApiT) `shouldBe` Outgoing
                        tx' ^. (#status . #getApiT) `shouldBe` Pending
                        insertedAt tx' `shouldBe` Nothing
                        pendingSince tx' `shouldBe` pendingSince tx

    it "TRANS_CREATE_01x - Single Output Transaction"
        $ \ctx -> runResourceT $ do
            let initialAmt = 3 * minUTxOValue (_mainEra ctx)
            wa <- fixtureWalletWith @n ctx [initialAmt]
            wb <- fixtureWalletWith @n ctx [initialAmt]
            let amt = minUTxOValue (_mainEra ctx) :: Natural

            payload <- liftIO $ mkTxPayload ctx wb amt fixturePassphrase

            (_, ApiFee (ApiAmount feeMin) (ApiAmount feeMax) minCoins _) <-
                unsafeRequest
                    ctx
                    (Link.getTransactionFeeOld @'Shelley wa)
                    payload
            rTx <-
                request @(ApiTransaction n)
                    ctx
                    (Link.createTransactionOld @'Shelley wa)
                    Default
                    payload
            ra <-
                request @ApiWallet
                    ctx
                    (Link.getWallet @'Shelley wa)
                    Default
                    Empty

            verify
                rTx
                [ expectSuccess
                , expectResponseCode HTTP.status202
                , expectField (#amount . #toNatural)
                    $ between (feeMin + amt, feeMax + amt)
                , const
                    $
                    -- The minimum ada quantities returned within an 'ApiFee'
                    -- object are computed using the 'computeMinimumCoinForUTxO'
                    -- function. This function produces quantities that are very
                    -- slightly higher than the Cardano API function on which it is
                    -- based, so as to guarantee that ada quantities can always be
                    -- safely increased while still retaining their validity.
                    --
                    -- To avoid making the test suite more brittle than necessary,
                    -- here we simply assert that the minimum values returned are
                    -- greater than or equal to the 'minUTxOValue' test suite
                    -- constant:
                    --
                    minCoins
                        `shouldSatisfy` all
                            (>= (ApiAmount $ minUTxOValue (_mainEra ctx)))
                , expectField #inputs $ \inputs' -> do
                    inputs' `shouldSatisfy` all (isJust . source)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` Pending)
                , expectField #metadata (`shouldBe` Nothing)
                ]

            verify
                ra
                [ expectSuccess
                , expectField (#balance . #total)
                    $ between
                        ( ApiAmount (initialAmt - feeMax - amt)
                        , ApiAmount (initialAmt - feeMin - amt)
                        )
                , expectField
                    (#balance . #available)
                    (`shouldBe` ApiAmount 0)
                ]

            let txid = getFromResponse #id rTx
            let linkSrc = Link.getTransaction @'Shelley wa (ApiTxId txid)
            let ApiAmount fee = getFromResponse #fee rTx
            eventually "transaction is no longer pending on source wallet" $ do
                rSrc <- request @(ApiTransaction n) ctx linkSrc Default Empty
                verify
                    rSrc
                    [ expectResponseCode HTTP.status200
                    , expectField (#amount . #toNatural)
                        $ between (feeMin + amt, feeMax + amt)
                    , expectField #inputs $ \inputs' -> do
                        inputs' `shouldSatisfy` all (isJust . source)
                    , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                    , expectField (#status . #getApiT) (`shouldBe` InLedger)
                    , expectField (#metadata) (`shouldBe` Nothing)
                    , expectField (#fee . #toNatural)
                        $ between (feeMin, feeMax)
                    ]

            let linkDest = Link.getTransaction @'Shelley wb (ApiTxId txid)
            eventually "transaction is discovered by destination wallet" $ do
                rDst <- request @(ApiTransaction n) ctx linkDest Default Empty
                verify
                    rDst
                    [ expectResponseCode HTTP.status200
                    , expectField (#amount . #toNatural) (`shouldBe` amt)
                    , expectField #inputs $ \inputs' -> do
                        inputs' `shouldSatisfy` all (isNothing . source)
                    , expectField (#direction . #getApiT) (`shouldBe` Incoming)
                    , expectField (#status . #getApiT) (`shouldBe` InLedger)
                    , expectField (#metadata) (`shouldBe` Nothing)
                    , expectField (#fee . #toNatural)
                        $ between (feeMin, feeMax)
                    ]

            eventually "wa and wb balances are as expected" $ do
                rb <-
                    request @ApiWallet
                        ctx
                        (Link.getWallet @'Shelley wb)
                        Default
                        Empty
                expectField
                    (#balance . #available)
                    (`shouldBe` ApiAmount (initialAmt + amt))
                    rb

                ra2 <-
                    request @ApiWallet
                        ctx
                        (Link.getWallet @'Shelley wa)
                        Default
                        Empty
                expectField
                    (#balance . #available)
                    (`shouldBe` ApiAmount (initialAmt - fee - amt))
                    ra2

    it "TRANS_CREATE_02x - Multiple Output Tx to single wallet"
        $ \ctx -> runResourceT $ do
            wSrc <- fixtureWallet ctx
            wDest <- emptyWallet ctx
            addrs <- listAddresses @n ctx wDest

            let amt = minUTxOValue (_mainEra ctx) :: Natural
            let destination1 = (addrs !! 1) ^. #id
            let destination2 = (addrs !! 2) ^. #id
            let payload =
                    Json
                        [json|{
                "payments": [{
                    "address": #{destination1},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                },
                {
                    "address": #{destination2},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]

            (_, ApiFee (ApiAmount feeMin) (ApiAmount feeMax) _ _) <-
                unsafeRequest
                    ctx
                    (Link.getTransactionFeeOld @'Shelley wSrc)
                    payload

            r <-
                request @(ApiTransaction n)
                    ctx
                    (Link.createTransactionOld @'Shelley wSrc)
                    Default
                    payload

            ra <-
                request @ApiWallet
                    ctx
                    (Link.getWallet @'Shelley wSrc)
                    Default
                    Empty

            verify
                r
                [ expectResponseCode HTTP.status202
                , expectField (#amount . #toNatural)
                    $ between (feeMin + (2 * amt), feeMax + (2 * amt))
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` Pending)
                , expectField #inputs $ \inputs' -> do
                    inputs' `shouldSatisfy` all (isJust . source)
                ]

            verify
                ra
                [ expectField (#balance . #total)
                    $ between
                        ( ApiAmount (faucetAmt - feeMax - (2 * amt))
                        , ApiAmount (faucetAmt - feeMin - (2 * amt))
                        )
                , expectField
                    (#balance . #available)
                    (.>= ApiAmount (faucetAmt - 2 * faucetUtxoAmt))
                ]
            eventually "wDest balance is as expected" $ do
                rd <-
                    request @ApiWallet
                        ctx
                        (Link.getWallet @'Shelley wDest)
                        Default
                        Empty
                verify
                    rd
                    [ expectField
                        (#balance . #available)
                        (`shouldBe` ApiAmount (2 * amt))
                    , expectField
                        (#balance . #total)
                        (`shouldBe` ApiAmount (2 * amt))
                    ]

    it "TRANS_CREATE_03 - 0 balance after transaction"
        $ \ctx -> runResourceT $ do
            liftIO
                $ pendingWith
                    "This test relies on knowing exactly how the underlying selection \
                    \implementation works. We may want to revise this test completely \
                    \otherwise we'll have to update it for every single change in \
                    \the fee calculation or selection algorithm."
            let amt = minUTxOValue (_mainEra ctx)

            wDest <- fixtureWalletWith @n ctx [amt]
            payload <- liftIO $ mkTxPayload ctx wDest amt fixturePassphrase

            (_, ApiFee (ApiAmount feeMin) _ _ _) <-
                unsafeRequest
                    ctx
                    (Link.getTransactionFeeOld @'Shelley wDest)
                    payload

            -- NOTE It's a little tricky to estimate the fee needed for a
            -- transaction with no change output, because in order to know the right
            -- amount of fees, we need to create a transaction spends precisely this
            -- amount.
            --
            -- Said differently, in order to know what amount we need, we need to
            -- know what the amount is... ¯\_(ツ)_/¯ ... So, we use a little
            -- hard-wired margin, which works with the current fee settings. If we
            -- ever change that, this test will fail.
            let margin = 400
            wSrc <- fixtureWalletWith @n ctx [feeMin + amt + margin]

            r <-
                request @(ApiTransaction n)
                    ctx
                    (Link.createTransactionOld @'Shelley wSrc)
                    Default
                    payload
            verify
                r
                [ expectResponseCode HTTP.status202
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` Pending)
                ]

            ra <-
                request @ApiWallet
                    ctx
                    (Link.getWallet @'Shelley wSrc)
                    Default
                    Empty
            verify
                ra
                [ expectField (#balance . #total) (`shouldBe` ApiAmount 0)
                , expectField (#balance . #available) (`shouldBe` ApiAmount 0)
                ]

            eventually "Wallet balance is as expected" $ do
                rd <-
                    request @ApiWallet
                        ctx
                        (Link.getWallet @'Shelley wDest)
                        Default
                        Empty
                verify
                    rd
                    [ expectField
                        (#balance . #available)
                        (`shouldBe` ApiAmount (2 * amt))
                    , expectField
                        (#balance . #total)
                        (`shouldBe` ApiAmount (2 * amt))
                    ]

            ra2 <-
                request @ApiWallet
                    ctx
                    (Link.getWallet @'Shelley wSrc)
                    Default
                    Empty
            verify
                ra2
                [ expectField (#balance . #total) (`shouldBe` ApiAmount 0)
                , expectField (#balance . #available) (`shouldBe` ApiAmount 0)
                ]

    it "TRANS_CREATE_04 - Can't cover fee" $ \ctx -> runResourceT $ do
        wDest <- fixtureWallet ctx

        let minUTxOValue' = minUTxOValue (_mainEra ctx)

        payload <-
            liftIO
                $ mkTxPayload ctx wDest minUTxOValue' fixturePassphrase
        (_, ApiFee (ApiAmount feeMin) _ _ _) <-
            unsafeRequest
                ctx
                (Link.getTransactionFeeOld @'Shelley wDest)
                payload

        wSrc <- fixtureWalletWith @n ctx [minUTxOValue' + (feeMin `div` 2)]

        r <-
            request @(ApiTransaction n)
                ctx
                (Link.createTransactionOld @'Shelley wSrc)
                Default
                payload
        verify
            r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403Fee
            ]

    it "TRANS_CREATE_04 - Not enough money" $ \ctx -> runResourceT $ do
        let minUTxOValue' = minUTxOValue (_mainEra ctx)
        let (srcAmt, reqAmt) = (minUTxOValue', 2 * minUTxOValue')
        wSrc <- fixtureWalletWith @n ctx [srcAmt]
        wDest <- emptyWallet ctx
        payload <- mkTxPayload ctx wDest reqAmt fixturePassphrase
        r <-
            request @(ApiTransaction n)
                ctx
                (Link.createTransactionOld @'Shelley wSrc)
                Default
                payload
        verify r [expectResponseCode HTTP.status403]
        decodeErrorInfo r `shouldSatisfy` \case
            NotEnoughMoney{} -> True
            _someOtherError -> False

    it "TRANS_CREATE_04 - Wrong password" $ \ctx -> runResourceT $ do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addr : _ <- listAddresses @n ctx wDest

        let destination = addr ^. #id
        let payload =
                Json
                    [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{minUTxOValue (_mainEra ctx)},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "This password is wrong"
            }|]
        r <-
            request @(ApiTransaction n)
                ctx
                (Link.createTransactionOld @'Shelley wSrc)
                Default
                payload
        verify
            r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403WrongPass
            ]

    it "TRANS_CREATE_07 - Deleted wallet" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        _ <-
            request @ApiWallet
                ctx
                (Link.deleteWallet @'Shelley w)
                Default
                Empty
        wDest <- emptyWallet ctx
        addr : _ <- listAddresses @n ctx wDest
        let destination = addr ^. #id
        let payload =
                Json
                    [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{minUTxOValue (_mainEra ctx) },
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]
        r <-
            request @(ApiTransaction n)
                ctx
                (Link.createTransactionOld @'Shelley w)
                Default
                payload
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    describe "TRANS_CREATE_08 - Bad payload" $ do
        let matrix =
                [ ("empty payload", NonJson "")
                , ("{} payload", NonJson "{}")
                ,
                    ( "non-json valid payload"
                    , NonJson
                        "{ payments: [{\
                        \\"address\": 12312323,\
                        \\"amount: {\
                        \\"quantity\": 1,\
                        \\"unit\": \"lovelace\"} }],\
                        \\"passphrase\": \"cardano-wallet\" }"
                    )
                ]

        forM_ matrix $ \(title, nonJson) -> it title $ \ctx -> runResourceT $ do
            w <- emptyWallet ctx
            let payload = nonJson
            r <-
                request @(ApiTransaction n)
                    ctx
                    (Link.createTransactionOld @'Shelley w)
                    Default
                    payload
            expectResponseCode HTTP.status400 r

    it "TRANS_ASSETS_CREATE_01 - Multi-asset balance"
        $ \ctx -> runResourceT $ do
            w <- fixtureMultiAssetWallet ctx
            r <- request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty
            verify
                r
                [ expectField
                    (#assets . #available)
                    (`shouldNotBe` mempty)
                , expectField
                    (#assets . #total)
                    (`shouldNotBe` mempty)
                ]

            r2 <- request @[ApiAsset] ctx (Link.listAssets w) Default Empty
            verify
                r2
                [ expectListField 0 #metadata (`shouldBe` Just steveToken)
                , expectListField 0 #metadataError (`shouldBe` Nothing)
                ]

    it "TRANS_ASSETS_CREATE_01a - Multi-asset transaction with Ada"
        $ \ctx -> runResourceT $ do
            wSrc <- fixtureMultiAssetWallet ctx
            wDest <- emptyWallet ctx
            ra <-
                request @ApiWallet
                    ctx
                    (Link.getWallet @'Shelley wSrc)
                    Default
                    Empty
            let (_, Right wal) = ra

            -- pick out an asset to send
            let assetsSrc = wal ^. #assets . #total
            assetsSrc `shouldNotBe` mempty
            let val = minUTxOValue (_mainEra ctx) <$ pickAnAsset assetsSrc

            addrs <- listAddresses @n ctx wDest
            let destination = (addrs !! 1) ^. #id

            -- use minimum coin value provided by the server
            payloadFee <- mkTxPayloadMA @n destination 0 [val] fixturePassphrase
            rFee <-
                request @ApiFee
                    ctx
                    (Link.getTransactionFeeOld @'Shelley wSrc)
                    Default
                    payloadFee
            let [ApiAmount minCoin] = getFromResponse #minimumCoins rFee

            payload <- mkTxPayloadMA @n destination minCoin [val] fixturePassphrase
            rtx <-
                request @(ApiTransaction n)
                    ctx
                    (Link.createTransactionOld @'Shelley wSrc)
                    Default
                    payload
            expectResponseCode HTTP.status202 rtx

            eventually "Payee wallet balance is as expected" $ do
                rb <-
                    request @ApiWallet
                        ctx
                        (Link.getWallet @'Shelley wDest)
                        Default
                        Empty
                verify
                    rb
                    [ expectField
                        (#assets . #available)
                        (`shouldNotBe` mempty)
                    , expectField
                        (#assets . #total)
                        (`shouldNotBe` mempty)
                    , expectField
                        (#balance . #total . #toNatural)
                        (`shouldBe` minCoin)
                    ]
    -- todo: asset balance values more exactly
    -- todo: assert payer wallet balance

    it
        "TRANS_ASSETS_CREATE_02 - \
        \Multi-asset transaction with small Ada amount"
        $ \ctx -> runResourceT $ do
            wSrc <- fixtureMultiAssetWallet ctx
            wDest <- emptyWallet ctx
            ra <-
                request @ApiWallet
                    ctx
                    (Link.getWallet @'Shelley wSrc)
                    Default
                    Empty
            let (_, Right wal) = ra

            -- pick out an asset to send
            let assetsSrc = wal ^. #assets . #total
            assetsSrc `shouldNotBe` mempty
            let val = minUTxOValue (_mainEra ctx) <$ pickAnAsset assetsSrc

            -- This is a non-zero ada amount, but less than the actual minimum utxo
            -- due to assets in the transaction.
            let coin = minUTxOValue (_mainEra ctx)

            addrs <- listAddresses @n ctx wDest
            let destination = (addrs !! 1) ^. #id
            payload <- mkTxPayloadMA @n destination coin [val] fixturePassphrase

            rtx <-
                request @(ApiTransaction n)
                    ctx
                    (Link.createTransactionOld @'Shelley wSrc)
                    Default
                    payload
            verify rtx
                [ expectResponseCode HTTP.status403
                , expectErrorInfo $ flip shouldSatisfy $ \case
                    UtxoTooSmall ApiErrorTxOutputLovelaceInsufficient {} ->
                        True
                    _anythingElse ->
                        False
                ]

    it "TRANS_ASSETS_CREATE_02a - Multi-asset transaction without Ada"
        $ \ctx -> runResourceT $ do
            wSrc <- fixtureMultiAssetWallet ctx
            wDest <- emptyWallet ctx
            ra <-
                request @ApiWallet
                    ctx
                    (Link.getWallet @'Shelley wSrc)
                    Default
                    Empty
            let (_, Right wal) = ra

            -- pick out an asset to send
            let assetsSrc = wal ^. #assets . #total
            assetsSrc `shouldNotBe` mempty
            let val = minUTxOValue (_mainEra ctx) <$ pickAnAsset assetsSrc

            addrs <- listAddresses @n ctx wDest
            let destination = (addrs !! 1) ^. #id
            payload <- mkTxPayloadMA @n destination 0 [val] fixturePassphrase

            rtx <-
                request @(ApiTransaction n)
                    ctx
                    (Link.createTransactionOld @'Shelley wSrc)
                    Default
                    payload
            expectResponseCode HTTP.status202 rtx

            eventually "Payee wallet balance is as expected" $ do
                rb <-
                    request @ApiWallet
                        ctx
                        (Link.getWallet @'Shelley wDest)
                        Default
                        Empty
                verify
                    rb
                    [ expectField
                        (#assets . #available)
                        (`shouldNotBe` mempty)
                    , expectField
                        (#assets . #total)
                        (`shouldNotBe` mempty)
                    ]

    it "TRANS_ASSETS_CREATE_02c - Send SeaHorses"
        $ \ctx -> runResourceT $ do
            -- Notes on the style of this test:
            -- - By doing the minting here it is easier to control, and tweak
            -- the values.
            -- - The current _mintSeaHorseAssets cannot be called concurrently
            -- - By grouping the setup of multiple wallets in a single test, we
            -- gain some time.

            -- 1. Setup by minting funds
            let assetsPerAddrScenarios = [64 .. 70]
            sourceWallets <- forM assetsPerAddrScenarios $ \nAssetsPerAddr -> do
                wSrc <- fixtureWallet ctx
                srcAddrs <-
                    map (CA.unsafeMkAddress . unAddress . apiAddress . view #id)
                        <$> listAddresses @n ctx wSrc
                let batchSize = 1
                let coinPerAddr = Coin 1000_000_000
                liftIO
                    $ _mintSeaHorseAssets
                        ctx
                        nAssetsPerAddr
                        batchSize
                        coinPerAddr
                        (take 2 srcAddrs)
                return (wSrc, nAssetsPerAddr)
            wDest <- emptyWallet ctx
            destAddr <- head . map (view #id) <$> listAddresses @n ctx wDest
            waitForTxImmutability ctx

            -- 2. Try spending from each wallet, and record the response.
            responses <- forM sourceWallets $ \(wSrc, nPerAddr) -> do
                let seaHorses = map $ \ix ->
                        (
                            ( toText seaHorsePolicyId
                            , toText $ seaHorseAssetName ix
                            )
                        , 1
                        )
                payload <-
                    mkTxPayloadMA @n
                        destAddr
                        0
                        (seaHorses [1, nPerAddr * 2])
                        -- Send one token from our first bundle, and one token from
                        -- our second bundle, to ensure the change output is large.
                        fixturePassphrase

                let verifyRes r = case r of
                        (s, Right _)
                            | s == HTTP.status202 -> Right ()
                            | otherwise ->
                                Left
                                    $ mconcat
                                        [ "impossible: request succeeded, but got "
                                        , "status code "
                                        , show s
                                        ]
                        (_, Left e) -> Left $ show e

                (nPerAddr,) . verifyRes
                    <$> request @(ApiTransaction n)
                        ctx
                        (Link.createTransactionOld @'Shelley wSrc)
                        Default
                        payload

            -- 3. They should all succeed
            responses `shouldBe` (map (,Right ()) assetsPerAddrScenarios)

    let hasAssetOutputs :: [AddressAmount (ApiAddress n)] -> Bool
        hasAssetOutputs = any ((/= mempty) . view #assets)

    it "TRANS_ASSETS_CREATE_02b - Multi-asset tx history"
        $ \ctx -> runResourceT $ do
            wSrc <- fixtureMultiAssetWallet ctx
            wDest <- emptyWallet ctx
            wal <- getWallet ctx wSrc

            -- pick out an asset to send
            let assetsSrc = wal ^. #assets . #total
            assetsSrc `shouldNotBe` mempty
            let val = minUTxOValue (_mainEra ctx) <$ pickAnAsset assetsSrc

            addrs <- listAddresses @n ctx wDest
            let destination = (addrs !! 1) ^. #id
            payload <- mkTxPayloadMA @n destination 0 [val] fixturePassphrase

            rtx <-
                request @(ApiTransaction n)
                    ctx
                    (Link.createTransactionOld @'Shelley wSrc)
                    Default
                    payload

            verify
                rtx
                [ expectSuccess
                , expectField (#status . #getApiT) (`shouldBe` Pending)
                , -- , expectField #assets (`shouldNotBe` mempty) -- TODO: ADP-683
                  expectField #outputs (`shouldSatisfy` hasAssetOutputs)
                ]

            eventually "asset transfer is confirmed in transaction list" $ do
                -- on src wallet
                let linkSrcList = Link.listTransactions @'Shelley wSrc
                rla <- request @([ApiTransaction n]) ctx linkSrcList Default Empty
                verify
                    rla
                    [ expectSuccess
                    , expectListField 0 (#status . #getApiT) (`shouldBe` InLedger)
                    , expectListField
                        0
                        (#direction . #getApiT)
                        (`shouldBe` Outgoing)
                    , -- TODO: ADP-683
                      -- , expectListField 0 #assets (`shouldBe` "fewer than before")
                      expectListField 0 #outputs (`shouldSatisfy` hasAssetOutputs)
                    ]
                -- on dst wallet
                let linkDestList = Link.listTransactions @'Shelley wDest
                rlb <- request @([ApiTransaction n]) ctx linkDestList Default Empty
                verify
                    rlb
                    [ expectSuccess
                    , expectListField 0 (#status . #getApiT) (`shouldBe` InLedger)
                    , expectListField
                        0
                        (#direction . #getApiT)
                        (`shouldBe` Incoming)
                    , -- TODO: ADP-683
                      -- , expectListField 0 #assets (`shouldNotBe` mempty)
                      expectListField 0 #outputs (`shouldSatisfy` hasAssetOutputs)
                    ]

            eventually "Payee wallet balance is as expected" $ do
                rb <-
                    request @ApiWallet
                        ctx
                        (Link.getWallet @'Shelley wDest)
                        Default
                        Empty
                verify
                    rb
                    [ expectField
                        (#assets . #available)
                        (`shouldNotBe` mempty)
                    , expectField
                        (#assets . #total)
                        (`shouldNotBe` mempty)
                    ]

    it "TRANS_ASSETS_LIST_01 - Asset list present"
        $ \ctx -> runResourceT $ do
            wal <- fixtureMultiAssetWallet ctx

            let assetsSrc = wal ^. (#assets . #total)
            assetsSrc `shouldNotBe` mempty
            let (polId, assName) =
                    bimap unsafeFromText unsafeFromText
                        $ fst
                        $ pickAnAsset assetsSrc
            let tokenFingerprint = mkTokenFingerprint polId assName

            r <- request @([ApiAsset]) ctx (Link.listAssets wal) Default Empty
            verify
                r
                [ expectSuccess
                , expectListSizeSatisfy (> 0)
                , expectListField 0 #policyId (`shouldBe` ApiT polId)
                , expectListField 0 #assetName (`shouldBe` ApiT assName)
                , expectListField
                    0
                    (#fingerprint . #getApiT)
                    (`shouldBe` tokenFingerprint)
                , expectListField 0 #metadata (`shouldBe` Just steveToken)
                , expectListField 0 #metadataError (`shouldBe` Nothing)
                ]

    it "TRANS_ASSETS_LIST_02 - Asset list present when not used"
        $ \ctx -> runResourceT $ do
            wal <- fixtureWallet ctx
            r <- request @([ApiAsset]) ctx (Link.listAssets wal) Default Empty
            verify
                r
                [ expectSuccess
                , expectListSize 0
                ]

    it "TRANS_ASSETS_LIST_02a - Asset list present when not used"
        $ \ctx -> runResourceT $ do
            wal <- emptyWallet ctx
            r <- request @([ApiAsset]) ctx (Link.listAssets wal) Default Empty
            verify
                r
                [ expectSuccess
                , expectListSize 0
                ]

    it "TRANS_ASSETS_GET_01 - Asset list present"
        $ \ctx -> runResourceT $ do
            wal <- fixtureMultiAssetWallet ctx

            -- pick an asset from the fixture wallet
            assetsSrc <- view (#assets . #total) <$> getWallet ctx wal
            assetsSrc `shouldNotBe` mempty
            let (polId, assName) =
                    bimap unsafeFromText unsafeFromText
                        $ fst
                        $ pickAnAsset assetsSrc
            let tokenFingerprint = mkTokenFingerprint polId assName
            let ep = Link.getAsset wal polId assName
            r <- request @(ApiAsset) ctx ep Default Empty
            verify
                r
                [ expectSuccess
                , expectField #policyId (`shouldBe` ApiT polId)
                , expectField #assetName (`shouldBe` ApiT assName)
                , expectField
                    (#fingerprint . #getApiT)
                    (`shouldBe` tokenFingerprint)
                , expectField #metadata (`shouldBe` Just steveToken)
                , expectField #metadataError (`shouldBe` Nothing)
                ]

    it "TRANS_ASSETS_GET_02 - Asset not present when isn't associated"
        $ \ctx -> runResourceT $ do
            wal <- fixtureMultiAssetWallet ctx
            let polId = TokenPolicy.UnsafeTokenPolicyId $ Hash $ BS.replicate 28 0
            let assName = AssetName.UnsafeAssetName $ B8.replicate 4 'x'
            let ep = Link.getAsset wal polId assName
            r <- request @(ApiAsset) ctx ep Default Empty
            expectResponseCode HTTP.status404 r
            expectErrorMessage errMsg404NoAsset r

    it "TRANS_ASSETS_GET_02a - Asset not present when isn't associated"
        $ \ctx -> runResourceT $ do
            wal <- fixtureMultiAssetWallet ctx
            let polId = TokenPolicy.UnsafeTokenPolicyId $ Hash $ BS.replicate 28 0
            let ep = Link.getAsset wal polId AssetName.empty
            r <- request @(ApiAsset) ctx ep Default Empty
            expectResponseCode HTTP.status404 r
            expectErrorMessage errMsg404NoAsset r

    it "TRANS_TTL_04 - Large TTL"
        $ \ctx -> runResourceT $ do
            (wa, wb) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
            let amt = minUTxOValue (_mainEra ctx) :: Natural
            let hugeTTL = 1e9 :: NominalDiffTime

            basePayload <- mkTxPayload ctx wb amt fixturePassphrase
            let payload = addTxTTL (realToFrac hugeTTL) basePayload

            r <-
                request @(ApiTransaction n)
                    ctx
                    (Link.createTransactionOld @'Shelley wa)
                    Default
                    payload

            -- If another HFC Era is added, then this payment request will fail
            -- because the expiry would be past the slotting horizon.
            verify
                r
                [ expectSuccess
                , expectField (#status . #getApiT) (`shouldBe` Pending)
                , expectField #expiresAt (`shouldSatisfy` isJust)
                ]

    describe "TRANSMETA_CREATE_01 - Including metadata within transactions"
        $ mapM_
            spec_createTransactionWithMetadata
            [ CreateTransactionWithMetadataTest
                { testName =
                    "transaction without any metadata (1 output)"
                , txOutputAdaQuantities = \minUTxOVal ->
                    [minUTxOVal]
                , txMetadata =
                    Nothing
                , expectedFee =
                    ApiAmount 129_500
                }
            , CreateTransactionWithMetadataTest
                { testName =
                    "transaction with simple textual metadata (1 output)"
                , txOutputAdaQuantities = \minUTxOVal ->
                    [minUTxOVal]
                , txMetadata =
                    Just $ TxMetadata $ Map.singleton 1 $ TxMetaText "hello"
                , expectedFee =
                    ApiAmount 134_200
                }
            , CreateTransactionWithMetadataTest
                { testName =
                    "transaction with metadata from ADP-1005 (1 output)"
                , txOutputAdaQuantities = \minUTxOVal ->
                    [minUTxOVal]
                , txMetadata =
                    Just txMetadata_ADP_1005
                , expectedFee =
                    ApiAmount 151_800
                }
            , CreateTransactionWithMetadataTest
                { testName =
                    "transaction with metadata from ADP-1005 (2 outputs)"
                , txOutputAdaQuantities = \_minUTxOVal ->
                    -- An ada quantity that is greater than or equal to the
                    -- minimum ada quantity for all known eras:
                    [ 1_107_670
                    , -- The exact ada quantity recorded in ADP-1005:
                      498_283_127
                    ]
                , txMetadata =
                    Just txMetadata_ADP_1005
                , expectedFee =
                    ApiAmount 165_200
                }
            ]

    it
        "TRANSMETA_CREATE_02a - \
        \Transaction with invalid metadata"
        $ \ctx -> runResourceT $ do
            (wa, wb) <- (,) <$> fixtureWallet ctx <*> fixtureWallet ctx
            let amt = minUTxOValue (_mainEra ctx) :: Natural

            basePayload <- mkTxPayload ctx wb amt fixturePassphrase

            let txMeta = [json|{ "1": { "string": #{T.replicate 65 "a"} } }|]
            let payload = addTxMetadata txMeta basePayload

            r <-
                request @(ApiTransaction n)
                    ctx
                    (Link.createTransactionOld @'Shelley wa)
                    Default
                    payload

            expectResponseCode HTTP.status400 r
            expectErrorMessage errMsg400TxMetadataStringTooLong r

    it
        "TRANSMETA_CREATE_02b - \
        \Transaction with invalid no-schema metadata"
        $ \ctx -> runResourceT $ do
            (wa, wb) <- (,) <$> fixtureWallet ctx <*> fixtureWallet ctx
            let amt = minUTxOValue (_mainEra ctx) :: Natural

            basePayload <- mkTxPayload ctx wb amt fixturePassphrase

            let txMeta = [json|{ "1": #{T.replicate 65 "a"} }|]
            let payload = addTxMetadata txMeta basePayload

            r <-
                request @(ApiTransaction n)
                    ctx
                    (Link.createTransactionOld @'Shelley wa)
                    Default
                    payload

            expectResponseCode HTTP.status400 r
            expectErrorMessage errMsg400TxMetadataStringTooLong r

    it "TRANSMETA_CREATE_03 - Transaction with too much metadata"
        $ \ctx -> runResourceT $ do
            (wa, wb) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
            let amt = minUTxOValue (_mainEra ctx) :: Natural

            basePayload <- mkTxPayload ctx wb amt fixturePassphrase

            -- This will encode to at least 32k of CBOR. The max tx size for the
            -- integration tests cluster is 16k.
            let txMeta =
                    Aeson.object
                        [ (Aeson.fromText $ toText @Int i, bytes)
                        | i <- [0 .. 511]
                        ]
                bytes = [json|{ "bytes": #{T.replicate 64 "a"} }|]
            let payload = addTxMetadata txMeta basePayload

            r <-
                request @(ApiTransaction n)
                    ctx
                    (Link.createTransactionOld @'Shelley wa)
                    Default
                    payload

            expectResponseCode HTTP.status403 r
            expectErrorInfo (`shouldBe` TransactionIsTooBig) r

    it
        "TRANSMETA_ESTIMATE_01a - \
        \fee estimation includes metadata"
        $ \ctx -> runResourceT $ do
            (wa, wb) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
            let amt = minUTxOValue (_mainEra ctx) :: Natural

            payload <- mkTxPayload ctx wb amt fixturePassphrase

            let txMeta = [json|{ "1": { "string": "hello" } }|]
            let payloadWithMetadata = addTxMetadata txMeta payload

            ra <-
                request @ApiFee
                    ctx
                    (Link.getTransactionFeeOld @'Shelley wa)
                    Default
                    payloadWithMetadata
            verify
                ra
                [ expectSuccess
                , expectResponseCode HTTP.status202
                ]
            let (ApiAmount feeEstMin) = getFromResponse #estimatedMin ra
            let (ApiAmount feeEstMax) = getFromResponse #estimatedMax ra

            -- check that it's estimated to have less fees for transactions without
            -- metadata.
            rb <-
                request @ApiFee
                    ctx
                    (Link.getTransactionFeeOld @'Shelley wa)
                    Default
                    payload
            verify
                rb
                [ expectResponseCode HTTP.status202
                , expectField (#estimatedMin . #toNatural) (.< feeEstMin)
                , expectField (#estimatedMax . #toNatural) (.< feeEstMax)
                ]

    it
        "TRANSMETA_ESTIMATE_01b - \
        \fee estimation includes no-schema metadata"
        $ \ctx -> runResourceT $ do
            (wa, wb) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
            let amt = minUTxOValue (_mainEra ctx) :: Natural

            payload <- mkTxPayload ctx wb amt fixturePassphrase

            let txMeta = [json|{ "1": "hello" }|]
            let payloadWithMetadata = addTxMetadata txMeta payload

            ra <-
                request @ApiFee
                    ctx
                    (Link.getTransactionFeeOld @'Shelley wa)
                    Default
                    payloadWithMetadata
            verify
                ra
                [ expectSuccess
                , expectResponseCode HTTP.status202
                ]
            let (ApiAmount feeEstMin) = getFromResponse #estimatedMin ra
            let (ApiAmount feeEstMax) = getFromResponse #estimatedMax ra

            -- check that it's estimated to have less fees for transactions without
            -- metadata.
            rb <-
                request @ApiFee
                    ctx
                    (Link.getTransactionFeeOld @'Shelley wa)
                    Default
                    payload
            verify
                rb
                [ expectResponseCode HTTP.status202
                , expectField (#estimatedMin . #toNatural) (.< feeEstMin)
                , expectField (#estimatedMax . #toNatural) (.< feeEstMax)
                ]

    it
        "TRANSMETA_ESTIMATE_02a - \
        \fee estimation with invalid metadata"
        $ \ctx -> runResourceT $ do
            (wa, wb) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
            let amt = minUTxOValue (_mainEra ctx) :: Natural

            basePayload <- mkTxPayload ctx wb amt fixturePassphrase

            let txMeta = [json|{ "1": { "string": #{T.replicate 65 "a"} } }|]
            let payload = addTxMetadata txMeta basePayload

            r <-
                request @ApiFee
                    ctx
                    (Link.getTransactionFeeOld @'Shelley wa)
                    Default
                    payload

            expectResponseCode HTTP.status400 r
            expectErrorMessage errMsg400TxMetadataStringTooLong r

    it
        "TRANSMETA_ESTIMATE_02b - \
        \fee estimation with invalid no-schema metadata"
        $ \ctx -> runResourceT $ do
            (wa, wb) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
            let amt = minUTxOValue (_mainEra ctx) :: Natural

            basePayload <- mkTxPayload ctx wb amt fixturePassphrase

            let txMeta = [json|{ "1": #{T.replicate 65 "a" } }|]
            let payload = addTxMetadata txMeta basePayload

            r <-
                request @ApiFee
                    ctx
                    (Link.getTransactionFeeOld @'Shelley wa)
                    Default
                    payload

            expectResponseCode HTTP.status400 r
            expectErrorMessage errMsg400TxMetadataStringTooLong r

    it "TRANSMETA_ESTIMATE_03 - fee estimation with too much metadata"
        $ \ctx -> runResourceT $ do
            (wa, wb) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
            let amt = minUTxOValue (_mainEra ctx) :: Natural

            basePayload <- mkTxPayload ctx wb amt fixturePassphrase

            -- This will encode to at least 32k of CBOR. The max tx size for the
            -- integration tests cluster is 16k.
            let txMeta =
                    Aeson.object
                        [ (Aeson.fromText $ toText @Int i, bytes)
                        | i <- [0 .. 511]
                        ]
                bytes = [json|{ "bytes": #{T.replicate 64 "a"} }|]
            let payload = addTxMetadata txMeta basePayload
            r <-
                request @ApiFee
                    ctx
                    (Link.getTransactionFeeOld @'Shelley wa)
                    Default
                    payload

            verify
                r
                [ expectResponseCode HTTP.status403
                , expectErrorInfo (`shouldBe` TransactionIsTooBig)
                ]

    describe "TRANS_ESTIMATE_08 - Bad payload" $ do
        let matrix =
                [ ("empty payload", NonJson "")
                , ("{} payload", NonJson "{}")
                ,
                    ( "non-json valid payload"
                    , NonJson
                        "{ payments: [{\
                        \\"address\": 12312323,\
                        \\"amount: {\
                        \\"quantity\": 1,\
                        \\"unit\": \"lovelace\"} }]\
                        \ }"
                    )
                ]

        forM_ matrix $ \(title, nonJson) -> it title $ \ctx -> runResourceT $ do
            w <- emptyWallet ctx
            let payload = nonJson
            r <-
                request @ApiFee
                    ctx
                    (Link.getTransactionFeeOld @'Shelley w)
                    Default
                    payload
            expectResponseCode HTTP.status400 r

    it "TRANS_ESTIMATE_03a - we see result when we can't cover fee"
        $ \ctx -> runResourceT $ do
            wSrc <- fixtureWallet ctx
            payload <- mkTxPayload ctx wSrc faucetAmt fixturePassphrase
            r <-
                request @ApiFee
                    ctx
                    (Link.getTransactionFeeOld @'Shelley wSrc)
                    Default
                    payload
            verify
                r
                [ expectResponseCode HTTP.status202
                , expectField (#estimatedMin . #toNatural) (.>= 0)
                , expectField (#estimatedMax . #toNatural) (.<= oneAda)
                ]

    it
        "TRANS_ESTIMATE_03b - \
        \we see result when we can't cover fee (with withdrawal)"
        $ \ctx -> runResourceT $ do
            liftIO
                $ pendingWith
                    "This now triggers a new error on the backend side which is harder \
                    \to catch without much logic changes. Since we are about to do a \
                    \complete revision of the way transaction are constructed, which \
                    \will result in the removal of the fee estimation altogether, I \
                    \won't bother fixing this particular test case which is pretty \
                    \minor / edge-case."

            (wSrc, _) <- rewardWallet ctx
            addr : _ <- fmap (view #id) <$> listAddresses @n ctx wSrc
            let totalBalance = wSrc ^. #balance . #total
            let payload =
                    Json
                        [json|{
                "withdrawal": "self",
                "payments": [{
                    "address": #{addr},
                    "amount": #{totalBalance}
                }],
                "passphrase": #{fixturePassphrase}
            }|]
            r <-
                request @ApiFee
                    ctx
                    (Link.getTransactionFeeOld @'Shelley wSrc)
                    Default
                    payload
            verify
                r
                [ expectResponseCode HTTP.status202
                , expectField (#estimatedMin . #toNatural) (.>= 0)
                , expectField (#estimatedMax . #toNatural) (.<= oneAda)
                ]

    it "TRANS_ESTIMATE_04 - Not enough money" $ \ctx -> runResourceT $ do
        let minUTxOValue' = minUTxOValue (_mainEra ctx)
        let (srcAmt, reqAmt) = (minUTxOValue', 2 * minUTxOValue')
        wSrc <- fixtureWalletWith @n ctx [srcAmt]
        wDest <- emptyWallet ctx
        payload <- mkTxPayload ctx wDest reqAmt fixturePassphrase
        r <-
            request @ApiFee
                ctx
                (Link.getTransactionFeeOld @'Shelley wSrc)
                Default
                payload
        verify r [expectResponseCode HTTP.status403]
        decodeErrorInfo r `shouldSatisfy` \case
            NotEnoughMoney{} -> True
            _someOtherError -> False

    it "TRANS_ESTIMATE_07 - Deleted wallet" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        _ <-
            request @ApiWallet
                ctx
                (Link.deleteWallet @'Shelley w)
                Default
                Empty
        wDest <- emptyWallet ctx
        let minUTxOValue' = minUTxOValue (_mainEra ctx)
        payload <- mkTxPayload ctx wDest minUTxOValue' fixturePassphrase
        r <-
            request @ApiFee
                ctx
                (Link.getTransactionFeeOld @'Shelley w)
                Default
                payload
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    it "TRANS_LIST_01 - Can list Incoming and Outgoing transactions"
        $ \ctx -> runResourceT $ do
            -- Make tx from fixtureWallet
            (wSrc, wDest) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
            addrs <- listAddresses @n ctx wDest

            let amt = minUTxOValue (_mainEra ctx) :: Natural
            let destination = (addrs !! 1) ^. #id
            let payload =
                    Json
                        [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]

            tx <-
                request @(ApiTransaction n)
                    ctx
                    (Link.createTransactionOld @'Shelley wSrc)
                    Default
                    payload
            expectResponseCode HTTP.status202 tx
            eventually "Wallet balance is as expected" $ do
                rGet <-
                    request @ApiWallet
                        ctx
                        (Link.getWallet @'Shelley wDest)
                        Default
                        Empty
                verify
                    rGet
                    [ expectField
                        (#balance . #total)
                        (`shouldBe` ApiAmount amt)
                    , expectField
                        (#balance . #available)
                        (`shouldBe` ApiAmount amt)
                    ]

            -- Verify Tx list contains Incoming and Outgoing
            let link = Link.listTransactions @'Shelley wSrc
            r <- request @([ApiTransaction n]) ctx link Default Empty
            expectResponseCode HTTP.status200 r

            verify
                r
                [ expectListField 0 (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 1 (#direction . #getApiT) (`shouldBe` Incoming)
                ]

    -- This scenario covers the following matrix of cases. Cases were generated
    -- using one of pairwise test cases generation tools available online.
    -- +---+----------+----------+------------+-----------+---------------+
    --     |  start   |   end    |   order    | max_count |  result       |
    -- +---+----------+----------+------------+-----------+---------------+
    --   1 | edge     | edge     | ascending  |  empty    |  2 ascending  |
    --   2 | edge     | edge + 1 | descending |  empty    |  2 descending |
    --   3 | edge     | edge - 1 | empty      |  empty    |  1st one      |
    --   4 | edge     | empty    | empty      |  empty    |  2 descending |
    --   5 | edge + 1 | edge + 1 | empty      |  empty    |  2nd one      |
    --   6 | edge + 1 | edge - 1 | empty      |  empty    |  none         |
    --   7 | edge + 1 | empty    | ascending  |  empty    |  2nd one      |
    --   8 | edge + 1 | edge     | descending |  empty    |  2nd one      |
    --   9 | edge - 1 | edge - 1 | ascending  |  empty    |  1st one      |
    --  10 | edge - 1 | empty    | descending |  empty    |  2 descending |
    --  11 | edge - 1 | edge     | empty      |  empty    |  2 descending |
    --  12 | edge - 1 | edge + 1 | empty      |  empty    |  2 descending |
    --  13 | empty    | empty    | empty      |  empty    |  2 descending |
    --  14 | empty    | edge     | empty      |  empty    |  2 descending |
    --  15 | empty    | edge + 1 | ascending  |  empty    |  2 ascending  |
    --  16 | empty    | edge - 1 | descending |  empty    |  1st one      |
    --  17 | t1       | t1       | empty      |  empty    |  1st one      |
    --  18 | t2       | t2       | descending |  empty    |  2nd one      |
    --  19 | empty    | empty    | empty      |    1      |  2nd one      |
    --  20 | empty    | empty    | ascending  |    1      |  1st one      |
    -- +---+----------+----------+------------+-----------+---------------+
    it "TRANS_LIST_02,03x - Can limit/order results with start, end and order"
        $ \ctx -> runResourceT $ do
            let minUTxOValue' = minUTxOValue (_mainEra ctx)
            let a1 = ApiAmount minUTxOValue'
            let a2 = ApiAmount (2 * minUTxOValue')
            (wSrc, w) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
            -- post txs
            let linkTx =
                    (wSrc, Link.createTransactionOld @'Shelley, "cardano-wallet")
            _ <- postTx @n ctx linkTx w minUTxOValue'
            verifyWalletBalance ctx w (ApiAmount minUTxOValue')

            _ <- postTx @n ctx linkTx w (2 * minUTxOValue')
            verifyWalletBalance ctx w (ApiAmount (3 * minUTxOValue'))

            txs <- eventually "I make sure there are exactly 2 transactions" $ do
                let linkList =
                        Link.listTransactions' @'Shelley
                            w
                            Nothing
                            Nothing
                            Nothing
                            Nothing
                            Nothing
                            Nothing
                rl <- request @([ApiTransaction n]) ctx linkList Default Empty
                verify rl [expectListSize 2]
                pure (getFromResponse Prelude.id rl)

            let [Just t2, Just t1] = fmap (fmap (view #time) . insertedAt) txs
            let matrix :: [TestCase [ApiTransaction n]] =
                    [ TestCase -- 1
                        { query =
                            toQueryString
                                [ ("start", utcIso8601ToText t1)
                                , ("end", utcIso8601ToText t2)
                                , ("order", "ascending")
                                ]
                        , assertions =
                            [ expectListSize 2
                            , expectListField 0 #amount (`shouldBe` a1)
                            , expectListField 1 #amount (`shouldBe` a2)
                            ]
                        }
                    , TestCase -- 2
                        { query =
                            toQueryString
                                [ ("start", utcIso8601ToText t1)
                                , ("end", utcIso8601ToText $ plusDelta t2)
                                , ("order", "descending")
                                ]
                        , assertions =
                            [ expectListSize 2
                            , expectListField 0 #amount (`shouldBe` a2)
                            , expectListField 1 #amount (`shouldBe` a1)
                            ]
                        }
                    , TestCase -- 3
                        { query =
                            toQueryString
                                [ ("start", utcIso8601ToText t1)
                                , ("end", utcIso8601ToText $ minusDelta t2)
                                ]
                        , assertions =
                            [ expectListSize 1
                            , expectListField 0 #amount (`shouldBe` a1)
                            ]
                        }
                    , TestCase -- 4
                        { query =
                            toQueryString
                                [("start", utcIso8601ToText t1)]
                        , assertions =
                            [ expectListSize 2
                            , expectListField 0 #amount (`shouldBe` a2)
                            , expectListField 1 #amount (`shouldBe` a1)
                            ]
                        }
                    , TestCase -- 5
                        { query =
                            toQueryString
                                [ ("start", utcIso8601ToText $ plusDelta t1)
                                , ("end", utcIso8601ToText $ plusDelta t2)
                                ]
                        , assertions =
                            [ expectListSize 1
                            , expectListField 0 #amount (`shouldBe` a2)
                            ]
                        }
                    , TestCase -- 6
                        { query =
                            toQueryString
                                [ ("start", utcIso8601ToText $ plusDelta t1)
                                , ("end", utcIso8601ToText $ minusDelta t2)
                                ]
                        , assertions =
                            [expectListSize 0]
                        }
                    , TestCase -- 7
                        { query =
                            toQueryString
                                [ ("start", utcIso8601ToText $ plusDelta t1)
                                , ("order", "ascending")
                                ]
                        , assertions =
                            [ expectListSize 1
                            , expectListField 0 #amount (`shouldBe` a2)
                            ]
                        }
                    , TestCase -- 8
                        { query =
                            toQueryString
                                [ ("order", "descending")
                                , ("start", utcIso8601ToText $ plusDelta t1)
                                , ("end", utcIso8601ToText t2)
                                ]
                        , assertions =
                            [ expectListSize 1
                            , expectListField 0 #amount (`shouldBe` a2)
                            ]
                        }
                    , TestCase -- 9
                        { query =
                            toQueryString
                                [ ("order", "ascending")
                                , ("start", utcIso8601ToText $ minusDelta t1)
                                , ("end", utcIso8601ToText $ minusDelta t2)
                                ]
                        , assertions =
                            [ expectListSize 1
                            , expectListField 0 #amount (`shouldBe` a1)
                            ]
                        }
                    , TestCase -- 10
                        { query =
                            toQueryString
                                [ ("order", "descending")
                                , ("start", utcIso8601ToText $ minusDelta t1)
                                ]
                        , assertions =
                            [ expectListSize 2
                            , expectListField 0 #amount (`shouldBe` a2)
                            , expectListField 1 #amount (`shouldBe` a1)
                            ]
                        }
                    , TestCase -- 11
                        { query =
                            toQueryString
                                [ ("start", utcIso8601ToText $ minusDelta t1)
                                , ("end", utcIso8601ToText t2)
                                ]
                        , assertions =
                            [ expectListSize 2
                            , expectListField 0 #amount (`shouldBe` a2)
                            , expectListField 1 #amount (`shouldBe` a1)
                            ]
                        }
                    , TestCase -- 12
                        { query =
                            toQueryString
                                [ ("start", utcIso8601ToText $ minusDelta t1)
                                , ("end", utcIso8601ToText $ plusDelta t2)
                                ]
                        , assertions =
                            [ expectListSize 2
                            , expectListField 0 #amount (`shouldBe` a2)
                            , expectListField 1 #amount (`shouldBe` a1)
                            ]
                        }
                    , TestCase -- 13
                        { query = mempty
                        , assertions =
                            [ expectListSize 2
                            , expectListField 0 #amount (`shouldBe` a2)
                            , expectListField 1 #amount (`shouldBe` a1)
                            ]
                        }
                    , TestCase -- 14
                        { query =
                            toQueryString
                                [("end", utcIso8601ToText t2)]
                        , assertions =
                            [ expectListSize 2
                            , expectListField 0 #amount (`shouldBe` a2)
                            , expectListField 1 #amount (`shouldBe` a1)
                            ]
                        }
                    , TestCase -- 15
                        { query =
                            toQueryString
                                [("end", utcIso8601ToText $ plusDelta t2)]
                        , assertions =
                            [ expectListSize 2
                            , expectListField 0 #amount (`shouldBe` a2)
                            , expectListField 1 #amount (`shouldBe` a1)
                            ]
                        }
                    , TestCase -- 16
                        { query =
                            toQueryString
                                [("end", utcIso8601ToText $ minusDelta t2)]
                        , assertions =
                            [ expectListSize 1
                            , expectListField 0 #amount (`shouldBe` a1)
                            ]
                        }
                    , TestCase -- 17
                        { query =
                            toQueryString
                                [ ("start", utcIso8601ToText t1)
                                , ("end", utcIso8601ToText t1)
                                ]
                        , assertions =
                            [ expectListSize 1
                            , expectListField 0 #amount (`shouldBe` a1)
                            ]
                        }
                    , TestCase -- 18
                        { query =
                            toQueryString
                                [ ("start", utcIso8601ToText t2)
                                , ("end", utcIso8601ToText t2)
                                ]
                        , assertions =
                            [ expectListSize 1
                            , expectListField 0 #amount (`shouldBe` a2)
                            ]
                        }
                    , TestCase -- 19
                        { query =
                            toQueryString
                                [ ("max_count", "1")
                                ]
                        , assertions =
                            [ expectListSize 1
                            , expectListField 0 #amount (`shouldBe` a2)
                            ]
                        }
                    , TestCase -- 20
                        { query =
                            toQueryString
                                [ ("max_count", "1")
                                , ("order", "ascending")
                                ]
                        , assertions =
                            [ expectListSize 1
                            , expectListField 0 #amount (`shouldBe` a1)
                            ]
                        }
                    ]

            let withQuery q (method, link) = (method, link <> q)

            liftIO $ forM_ matrix $ \tc -> do
                let link = withQuery (query tc) $ Link.listTransactions @'Shelley w
                rf <- request @([ApiTransaction n]) ctx link Default Empty
                verify rf (assertions tc)

    describe "TRANS_LIST_02,03 - Faulty start, end, order values" $ do
        let orderErr =
                "Please specify one of the following values:\
                \ ascending, descending."
        let startEndErr =
                "Expecting ISO 8601 date-and-time format\
                \ (basic or extended), e.g. 2012-09-25T10:15:00Z."
        let queries :: [TestCase [ApiTransaction n]] =
                [ TestCase
                    { query = toQueryString [("start", "2009")]
                    , assertions =
                        [ expectResponseCode HTTP.status400
                        , expectErrorMessage startEndErr
                        ]
                    }
                , TestCase
                    { query =
                        toQueryString
                            [ ("start", "2012-09-25T10:15:00Z")
                            , ("end", "2016-11-21")
                            ]
                    , assertions =
                        [ expectResponseCode HTTP.status400
                        , expectErrorMessage startEndErr
                        ]
                    }
                , TestCase
                    { query =
                        toQueryString
                            [ ("start", "2012-09-25")
                            , ("end", "2016-11-21T10:15:00Z")
                            ]
                    , assertions =
                        [ expectResponseCode HTTP.status400
                        , expectErrorMessage startEndErr
                        ]
                    }
                , TestCase
                    { query =
                        toQueryString
                            [ ("end", "2012-09-25T10:15:00Z")
                            , ("start", "2016-11-21")
                            ]
                    , assertions =
                        [ expectResponseCode HTTP.status400
                        , expectErrorMessage startEndErr
                        ]
                    }
                , TestCase
                    { query = toQueryString [("order", "scending")]
                    , assertions =
                        [ expectResponseCode HTTP.status400
                        , expectErrorMessage orderErr
                        ]
                    }
                , TestCase
                    { query =
                        toQueryString
                            [ ("start", "2012-09-25T10:15:00Z")
                            , ("order", "asc")
                            ]
                    , assertions =
                        [ expectResponseCode HTTP.status400
                        , expectErrorMessage orderErr
                        ]
                    }
                ]

        let withQuery q (method, link) = (method, link <> q)

        forM_ queries $ \tc -> it (T.unpack $ query tc)
            $ \ctx -> runResourceT $ do
                w <- emptyWallet ctx
                let link =
                        withQuery (query tc)
                            $ Link.listTransactions @'Shelley w
                r <- request @([ApiTransaction n]) ctx link Default Empty
                liftIO $ verify r (assertions tc)

    it "TRANS_LIST_02 - Start time shouldn't be later than end time"
        $ \ctx -> runResourceT $ do
            w <- emptyWallet ctx
            let startTime = "2009-09-09T09:09:09Z"
            let endTime = "2001-01-01T01:01:01Z"
            let link =
                    Link.listTransactions' @'Shelley
                        w
                        Nothing
                        (either (const Nothing) Just $ fromText $ T.pack startTime)
                        (either (const Nothing) Just $ fromText $ T.pack endTime)
                        Nothing
                        Nothing
                        Nothing
            r <- request @([ApiTransaction n]) ctx link Default Empty
            expectResponseCode HTTP.status400 r
            expectErrorMessage
                (errMsg400StartTimeLaterThanEndTime startTime endTime)
                r
            pure ()

    it "TRANS_LIST_03 - Minimum withdrawal shouldn't be 0"
        $ \ctx -> runResourceT $ do
            w <- emptyWallet ctx
            let link =
                    Link.listTransactions' @'Shelley
                        w
                        (Just 0)
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
            r <- request @([ApiTransaction n]) ctx link Default Empty
            expectResponseCode HTTP.status400 r
            decodeErrorInfo r `shouldBe` MinWithdrawalWrong

    it
        "TRANS_LIST_03 - \
        \Minimum withdrawal can be 1, shows empty when no withdrawals"
        $ \ctx -> runResourceT $ do
            w <- emptyWallet ctx
            let link =
                    Link.listTransactions' @'Shelley
                        w
                        (Just 1)
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
            r <- request @([ApiTransaction n]) ctx link Default Empty
            expectResponseCode HTTP.status200 r
            let txs = getFromResponse Prelude.id r
            txs `shouldBe` []

    it "TRANS_LIST_04 - Deleted wallet" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        _ <-
            request @ApiWallet
                ctx
                (Link.deleteWallet @'Shelley w)
                Default
                Empty
        r <-
            request @([ApiTransaction n])
                ctx
                (Link.listTransactions @'Shelley w)
                Default
                Empty
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    it
        "TRANS_LIST_RANGE_01 - \
        \Transaction at time t is SELECTED by small ranges that cover it"
        $ \ctx -> runResourceT $ do
            w <- fixtureWalletWith @n ctx [minUTxOValue (_mainEra ctx)]
            t <- unsafeGetTransactionTime =<< listAllTransactions @n ctx w
            let (te, tl) = (utcTimePred t, utcTimeSucc t)
            txs1 <-
                listTransactions @n
                    ctx
                    w
                    (Just t)
                    (Just t)
                    Nothing
                    Nothing
            txs2 <-
                listTransactions @n
                    ctx
                    w
                    (Just te)
                    (Just t)
                    Nothing
                    Nothing
            txs3 <-
                listTransactions @n
                    ctx
                    w
                    (Just t)
                    (Just tl)
                    Nothing
                    Nothing
            txs4 <-
                listTransactions @n
                    ctx
                    w
                    (Just te)
                    (Just tl)
                    Nothing
                    Nothing
            length <$> [txs1, txs2, txs3, txs4] `shouldSatisfy` all (== 1)

    it
        "TRANS_LIST_RANGE_02 - \
        \Transaction at time t is NOT selected by range (t + 𝛿t, ...)"
        $ \ctx -> runResourceT $ do
            w <- fixtureWalletWith @n ctx [minUTxOValue (_mainEra ctx)]
            t <- unsafeGetTransactionTime =<< listAllTransactions @n ctx w
            let tl = utcTimeSucc t
            txs1 <-
                listTransactions @n
                    ctx
                    w
                    (Just tl)
                    (Nothing)
                    Nothing
                    Nothing
            txs2 <-
                listTransactions @n
                    ctx
                    w
                    (Just tl)
                    (Just tl)
                    Nothing
                    Nothing
            length <$> [txs1, txs2] `shouldSatisfy` all (== 0)

    it
        "TRANS_LIST_RANGE_03 - \
        \Transaction at time t is NOT selected by range (..., t - 𝛿t)"
        $ \ctx -> runResourceT $ do
            w <- fixtureWalletWith @n ctx [minUTxOValue (_mainEra ctx)]
            t <- unsafeGetTransactionTime =<< listAllTransactions @n ctx w
            let te = utcTimePred t
            txs1 <-
                listTransactions @n
                    ctx
                    w
                    (Nothing)
                    (Just te)
                    Nothing
                    Nothing
            txs2 <-
                listTransactions @n
                    ctx
                    w
                    (Just te)
                    (Just te)
                    Nothing
                    Nothing
            length <$> [txs1, txs2] `shouldSatisfy` all (== 0)

    it
        "TRANS_LIST_LIMIT_01 - \
        \Transactions can be limited"
        $ \ctx -> runResourceT $ do
            w <- fixtureWallet ctx
            txs <- listLimitedTransactions @n ctx w 9
            length txs `shouldBe` 9

    it "TRANS_GET_01 - Can get Incoming and Outgoing transaction"
        $ \ctx -> runResourceT $ do
            (wSrc, wDest) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
            -- post tx
            let amt = minUTxOValue (_mainEra ctx) :: Natural
            rMkTx <-
                postTx @n
                    ctx
                    (wSrc, Link.createTransactionOld @'Shelley, "cardano-wallet")
                    wDest
                    amt
            let txid = getFromResponse #id rMkTx
            verify
                rMkTx
                [ expectSuccess
                , expectResponseCode HTTP.status202
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` Pending)
                ]

            eventually "Wallet balance is as expected" $ do
                rGet <-
                    request @ApiWallet
                        ctx
                        (Link.getWallet @'Shelley wDest)
                        Default
                        Empty
                verify
                    rGet
                    [ expectField
                        (#balance . #total)
                        (`shouldBe` ApiAmount amt)
                    , expectField
                        (#balance . #available)
                        (`shouldBe` ApiAmount amt)
                    ]

            eventually "Transactions are available and in ledger" $ do
                -- Verify Tx in source wallet is Outgoing and InLedger
                let linkSrc =
                        Link.getTransaction @'Shelley
                            wSrc
                            (ApiTxId txid)
                r1 <- request @(ApiTransaction n) ctx linkSrc Default Empty
                verify
                    r1
                    [ expectResponseCode HTTP.status200
                    , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                    , expectField (#status . #getApiT) (`shouldBe` InLedger)
                    ]

                -- Verify Tx in destination wallet is Incoming and InLedger
                let linkDest =
                        Link.getTransaction
                            @'Shelley
                            wDest
                            (ApiTxId txid)
                r2 <- request @(ApiTransaction n) ctx linkDest Default Empty
                verify
                    r2
                    [ expectResponseCode HTTP.status200
                    , expectField (#direction . #getApiT) (`shouldBe` Incoming)
                    , expectField (#status . #getApiT) (`shouldBe` InLedger)
                    ]

    it "TRANS_GET_02 - Deleted wallet" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        _ <-
            request @ApiWallet
                ctx
                (Link.deleteWallet @'Shelley w)
                Default
                Empty
        let txid = ApiT $ Hash $ BS.pack $ replicate 32 1
        let link = Link.getTransaction @'Shelley w (ApiTxId txid)
        r <- request @(ApiTransaction n) ctx link Default Empty
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    it "TRANS_GET_03 - Using wrong transaction id" $ \ctx -> runResourceT $ do
        (wSrc, wDest) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
        -- post tx
        let amt = minUTxOValue (_mainEra ctx) :: Natural
        rMkTx <-
            postTx @n
                ctx
                (wSrc, Link.createTransactionOld @'Shelley, "cardano-wallet")
                wDest
                amt
        verify
            rMkTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            ]

        let txid = Hash $ BS.pack $ replicate 32 1
        let link =
                Link.getTransaction @'Shelley
                    wSrc
                    (ApiTxId $ ApiT txid)
        r <- request @(ApiTransaction n) ctx link Default Empty
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404CannotFindTx $ toText txid) r

    it "TRANS_GET_04 - Sumbitted transactions result in pending state"
        $ \ctx -> runResourceT $ do
            (wSrc, wDest) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
            -- post tx
            let amt = minUTxOValue (_mainEra ctx) :: Natural
            rMkTx <-
                postTx @n
                    ctx
                    (wSrc, Link.createTransactionOld @'Shelley, "cardano-wallet")
                    wDest
                    amt
            verify
                rMkTx
                [ expectSuccess
                , expectResponseCode HTTP.status202
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` Pending)
                ]

    it
        "TRANS_DELETE_01 -\
        \ Shelley: Can forget pending transaction"
        $ \ctx -> runResourceT $ do
            (wSrc, wDest) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
            -- post tx
            let amt = minUTxOValue (_mainEra ctx) :: Natural
            rMkTx <-
                postTx @n
                    ctx
                    (wSrc, Link.createTransactionOld @'Shelley, "cardano-wallet")
                    wDest
                    amt
            let txid = getFromResponse #id rMkTx
            verify
                rMkTx
                [ expectSuccess
                , expectResponseCode HTTP.status202
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` Pending)
                ]

            -- forget transaction
            (statusDelete, _) <-
                request @ApiTxId
                    ctx
                    (Link.deleteTransaction @'Shelley wSrc (ApiTxId txid))
                    Default
                    Empty
            rBalance <-
                getFromResponse (#balance . #total)
                    <$> request @ApiWallet
                        ctx
                        (Link.getWallet @'Shelley wSrc)
                        Default
                        Empty

            let assertSourceTx = do
                    let ep = Link.listTransactions @'Shelley wSrc
                    request @[ApiTransaction n] ctx ep Default Empty
                        >>= flip
                            verify
                            [ expectListField
                                0
                                (#direction . #getApiT)
                                (`shouldBe` Outgoing)
                            , expectListField
                                0
                                (#status . #getApiT)
                                (`shouldBe` InLedger)
                            ]

            -- We cannot guarantee that we're able to forget the tx before it is
            -- accepted. The slot length is really fast in the integration tests.
            --
            -- As a workaround we also pass if the tx is already accepted.
            case (statusDelete, rBalance) of
                (s, balance)
                    | s == HTTP.status204 && balance == ApiAmount faucetAmt ->
                        eventually
                            "transaction eventually is in source wallet"
                            assertSourceTx
                    | s == HTTP.status204 && balance < ApiAmount faucetAmt ->
                        assertSourceTx
                    | s == HTTP.status403 -> do
                        assertSourceTx
                        balance .< ApiAmount faucetAmt
                _ ->
                    expectationFailure
                        $ "invalid combination of results: "
                            <> show statusDelete
                            <> ", wallet balance="
                            <> show rBalance

            eventually "transaction eventually is in target wallet" $ do
                let ep = Link.listTransactions @'Shelley wDest
                request @[ApiTransaction n] ctx ep Default Empty
                    >>= flip
                        verify
                        [ expectListField
                            0
                            (#direction . #getApiT)
                            (`shouldBe` Incoming)
                        , expectListField
                            0
                            (#status . #getApiT)
                            (`shouldBe` InLedger)
                        ]

    it
        "TRANS_DELETE_02 -\
        \ Shelley: Cannot forget tx that is already in ledger"
        $ \ctx -> runResourceT $ do
            (wSrc, wDest) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx

            -- post transaction
            rTx <-
                postTx @n
                    ctx
                    (wSrc, Link.createTransactionOld @'Shelley, "cardano-wallet")
                    wDest
                    (minUTxOValue (_mainEra ctx) :: Natural)
            let txid = getFromResponse #id rTx

            eventually "Transaction is accepted" $ do
                let ep = Link.listTransactions @'Shelley wSrc
                request @([ApiTransaction n]) ctx ep Default Empty
                    >>= flip
                        verify
                        [ expectListField
                            0
                            (#direction . #getApiT)
                            (`shouldBe` Outgoing)
                        , expectListField
                            0
                            (#status . #getApiT)
                            (`shouldBe` InLedger)
                        ]

            -- Try Forget transaction once it's no longer pending
            let ep = Link.deleteTransaction @'Shelley wSrc (ApiTxId txid)
            rDel <- request @ApiTxId ctx ep Default Empty
            expectResponseCode HTTP.status403 rDel
            let err = errMsg403AlreadyInLedger (toUrlPiece (ApiTxId txid))
            expectErrorMessage err rDel

    describe "TRANS_DELETE_03 - checking no transaction id error for " $ do
        txDeleteNotExistsingTxIdTest emptyWallet "wallets"
        txDeleteNotExistsingTxIdTest emptyRandomWallet "byron-wallets"

    describe
        "TRANS_DELETE_06 -\
        \ Cannot forget tx that is performed from different wallet"
        $ do
            txDeleteFromDifferentWalletTest emptyWallet "wallets"
            txDeleteFromDifferentWalletTest emptyRandomWallet "byron-wallets"

    it "SHELLEY_TX_REDEEM_01 - Can redeem rewards from self"
        $ \ctx -> runResourceT $ do
            (wSrc, _) <- rewardWallet ctx
            addr : _ <- fmap (view #id) <$> listAddresses @n ctx wSrc

            let payload =
                    Json
                        [json|{
                "withdrawal": "self",
                "payments": [{
                    "address": #{addr},
                    "amount": {
                        "quantity": #{minUTxOValue (_mainEra ctx)},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": #{fixturePassphrase}
            }|]
            rTx <-
                request @(ApiTransaction n)
                    ctx
                    (Link.createTransactionOld @'Shelley wSrc)
                    Default
                    payload
            verify
                rTx
                [ expectResponseCode HTTP.status202
                , expectField
                    #withdrawals
                    (`shouldSatisfy` (not . null))
                ]

            eventually "rewards are transferred from self to self" $ do
                rW <-
                    request @ApiWallet
                        ctx
                        (Link.getWallet @'Shelley wSrc)
                        Default
                        payload
                verify
                    rW
                    [ expectField
                        (#balance . #available)
                        (.> (wSrc ^. #balance . #available))
                    , expectField
                        (#balance . #reward)
                        (`shouldBe` ApiAmount 0)
                    ]

    it "SHELLEY_TX_REDEEM_02 - Can redeem rewards from other"
        $ \ctx -> runResourceT $ do
            wSelf <- fixtureWallet ctx
            addr : _ <- fmap (view #id) <$> listAddresses @n ctx wSelf
            (wOther, SomeMnemonic mw) <- rewardWallet ctx
            let rewardBalance = wOther ^. #balance . #reward . #toNatural

            let payload =
                    Json
                        [json|{
                "withdrawal": #{mnemonicToText mw},
                "payments": [{
                    "address": #{addr},
                    "amount": {
                        "quantity": #{minUTxOValue (_mainEra ctx)},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": #{fixturePassphrase}
            }|]
            (_, ApiFee (ApiAmount estimatedFeeMin) (ApiAmount estimatedFeeMax) _ _) <-
                unsafeRequest
                    ctx
                    (Link.getTransactionFeeOld @'Shelley wSelf)
                    payload
            rTx <-
                request @(ApiTransaction n)
                    ctx
                    (Link.createTransactionOld @'Shelley wSelf)
                    Default
                    payload
            let ApiAmount fee = getFromResponse #fee rTx
            verify
                rTx
                [ expectResponseCode HTTP.status202
                , expectField
                    #withdrawals
                    (`shouldSatisfy` (not . null))
                , expectField
                    (#direction . #getApiT)
                    (`shouldBe` Incoming)
                , expectField
                    (#amount . #toNatural)
                    (.>= (rewardBalance - fee))
                , -- TODO: Drop https://cardanofoundation.atlassian.net/browse/ADP-2935
                  expectField
                    (#fee . #toNatural)
                    (between (estimatedFeeMin, estimatedFeeMax))
                ]
            let tid = getFromResponse Prelude.id rTx

            eventually "rewards disappear from other" $ do
                rWOther <-
                    request @ApiWallet
                        ctx
                        (Link.getWallet @'Shelley wOther)
                        Default
                        payload
                verify
                    rWOther
                    [ expectField
                        (#balance . #reward)
                        (`shouldBe` ApiAmount 0)
                    ]

            eventually "withdrawal transaction is listed on other" $ do
                rTxOther <-
                    request @(ApiTransaction n)
                        ctx
                        (Link.getTransaction @'Shelley wOther tid)
                        Default
                        payload
                verify
                    rTxOther
                    [ expectResponseCode
                        HTTP.status200
                    , expectField
                        #withdrawals
                        (`shouldSatisfy` (not . null))
                    , expectField
                        (#direction . #getApiT)
                        (`shouldBe` Outgoing)
                    , expectField
                        (#amount . #toNatural)
                        (.>= rewardBalance)
                    ]

            eventually "rewards appear on self" $ do
                rWSelf <-
                    request @ApiWallet
                        ctx
                        (Link.getWallet @'Shelley wSelf)
                        Default
                        payload
                verify
                    rWSelf
                    [ expectField
                        (#balance . #available)
                        (.> (wSelf ^. #balance . #available))
                    ]

            eventually "withdrawal transaction is listed on self" $ do
                rTxSelf <-
                    request @(ApiTransaction n)
                        ctx
                        (Link.getTransaction @'Shelley wSelf tid)
                        Default
                        payload
                verify
                    rTxSelf
                    [ expectResponseCode
                        HTTP.status200
                    , expectField
                        #withdrawals
                        (`shouldSatisfy` (not . null))
                    , expectField
                        (#direction . #getApiT)
                        (`shouldBe` Incoming)
                    , expectField
                        (#amount . #toNatural)
                        (.>= (rewardBalance - estimatedFeeMax))
                    , expectField
                        (#status . #getApiT)
                        (`shouldBe` InLedger)
                    ]

    it "SHELLEY_TX_REDEEM_03 - Can't redeem rewards from other if none left"
        $ \ctx -> runResourceT $ do
            (wOther, SomeMnemonic mw) <- rewardWallet ctx
            wSelf <- fixtureWallet ctx
            addr : _ <- fmap (view #id) <$> listAddresses @n ctx wSelf

            let payload =
                    Json
                        [json|{
                "withdrawal": #{mnemonicToText mw},
                "payments": [{
                    "address": #{addr},
                    "amount": {
                        "quantity": #{minUTxOValue (_mainEra ctx) },
                        "unit": "lovelace"
                    }
                }],
                "passphrase": #{fixturePassphrase}
            }|]

            -- Withdraw rewards from the other wallet.
            _ <-
                request @(ApiTransaction n)
                    ctx
                    (Link.createTransactionOld @'Shelley wSelf)
                    Default
                    payload
            eventually "rewards disappear from other" $ do
                rWOther <-
                    request @ApiWallet
                        ctx
                        (Link.getWallet @'Shelley wOther)
                        Default
                        payload
                verify
                    rWOther
                    [ expectField
                        (#balance . #reward)
                        (`shouldBe` ApiAmount 0)
                    ]

            -- Try withdrawing AGAIN, rewards that aren't there anymore.
            rTx <-
                request @(ApiTransaction n)
                    ctx
                    (Link.createTransactionOld @'Shelley wSelf)
                    Default
                    payload
            verify
                rTx
                [ expectResponseCode HTTP.status403
                , expectErrorMessage errMsg403WithdrawalNotBeneficial
                ]

    it "SHELLEY_TX_REDEEM_04 - Can always ask for self redemption"
        $ \ctx -> runResourceT $ do
            wSelf <- fixtureWallet ctx
            addr : _ <- fmap (view #id) <$> listAddresses @n ctx wSelf

            let payload =
                    Json
                        [json|{
                "withdrawal": "self",
                "payments": [{
                    "address": #{addr},
                    "amount": {
                        "quantity": #{minUTxOValue (_mainEra ctx) },
                        "unit": "lovelace"
                    }
                }],
                "passphrase": #{fixturePassphrase}
            }|]

            rTx <-
                request @(ApiTransaction n)
                    ctx
                    (Link.createTransactionOld @'Shelley wSelf)
                    Default
                    payload
            verify
                rTx
                [ expectResponseCode HTTP.status202
                , expectField #withdrawals (`shouldSatisfy` null)
                ]

    it "SHELLEY_TX_REDEEM_05 - Can't redeem rewards from unknown key"
        $ \ctx -> runResourceT $ do
            wSelf <- fixtureWallet ctx
            addr : _ <- fmap (view #id) <$> listAddresses @n ctx wSelf

            mw <- liftIO $ entropyToMnemonic <$> genEntropy @160
            let payload =
                    Json
                        [json|{
                "withdrawal": #{mnemonicToText mw},
                "payments": [{
                    "address": #{addr},
                    "amount": {
                        "quantity": #{minUTxOValue (_mainEra ctx)},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": #{fixturePassphrase}
            }|]

            rTx <-
                request @(ApiTransaction n)
                    ctx
                    (Link.createTransactionOld @'Shelley wSelf)
                    Default
                    payload
            verify
                rTx
                [ expectResponseCode HTTP.status403
                , expectErrorMessage errMsg403WithdrawalNotBeneficial
                ]

    it "SHELLEY_TX_REDEEM_06 - Can't redeem rewards using byron wallet"
        $ \ctx -> runResourceT $ do
            (wSelf, addrs) <- fixtureIcarusWalletAddrs @n ctx
            let addr = encodeAddress (sNetworkId @n) (head addrs)

            let payload =
                    Json
                        [json|{
                "withdrawal": "self",
                "payments": [{
                    "address": #{addr},
                    "amount": {
                        "quantity": #{minUTxOValue (_mainEra ctx) },
                        "unit": "lovelace"
                    }
                }],
                "passphrase": #{fixturePassphrase}
            }|]

            rTx <-
                request @(ApiTransaction n)
                    ctx
                    (Link.createTransactionOld @'Byron wSelf)
                    Default
                    payload
            verify
                rTx
                [ expectResponseCode HTTP.status403
                ]
            decodeErrorInfo rTx `shouldBe` InvalidWalletType

    it "SHELLEY_TX_REDEEM_06a - Can't redeem rewards if utxo = 0 from other"
        $ \ctx -> runResourceT $ do
            (_, SomeMnemonic mw) <- rewardWallet ctx
            wSelf <- emptyWallet ctx
            addr : _ <- fmap (view #id) <$> listAddresses @n ctx wSelf

            let payload =
                    Json
                        [json|{
                "withdrawal": #{mnemonicToText mw},
                "payments": [{
                    "address": #{addr},
                    "amount": {
                        "quantity": #{minUTxOValue (_mainEra ctx) },
                        "unit": "lovelace"
                    }
                }],
                "passphrase": #{fixturePassphrase}
            }|]

            -- Try withdrawing when no UTxO on a wallet
            rTx <-
                request @(ApiTransaction n)
                    ctx
                    (Link.createTransactionOld @'Shelley wSelf)
                    Default
                    payload
            verify rTx [expectResponseCode HTTP.status403]
            decodeErrorInfo rTx `shouldBe` NoUtxosAvailable

    it "SHELLEY_TX_REDEEM_06b - Can't redeem rewards if utxo = 0 from self"
        $ \ctx -> runResourceT $ do
            liftIO $ pendingWith "Migration endpoints temporarily disabled"
            (wRewards, SomeMnemonic mw) <- rewardWallet ctx
            wOther <- emptyWallet ctx

            -- migrate all utxo from rewards wallet
            addr : _ <- fmap (view #id) <$> listAddresses @n ctx wOther
            let payloadMigr =
                    Json
                        [json|{
                "passphrase": #{fixturePassphrase},
                "addresses": [#{addr}]
            }|]

            let ep = Link.migrateWallet @'Shelley wRewards
            rM <- request @[ApiTransaction n] ctx ep Default payloadMigr
            expectResponseCode HTTP.status202 rM

            eventually "No UTxO is on rewards wallet" $ do
                rWOther <-
                    request @ApiWallet
                        ctx
                        (Link.getWallet @'Shelley wRewards)
                        Default
                        Empty
                verify
                    rWOther
                    [ expectField
                        (#balance . #available)
                        (`shouldBe` ApiAmount 0)
                    ]

            -- Try withdrawing when no UTxO on a wallet
            let payload =
                    Json
                        [json|{
                "withdrawal": #{mnemonicToText mw},
                "payments": [{
                    "address": #{addr},
                    "amount": { "quantity": #{oneAda}, "unit": "lovelace" }
                }],
                "passphrase": #{fixturePassphrase}
            }|]
            rTx <-
                request @(ApiTransaction n)
                    ctx
                    (Link.createTransactionOld @'Shelley wRewards)
                    Default
                    payload
            verify rTx [expectResponseCode HTTP.status403]
            decodeErrorInfo rTx `shouldSatisfy` \case
                NotEnoughMoney{} -> True
                _someOtherError -> False

    it "SHELLEY_TX_REDEEM_07b - Can't redeem rewards if not enough money"
        $ \ctx -> runResourceT $ do
            (_, SomeMnemonic mw) <- rewardWallet ctx
            wSelf <- fixtureWalletWith @n ctx [oneThousandAda]
            addr : _ <- fmap (view #id) <$> listAddresses @n ctx wSelf
            let amt = oneThousandAda + oneMillionAda + oneAda

            let payload =
                    Json
                        [json|{
                "withdrawal": #{mnemonicToText mw},
                "payments": [{
                    "address": #{addr},
                    "amount": { "quantity": #{amt}, "unit": "lovelace" }
                }],
                "passphrase": #{fixturePassphrase}
            }|]

            -- Try withdrawing when no not enough money
            rTx <-
                request @(ApiTransaction n)
                    ctx
                    (Link.createTransactionOld @'Shelley wSelf)
                    Default
                    payload
            verify rTx [expectResponseCode HTTP.status403]
            decodeErrorInfo rTx `shouldSatisfy` \case
                NotEnoughMoney{} -> True
                _someOtherError -> False
  where
    spec_createTransactionWithMetadata
        :: CreateTransactionWithMetadataTest
        -> SpecWith Context
    spec_createTransactionWithMetadata testData =
        let CreateTransactionWithMetadataTest
                { testName
                , txOutputAdaQuantities
                , txMetadata
                , expectedFee
                } = testData
        in  it testName $ \ctx -> runResourceT $ do
                when (_mainEra ctx < ApiBabbage)
                    $ liftIO
                    $ pendingWith
                        "expected fees have been updated to Babbage and these \
                        \tests marked pending on earlier eras."

                let maybeAddTxMetadata =
                        maybe
                            (Prelude.id)
                            (addTxMetadata . Aeson.toJSON . ApiT)
                            (txMetadata)

                (wa, wb) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx

                let minUTxOValue' = minUTxOValue (_mainEra ctx)
                let outputQuantities = txOutputAdaQuantities minUTxOValue'
                let paymentCount = length outputQuantities
                targetAddresses <-
                    take paymentCount
                        . fmap (view #id)
                        <$> listAddresses @n ctx wb
                let targetAssets = repeat mempty
                let payments =
                        NE.fromList
                            $ map ($ mempty)
                            $ zipWith
                                (AddressAmount)
                                (targetAddresses)
                                (ApiAmount <$> outputQuantities)
                let outputs =
                        zipWith3
                            ApiCoinSelectionOutput
                            (targetAddresses)
                            (ApiAmount <$> outputQuantities)
                            (targetAssets)

                -- First, perform a dry-run selection using the 'selectCoins' endpoint.
                -- This will allow us to confirm that the 'selectCoins' endpoint
                -- produces a selection whose fee is similar to the selection
                -- produced by the 'postTransaction' endpoint.
                coinSelectionResponse <-
                    selectCoinsWith @n @'Shelley ctx wa payments maybeAddTxMetadata
                verify
                    coinSelectionResponse
                    [ expectResponseCode HTTP.status200
                    , expectField
                        #inputs
                        (`shouldSatisfy` (not . null))
                    , expectField
                        #outputs
                        (`shouldSatisfy` ((Set.fromList outputs ==) . Set.fromList))
                    , expectField
                        #change
                        (`shouldSatisfy` (not . null))
                    ]

                let apiCoinSelection = getFromResponse Prelude.id coinSelectionResponse
                let fee = computeApiCoinSelectionFee apiCoinSelection
                let withinToleranceTo (ApiAmount expected) tol =
                        between (ApiAmount $ expected - tol, ApiAmount $ expected + tol)

                -- Some tolerance is needed as 'selectCoins' doesn't make room for a
                -- validity interval / TTL, whereas 'postTransaction' does. We seem to
                -- need 6 bytes / 600 lovelace, but using a bit more to rule out
                -- flakiness from variable size encoding of slot numbers.
                --
                --
                -- TODO Consider replacing with golden tests on the unit level
                -- for fee values.
                --
                -- Related to https://cardanofoundation.atlassian.net/browse/ADP-2935
                liftIO
                    $ withinToleranceTo
                        expectedFee
                        1_000
                        (ApiAmount.fromCoin fee)

                -- Next, actually create a transaction and submit it to the network.
                -- This transaction should have a fee that is identical to the fee
                -- of the dry-run coin selection produced in the previous step.
                let payload =
                        Json
                            [json|
                { "payments": #{payments}
                , "passphrase": #{fixturePassphrase}
                , "metadata": #{ApiT <$> txMetadata}
                }|]
                ra <-
                    request @(ApiTransaction n)
                        ctx
                        (Link.createTransactionOld @'Shelley wa)
                        Default
                        payload

                verify
                    ra
                    [ expectSuccess
                    , expectResponseCode HTTP.status202
                    , expectField (#status . #getApiT) (`shouldBe` Pending)
                    , expectField
                        #metadata
                        (`shouldBe` detailedMetadata <$> txMetadata)
                    , expectField
                        #fee
                        (withinToleranceTo expectedFee 1_000)
                        -- Rule out flakiness from variable size of slot numbers.
                        --
                        -- TODO Consider replacing with golden tests on the unit level
                        -- for fee values.
                        --
                        -- Related to https://cardanofoundation.atlassian.net/browse/ADP-2935
                    ]

                eventually "metadata is confirmed in transaction list" $ do
                    -- on src wallet
                    let linkSrcList = Link.listTransactions @'Shelley wa
                    rla <- request @([ApiTransaction n]) ctx linkSrcList Default Empty
                    verify
                        rla
                        [ expectResponseCode HTTP.status200
                        , expectListField
                            0
                            (#status . #getApiT)
                            (`shouldBe` InLedger)
                        , expectListField
                            0
                            (#direction . #getApiT)
                            (`shouldBe` Outgoing)
                        , expectListField
                            0
                            (#metadata)
                            (`shouldBe` detailedMetadata <$> txMetadata)
                        ]
                    -- on dst wallet
                    let linkDstList = Link.listTransactions @'Shelley wb
                    rlb <- request @([ApiTransaction n]) ctx linkDstList Default Empty
                    verify
                        rlb
                        [ expectResponseCode HTTP.status200
                        , expectListField
                            0
                            (#status . #getApiT)
                            (`shouldBe` InLedger)
                        , expectListField
                            0
                            (#direction . #getApiT)
                            (`shouldBe` Incoming)
                        , expectListField
                            0
                            (#metadata)
                            (`shouldBe` detailedMetadata <$> txMetadata)
                        ]

                let txid = getFromResponse #id ra
                eventually "metadata is confirmed in transaction get" $ do
                    -- on src wallet
                    let linkSrc = Link.getTransaction @'Shelley wa (ApiTxId txid)
                    rg1 <- request @(ApiTransaction n) ctx linkSrc Default Empty
                    verify
                        rg1
                        [ expectResponseCode HTTP.status200
                        , expectField
                            (#direction . #getApiT)
                            (`shouldBe` Outgoing)
                        , expectField
                            (#status . #getApiT)
                            (`shouldBe` InLedger)
                        , expectField
                            #metadata
                            (`shouldBe` detailedMetadata <$> txMetadata)
                        ]
                    -- on dst wallet
                    let linkDst = Link.getTransaction @'Shelley wb (ApiTxId txid)
                    rg2 <- request @(ApiTransaction n) ctx linkDst Default Empty
                    verify
                        rg2
                        [ expectResponseCode HTTP.status200
                        , expectField
                            (#direction . #getApiT)
                            (`shouldBe` Incoming)
                        , expectField
                            (#status . #getApiT)
                            (`shouldBe` InLedger)
                        , expectField
                            #metadata
                            (`shouldBe` detailedMetadata <$> txMetadata)
                        ]

    txDeleteNotExistsingTxIdTest eWallet resource =
        it resource $ \ctx -> runResourceT $ do
            w <- eWallet ctx
            let walId = w ^. walletId
            let txid = "3e6ec12da4414aa0781ff8afa9717ae53ee8cb4aa55d622f65bc62619a4f7b12"
            let endpoint =
                    "v2/"
                        <> T.pack resource
                        <> "/"
                        <> walId
                        <> "/transactions/"
                        <> txid
            ra <- request @ApiTxId ctx ("DELETE", endpoint) Default Empty
            expectResponseCode HTTP.status404 ra
            expectErrorMessage (errMsg404CannotFindTx txid) ra

    txDeleteFromDifferentWalletTest
        :: (HasType (ApiT WalletId) wal)
        => (Context -> ResourceT IO wal)
        -> String
        -> SpecWith Context
    txDeleteFromDifferentWalletTest eWallet resource =
        it resource $ \ctx -> runResourceT $ do
            -- post tx
            (wSrc, wDest) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
            rMkTx <-
                postTx @n
                    ctx
                    (wSrc, Link.createTransactionOld @'Shelley, "cardano-wallet")
                    wDest
                    (minUTxOValue (_mainEra ctx) :: Natural)

            -- try to forget from different wallet
            wDifferent <- eWallet ctx
            let txid = toText $ getApiT $ getFromResponse #id rMkTx
            let endpoint =
                    "v2/"
                        <> T.pack resource
                        <> "/"
                        <> wDifferent
                        ^. walletId
                        <> "/transactions/"
                        <> txid
            ra <- request @ApiTxId ctx ("DELETE", endpoint) Default Empty
            expectResponseCode HTTP.status404 ra
            expectErrorMessage (errMsg404CannotFindTx txid) ra

    verifyWalletBalance
        :: MonadUnliftIO m
        => Context
        -> ApiWallet
        -> ApiAmount
        -> m ()
    verifyWalletBalance ctx wallet amt = do
        eventually "Wallet Ada balance is as expected" $ do
            rGet <-
                request @ApiWallet
                    ctx
                    (Link.getWallet @'Shelley wallet)
                    Default
                    Empty
            verify
                rGet
                [ expectField
                    (#balance . #total)
                    (`shouldBe` amt)
                , expectField
                    (#balance . #available)
                    (`shouldBe` amt)
                , expectField
                    (#assets . #total)
                    (`shouldBe` mempty)
                , expectField
                    (#assets . #available)
                    (`shouldBe` mempty)
                ]

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
        return
            $ Json
                [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": #{passphrase}
            }|]

    addTxTTL :: Double -> Payload -> Payload
    addTxTTL t (Json (Aeson.Object o)) = Json (Aeson.Object (o <> ttl))
      where
        ttl = "time_to_live" .= [json|{ "quantity": #{t}, "unit": "second"}|]
    addTxTTL _ _ = error "addTxTTL: can't do that"

    addTxMetadata :: Aeson.Value -> Payload -> Payload
    addTxMetadata md (Json (Aeson.Object o)) =
        Json (Aeson.Object (o <> ("metadata" .= md)))
    addTxMetadata _ _ = error "can't do that"

    plusDelta, minusDelta :: UTCTime -> UTCTime
    plusDelta = addUTCTime (toEnum 1_000_000_000)
    minusDelta = addUTCTime (toEnum (-1_000_000_000))

    oneAda :: Natural
    oneAda = 1_000_000

    oneThousandAda :: Natural
    oneThousandAda = 1_000 * oneAda

    oneMillionAda :: Natural
    oneMillionAda = 1_000 * oneThousandAda

data CreateTransactionWithMetadataTest = CreateTransactionWithMetadataTest
    { testName
        :: String
    , txOutputAdaQuantities
        :: Natural
        -> [Natural]
    -- ^ Takes `minUTxOValue` as argument.
    , txMetadata
        :: Maybe TxMetadata
    , expectedFee
        :: ApiAmount
    }
