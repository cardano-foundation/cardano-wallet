{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- HLINT ignore "Use head" -}

module Test.Integration.Scenario.API.Shelley.TransactionsNew
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPub, xpubPublicKey )
import Cardano.Api
    ( CardanoEra (..), InAnyCardanoEra (..) )
import Cardano.Crypto.DSIGN.Class
    ( rawDeserialiseVerKeyDSIGN )
import Cardano.Mnemonic
    ( SomeMnemonic (..), mnemonicToText )
import Cardano.Wallet.Api.Types
    ( AddressAmount (..)
    , ApiAddress (..)
    , ApiAnyCertificate (..)
    , ApiCertificate (..)
    , ApiCoinSelection (withdrawals)
    , ApiConstructTransaction (..)
    , ApiDecodedTransaction
    , ApiDeregisterPool (..)
    , ApiExternalCertificate (..)
    , ApiRegisterPool (..)
    , ApiSerialisedTransaction (..)
    , ApiStakePool
    , ApiT (..)
    , ApiTransaction
    , ApiTxId (..)
    , ApiTxInput (..)
    , ApiTxInputGeneral (..)
    , ApiTxMetadata (..)
    , ApiTxOutputGeneral (..)
    , ApiWallet
    , ApiWalletInput (..)
    , ApiWalletOutput (..)
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress (..)
    , EncodeStakeAddress
    , ResourceContext (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DerivationIndex (..)
    , HardDerivation (..)
    , PaymentAddress (..)
    , Role (..)
    , WalletKey (..)
    , hex
    )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.Types
    ( EpochNo (..)
    , NonWalletCertificate (..)
    , PoolId (..)
    , PoolOwner (..)
    , StakePoolMetadataHash (..)
    , StakePoolMetadataUrl (..)
    , decodePoolIdBech32
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( AssetId (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..), TokenPolicyId (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (..)
    , SealedTx (..)
    , TxIn (..)
    , TxMetadata (..)
    , TxMetadataValue (..)
    , TxScriptValidity (..)
    , TxStatus (..)
    , cardanoTx
    , getSealedTxBody
    , sealedTxFromCardanoBody
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex, unsafeMkMnemonic )
import Control.Arrow
    ( second )
import Control.Monad
    ( foldM_, forM_ )
import Control.Monad.IO.Unlift
    ( MonadIO (..), MonadUnliftIO (..), liftIO )
import Control.Monad.Trans.Resource
    ( runResourceT )
import Data.Aeson
    ( toJSON, (.=) )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Generics.Sum
    ( _Ctor )
import Data.Maybe
    ( fromJust, isJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..), mkPercentage )
import Data.Ratio
    ( (%) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( toText )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith, describe, pendingWith, shouldContain, shouldNotContain )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldNotBe, shouldNotSatisfy, shouldSatisfy )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , arbitraryStake
    , delegating
    , emptyIcarusWallet
    , emptyRandomWallet
    , emptyWallet
    , eventually
    , expectErrorMessage
    , expectField
    , expectListField
    , expectListSize
    , expectResponseCode
    , expectSuccess
    , fixtureMultiAssetWallet
    , fixturePassphrase
    , fixtureWallet
    , fixtureWalletWith
    , fixtureWalletWithMnemonics
    , getFromResponse
    , getSomeVerificationKey
    , json
    , listAddresses
    , minUTxOValue
    , notDelegating
    , pickAnAsset
    , request
    , rewardWallet
    , selectCoins
    , signTx
    , submitTxWithWid
    , unsafeRequest
    , verify
    , waitForNextEpoch
    , waitForTxImmutability
    , walletId
    , (.>)
    )
import Test.Integration.Framework.TestData
    ( errMsg403Collateral
    , errMsg403Fee
    , errMsg403ForeignTransaction
    , errMsg403InvalidConstructTx
    , errMsg403MinUTxOValue
    , errMsg403MissingWitsInTransaction
    , errMsg403MultiaccountTransaction
    , errMsg403MultidelegationTransaction
    , errMsg403NonNullReward
    , errMsg403NotDelegating
    , errMsg403NotEnoughMoney
    , errMsg404NoSuchPool
    , errMsg404NoWallet
    )
import UnliftIO.Exception
    ( fromEither )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Crypto as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Wallet.Api.Link as Link
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shelley as Shelley
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types.Status as HTTP
import qualified Test.Integration.Plutus as PlutusScenario


spec :: forall n.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeStakeAddress n
    , EncodeAddress n
    , PaymentAddress n IcarusKey
    ) => SpecWith Context
spec = describe "NEW_SHELLEY_TRANSACTIONS" $ do
    it "TRANS_NEW_CREATE_01a - Empty payload is not allowed" $ \ctx -> runResourceT $ do
        wa <- fixtureWallet ctx
        let emptyPayload = Json [json|{}|]

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default emptyPayload
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403InvalidConstructTx
            ]

    it "TRANS_NEW_CREATE_01b - Validity interval only is not allowed" $ \ctx -> runResourceT $ do
        wa <- fixtureWallet ctx
        let validityInterval = Json [json|{
                "validity_interval": {
                    "invalid_before": {
                      "quantity": 10,
                      "unit": "second"
                    },
                    "invalid_hereafter": {
                      "quantity": 50,
                      "unit": "second"
                    }
                  }
                }|]

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default validityInterval
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403InvalidConstructTx
            ]

    it "TRANS_NEW_CREATE_01c - No payload is bad request" $ \ctx -> runResourceT $ do
        wa <- fixtureWallet ctx

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default Empty
        verify rTx
            [ expectResponseCode HTTP.status400
            ]

    it "TRANS_NEW_CREATE_02 - Only metadata" $ \ctx -> runResourceT $ do
        wa <- fixtureWallet ctx
        let metadata = Json [json|{ "metadata": { "1": { "string": "hello" } } }|]

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default metadata
        verify rTx
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #metadata) (`shouldSatisfy` isJust)
            , expectField (#fee . #getQuantity) (`shouldSatisfy` (>0))
            ]

        let expectedFee = getFromResponse (#fee . #getQuantity) rTx
        let apiTx = getFromResponse #transaction rTx
        signedTx <- signTx ctx wa apiTx [ expectResponseCode HTTP.status202 ]

        -- Check for the presence of metadata on signed transaction
        let
            getMetadata (InAnyCardanoEra _ tx) = Cardano.getTxBody tx
                        & (\(Cardano.TxBody bodyContent) ->
                               Cardano.txMetadata bodyContent
                               & \case Cardano.TxMetadataNone ->
                                           Nothing
                                       Cardano.TxMetadataInEra _ (Cardano.TxMetadata m) ->
                                           Just m
                          )

        case getMetadata (cardanoTx $ getApiT (signedTx ^. #transaction)) of
            Nothing -> error "Tx doesn't include metadata"
            Just m  -> case Map.lookup 1 m of
                Nothing -> error "Tx doesn't include metadata"
                Just (Cardano.TxMetaText "hello") -> pure ()
                Just _ -> error "Tx metadata incorrect"

        let txCbor = getFromResponse #transaction (HTTP.status202, Right signedTx)
        let decodePayload = Json (toJSON $ ApiSerialisedTransaction txCbor)
        let expMetadata = ApiT (TxMetadata (Map.fromList [(1,TxMetaText "hello")]))
        rDecodedTx <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley wa) Default decodePayload
        verify rDecodedTx
            [ expectResponseCode HTTP.status202
            , expectField #metadata (`shouldBe` (ApiTxMetadata (Just expMetadata)))
            ]

        -- Submit tx
        submittedTx <- submitTxWithWid ctx wa signedTx
        verify submittedTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        -- Make sure only fee is deducted from fixtureWallet
        eventually "Wallet balance is as expected" $ do
            rWa <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wa) Default Empty
            verify rWa
                [ expectSuccess
                , expectField
                        (#balance . #available . #getQuantity)
                        (`shouldBe` (fromIntegral oneMillionAda - expectedFee))
                ]

    it "TRANS_NEW_CREATE_03a - Withdrawal from self, 0 rewards" $ \ctx -> runResourceT $ do
        wa <- fixtureWallet ctx
        let initialBalance = wa ^. #balance . #available . #getQuantity
        let withdrawal = Json [json|{ "withdrawal": "self" }|]

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default withdrawal
        verify rTx
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #metadata) (`shouldBe` Nothing)
            , expectField (#coinSelection . #withdrawals) (`shouldSatisfy` (not . null))
            ]
        let expectedFee = getFromResponse (#fee . #getQuantity) rTx

        let apiTx = getFromResponse #transaction rTx

        signedTx <- signTx ctx wa apiTx [ expectResponseCode HTTP.status202 ]

        let txCbor = getFromResponse #transaction (HTTP.status202, Right signedTx)
        let decodePayload = Json (toJSON $ ApiSerialisedTransaction txCbor)
        rDecodedTx <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley wa) Default decodePayload
        verify rDecodedTx
            [ expectResponseCode HTTP.status202
            , expectField #withdrawals (`shouldBe` [])
            ]

        -- Submit tx
        submittedTx <- submitTxWithWid ctx wa signedTx
        verify submittedTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        -- Make sure wallet balance is decreased by fee, since rewards = 0
        eventually "Wallet balance is decreased by fee" $ do
            rWa <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wa) Default Empty
            verify rWa
                [ expectSuccess
                , expectField
                        (#balance . #available . #getQuantity)
                        (`shouldBe` initialBalance - fromIntegral expectedFee)
                , expectField
                        (#balance . #reward . #getQuantity)
                        (`shouldBe` 0)
                ]

    it "TRANS_NEW_CREATE_03a - Withdrawal from self" $ \ctx -> runResourceT $ do
        (wa, _) <- rewardWallet ctx
        let withdrawal = Json [json|{ "withdrawal": "self" }|]
        let withdrawalAmt = 1_000_000_000_000
        let rewardInitialBalance = 100_000_000_000

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default withdrawal
        verify rTx
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #metadata) (`shouldBe` Nothing)
            , expectField (#fee . #getQuantity) (`shouldSatisfy` (>0))
            , expectField (#coinSelection . #withdrawals) (`shouldSatisfy` (not . null))
            ]

        let expectedFee = getFromResponse (#fee . #getQuantity) rTx
        let apiTx = getFromResponse #transaction rTx

        signedTx <- signTx ctx wa apiTx [ expectResponseCode HTTP.status202 ]

        let txCbor = getFromResponse #transaction (HTTP.status202, Right signedTx)
        let decodePayload = Json (toJSON $ ApiSerialisedTransaction txCbor)
        let withdrawalWith ownership wdrls = case wdrls of
                [wdrl] ->
                    wdrl ^. #amount == Quantity withdrawalAmt &&
                    wdrl ^. #context == ownership
                _ -> False

        rDecodedTx <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley wa) Default decodePayload
        verify rDecodedTx
            [ expectResponseCode HTTP.status202
            , expectField #withdrawals (`shouldSatisfy` (withdrawalWith Our))
            ]

        -- Submit tx
        submittedTx <- submitTxWithWid ctx wa signedTx
        verify submittedTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        -- Make sure wallet balance is increased by withdrawalAmt - fee
        eventually "Wallet balance is increased by withdrawalAmt - fee" $ do
            rWa <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wa) Default Empty
            verify rWa
                [ expectSuccess
                , expectField
                        (#balance . #available . #getQuantity)
                        (`shouldBe` rewardInitialBalance + (withdrawalAmt - fromIntegral expectedFee))
                ]

        eventually "Reward balance is 0" $ do
            rWa <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wa) Default Empty
            verify rWa
                [ expectSuccess
                , expectField
                        (#balance . #reward . #getQuantity)
                        (`shouldBe` 0)
                ]

        wb <- fixtureWallet ctx
        rDecodedTx' <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley wb) Default decodePayload
        verify rDecodedTx'
            [ expectResponseCode HTTP.status202
            , expectField #withdrawals (`shouldSatisfy` (withdrawalWith External))
            ]

    it "TRANS_NEW_CREATE_03b - Withdrawal from external wallet" $ \ctx -> runResourceT $ do

        liftIO $ pendingWith "ADP-1189: Issues with withdrawals from external wallets"

        (wr, mw) <- rewardWallet ctx
        wa <- fixtureWallet ctx
        let withdrawal = Json [json|{ "withdrawal": #{mnemonicToText mw} }|]
        let withdrawalAmt = 1000000000000
        let rewardInitialBalance = 100_000_000_000

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default withdrawal
        verify rTx
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #metadata) (`shouldBe` Nothing)
            , expectField (#fee . #getQuantity) (`shouldSatisfy` (>0))
            , expectField (#coinSelection . #withdrawals) (`shouldSatisfy` (not . null))
            ]

        let expectedFee = getFromResponse (#fee . #getQuantity) rTx
        let apiTx = getFromResponse #transaction rTx

        signedTx <- signTx ctx wa apiTx [ expectResponseCode HTTP.status202 ]

        submittedTx <- submitTxWithWid ctx wa signedTx
        verify submittedTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        -- Make sure wallet balance is increased by withdrawalAmt - fee
        eventually "Wallet balance is increased by withdrawalAmt - fee" $ do
            rWa <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wa) Default Empty
            verify rWa
                [ expectSuccess
                , expectField
                        (#balance . #available . #getQuantity)
                        (`shouldBe` rewardInitialBalance + (withdrawalAmt - fromIntegral expectedFee))
                ]

        -- Check that reward account is 0 on external rewardWallet,
        eventually "Reward wallet balance is zero" $ do
            rWr <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wr) Default Empty
            verify rWr
                [ expectSuccess
                , expectField
                        (#balance . #available . #getQuantity)
                        (`shouldBe` rewardInitialBalance)
                ]

    it "TRANS_NEW_CREATE_04a - Single Output Transaction with decode transaction" $ \ctx -> runResourceT $ do

        let initialAmt = 3 * minUTxOValue (_mainEra ctx)
        wa <- fixtureWalletWith @n ctx [initialAmt]
        wb <- emptyWallet ctx
        let amt = (minUTxOValue (_mainEra ctx) :: Natural)

        payload <- liftIO $ mkTxPayload ctx wb amt

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#coinSelection . #inputs) (`shouldSatisfy` (not . null))
            , expectField (#coinSelection . #outputs) (`shouldSatisfy` (not . null))
            , expectField (#coinSelection . #change) (`shouldSatisfy` (not . null))
            , expectField (#fee . #getQuantity) (`shouldSatisfy` (> 0))
            ]
        let expectedFee = getFromResponse (#fee . #getQuantity) rTx
        let txCbor = getFromResponse #transaction rTx
        let decodePayload = Json (toJSON $ ApiSerialisedTransaction txCbor)
        let sharedExpectationsBetweenWallets =
                [ expectResponseCode HTTP.status202
                , expectField (#fee . #getQuantity) (`shouldBe` expectedFee)
                , expectField #withdrawals (`shouldBe` [])
                , expectField #collateral (`shouldBe` [])
                , expectField #metadata (`shouldBe` (ApiTxMetadata Nothing))
                , expectField #scriptValidity (`shouldBe` (Just $ ApiT TxScriptValid))
                ]

        -- After constructing tx the cbor is as expected, both wallets share common information
        -- source wallet sees inputs as his, target wallet sees them as external
        let isInpOurs inp = case inp of
                ExternalInput _ -> False
                WalletInput _ -> True
        let areOurs = all isInpOurs
        addrs <- listAddresses @n ctx wb
        let addrIx = 1
        let addrDest = (addrs !! addrIx) ^. #id
        let expectedTxOutTarget = WalletOutput $ ApiWalletOutput
                { address = addrDest
                , amount = Quantity amt
                , assets = ApiT TokenMap.empty
                , derivationPath = NE.fromList
                    [ ApiT (DerivationIndex 2147485500)
                    , ApiT (DerivationIndex 2147485463)
                    , ApiT (DerivationIndex 2147483648)
                    , ApiT (DerivationIndex 0)
                    , ApiT (DerivationIndex $ fromIntegral addrIx)
                    ]
                }
        let isOutOurs out = case out of
                WalletOutput _ -> False
                ExternalOutput _ -> True

        rDecodedTxSource <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley wa) Default decodePayload
        verify rDecodedTxSource $
            sharedExpectationsBetweenWallets ++
            [ expectField #inputs (`shouldSatisfy` areOurs)
            , expectField #outputs (`shouldNotContain` [expectedTxOutTarget])

            -- Check that the change output is there:
            , expectField (#outputs) ((`shouldBe` 1) . length . filter isOutOurs)
            ]

        rDecodedTxTarget <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley wb) Default decodePayload
        verify rDecodedTxTarget $
            sharedExpectationsBetweenWallets ++
            [ expectField #inputs (`shouldNotSatisfy` areOurs)
            , expectField #outputs (`shouldContain` [expectedTxOutTarget])
            ]

        let filterInitialAmt =
                filter $ \(ApiWalletInput _ _ _ _ amt' _) ->
                    amt' == Quantity initialAmt
        let coinSelInputs = filterInitialAmt $
                getFromResponse (#coinSelection . #inputs) rTx
        length coinSelInputs `shouldBe` 1

        let apiTx = getFromResponse #transaction rTx

        signedTx <- signTx ctx wa apiTx [ expectResponseCode HTTP.status202 ]

        submittedTx <- submitTxWithWid ctx wa signedTx
        verify submittedTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        eventually "Target wallet balance is decreased by amt + fee" $ do
            rWa <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wb) Default Empty
            verify rWa
                [ expectSuccess
                , expectField
                        (#balance . #available . #getQuantity)
                        (`shouldBe` amt)
                ]

        eventually "Source wallet balance is decreased by (amt + expectedFee)" $ do
            rWa <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wa) Default Empty
            verify rWa
                [ expectSuccess
                , expectField
                        (#balance . #available . #getQuantity)
                        (`shouldBe` (initialAmt - amt - expectedFee))
                ]

        -- After signing tx the cbor is as before modulo added wtinesses,
        -- and in line what was there after construction. Also as we tx was
        -- accommodated in ledger output change in amount for target wallet
        let expectedTxOutTarget' = WalletOutput $ ApiWalletOutput
                { address = addrDest
                , amount = Quantity amt
                , assets = ApiT TokenMap.empty
                , derivationPath = NE.fromList
                    [ ApiT (DerivationIndex 2147485500)
                    , ApiT (DerivationIndex 2147485463)
                    , ApiT (DerivationIndex 2147483648)
                    , ApiT (DerivationIndex 0)
                    , ApiT (DerivationIndex $ fromIntegral addrIx)
                    ]
                }
        addrsSourceAll <- listAddresses @n ctx wa
        --we expect change address here with x=0 as this wallet does not participated in outcoming tx before this one
        let derPath = NE.fromList
                [ ApiT (DerivationIndex 2147485500)
                , ApiT (DerivationIndex 2147485463)
                , ApiT (DerivationIndex 2147483648)
                , ApiT (DerivationIndex 1)
                , ApiT (DerivationIndex 0)
                ]
        let addrSourceChange:_ =
                filter (\(ApiAddress _ _ derPath') -> derPath == derPath') addrsSourceAll
        let addrSrc =  addrSourceChange ^. #id
        let expectedTxOutSource = WalletOutput $ ApiWalletOutput
                { address = addrSrc
                , amount = Quantity $ initialAmt - (amt + fromIntegral expectedFee)
                , assets = ApiT TokenMap.empty
                , derivationPath = derPath
                }
        let txCbor' = getFromResponse #transaction (HTTP.status202, Right signedTx)
        let decodePayload' = Json (toJSON $ ApiSerialisedTransaction txCbor')
        rDecodedTxSource' <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley wa) Default decodePayload'
        verify rDecodedTxSource' $
            sharedExpectationsBetweenWallets ++
            [ expectField #inputs (`shouldNotSatisfy` areOurs) -- the input is not anymore belonging to wallet
            , expectField #outputs (`shouldNotContain` [expectedTxOutTarget'])
            , expectField #outputs (`shouldContain` [expectedTxOutSource])
            ]

        rDecodedTxTarget' <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley wb) Default decodePayload'
        verify rDecodedTxTarget' $
            sharedExpectationsBetweenWallets ++
            [ expectField #inputs (`shouldNotSatisfy` areOurs)
            , expectField #outputs (`shouldContain` [expectedTxOutTarget'])
            , expectField #outputs (`shouldNotContain` [expectedTxOutSource])
            ]

    it "TRANS_NEW_CREATE_04ab - Constructed inputs = Decoded inputs" $ \ctx -> runResourceT $ do
        wa <- fixtureWallet ctx
        wb <- emptyWallet ctx
        let amt = (minUTxOValue (_mainEra ctx) :: Natural)
        payload <- liftIO $ mkTxPayload ctx wb amt

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx [ expectSuccess ]

        let expectedInputs = getFromResponse (#coinSelection . #inputs) rTx
        let txCbor = getFromResponse #transaction rTx
        let decodePayload = Json (toJSON $ ApiSerialisedTransaction txCbor)

        rDecodedTxSource <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley wa) Default decodePayload
        let decodedInputs = getFromResponse #inputs rDecodedTxSource

        WalletInput <$> expectedInputs `shouldBe` decodedInputs

    it "TRANS_NEW_CREATE_04b - Cannot spend less than minUTxOValue" $ \ctx -> runResourceT $ do
        wa <- fixtureWallet ctx
        wb <- emptyWallet ctx
        let amt = minUTxOValue (_mainEra ctx) - 1

        payload <- liftIO $ mkTxPayload ctx wb amt

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403MinUTxOValue
            ]

    it "TRANS_NEW_CREATE_04c - Can't cover fee" $ \ctx -> runResourceT $ do
        wa <- fixtureWalletWith @n ctx [minUTxOValue (_mainEra ctx) + 1]
        wb <- emptyWallet ctx

        payload <- liftIO $ mkTxPayload ctx wb (minUTxOValue (_mainEra ctx))

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403Fee
            ]

    it "TRANS_NEW_CREATE_04d - Not enough money" $ \ctx -> runResourceT $ do
        let minUTxOValue' = minUTxOValue (_mainEra ctx)
        let (srcAmt, reqAmt) = (minUTxOValue', 2 * minUTxOValue')
        wa <- fixtureWalletWith @n ctx [srcAmt]
        wb <- emptyWallet ctx

        payload <- liftIO $ mkTxPayload ctx wb reqAmt

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403NotEnoughMoney
            ]

    it "TRANS_NEW_CREATE_04d - Not enough money emptyWallet" $ \ctx -> runResourceT $ do
        wa <- emptyWallet ctx
        wb <- emptyWallet ctx

        payload <- liftIO $ mkTxPayload ctx wb (minUTxOValue (_mainEra ctx))

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403NotEnoughMoney
            ]

    it "TRANS_NEW_CREATE_04e- Multiple Output Tx to single wallet" $ \ctx -> runResourceT $ do
        wa <- fixtureWallet ctx
        wb <- emptyWallet ctx
        addrs <- listAddresses @n ctx wb
        initialAmt <- getFromResponse (#balance . #available . #getQuantity) <$>
                          request @ApiWallet ctx (Link.getWallet @'Shelley wa) Default Empty

        let amt = minUTxOValue (_mainEra ctx) :: Natural
        let destination1 = (addrs !! 1) ^. #id
        let destination2 = (addrs !! 2) ^. #id
        let payload = Json [json|{
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
                }]
            }|]

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#coinSelection . #inputs) (`shouldSatisfy` (not . null))
            , expectField (#coinSelection . #outputs) (`shouldSatisfy` (not . null))
            , expectField (#coinSelection . #change) (`shouldSatisfy` (not . null))
            , expectField (#fee . #getQuantity) (`shouldSatisfy` (> 0))
            ]

        let expectedFee = getFromResponse (#fee . #getQuantity) rTx
        let apiTx = getFromResponse #transaction rTx

        signedTx <- signTx ctx wa apiTx [ expectResponseCode HTTP.status202 ]

        submittedTx <- submitTxWithWid ctx wa signedTx
        verify submittedTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        eventually "Target wallet balance is increased by 2*amt" $ do
            rWb <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wb) Default Empty
            verify rWb
                [ expectSuccess
                , expectField
                        (#balance . #available . #getQuantity)
                        (`shouldBe` 2*amt)
                ]

        eventually "Source wallet balance is decreased by (2*amt + expectedFee)" $ do
            rWa <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wa) Default Empty
            verify rWa
                [ expectSuccess
                , expectField
                        (#balance . #available . #getQuantity)
                        (`shouldBe` (initialAmt - 2*amt - expectedFee))
                ]

    it "TRANS_NEW_CREATE_04a - Single Output Transaction with submitWithWid" $ \ctx -> runResourceT $ do

        let initialAmt = 3 * minUTxOValue (_mainEra ctx)
        wa <- fixtureWalletWith @n ctx [initialAmt]
        wb <- emptyWallet ctx
        let amt = (minUTxOValue (_mainEra ctx) :: Natural)

        payload <- liftIO $ mkTxPayload ctx wb amt

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#coinSelection . #inputs) (`shouldSatisfy` (not . null))
            , expectField (#coinSelection . #outputs) (`shouldSatisfy` (not . null))
            , expectField (#coinSelection . #change) (`shouldSatisfy` (not . null))
            ]
        let expectedFee = getFromResponse (#fee . #getQuantity) rTx

        let apiTx = getFromResponse #transaction rTx

        signedTx <- signTx ctx wa apiTx [ expectResponseCode HTTP.status202 ]

        submittedTx <- submitTxWithWid ctx wa signedTx
        verify submittedTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]
        let txid = getFromResponse (#id) submittedTx

        let queryTx = Link.getTransaction @'Shelley wa (ApiTxId txid)
        rGetTx <- request @(ApiTransaction n) ctx queryTx Default Empty
        verify rGetTx
            [ expectResponseCode HTTP.status200
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]

        eventually "Source wallet balance is decreased by amt + fee" $ do
            rWa <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wa) Default Empty
            verify rWa
                [ expectSuccess
                , expectField
                        (#balance . #available . #getQuantity)
                        (`shouldBe` initialAmt - (amt + fromIntegral expectedFee))
                ]

        eventually "transaction is eventually in ledger after submitting" $ do
            let queryTx' = Link.getTransaction @'Shelley wa (ApiTxId txid)
            rSrc <- request @(ApiTransaction n) ctx queryTx' Default Empty
            verify rSrc
                [ expectResponseCode HTTP.status200
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                ]

        eventually "Target wallet balance is amt" $ do
            rWr <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wb) Default Empty
            verify rWr
                [ expectSuccess
                , expectField
                        (#balance . #available . #getQuantity)
                        (`shouldBe` amt)
                ]

    it "TRANS_NEW_ASSETS_CREATE_01a - Multi-asset tx with Ada" $ \ctx -> runResourceT $ do
        wa <- fixtureMultiAssetWallet ctx
        wb <- emptyWallet ctx
        initialAmt <- getFromResponse (#balance . #available . #getQuantity) <$>
                          request @ApiWallet ctx (Link.getWallet @'Shelley wa) Default Empty

        -- pick out an asset to send
        let assetsSrc = wa ^. #assets . #total . #getApiT
        assetsSrc `shouldNotBe` mempty
        let val = minUTxOValue (_mainEra ctx) <$ pickAnAsset assetsSrc

        -- create payload
        addrs <- listAddresses @n ctx wb
        let destination = (addrs !! 1) ^. #id
        let amt = 2 * minUTxOValue (_mainEra ctx)
        payload <- mkTxPayloadMA @n destination amt [val]

        --construct transaction
        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#coinSelection . #inputs) (`shouldSatisfy` (not . null))
            , expectField (#coinSelection . #outputs) (`shouldSatisfy` (not . null))
            , expectField (#coinSelection . #change) (`shouldSatisfy` (not . null))
            , expectField (#fee . #getQuantity) (`shouldSatisfy` (> 0))
            ]

        let expectedFee = getFromResponse (#fee . #getQuantity) rTx
        let apiTx = getFromResponse #transaction rTx

        signedTx <- signTx ctx wa apiTx [ expectResponseCode HTTP.status202 ]

        submittedTx <- submitTxWithWid ctx wa signedTx
        verify submittedTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        eventually "Target wallet balance is increased by amt and assets" $ do
            rWb <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wb) Default Empty
            verify rWb
                [ expectSuccess
                , expectField
                        (#balance . #available . #getQuantity)
                        (`shouldBe` amt)
                , expectField (#assets . #available . #getApiT) (`shouldNotBe` TokenMap.empty)
                , expectField (#assets . #total . #getApiT) (`shouldNotBe` TokenMap.empty)
                ]

        eventually "Source wallet balance is decreased by (amt + expectedFee)" $ do
            rWa <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wa) Default Empty
            verify rWa
                [ expectSuccess
                , expectField
                        (#balance . #available . #getQuantity)
                        (`shouldBe` (initialAmt - amt - expectedFee))
                ]

    it "TRANS_NEW_ASSETS_CREATE_01b - Multi-asset tx with not enough Ada" $ \ctx -> runResourceT $ do
        wa <- fixtureMultiAssetWallet ctx
        wb <- emptyWallet ctx
        ra <- request @ApiWallet ctx (Link.getWallet @'Shelley wa) Default Empty
        let (_, Right wal) = ra

        -- pick out an asset to send
        let assetsSrc = wal ^. #assets . #total . #getApiT
        assetsSrc `shouldNotBe` mempty
        let val = minUTxOValue (_mainEra ctx) <$ pickAnAsset assetsSrc

        -- create payload
        addrs <- listAddresses @n ctx wb
        let destination = (addrs !! 1) ^. #id
        let amt = minUTxOValue (_mainEra ctx)
        payload <- mkTxPayloadMA @n destination amt [val]

        --construct transaction
        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage "Some outputs have ada values that are too small."
            ]

    it "TRANS_NEW_ASSETS_CREATE_01c - Multi-asset tx without Ada" $ \ctx -> runResourceT $ do
        wa <- fixtureMultiAssetWallet ctx
        wb <- emptyWallet ctx
        initialAmt <- getFromResponse (#balance . #available . #getQuantity) <$>
                          request @ApiWallet ctx (Link.getWallet @'Shelley wa) Default Empty

        -- pick out an asset to send
        let assetsSrc = wa ^. #assets . #total . #getApiT
        assetsSrc `shouldNotBe` mempty
        let val = minUTxOValue (_mainEra ctx) <$ pickAnAsset assetsSrc

        -- create payload
        addrs <- listAddresses @n ctx wb
        let destination = (addrs !! 1) ^. #id
        let amt = 0
        payload <- mkTxPayloadMA @n destination amt [val]

        --construct transaction
        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#coinSelection . #inputs) (`shouldSatisfy` (not . null))
            , expectField (#coinSelection . #outputs) (`shouldSatisfy` (not . null))
            , expectField (#coinSelection . #change) (`shouldSatisfy` (not . null))
            , expectField (#fee . #getQuantity) (`shouldSatisfy` (> 0))
            ]

        let expectedFee = getFromResponse (#fee . #getQuantity) rTx
        let apiTx = getFromResponse #transaction rTx

        signedTx <- signTx ctx wa apiTx [ expectResponseCode HTTP.status202 ]

        submittedTx <- submitTxWithWid ctx wa signedTx
        verify submittedTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]
        let txId = getFromResponse (#id) submittedTx

        outTxAmt <- eventually "Transactions is in ledger" $ do
            let linkSrc = Link.getTransaction @'Shelley wa txId
            r1 <- request @(ApiTransaction n) ctx linkSrc Default Empty
            verify r1
                [ expectResponseCode HTTP.status200
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                ]
            pure $ getFromResponse (#amount . #getQuantity) r1

        let inTxAmt = outTxAmt - expectedFee
        eventually "Target wallet balance is increased by inTxAmt and assets" $ do
            rWb <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wb) Default Empty
            verify rWb
                [ expectSuccess
                , expectField
                        (#balance . #available . #getQuantity)
                        (`shouldBe` inTxAmt)
                , expectField (#assets . #available . #getApiT) (`shouldNotBe` TokenMap.empty)
                , expectField (#assets . #total . #getApiT) (`shouldNotBe` TokenMap.empty)
                ]

        eventually "Source wallet balance is decreased by outTxAmt" $ do
            rWa <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wa) Default Empty
            verify rWa
                [ expectSuccess
                , expectField
                        (#balance . #available . #getQuantity)
                        (`shouldBe` (initialAmt - outTxAmt))
                ]

    it "TRANS_NEW_ASSETS_CREATE_01d - Multi-asset tx with not enough assets" $ \ctx -> runResourceT $ do
        wa <- fixtureMultiAssetWallet ctx
        wb <- emptyWallet ctx
        ra <- request @ApiWallet ctx (Link.getWallet @'Shelley wa) Default Empty
        let (_, Right wal) = ra

        let minUTxOValue' = minUTxOValue (_mainEra ctx)

        -- pick out an asset to send
        let assetsSrc = wal ^. #assets . #total . #getApiT
        assetsSrc `shouldNotBe` mempty
        let val = (minUTxOValue' * minUTxOValue') <$ pickAnAsset assetsSrc

        -- create payload
        addrs <- listAddresses @n ctx wb
        let destination = (addrs !! 1) ^. #id
        let amt = 0
        payload <- mkTxPayloadMA @n destination amt [val]

        --construct transaction
        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403NotEnoughMoney
            ]

    it "TRANS_NEW_VALIDITY_INTERVAL_01a - Validity interval with second" $ \ctx -> runResourceT $ do
        wa <- fixtureWallet ctx
        wb <- emptyWallet ctx
        addrs <- listAddresses @n ctx wb
        let destination = (addrs !! 1) ^. #id
        let amt = minUTxOValue (_mainEra ctx)

        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "validity_interval": {
                    "invalid_before": {
                      "quantity": 0,
                      "unit": "second"
                    },
                    "invalid_hereafter": {
                      "quantity": 500,
                      "unit": "second"
                    }
                  }
                }|]

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectResponseCode HTTP.status202
            ]

        let apiTx = getFromResponse #transaction rTx

        signedTx <- signTx ctx wa apiTx [ expectResponseCode HTTP.status202 ]

        submittedTx <- submitTxWithWid ctx wa signedTx
        verify submittedTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

    it "TRANS_NEW_VALIDITY_INTERVAL_01b - Validity interval with slot" $ \ctx -> runResourceT $ do
        wa <- fixtureWallet ctx

        let payload = Json [json|{
                "withdrawal": "self",
                "validity_interval": {
                    "invalid_before": {
                      "quantity": 0,
                      "unit": "slot"
                    },
                    "invalid_hereafter": {
                      "quantity": 500,
                      "unit": "slot"
                    }
                  }
                }|]

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectResponseCode HTTP.status202
            ]

        let apiTx = getFromResponse #transaction rTx

        signedTx <- signTx ctx wa apiTx [ expectResponseCode HTTP.status202 ]

        submittedTx <- submitTxWithWid ctx wa signedTx
        verify submittedTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

    it "TRANS_NEW_VALIDITY_INTERVAL_02 - Validity interval second should be >= 0" $ \ctx -> runResourceT $ do

        liftIO $ pendingWith "Accepted but should be 403 - to be fixed in ADP-1189"

        wa <- fixtureWallet ctx

        let payload = Json [json|{
                "withdrawal": "self",
                "validity_interval": {
                    "invalid_before": {
                      "quantity": -1,
                      "unit": "second"
                    },
                    "invalid_hereafter": {
                      "quantity": -1,
                      "unit": "second"
                    }
                  }
                }|]

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectResponseCode HTTP.status403
            ]

    it "TRANS_NEW_VALIDITY_INTERVAL_02 - Validity interval slot should be >= 0" $ \ctx -> runResourceT $ do

        liftIO $ pendingWith "Returns 400, I think it should be 403 - to be fixed in ADP-1189"

        wa <- fixtureWallet ctx

        let payload = Json [json|{
                "withdrawal": "self",
                "validity_interval": {
                    "invalid_before": {
                      "quantity": -1,
                      "unit": "slot"
                    },
                    "invalid_hereafter": {
                      "quantity": -1,
                      "unit": "slot"
                    }
                  }
                }|]

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectResponseCode HTTP.status403
            ]

    it "TRANS_NEW_VALIDITY_INTERVAL_02 - Validity interval 'unspecified'" $ \ctx -> runResourceT $ do

        liftIO $ pendingWith
          "Currently throws: \
          \parsing ApiValidityBound object failed, \
          \expected Object, but encountered String \
          \- to be fixed in ADP-1189"

        wa <- fixtureWallet ctx

        let payload = Json [json|{
                "withdrawal": "self",
                "validity_interval": {
                    "invalid_before": "unspecified",
                    "invalid_hereafter": "unspecified"
                  }
                }|]

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectResponseCode HTTP.status202
            ]

    it "TRANS_DECODE_01a - multiple-output transaction with all covering inputs" $ \ctx -> runResourceT $ do

        -- constructing source wallet
        let initialAmt = minUTxOValue (_mainEra ctx)
        wa <- fixtureWalletWith @n ctx [initialAmt]

        -- The normal tx was created for some wallets and they are different than the wa.
        -- The transaction involves four outputs with the amounts :
        -- 999978
        -- 999978
        -- 49998927722
        -- 49998927722
        -- incurs the fee of
        -- 144600
        -- and involves one external input
        -- 100000000000
        -- no metadata, no collaterals, no withdrawals
        let serializedTxHex =
                "84a600818258200eaa33be8780935ca5a7c1e628a2d54402446f96236c\
                \a8f1770e07fa22ba8648000d80018482583901a65f0e7aea387adbc109\
                \123a571cfd8d0d139739d359caaf966aa5b9a062de6ec013404d4f9909\
                \877d452fc57dfe4f8b67f94e0ea1e8a0ba1a000f422a82583901ac9a56\
                \280ec283eb7e12146726bfe68dcd69c7a85123ce2f7a10e7afa062de6e\
                \c013404d4f9909877d452fc57dfe4f8b67f94e0ea1e8a0ba1a000f422a\
                \825839011a2f2f103b895dbe7388acc9cc10f90dc4ada53f46c841d2ac\
                \44630789fc61d21ddfcbd4d43652bf05c40c346fa794871423b65052d7\
                \614c1b0000000ba42b176a82583901c59701fee28ad31559870ecd6ea9\
                \2b143b1ce1b68ccb62f8e8437b3089fc61d21ddfcbd4d43652bf05c40c\
                \346fa794871423b65052d7614c1b0000000ba42b176a021a000234d803\
                \198ceb0e80a0f5f6" :: Text

        let theTxHash = Hash "\SO\170\&3\190\135\128\147\\\165\167\193\230(\162\213D\STXDo\150#l\168\241w\SO\a\250\"\186\134H"

        let decodePayload = Json [json|{
              "transaction": #{serializedTxHex}
          }|]
        rTx <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley wa) Default decodePayload
        verify rTx
            [ expectResponseCode HTTP.status202
            , expectField (#fee . #getQuantity) (`shouldBe` 144600)
            , expectField #withdrawals (`shouldBe` [])
            , expectField #collateral (`shouldBe` [])
            , expectField #metadata (`shouldBe` (ApiTxMetadata Nothing))
            , expectField #inputs
                  (`shouldBe` [ExternalInput (ApiT (TxIn theTxHash 0))])
            ]

    it "TRANS_DECODE_02 - transaction with minting/burning assets" $ \ctx -> runResourceT $ do

        -- constructing wallet
        let initialAmt = minUTxOValue (_mainEra ctx)
        wa <- fixtureWalletWith @n ctx [initialAmt]

        -- constructing minting asset tx in cardano-cli
        --- $ cardano-cli transaction build-raw --fee 202725 \
        --- > --tx-in 637255c96ff39303573047ba4a53064c18fbdf8ce8cee71431e8cd5333e4bdfd#0 \
        --- > --tx-out="addr1zyqmnmwuh85e0fxaggl6ac2hfeqncg76gsr0ld8qdjd84af5rh7cflza8t3m5wlaj45sg53nvtwpc73mqk90ghv7vv7srr0dle+4623486815" \
        --- > --tx-out="addr1y9qthemrg5kczwfjjnahwt65elhrl95e9hcgufnajtp6wfgph8kaew0fj7jd6s3l4ms4wnjp8s3a53qxl76wqmy60t6ssqcamq+1500000+50000 919e8a1922aaa764b1d66407c6f62244e77081215f385b60a6209149.4861707079436f696e" \
        --- > --mint="50000 919e8a1922aaa764b1d66407c6f62244e77081215f385b60a6209149.4861707079436f696e" \
        --- > --metadata-json-file metadata.json \
        --- > --mint-script-file policy.script \
        --- > --alonzo-era
        --- > --out-file "txMint"
        let cborHexWithMinting =
                "86a70081825820637255c96ff39303573047ba4a53064c18fbdf8ce8cee714\
                \31e8cd5333e4bdfd000d8001828258391101b9eddcb9e997a4dd423faee157\
                \4e413c23da4406ffb4e06c9a7af5341dfd84fc5d3ae3ba3bfd956904523362\
                \dc1c7a3b058af45d9e633d1b000000011394cf5f8258392140bbe763452d81\
                \393294fb772f54cfee3f96992df08e267d92c3a72501b9eddcb9e997a4dd42\
                \3faee1574e413c23da4406ffb4e06c9a7af5821a0016e360a1581c919e8a19\
                \22aaa764b1d66407c6f62244e77081215f385b60a6209149a1494861707079\
                \436f696e19c350021a000317e50e8009a1581c919e8a1922aaa764b1d66407\
                \c6f62244e77081215f385b60a6209149a1494861707079436f696e19c35007\
                \5820a377b9f8fcbffce4e869c7d02e2052fda1646b59ec534e6fc39de9b505\
                \d565e99f82008201818200581c69303ce3536df260efddbc949ccb94e69933\
                \02b10b778d8b4d98bfb5ff8080f5d90103a100a11902d1a178386565316365\
                \39643735363066343861346261333836373033376462656332643866656437\
                \3736643934646436623030613335333039303733a160a265696d6167657369\
                \7066733a2f2f58585858595959595a5a5a5a646e616d656a54657374204e46\
                \542023" :: Text

        let tokenPolicyId' =
                UnsafeTokenPolicyId (Hash "\145\158\138\EM\"\170\167d\177\214d\a\198\246\"D\231p\129!_8[`\166 \145I")
        let tokens = TokenMap.singleton
                (AssetId tokenPolicyId' (UnsafeTokenName "HappyCoin"))
                (TokenQuantity 50_000)

        let cborHexMint = fromTextEnvelope cborHexWithMinting
        let decodeMintPayload = Json [json|{
              "transaction": #{cborHexMint}
          }|]
        rTx <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley wa) Default decodeMintPayload
        verify rTx
            [ expectResponseCode HTTP.status202
            , expectField (#fee . #getQuantity) (`shouldBe` 202_725)
            , expectField #assetsMinted (`shouldBe` ApiT tokens)
            , expectField #assetsBurned (`shouldBe` ApiT TokenMap.empty)
            ]

        -- constructing burning asset tx in cardano-cli
        --- $ cardano-cli transaction build-raw --fee  202725 \
        --- > --tx-in 72ca58d82fb9e89f91bdd546c3d84bcce92825ec1c49d2c3a180ecc8ab128a52#1 \
        --- > --tx-out="addr1zyqmnmwuh85e0fxaggl6ac2hfeqncg76gsr0ld8qdjd84af5rh7cflza8t3m5wlaj45sg53nvtwpc73mqk90ghv7vv7srr0dle+4623486815" \
        --- > --mint="-50000 919e8a1922aaa764b1d66407c6f62244e77081215f385b60a6209149.HappyCoin" \
        --- > --mint-script-file policy.script \
        --- > --out-file "txBurn" \
        --- > --alonzo-era
        let cborHexWithBurning =
                "86a6008182582072ca58d82fb9e89f91bdd546c3d84bcce92825ec1c49d2c3\
                \a180ecc8ab128a52010d8001818258391101b9eddcb9e997a4dd423faee157\
                \4e413c23da4406ffb4e06c9a7af5341dfd84fc5d3ae3ba3bfd956904523362\
                \dc1c7a3b058af45d9e633d1b000000011394cf5f021a000317e50e8009a158\
                \1c919e8a1922aaa764b1d66407c6f62244e77081215f385b60a6209149a149\
                \4861707079436f696e39c34f9f82008201818200581c69303ce3536df260ef\
                \ddbc949ccb94e6993302b10b778d8b4d98bfb5ff8080f5f6" :: Text

        let cborHexBurn = fromTextEnvelope cborHexWithBurning
        let decodeBurnPayload = Json [json|{
              "transaction": #{cborHexBurn}
          }|]

        rTx' <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley wa) Default decodeBurnPayload
        verify rTx'
            [ expectResponseCode HTTP.status202
            , expectField (#fee . #getQuantity) (`shouldBe` 202_725)
            , expectField #assetsMinted (`shouldBe` ApiT TokenMap.empty)
            , expectField #assetsBurned (`shouldBe` ApiT tokens)
            ]

    it "TRANS_DECODE_03 - transaction with external delegation certificates" $ \ctx -> runResourceT $ do

        -- constructing source wallet
        let initialAmt = minUTxOValue (_mainEra ctx)
        wa <- fixtureWalletWith @n ctx [initialAmt]

        -- tx within some wallet, so other that wallet wa, that contains
        -- registration of reward account and joining to some pool using this
        -- reward account
        let serializedTxHexJoin =
                "84a700818258200eaa33be8780935ca5a7c1e628a2d54402446f96236ca8f1\
                \770e07fa22ba8648060d800181825839011a2f2f103b895dbe7388acc9cc10\
                \f90dc4ada53f46c841d2ac44630789fc61d21ddfcbd4d43652bf05c40c346f\
                \a794871423b65052d7614c1b0000001748656dc8021a000237f803198d1904\
                \8282008200581c89fc61d21ddfcbd4d43652bf05c40c346fa794871423b650\
                \52d7614c83028200581c89fc61d21ddfcbd4d43652bf05c40c346fa7948714\
                \23b65052d7614c581cec28f33dcbe6d6400a1e5e339bd0647c0973ca6c0cf9\
                \c2bbe6838dc60e80a10082825820a922e88e8148f1bb9b9578e2640704ae86\
                \99bdd17bb9c26ed35881343c15ec485840995587f2e29c72c7ed2eb6e2381b\
                \e4745503d7d2ba8de30c6cf176f82fb9074854c39ab85cb7d8e1dcdf9fb990\
                \07de2b044ba7a05ea7712b7084b4d352aa8502825820a1a07799ec226d8f2b\
                \ea80dc1a4fdb25e1b2cb0dbd312a1b004b0401e901225358403ad81b75a057\
                \c419d48e1840bdeba8338206cf3df799d04328c4208b4d7a87248e604a7844\
                \7c2a7acfa4488f3df5c92b01535f756e6ae6e1f23ddb9f438de10ef5f6" :: Text

        let rewardAccount' = ApiT $ RewardAccount "\137\252a\210\GS\223\203\212\212\&6R\191\ENQ\196\f4o\167\148\135\DC4#\182PR\215aL"
        let pool' = ApiT $ PoolId "\236(\243=\203\230\214@\n\RS^3\155\208d|\ts\202l\f\249\194\187\230\131\141\198"

        let certsJoin =
                [ DelegationCertificate (RegisterRewardAccountExternal (rewardAccount', Proxy))
                , DelegationCertificate (JoinPoolExternal (rewardAccount', Proxy) pool')
                ]

        let decodePayloadJoin = Json [json|{
              "transaction": #{serializedTxHexJoin}
          }|]
        rTxJoin <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley wa) Default decodePayloadJoin
        verify rTxJoin
            [ expectResponseCode HTTP.status202
            , expectField #certificates (`shouldBe` certsJoin)
            ]

        -- tx within other wallet than wa, containing quitting the pool
        let serializedTxHexQuit =
                "84a700818258200eaa33be8780935ca5a7c1e628a2d54402446f96236ca8f1\
                \770e07fa22ba8648000d800181825839012c6f08b29f901657f4daf134a36a\
                \64b7b53977eb00866aadf6b8fb4d89fc61d21ddfcbd4d43652bf05c40c346f\
                \a794871423b65052d7614c1b0000001748840b48021a00021ef803198f0304\
                \8182018200581c89fc61d21ddfcbd4d43652bf05c40c346fa794871423b650\
                \52d7614c0e80a10082825820a1a07799ec226d8f2bea80dc1a4fdb25e1b2cb\
                \0dbd312a1b004b0401e901225358408fec23c16be743ee592a948a4a1c9d9a\
                \4529a868d3217386985c30c6d1797c50c4928b6c3aea2825fc0677ea9550ef\
                \e2270447514f1f6e189b73a0234029a802825820c15b990344122b12494a5e\
                \dd1020d9eb32e34b0f82691f8e31645ddab712ff2b58402504005e990a2a6e\
                \db71e6bb1dee0c59e9328e5d698a97139567db4b7754d960ddcb738b852746\
                \c9571d7175c9263a12e40139c93929ef6ba11c1815f792b40cf5f6" :: Text

        let certsQuit =
                [ DelegationCertificate (QuitPoolExternal (rewardAccount', Proxy))
                ]

        let decodePayloadQuit = Json [json|{
              "transaction": #{serializedTxHexQuit}
          }|]
        rTxQuit <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley wa) Default decodePayloadQuit
        verify rTxQuit
            [ expectResponseCode HTTP.status202
            , expectField #certificates (`shouldBe` certsQuit)
            ]

    it "TRANS_DECODE_04 - transaction with mir certificate" $ \ctx -> runResourceT $ do

        -- constructing source wallet
        let initialAmt = minUTxOValue (_mainEra ctx)
        wa <- fixtureWalletWith @n ctx [initialAmt]

        -- this is tx integration cluster sends when setting up, ie. a lot of MIRs
        -- and reward account registrations
        let cborHex =
                "83a50081825820e6f53fa3a753f637c62ee198421bbaef604faee230fe4262\
                \340eedf13a8d0bba00018182581d61161a20f92ea667c30de21e49f01cc4ba\
                \7aadaa9b685a3dab7d4b040a1a000f4240021b00038d7ea3678c40031a3b9a\
                \c9ff049f82008200581cfb3c13a29d3798f1b77b47f2ddb31c19326b87ed6f\
                \71fb9a27133ad582068200a18200581cfb3c13a29d3798f1b77b47f2ddb31c\
                \19326b87ed6f71fb9a27133ad51b000000e8d4a5100082008200581ceb220e\
                \40c3ca1de87c448972443020d8fa08d111a699a4d7b51ba4bc82068200a182\
                \00581ceb220e40c3ca1de87c448972443020d8fa08d111a699a4d7b51ba4bc\
                \1b000000e8d4a5100082008200581cc72a6827138da1341c9ea1e55a04bd10\
                \96824f0c66a0ec282d5daad382068200a18200581cc72a6827138da1341c9e\
                \a1e55a04bd1096824f0c66a0ec282d5daad31b000000e8d4a5100082008200\
                \581c3b525e261b6434b75f949404fbc5b3ef4acf686a31af9facea74687082\
                \068200a18200581c3b525e261b6434b75f949404fbc5b3ef4acf686a31af9f\
                \acea7468701b000000e8d4a5100082008200581c8bf7bb98dd01953c5754fc\
                \f49460aaaa3368faf9476267411a38d33c82068200a18200581c8bf7bb98dd\
                \01953c5754fcf49460aaaa3368faf9476267411a38d33c1b000000e8d4a510\
                \0082008200581c917d3b19a2c9fe13ca2dea4c9b1555257af2f185b58ad483\
                \7e801c1782068200a18200581c917d3b19a2c9fe13ca2dea4c9b1555257af2\
                \f185b58ad4837e801c171b000000e8d4a5100082008200581c37457aadf2fa\
                \6292ebca8460e01ff4e813a495466512b930eb564ec782068200a18200581c\
                \37457aadf2fa6292ebca8460e01ff4e813a495466512b930eb564ec71b0000\
                \00e8d4a5100082008200581cc1e7e3ea91ee7a92f0b33afceab00a41c7039b\
                \f083636689d2de1f0482068200a18200581cc1e7e3ea91ee7a92f0b33afcea\
                \b00a41c7039bf083636689d2de1f041b000000e8d4a5100082008200581c8a\
                \dbd72dd6b5b46b27df79b1f8bcbcf0ac780d515f5b57cb08a2c9a782068200\
                \a18200581c8adbd72dd6b5b46b27df79b1f8bcbcf0ac780d515f5b57cb08a2\
                \c9a71b000000e8d4a5100082008200581c9c19c3caa333ee30c6d5d9f0ddd0\
                \1ad09258a47dec0380519bcae7ac82068200a18200581c9c19c3caa333ee30\
                \c6d5d9f0ddd01ad09258a47dec0380519bcae7ac1b000000e8d4a510008200\
                \8200581c6433cd346858f15142171023c633ae0646bdc0470de5ae6f110bdf\
                \0682068200a18200581c6433cd346858f15142171023c633ae0646bdc0470d\
                \e5ae6f110bdf061b000000e8d4a5100082008200581c63ec6f04e6fa18e830\
                \05a02bc57d72f7afa3a04523d016010076b40a82068200a18200581c63ec6f\
                \04e6fa18e83005a02bc57d72f7afa3a04523d016010076b40a1b000000e8d4\
                \a5100082008200581c990d9d698730cbc4c09f95be75f54e47c524c3e7ad48\
                \4de626ef321482068200a18200581c990d9d698730cbc4c09f95be75f54e47\
                \c524c3e7ad484de626ef32141b000000e8d4a5100082008200581ccc8116d5\
                \0326ea87caa3e46597b54e56725ff1fe39d1bc08361bc20682068200a18200\
                \581ccc8116d50326ea87caa3e46597b54e56725ff1fe39d1bc08361bc2061b\
                \000000e8d4a5100082008200581c246a121534ab486f4e47618cb192568b77\
                \a491cf4db613a80ced4d7682068200a18200581c246a121534ab486f4e4761\
                \8cb192568b77a491cf4db613a80ced4d761b000000e8d4a510008200820058\
                \1c36cf17310d216fc7a2ac6122088766bbc5761129b1155d495bf8112b8206\
                \8200a18200581c36cf17310d216fc7a2ac6122088766bbc5761129b1155d49\
                \5bf8112b1b000000e8d4a5100082008200581c1bb104a403de68b0c03438a1\
                \0d9142a595f4a9ff8162b195493bf55082068200a18200581c1bb104a403de\
                \68b0c03438a10d9142a595f4a9ff8162b195493bf5501b000000e8d4a51000\
                \82008200581cc2a47d500058e60176c437fc61be7d0cd07f0d8c871abe6d00\
                \2636a182068200a18200581cc2a47d500058e60176c437fc61be7d0cd07f0d\
                \8c871abe6d002636a11b000000e8d4a5100082008200581ce8b783e08083c2\
                \3c2682afcd4b7a5cb0851239e5f9c04beb2455979582068200a18200581ce8\
                \b783e08083c23c2682afcd4b7a5cb0851239e5f9c04beb245597951b000000\
                \e8d4a5100082008200581cedf156a660897651bd753aefa7af7f81d6d83dfc\
                \9bcdc4afbd36fad382068200a18200581cedf156a660897651bd753aefa7af\
                \7f81d6d83dfc9bcdc4afbd36fad31b000000e8d4a5100082008200581c30c6\
                \00d4fcf006fc2067721c55fc7d3a696b93a4f382aaad75e3516682068200a1\
                \8200581c30c600d4fcf006fc2067721c55fc7d3a696b93a4f382aaad75e351\
                \661b000000e8d4a5100082008200581cc7d5e024d22767a891e02834b27588\
                \5e06ed04869747cff43cec91e082068200a18200581cc7d5e024d22767a891\
                \e02834b275885e06ed04869747cff43cec91e01b000000e8d4a51000ff9fff\
                \f6" :: Text

        let cborHexMIR = fromTextEnvelope cborHex

        let containMIR certs = OtherCertificate (ApiT MIRCertificate) `elem` certs

        let decodePayloadJoin = Json [json|{
              "transaction": #{cborHexMIR}
          }|]
        rTxJoin <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley wa) Default decodePayloadJoin
        verify rTxJoin
            [ expectResponseCode HTTP.status202
            , expectField #certificates (`shouldSatisfy` containMIR)
            ]

    it "TRANS_DECODE_05 - transaction with pool registration and deregistration certificates" $ \ctx -> runResourceT $ do

        -- constructing source wallet
        let initialAmt = minUTxOValue (_mainEra ctx)
        wa <- fixtureWalletWith @n ctx [initialAmt]

        -- this is tx integration cluster sends when registering one of 4 pools
        let cborHex =
                "83a50081825820fe13857230b6db7f4d30acb043c6cd0c36595657ef79212f\
                \6b4e0cc5d5af1c8b000181825839019ae3b4936cb9e6e6e4fb854d17ed867c\
                \e10f3acdc19cdab52ab4a6de124827f09f6d5029a46b7d09854ac6a9ab16f3\
                \a991c3e2b19ac029511b000000e8d4a51000021b00038c95d0122dc0031901\
                \90048482008200581c124827f09f6d5029a46b7d09854ac6a9ab16f3a991c3\
                \e2b19ac029518a03581cbb114cb37d75fa05260328c235a3dae295a33d0ba6\
                \74a5eb1e3e568e5820ff097f7a12be27b1c4445b43a2d2279cc714a3c47f47\
                \7a6eac3fc0ea54032ab21b000000e8d4a5100000d81e82010a581de1124827\
                \f09f6d5029a46b7d09854ac6a9ab16f3a991c3e2b19ac0295181581c124827\
                \f09f6d5029a46b7d09854ac6a9ab16f3a991c3e2b19ac02951808278246874\
                \74703a2f2f6c6f63616c686f73743a34343130372f6d657461646174612e6a\
                \736f6e5820f1941b06d889a1a9bd8a7dd72d2160aa294d81a4494f99353c6b\
                \bb120746808983028200581c124827f09f6d5029a46b7d09854ac6a9ab16f3\
                \a991c3e2b19ac02951581cbb114cb37d75fa05260328c235a3dae295a33d0b\
                \a674a5eb1e3e568e8304581cbb114cb37d75fa05260328c235a3dae295a33d\
                \0ba674a5eb1e3e568e1a000f42409ffff6" :: Text

        let cborHexPool = fromTextEnvelope cborHex

        let (Right percentage) = mkPercentage (1 % 10)
        let poolId' = PoolId "\187\DC1L\179}u\250\ENQ&\ETX(\194\&5\163\218\226\149\163=\v\166t\165\235\RS>V\142"
        let containRegPool = elem
                (StakePoolRegister ApiRegisterPool
                 { poolId = ApiT poolId'
                 , poolOwners = [ ApiT (PoolOwner "\DC2H'\240\159mP)\164k}\t\133J\198\169\171\SYN\243\169\145\195\226\177\154\192)Q") ]
                 , poolMargin = Quantity percentage
                 , poolCost = Quantity 0
                 , poolPledge = Quantity 1000000000000
                 , poolMetadata =
                         Just (ApiT (StakePoolMetadataUrl "http://localhost:44107/metadata.json")
                              ,ApiT (StakePoolMetadataHash "\241\148\ESC\ACK\216\137\161\169\189\138}\215-!`\170)M\129\164IO\153\&5<k\187\DC2\aF\128\137"))
                 })

        let containDeregPool = elem
                (StakePoolDeregister ApiDeregisterPool
                 { poolId = ApiT poolId'
                 , retirementEpoch = ApiT (EpochNo 1000000)
                 })

        let decodePayloadJoin = Json [json|{
              "transaction": #{cborHexPool}
          }|]
        rTxJoin <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley wa) Default decodePayloadJoin
        verify rTxJoin
            [ expectResponseCode HTTP.status202
            , expectField #certificates (`shouldSatisfy` containRegPool)
            , expectField #certificates (`shouldSatisfy` containDeregPool)
            ]

    it "TRANS_NEW_BALANCE_01d - single-output transaction with missing covering inputs" $ \ctx -> runResourceT $ do

        -- constructing source wallet
        let initialAmt = 110_000_000_000
        let inpAmt = minUTxOValue (_mainEra ctx)
        wa <- fixtureWalletWith @n ctx [initialAmt]

        let serializedTx =
                "84a600818258200eaa33be8780935ca5a7c1e628a2d54402446f96236ca8f1\
                \770e07fa22ba86480d0d800182825839010acce4f85ade867308f048fe4516\
                \c0383b38cc04602ea6f7a6a1e75f29450899547b0e4bb194132452d45fea30\
                \212aebeafc69bca8744ea61a002dc67e8258390110a9b4666ba80e4878491d\
                \1ac20465c9893a8df5581dc705770626203d4d23fe6a7acdda5a1b41f56100\
                \f02bfa270a3c560c4e55cf8312331b00000017484721ca021a0001ffb80319\
                \8d280e80a0f5f6" :: Text
        let balancePayload = Json [json|{
              "transaction": #{serializedTx},
              "redeemers": [],
              "inputs": [
                  { "id" : "0eaa33be8780935ca5a7c1e628a2d54402446f96236ca8f1770e07fa22ba8648"
                  , "index": 13
                  , "address": "addr1vxtlefx3dd5ga5d3cqcfycxsc5tv20txpx7qlmlt2kwnfds2mywcr"
                  , "amount":
                      { "quantity": #{inpAmt}
                      , "unit": "lovelace"
                      }
                  , "assets": []
                  }
              ]
            }|]
        rTx <- request @ApiSerialisedTransaction ctx
            (Link.balanceTransaction @'Shelley wa) Default balancePayload
        verify rTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        -- let apiTx = getFromResponse #transaction rTx
        --
        -- signedTx <- signTx ctx wa apiTx [ expectResponseCode HTTP.status202 ]
        --
        -- void $ submitTx ctx signedTx [ expectResponseCode HTTP.status202 ]

    it "TRANS_NEW_BALANCE_01e - plutus with missing covering inputs wallet enough funds" $ \ctx -> runResourceT $ do

        -- constructing source wallet
        let initialAmt = 110_000_000_000
        wa <- fixtureWalletWith @n ctx [initialAmt]

        let balancePayload = Json [json|{
              "transaction": #{serializedPlutusTx},
              "redeemers": [],
              "inputs": [
                  { "id" : "888963613d2bb4c5c55cee335f724624cbc54b185ecaa2fb1eb07545ed5db421"
                  , "index": 1
                  , "address": "addr1wygn2yjfcgmahn3d62f7wqylstqzlde34s6p0w4esnfzsyqwr2xhg"
                  , "amount":
                      { "quantity": 10
                      , "unit": "lovelace"
                      }
                  , "assets": []
                  }
              ]
          }|]
        rTx <- request @ApiSerialisedTransaction ctx
            (Link.balanceTransaction @'Shelley wa) Default balancePayload
        verify rTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

    it "TRANS_NEW_BALANCE_02a - Cannot balance on empty wallet" $
        \ctx -> runResourceT $ do
        wa <- emptyWallet ctx
        let balancePayload = Json PlutusScenario.pingPong_1
        rTx <- request @ApiSerialisedTransaction ctx
            (Link.balanceTransaction @'Shelley wa) Default balancePayload
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403NotEnoughMoney
            ]

    it "TRANS_NEW_BALANCE_02b - Cannot balance when I cannot afford fee" $
        \ctx -> runResourceT $ do
        wa <- fixtureWalletWith @n ctx [2 * 1_000_000]
        let balancePayload = Json PlutusScenario.pingPong_1
        rTx <- request @ApiSerialisedTransaction ctx
            (Link.balanceTransaction @'Shelley wa) Default balancePayload
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403Fee
            ]

    it "TRANS_NEW_BALANCE_02c - \
        \Cannot balance when I cannot afford collateral" $
        \ctx -> runResourceT $ do
        wa <- fixtureWalletWith @n ctx
            [ 2_000_000
            , 2_000_000
            ]
        let toBalance = Json PlutusScenario.pingPong_1
        rTx <- request @ApiSerialisedTransaction ctx
            (Link.balanceTransaction @'Shelley wa) Default toBalance
        verify rTx
            [ expectResponseCode HTTP.status202
            ]

        let apiTx = getFromResponse #transaction rTx
        signedTx <- signTx ctx wa apiTx [ expectResponseCode HTTP.status202 ]
        submittedTx <- submitTxWithWid ctx wa signedTx
        verify submittedTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]
        let txId = getFromResponse (#id) submittedTx

        waitForTxImmutability ctx
        partialTx' <- PlutusScenario.pingPong_2 $ Aeson.object
            [ "transactionId" .= txId ]
        let toBalance' = Json (toJSON partialTx')

        rTx' <- request @ApiSerialisedTransaction ctx
            (Link.balanceTransaction @'Shelley wa) Default toBalance'
        verify rTx'
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403Collateral
            , expectErrorMessage $ unwords
                [ "I need an ada amount of at least:"
                , "2.779500"
                ]
            , expectErrorMessage $ unwords
                [ "The largest combination of pure ada UTxOs I could find is:"
                , "[1.853000]"
                ]
            ]

    it "TRANS_NEW_BALANCE_03 - I can balance base-64 encoded tx" $
        \ctx -> runResourceT $ do
        wa <- fixtureWallet ctx
        let pingPong1Base64 = Json [json|{
            "transaction": "hKUAgA2AAYGDWB1xTXLPVpozmhin2TAjE5g/VuDZbNRb3LHWUS3KahoAHoSAWCCSORjkA79Dw0tO9rSOsu4Eur7RcyDY0bn/mtCG6G9E7AIADoChBIHYeYD19g==",
            "redeemers": [],
            "inputs": []
        }|]
        rTx <- request @ApiSerialisedTransaction ctx
            (Link.balanceTransaction @'Shelley wa) Default pingPong1Base64
        verify rTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        let apiTx = getFromResponse #transaction rTx

        signedTx <- signTx ctx wa apiTx [ expectResponseCode HTTP.status202 ]

        submittedTx <- submitTxWithWid ctx wa signedTx
        verify submittedTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

    it "TRANS_NEW_BALANCE_04a - \
        \I get proper error message when payload is not hex or base64 encoded" $
        \ctx -> runResourceT $ do
        liftIO $ pendingWith
            "ADP-1225: revise error messages to reflect supported formats"
        wa <- fixtureWallet ctx
        -- transaction is invalid hex / base64
        let payload = Json [json|{
            "transaction": "!!!!#",
            "redeemers": [],
            "inputs": []
        }|]
        rTx <- request @ApiSerialisedTransaction ctx
            (Link.balanceTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectResponseCode HTTP.status400
            , expectErrorMessage "Parse error. Expecting CBOR-encoded transaction represented in either hex or base64 encoding."
            -- returns: Parse error. Expecting Base64-encoded format.
            ]

    it "TRANS_NEW_BALANCE_04b - \
        \I get proper error message when payload cannot be decoded" $
        \ctx -> runResourceT $ do
        liftIO $ pendingWith
            "ADP-1225: revise error messages to reflect supported formats"
        wa <- fixtureWallet ctx
        -- transaction is a VALID hex, but invalid transaction format
        let payload = Json [json|{
            "transaction": "11",
            "redeemers": [],
            "inputs": []
        }|]
        rTx <- request @ApiSerialisedTransaction ctx
            (Link.balanceTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectResponseCode HTTP.status400
            , expectErrorMessage "Parse error. Cannot deserialize transaction. Make sure it is valid CBOR-encoded transaction represented in either hex or base64 encoding."
            -- returns: Parse error. Expecting Base64-encoded format.
            ]

    it "TRANS_NEW_BALANCE_04c - \
        \I get proper error message when payload cannot be decoded" $
        \ctx -> runResourceT $ do
        liftIO $ pendingWith
            "ADP-1225: revise error messages to reflect supported formats"
        wa <- fixtureWallet ctx
        -- transaction is a VALID hex, but invalid transaction format
        let payload = Json [json|{
            "transaction": "11111",
            "redeemers": [],
            "inputs": []
        }|]
        rTx <- request @ApiSerialisedTransaction ctx
            (Link.balanceTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectResponseCode HTTP.status400
            , expectErrorMessage "Parse error. Cannot deserialize transaction. Make sure it is valid CBOR-encoded transaction represented in either hex or base64 encoding."
            -- returns: Parse error. Expecting Base64-encoded format.
            ]

    it "TRANS_NEW_BALANCE_04d - \
        \I get proper error message when payload cannot be decoded" $
        \ctx -> runResourceT $ do
        liftIO $ pendingWith
            "ADP-1225: revise error messages to reflect supported formats"
        wa <- fixtureWallet ctx
        -- transaction is a VALID base64, but invalid transaction format
        let payload = Json [json|{
            "transaction": "EQ==",
            "redeemers": [],
            "inputs": []
        }|]
        rTx <- request @ApiSerialisedTransaction ctx
            (Link.balanceTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectResponseCode HTTP.status400
            , expectErrorMessage "Parse error. Cannot deserialize transaction. Make sure it is valid CBOR-encoded transaction represented in either hex or base64 encoding."
            -- returns: Deserialisation failure while decoding Shelley Tx. CBOR failed with error: DeserialiseFailure 0 'expected list len or indef'
            ]

    it "TRANS_NEW_BALANCE_05/ADP-1286 - \
        \I can balance correctly in case I need to spend my remaining ADA for fee" $
        \ctx -> runResourceT $ do
        liftIO $ pendingWith
            "ADP-1286 - ValueNotConservedUTxO: Transaction seems balanced incorrectly \
            \in case when less than minUtxOValue is left on the wallet"
        wa <- fixtureWalletWith @n ctx [3_000_000]
        -- PlutusScenario.pingPong_1 is sending out 2₳ therefore tx fee
        -- needs to be 1₳ to comply with minUTxOValue constraint
        let expectedFee = 1_000_000
        let balancePayload = Json PlutusScenario.pingPong_1
        let hasExpectedFee = [expectField (#fee . #getQuantity) (`shouldBe` expectedFee)]

        rTx <- request @ApiSerialisedTransaction ctx
            (Link.balanceTransaction @'Shelley wa) Default balancePayload
        verify rTx [ expectResponseCode HTTP.status202 ]
        let apiTx = getFromResponse #transaction rTx
        let decodePayload = Json (toJSON $ ApiSerialisedTransaction apiTx)

        rDecodedTx <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley wa) Default decodePayload
        verify rDecodedTx hasExpectedFee

        signedTx <- signTx ctx wa apiTx [ expectResponseCode HTTP.status202 ]
        submittedTx <- submitTxWithWid ctx wa signedTx
        verify submittedTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        eventually "Wallet balance is as expected" $ do
            rWa <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wa) Default Empty
            verify rWa
                [ expectSuccess
                , expectField
                        (#balance . #available . #getQuantity)
                        (`shouldBe` 0)
                ]

    it "TRANS_NEW_SIGN_01 - Sign single-output transaction" $ \ctx -> runResourceT $ do
        w <- fixtureWallet ctx

        -- Construct tx
        payload <- mkTxPayload ctx w $ minUTxOValue (_mainEra ctx)
        let constructEndpoint = Link.createUnsignedTransaction @'Shelley w
        sealedTx <- getFromResponse #transaction <$>
            request @(ApiConstructTransaction n) ctx constructEndpoint Default payload

        -- Sign tx
        let toSign = Json [json|
                { "transaction": #{sealedTx}
                , "passphrase": #{fixturePassphrase}
                }|]
        let signEndpoint = Link.signTransaction @'Shelley w
        signedTx <- getFromResponse Prelude.id <$>
            request @ApiSerialisedTransaction ctx signEndpoint Default toSign

        -- Submit tx
        submittedTx <- submitTxWithWid ctx w signedTx
        verify submittedTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

    it "TRANS_NEW_SIGN_02 - Rejects unsigned transaction" $ \ctx -> runResourceT $ do
        w <- fixtureWallet ctx

        -- Construct tx
        payload <- mkTxPayload ctx w $ minUTxOValue (_mainEra ctx)
        let constructEndpoint = Link.createUnsignedTransaction @'Shelley w
        sealedTx <- getFromResponse #transaction <$>
            request @(ApiConstructTransaction n) ctx constructEndpoint Default payload

        -- Submit tx
        submittedTx <- submitTxWithWid ctx w (ApiSerialisedTransaction sealedTx)
        verify submittedTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage (errMsg403MissingWitsInTransaction 1 0)
            ]

    it "TRANS_NEW_SIGN_03 - Sign withdrawals" $ \ctx -> runResourceT $ do
        (w, _) <- rewardWallet ctx

        -- Construct tx
        let payload = Json [json|{"withdrawal": "self"}|]
        let constructEndpoint = Link.createUnsignedTransaction @'Shelley w
        (_, apiTx) <- unsafeRequest @(ApiConstructTransaction n) ctx constructEndpoint payload
        length (withdrawals $ coinSelection apiTx) `shouldBe` 1

        -- Sign tx
        let sealedTx = apiTx ^. #transaction
            toSign = Json [json|
                { "transaction": #{sealedTx}
                , "passphrase": #{fixturePassphrase}
                }|]
        let signEndpoint = Link.signTransaction @'Shelley w
        signedTx <- getFromResponse Prelude.id
            <$> request @ApiSerialisedTransaction ctx signEndpoint Default toSign

        -- Submit tx
        submittedTx <- submitTxWithWid ctx w signedTx
        verify submittedTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

    it "TRANS_NEW_SIGN_04 - Sign extra required signatures" $ \ctx -> runResourceT $ do
        (w, mw) <- second (unsafeMkMnemonic @15) <$> fixtureWalletWithMnemonics (Proxy @"shelley") ctx

        -- Construct tx
        payload <- mkTxPayload ctx w $ minUTxOValue (_mainEra ctx)
        let constructEndpoint = Link.createUnsignedTransaction @'Shelley w
        apiTx <- getFromResponse (#transaction . #getApiT) <$>
            request @(ApiConstructTransaction n) ctx constructEndpoint Default payload

        -- NOTE: Picking a key that is
        --
        -- (a) known of the wallet (so within the address pool gap)
        -- (b) not to small, so that it's not already present from the selected
        -- inputs witnesses!
        let rootSk = Shelley.generateKeyFromSeed (SomeMnemonic mw, Nothing) mempty
            acctSk = deriveAccountPrivateKey mempty rootSk minBound
            addrSk = deriveAddressPrivateKey mempty acctSk UtxoExternal (toEnum 14)
            addrVk = getRawKey $ publicKey addrSk
        let apiTx' = ApiT (apiTx `addRequiredSigners` [addrVk])

        -- Sign Tx
        let toSign = Json [json|
                { "transaction": #{apiTx'}
                , "passphrase": #{fixturePassphrase}
                }|]
        let signEndpoint = Link.signTransaction @'Shelley w
        signedTx <- getFromResponse Prelude.id <$>
            request @ApiSerialisedTransaction ctx signEndpoint Default toSign

        -- Submit Tx
        -- TODO:
        -- The submission currently fails because we manually add the
        -- 'requiredSigners' to the transaction and it becomes unbalanced. BUT,
        -- it fails not because of a missing signature, but because the fees are
        -- too small, which is good enough for this test... Yet really, we
        -- should be able to construct transactions with extra signers from the
        -- API!
        submittedTx <- submitTxWithWid ctx w signedTx
        verify submittedTx
            [ expectResponseCode HTTP.status500
            , expectErrorMessage "FeeTooSmallUTxO"
            ]

    describe "TRANS_NEW_SUBMIT_01 - Submitting on foreign wallet is forbidden" $ do
        let scenarios =
                  [ ( "empty", emptyWallet )
                  , ( "fixture", fixtureWallet )
                  ]
        forM_ scenarios $ \(title, foreignWallet) -> it title $ \ctx -> runResourceT $ do
            wa <- fixtureWallet ctx
            wb <- foreignWallet ctx

            -- Construct tx
            payload <- mkTxPayload ctx wb $ minUTxOValue (_mainEra ctx)
            let constructEndpoint = Link.createUnsignedTransaction @'Shelley wa
            sealedTx <- getFromResponse #transaction <$>
                request @(ApiConstructTransaction n) ctx constructEndpoint Default payload

            -- Sign tx
            let toSign = Json [json|
                    { "transaction": #{sealedTx}
                    , "passphrase": #{fixturePassphrase}
                    }|]
            let signEndpoint = Link.signTransaction @'Shelley wa
            signedTx <- getFromResponse Prelude.id <$>
                request @ApiSerialisedTransaction ctx signEndpoint Default toSign

            -- Submit tx (from wb)
            submittedTx <- submitTxWithWid ctx wb signedTx
            verify submittedTx
                [ expectResponseCode HTTP.status403
                , expectErrorMessage errMsg403ForeignTransaction
                ]

    describe "TRANS_NEW_SUBMIT_02 - Submitting on foreign Byron wallet is forbidden" $ do
        let scenarios =
                  [ ( "Byron random", emptyRandomWallet )
                  , ( "Byron icarus", emptyIcarusWallet )
                  ]
        forM_ scenarios $ \(title, foreignByronWallet) -> it title $ \ctx -> runResourceT $ do
            wa <- fixtureWallet ctx
            wb <- foreignByronWallet ctx
            let wid = wb ^. walletId

            -- Construct tx
            payload <- mkTxPayload ctx wa $ minUTxOValue (_mainEra ctx)
            let constructEndpoint = Link.createUnsignedTransaction @'Shelley wa
            sealedTx <- getFromResponse #transaction <$>
                request @(ApiConstructTransaction n) ctx constructEndpoint Default payload

            -- Sign tx
            let toSign = Json [json|
                    { "transaction": #{sealedTx}
                    , "passphrase": #{fixturePassphrase}
                    }|]
            let signEndpoint = Link.signTransaction @'Shelley wa
            signedTx <- getFromResponse Prelude.id <$>
                request @ApiSerialisedTransaction ctx signEndpoint Default toSign

            -- Submit tx (from wb)
            let submitEndpoint = ("POST", "v2/wallets/" <> wid <> "/transactions-submit")
            let submitPayload = Json $ Aeson.toJSON signedTx
            submittedTx <- request @ApiTxId ctx submitEndpoint Default submitPayload

            verify submittedTx
                [ expectResponseCode HTTP.status404
                , expectErrorMessage (errMsg404NoWallet wid)
                ]

    it "TRANS_NEW_SUBMIT_03 - Can submit transaction encoded in base16" $ \ctx -> runResourceT $ do
        wa <- fixtureWallet ctx

        -- Construct tx
        payload <- mkTxPayload ctx wa $ minUTxOValue (_mainEra ctx)
        let constructEndpoint = Link.createUnsignedTransaction @'Shelley wa
        sealedTx <- getFromResponse #transaction <$>
            request @(ApiConstructTransaction n) ctx constructEndpoint Default payload

        -- Sign tx
        let toSign = Json [json|
                { "transaction": #{sealedTx}
                , "passphrase": #{fixturePassphrase}
                }|]
        let signEndpoint = Link.signTransaction @'Shelley wa
        signedTx <- getFromResponse #transaction <$>
            request @ApiSerialisedTransaction ctx signEndpoint Default toSign
        let signedTxHex = PlutusScenario.toHex $ serialisedTx $ getApiT signedTx

        -- Submit tx
        let submitEndpoint = Link.submitTransaction @'Shelley wa
        let toSend = Json [json|
                { "transaction": #{signedTxHex}
                }|]
        submittedTx <- request @ApiTxId ctx submitEndpoint Default toSend
        verify submittedTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

    describe "Plutus scenarios" $ do
        let scenarios =
                [ ( "ping-pong"
                  , \_ _ -> pure
                      ( PlutusScenario.pingPong_1
                      , [ PlutusScenario.pingPong_2 ]
                      )
                  )
                , ( "game state-machine"
                  , \_ _ -> pure
                      ( PlutusScenario.game_1
                      , [ PlutusScenario.game_2
                        , PlutusScenario.game_3
                        ]
                      )
                  )
                , ( "mint-burn"
                  , \ctx w -> do
                        (_vk, vkHash) <- getSomeVerificationKey ctx w
                        let (policy, policyId) = PlutusScenario.mkSignerPolicy [json|{
                                "vkHash": #{vkHash} }
                            |]
                        mint <- PlutusScenario.mintBurn_1 [json|{
                            "policy": #{policy},
                            "policyId": #{policyId},
                            "vkHash": #{vkHash}
                        }|]
                        let burn = \_ -> PlutusScenario.mintBurn_2 [json|{
                                "policy": #{policy},
                                "policyId": #{policyId},
                                "vkHash": #{vkHash}
                            }|]
                        pure (mint, [burn])
                  )
                , ( "withdrawal"
                  , \ctx _w -> do
                        let (script, _scriptHash) = PlutusScenario.alwaysTrueValidator
                        liftIO $ _moveRewardsToScript ctx
                            ( unsafeFromHex $ T.encodeUtf8 script
                            , Coin 42000000
                            )
                        waitForNextEpoch ctx
                        withdrawal <- PlutusScenario.withdrawScript_1
                        pure (withdrawal, [])
                  )
                , ( "currency", \ctx w -> do
                    (addr,proxy) <- view #id . head <$> listAddresses @n ctx w
                    let getFreshUTxO = do
                            -- To obtain a fresh UTxO, we perform
                            -- coin selection and just pick the first input
                            -- that has been selected.
                            let singleton = pure -- specialized to NonEmpty
                            (_, result) <- selectCoins @_ @'Shelley ctx w $
                                singleton $ AddressAmount
                                    { address = (addr, proxy)
                                    , amount  = Quantity 10_000_000
                                    , assets  = ApiT TokenMap.empty
                                    }
                            pure $ head . view #inputs <$> result
                    txOutRef <- fromEither =<< getFreshUTxO
                    let mint = PlutusScenario.currencyTx txOutRef
                    pure (mint, [])
                  )
                ]

        forM_ scenarios $ \(title, setupContract) -> it title $ \ctx -> runResourceT $ do
            w <- fixtureWallet ctx
            let balanceEndpoint = Link.balanceTransaction @'Shelley w
            let signEndpoint = Link.signTransaction @'Shelley w

            (setup, steps) <- setupContract ctx w

            -- Balance
            let toBalance = Json setup
            (_, sealedTx) <- second (view #transaction) <$>
                unsafeRequest @ApiSerialisedTransaction ctx balanceEndpoint toBalance

            -- Sign
            let toSign = Json [json|
                    { "transaction": #{sealedTx}
                    , "passphrase": #{fixturePassphrase}
                    }|]
            (_, signedTx) <-
                unsafeRequest @ApiSerialisedTransaction ctx signEndpoint toSign

            -- Submit
            submittedTx <- submitTxWithWid ctx w signedTx
            verify submittedTx
                [ expectSuccess
                , expectResponseCode HTTP.status202
                ]
            let txid = getFromResponse Prelude.id submittedTx

            let runStep = \previous step -> do
                    waitForTxImmutability ctx

                    -- Balance
                    partialTx' <- step $ Aeson.object [ "transactionId" .= view #id previous ]
                    let toBalance' = Json (toJSON partialTx')

                    -- Sign
                    (_, sealedTx') <- second (view #transaction) <$>
                        unsafeRequest @ApiSerialisedTransaction ctx balanceEndpoint toBalance'
                    let toSign' = Json [json|
                            { "transaction": #{sealedTx'}
                            , "passphrase": #{fixturePassphrase}
                            }|]
                    (_, signedTx') <-
                        unsafeRequest @ApiSerialisedTransaction ctx signEndpoint toSign'

                    -- Submit
                    submittedTx' <- submitTxWithWid ctx w signedTx'
                    verify submittedTx'
                        [ expectSuccess
                        , expectResponseCode HTTP.status202
                        ]
                    pure $ getFromResponse Prelude.id submittedTx'

            foldM_ runStep txid steps

    it "TRANS_NEW_JOIN_01a - Can join stakepool, rejoin another and quit" $ \ctx -> runResourceT $ do

        let initialAmt = 10 * minUTxOValue (_mainEra ctx)
        src <- fixtureWalletWith @n ctx [initialAmt]
        dest <- emptyWallet ctx
        let depositAmt = Quantity 1000000

        pool1:pool2:_ <- map (view #id) . snd <$> unsafeRequest
            @[ApiStakePool]
            ctx (Link.listStakePools arbitraryStake) Empty

        let delegationJoin = Json [json|{
                "delegations": [{
                    "join": {
                        "pool": #{pool1},
                        "stake_key_index": "0H"
                    }
                }]
            }|]
        rTx1 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley src) Default delegationJoin
        verify rTx1
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #depositsTaken) (`shouldBe` [depositAmt])
            , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
            ]

        let apiTx1 = getFromResponse #transaction rTx1
        signedTx1 <- signTx ctx src apiTx1 [ expectResponseCode HTTP.status202 ]

        -- as we are joining for the first time we expect two certificates
        let stakeKeyDerPath = NE.fromList
                [ ApiT (DerivationIndex 2147485500)
                , ApiT (DerivationIndex 2147485463)
                , ApiT (DerivationIndex 2147483648)
                , ApiT (DerivationIndex 2)
                , ApiT (DerivationIndex 0)
                ]
        let registerStakeKeyCert =
                WalletDelegationCertificate $ RegisterRewardAccount stakeKeyDerPath
        let delegatingCert =
                WalletDelegationCertificate $ JoinPool stakeKeyDerPath pool1

        let txCbor1 = getFromResponse #transaction (HTTP.status202, Right signedTx1)
        let decodePayload1 = Json (toJSON $ ApiSerialisedTransaction txCbor1)
        rDecodedTx1 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley src) Default decodePayload1
        verify rDecodedTx1
            [ expectResponseCode HTTP.status202
            , expectField #certificates (`shouldBe` [registerStakeKeyCert, delegatingCert])
            , expectField #depositsTaken (`shouldBe` [depositAmt])
            , expectField #depositsReturned (`shouldBe` [])
            ]

        -- Submit tx
        submittedTx1 <- submitTxWithWid ctx src signedTx1
        verify submittedTx1
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        eventually "Wallet has joined pool and deposit info persists" $ do
            rJoin' <- request @(ApiTransaction n) ctx
                (Link.getTransaction @'Shelley src
                    (getFromResponse Prelude.id submittedTx1))
                Default Empty
            verify rJoin'
                [ expectResponseCode HTTP.status200
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField #depositTaken (`shouldBe` depositAmt)
                , expectField #depositReturned (`shouldBe` Quantity 0)
                ]

        let txId1 = getFromResponse #id submittedTx1
        let link = Link.getTransaction @'Shelley src (ApiTxId txId1)
        eventually "delegation transaction is in ledger" $ do
            rSrc <- request @(ApiTransaction n) ctx link Default Empty
            verify rSrc
                [ expectResponseCode HTTP.status200
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField (#metadata . #getApiTxMetadata) (`shouldBe` Nothing)
                , expectField #inputs $ \inputs' -> do
                    inputs' `shouldSatisfy` all (isJust . source)
                ]

        waitForNextEpoch ctx
        waitForNextEpoch ctx
        eventually "Wallet gets rewards from pool1" $ do
            r <- request @ApiWallet ctx (Link.getWallet @'Shelley src) Default Empty
            verify r
                [ expectField
                      (#balance . #reward)
                      (.> (Quantity 0))
                ]

        eventually "Wallet is delegating to pool1" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley src) Default Empty
                >>= flip verify
                    [ expectField #delegation (`shouldBe` delegating pool1 [])
                    ]

        -- join another stake pool
        let delegationRejoin = Json [json|{
                "delegations": [{
                    "join": {
                        "pool": #{pool2},
                        "stake_key_index": "0H"
                    }
                }]
            }|]
        rTx2 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley src) Default delegationRejoin
        verify rTx2
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #depositsTaken) (`shouldBe` [])
            , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
            ]
        let apiTx2 = getFromResponse #transaction rTx2
        signedTx2 <- signTx ctx src apiTx2 [ expectResponseCode HTTP.status202 ]
        let delegatingCert2 =
                WalletDelegationCertificate $ JoinPool stakeKeyDerPath pool2

        let txCbor2 = getFromResponse #transaction (HTTP.status202, Right signedTx2)
        let decodePayload2 = Json (toJSON $ ApiSerialisedTransaction txCbor2)
        rDecodedTx2 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley src) Default decodePayload2
        verify rDecodedTx2
            [ expectResponseCode HTTP.status202
            , expectField #certificates (`shouldBe` [delegatingCert2])
            , expectField #depositsTaken (`shouldBe` [])
            , expectField #depositsReturned (`shouldBe` [])
            ]
        submittedTx2 <- submitTxWithWid ctx src signedTx2
        verify submittedTx2
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        let txid2 = getFromResponse (#id) submittedTx2
        let queryTx2 = Link.getTransaction @'Shelley src (ApiTxId txid2)
        rGetTx2 <- request @(ApiTransaction n) ctx queryTx2 Default Empty
        verify rGetTx2
            [ expectResponseCode HTTP.status200
            , expectField #depositTaken (`shouldBe` Quantity 0)
            , expectField #depositReturned (`shouldBe` Quantity 0)
            ]

        -- Wait for the certificate to be inserted
        eventually "Certificates are inserted" $ do
            let ep = Link.listTransactions @'Shelley src
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListField 1
                    (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 1
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]

        eventually "Wallet is delegating to pool2" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley src) Default Empty
                >>= flip verify
                    [ expectField #delegation (`shouldBe` delegating pool2 [])
                    ]

        -- there's currently no withdrawals in the wallet
        rw1 <- request @[ApiTransaction n] ctx
            (Link.listTransactions' @'Shelley src (Just 1)
                Nothing Nothing Nothing)
            Default Empty
        verify rw1 [ expectListSize 0 ]

        -- can use rewards with an explicit withdrawal request to self.
        addrs <- listAddresses @n ctx dest
        let coin = minUTxOValue (_mainEra ctx) :: Natural
        let addr = (addrs !! 1) ^. #id
        let payloadWithdrawal = [json|
                { "payments":
                    [ { "address": #{addr}
                      , "amount":
                        { "quantity": #{coin}
                        , "unit": "lovelace"
                        }
                      }
                    ]
                , "passphrase": #{fixturePassphrase},
                  "withdrawal": "self"
                }|]

        --TODO: ADP-1192 (take care of withdrawals in new tx workflow)
        rGet <- request @ApiWallet ctx (Link.getWallet @'Shelley src)
                    Default Empty
        let previousBalance = getFromResponse (#balance . #available) rGet
        rTx3 <- request @(ApiTransaction n) ctx
            (Link.createTransactionOld @'Shelley src)
            Default (Json payloadWithdrawal)
        verify rTx3
            [ expectField #amount (.> (Quantity coin))
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]

        -- Rewards are have been consumed.
        eventually "Wallet has consumed rewards" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley src) Default Empty
                >>= flip verify
                    [ expectField
                        (#balance . #reward)
                        (`shouldBe` (Quantity 0))
                    , expectField
                        (#balance . #available)
                        (.> previousBalance)
                    ]

        -- now we can quit
        let delegationQuit = Json [json|{
                "delegations": [{
                    "quit": {
                        "stake_key_index": "0H"
                    }
                }]
            }|]
        rTx4 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley src) Default delegationQuit
        verify rTx4
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #depositsTaken) (`shouldBe` [])
            , expectField (#coinSelection . #depositsReturned) (`shouldBe` [depositAmt])
            ]
        let apiTx4 = getFromResponse #transaction rTx4
        signedTx4 <- signTx ctx src apiTx4 [ expectResponseCode HTTP.status202 ]
        let quittingCert =
                WalletDelegationCertificate $ QuitPool stakeKeyDerPath

        let txCbor4 = getFromResponse #transaction (HTTP.status202, Right signedTx4)
        let decodePayload4 = Json (toJSON $ ApiSerialisedTransaction txCbor4)
        rDecodedTx4 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley src) Default decodePayload4
        verify rDecodedTx4
            [ expectResponseCode HTTP.status202
            , expectField #certificates (`shouldBe` [quittingCert])
            , expectField #depositsReturned (`shouldBe` [depositAmt])
            , expectField #depositsTaken (`shouldBe` [])
            ]
        submittedTx4 <- submitTxWithWid ctx src signedTx4
        verify submittedTx4
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        let txid3 = getFromResponse (#id) submittedTx4
        let queryTx3 = Link.getTransaction @'Shelley src (ApiTxId txid3)
        rGetTx3 <- request @(ApiTransaction n) ctx queryTx3 Default Empty
        verify rGetTx3
            [ expectResponseCode HTTP.status200
            , expectField #depositTaken (`shouldBe` Quantity 0)
            , expectField #depositReturned (`shouldBe` depositAmt)
            ]

        eventually "Wallet is not delegating" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley src) Default Empty
                >>= flip verify
                    [ expectField #delegation (`shouldBe` notDelegating [])
                    ]

        -- transaction history shows deposit returned
        rGetTx4 <- request @(ApiTransaction n) ctx queryTx3 Default Empty
        verify rGetTx4
            [ expectResponseCode HTTP.status200
            , expectField #depositTaken (`shouldBe` Quantity 0)
            , expectField #depositReturned (`shouldBe` depositAmt)
            ]

    it "TRANS_NEW_JOIN_01b - Invalid pool id" $ \ctx -> runResourceT $ do

        wa <- fixtureWallet ctx

        let invalidPoolId = T.replicate 32 "1"
        let delegation = Json [json|{
                "delegations": [{
                    "join": {
                        "pool": #{invalidPoolId},
                        "stake_key_index": "0H"
                    }
                }]
            }|]
        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default delegation
        verify rTx
            [ expectResponseCode HTTP.status400
            , expectErrorMessage "Invalid stake pool id"
            ]

    it "TRANS_NEW_JOIN_01b - Absent pool id" $ \ctx -> runResourceT $ do

        wa <- fixtureWallet ctx
        let absentPoolIdBech32 = "pool1mgjlw24rg8sp4vrzctqxtf2nn29rjhtkq2kdzvf4tcjd5pl547k"
        (Right absentPoolId) <- pure $ decodePoolIdBech32 absentPoolIdBech32
        let delegation = Json [json|{
                "delegations": [{
                    "join": {
                        "pool": #{absentPoolIdBech32},
                        "stake_key_index": "0H"
                    }
                }]
            }|]
        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default delegation
        verify rTx
            [ expectResponseCode HTTP.status404
            , expectErrorMessage (errMsg404NoSuchPool (toText absentPoolId))
            ]

    it "TRANS_NEW_JOIN_01c - Multidelegation not supported" $ \ctx -> runResourceT $ do

        wa <- fixtureWallet ctx
        let pool' = "pool1mgjlw24rg8sp4vrzctqxtf2nn29rjhtkq2kdzvf4tcjd5pl547k" :: Text
        let delegations = Json [json|{
                "delegations": [{
                    "join": {
                        "pool": #{pool'},
                        "stake_key_index": "0H"
                    }
                },{
                    "quit": {
                        "stake_key_index": "0H"
                    }
                }]
            }|]
        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default delegations
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403MultidelegationTransaction
            ]

    it "TRANS_NEW_JOIN_01d - Multiaccount not supported" $ \ctx -> runResourceT $ do

        wa <- fixtureWallet ctx
        let pool' = "pool1mgjlw24rg8sp4vrzctqxtf2nn29rjhtkq2kdzvf4tcjd5pl547k" :: Text
        let delegations = Json [json|{
                "delegations": [{
                    "join": {
                        "pool": #{pool'},
                        "stake_key_index": "0H"
                    }
                },{
                    "join": {
                        "pool": #{pool'},
                        "stake_key_index": "1H"
                    }
                }]
            }|]
        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default delegations
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403MultiaccountTransaction
            ]

    it "TRANS_NEW_JOIN_01e - Can re-join and withdraw at once"  $ \ctx -> runResourceT $ do
        (src, _) <- rewardWallet ctx
        pool1:_ <- map (view #id) . snd <$> unsafeRequest
            @[ApiStakePool]
            ctx (Link.listStakePools arbitraryStake) Empty

        let delegationJoin = Json [json|{
                "delegations": [{
                    "join": {
                        "pool": #{pool1},
                        "stake_key_index": "0H"
                    }
                }]
                , "withdrawal": "self"
            }|]
        rTx1 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley src) Default delegationJoin
        let apiTx1 = getFromResponse #transaction rTx1
        signedTx1 <- signTx ctx src apiTx1 [ expectResponseCode HTTP.status202 ]
        submittedTx1 <- submitTxWithWid ctx src signedTx1
        verify submittedTx1
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]
        eventually "Wallet has joined pool and deposit info persists" $ do
            rJoin' <- request @(ApiTransaction n) ctx
                (Link.getTransaction @'Shelley src
                    (getFromResponse Prelude.id submittedTx1))
                Default Empty
            verify rJoin'
                [ expectResponseCode HTTP.status200
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField #withdrawals (`shouldNotBe` [])
                ]

        let txId1 = getFromResponse #id submittedTx1
        let link = Link.getTransaction @'Shelley src (ApiTxId txId1)
        eventually "delegation transaction is in ledger" $ do
            rSrc <- request @(ApiTransaction n) ctx link Default Empty
            verify rSrc
                [ expectResponseCode HTTP.status200
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField (#metadata . #getApiTxMetadata) (`shouldBe` Nothing)
                , expectField #inputs $ \inputs' -> do
                    inputs' `shouldSatisfy` all (isJust . source)
                ]

        waitForNextEpoch ctx
        waitForNextEpoch ctx
        eventually "Wallet gets rewards from pool1" $ do
            r <- request @ApiWallet ctx (Link.getWallet @'Shelley src) Default Empty
            verify r
                [ expectField
                      (#balance . #reward)
                      (.> (Quantity 0))
                ]

        eventually "Wallet is delegating to pool1" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley src) Default Empty
                >>= flip verify
                    [ expectField #delegation (`shouldBe` delegating pool1 [])
                    ]

    it "TRANS_NEW_JOIN_02 - Can join stakepool in case I have many UTxOs on 1 address"
        $ \ctx -> runResourceT $ do
        let amt = 1_000_000
        src <- emptyWallet ctx
        wa <- fixtureWallet ctx

        -- Send ada to single wallet address to have [1A, 1A, 1A]
        addrs <- listAddresses @n ctx src
        let destination = (addrs !! 1) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                },
                {
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                },
                {
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }]
            }|]


        -- send transaction
        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx [ expectSuccess ]

        let apiTx = getFromResponse #transaction rTx
        signedTx <- signTx ctx wa apiTx [ expectResponseCode HTTP.status202 ]

        submittedTx <- submitTxWithWid ctx wa signedTx
        verify submittedTx [ expectSuccess ]

        eventually "Target wallet balance is increased by amt and assets" $ do
            rWb <- request @ApiWallet ctx
                (Link.getWallet @'Shelley src) Default Empty
            verify rWb
                [ expectSuccess
                , expectField
                        (#balance . #available . #getQuantity)
                        (`shouldBe` (3 * amt))
                ]

        -- Delegate from src wallet
        pool1:_ <- map (view #id) . snd <$> unsafeRequest
            @[ApiStakePool]
            ctx (Link.listStakePools arbitraryStake) Empty

        let delegationJoin = Json [json|{
                "delegations": [{
                    "join": {
                        "pool": #{pool1},
                        "stake_key_index": "0H"
                    }
                }]
            }|]
        rTx1 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley src) Default delegationJoin
        verify rTx1
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #depositsTaken) (`shouldBe` [Quantity 1000000])
            , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
            ]

        let apiTx1 = getFromResponse #transaction rTx1
        signedTx1 <- signTx ctx src apiTx1 [ expectResponseCode HTTP.status202 ]

        let txCbor1 = getFromResponse #transaction (HTTP.status202, Right signedTx1)
        let decodePayload1 = Json (toJSON $ ApiSerialisedTransaction txCbor1)
        rDecodedTx1 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley src) Default decodePayload1
        verify rDecodedTx1
            [ expectResponseCode HTTP.status202
            , expectField #depositsTaken (`shouldBe` [Quantity 1000000])
            , expectField #depositsReturned (`shouldBe` [])
            ]

        -- Submit tx
        submittedTx1 <- submitTxWithWid ctx src signedTx1
        verify submittedTx1
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        eventually "Wallet has joined pool and deposit info persists" $ do
            rJoin' <- request @(ApiTransaction n) ctx
                (Link.getTransaction @'Shelley src
                    (getFromResponse Prelude.id submittedTx1))
                Default Empty
            verify rJoin'
                [ expectResponseCode HTTP.status200
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                -- , expectField #depositTaken (`shouldBe` Quantity 1000000)
                -- , expectField #depositReturned (`shouldBe` Quantity 0)
                ]

    it "TRANS_NEW_QUIT_01 - Cannot quit if not joined" $ \ctx -> runResourceT $ do

        wa <- fixtureWallet ctx
        let delegation = Json [json|{
                "delegations": [{
                    "quit": {
                        "stake_key_index": "0H"
                    }
                }]
            }|]
        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default delegation
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403NotDelegating
            ]

    it "TRANS_NEW_QUIT_02a - Cannot quit with rewards without explicit withdrawal"
        $ \ctx -> runResourceT $ do
        (w, _) <- rewardWallet ctx

        pool1:_:_ <- map (view #id) . snd
            <$> unsafeRequest @[ApiStakePool]
                ctx (Link.listStakePools arbitraryStake) Empty

        let payload = Json [json|{
                "delegations": [{
                    "join": {
                        "pool": #{pool1},
                        "stake_key_index": "0H"
                    }
                }]
            }|]
        unsignedTx1 <- view #transaction . snd
            <$> unsafeRequest @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley w) payload
        signedTx1 <- signTx ctx w unsignedTx1 [ expectResponseCode HTTP.status202 ]
        submitTxWithWid ctx w signedTx1 >>= flip verify
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        waitForTxImmutability ctx

        let payload2 = Json [json|{
                "delegations": [{
                    "quit": {
                        "stake_key_index": "0H"
                    }
                }]
            }|]
        request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley w) Default payload2
            >>= flip verify
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403NonNullReward
            ]

    it "TRANS_NEW_QUIT_02b - Can quit with rewards with explicit withdrawal"
        $ \ctx -> runResourceT $ do
        (w, _) <- rewardWallet ctx

        pool1:_:_ <- map (view #id) . snd
            <$> unsafeRequest @[ApiStakePool]
                ctx (Link.listStakePools arbitraryStake) Empty

        let payload = Json [json|{
                "delegations": [{
                    "join": {
                        "pool": #{pool1},
                        "stake_key_index": "0H"
                    }
                }]
            }|]
        rUnsignedTx1 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley w) Default payload
        let unsignedTx1 = getFromResponse #transaction rUnsignedTx1
        verify rUnsignedTx1
            [ expectField (#coinSelection . #depositsReturned)
                (`shouldBe` [])
            , expectField (#coinSelection . #depositsTaken)
                (`shouldBe` []) -- key already registered
            ]
        signedTx1 <- signTx ctx w unsignedTx1 [ expectResponseCode HTTP.status202 ]
        submitTxWithWid ctx w signedTx1 >>= flip verify
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        waitForTxImmutability ctx

        let payload2 = Json [json|{
                "delegations": [{
                    "quit": {
                        "stake_key_index": "0H"
                    }
                }],
                "withdrawal": "self"
            }|]
        rUnsignedTx2 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley w) Default payload2
        let unsignedTx2 = getFromResponse #transaction rUnsignedTx2
        verify rUnsignedTx2
            [ expectField (#coinSelection . #depositsReturned)
                (`shouldBe` [Quantity 1000000])
            , expectField (#coinSelection . #depositsTaken)
                (`shouldBe` [])
            ]
        signedTx2 <- signTx ctx w unsignedTx2 [ expectResponseCode HTTP.status202 ]
        submitTxWithWid ctx w signedTx2 >>= flip verify
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

    it "TRANS_NEW_CREATE_MULTI_TX - Tx including payments, delegation, metadata, withdrawals, validity_interval" $ \ctx -> runResourceT $ do
        wa <- fixtureWallet ctx
        wb <- emptyWallet ctx
        addrs <- listAddresses @n ctx wb

        let amt = minUTxOValue (_mainEra ctx) :: Natural
        let destination1 = (addrs !! 1) ^. #id
        let destination2 = (addrs !! 2) ^. #id
        let deposit = fromIntegral oneAda
        pool':_ <- map (view #id) . snd <$> unsafeRequest
            @[ApiStakePool]
            ctx (Link.listStakePools arbitraryStake) Empty

        let payload = Json [json|{
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
                "delegations": [{
                    "join": {
                        "pool": #{pool'},
                        "stake_key_index": "0H"
                    }
                }],
                "withdrawal": "self",
                "metadata": { "1": { "string": "hello" } },
                "validity_interval": {
                    "invalid_before": {
                      "quantity": 0,
                      "unit": "second"
                    },
                    "invalid_hereafter": {
                      "quantity": 1000,
                      "unit": "second"
                    }
                  }
            }|]

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley wa) Default payload
        verify rTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#coinSelection . #inputs) (`shouldSatisfy` (not . null))
            , expectField (#coinSelection . #outputs) (`shouldSatisfy` (not . null))
            , expectField (#coinSelection . #depositsTaken) (`shouldBe` [Quantity deposit])
            , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
            , expectField (#coinSelection . #change) (`shouldSatisfy` (not . null))
            , expectField (#fee . #getQuantity) (`shouldSatisfy` (>0))
            ]

        let expectedFee = getFromResponse (#fee . #getQuantity) rTx

        -- Sign tx
        let apiTx = getFromResponse #transaction rTx
        signedTx <- signTx ctx wa apiTx [ expectResponseCode HTTP.status202 ]
        -- Submit tx
        submittedTx <- submitTxWithWid ctx wa signedTx
        verify submittedTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]
        let txId = getFromResponse (#id) submittedTx

        eventually "Metadata is on-chain" $ do
            rWa <- request @(ApiTransaction n) ctx
                (Link.getTransaction @'Shelley wa txId) Default Empty
            verify rWa
                [ expectSuccess
                , expectField
                        (#status . #getApiT)
                        (`shouldBe` InLedger)
                , expectField
                        (#metadata . #getApiTxMetadata . _Ctor @"Just" . #getApiT)
                        (`shouldBe` Cardano.TxMetadata (Map.fromList [(1, Cardano.TxMetaText "hello")]))
                ]

        eventually "Delegation certificates are inserted" $ do
            rWa <- request @(ApiWallet) ctx
                (Link.getWallet @'Shelley wa) Default Empty
            verify rWa
                [ expectSuccess
                , expectField
                        #delegation
                        (`shouldBe` delegating pool' [])
                ]

        eventually "Destination wallet balance is as expected" $ do
            rWb <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wb) Default Empty
            verify rWb
                [ expectSuccess
                , expectField
                        (#balance . #available . #getQuantity)
                        (`shouldBe` amt * 2)
                ]

        eventually "Source wallet balance is as expected" $ do
            let balance = fromIntegral oneMillionAda - amt * 2 - expectedFee - deposit
            rWa <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wa) Default Empty
            verify rWa
                [ expectSuccess
                , expectField
                        (#balance . #available . #getQuantity)
                        (`shouldBe` balance)
                ]
  where

    -- | Just one million Ada, in Lovelace.
    oneMillionAda :: Integer
    oneMillionAda = 1_000_000 * oneAda

    oneAda :: Integer
    oneAda = 1_000_000

    -- Construct a JSON payment request for the given quantity of lovelace.
    mkTxPayload
        :: MonadUnliftIO m
        => Context
        -> ApiWallet
        -> Natural
        -> m Payload
    mkTxPayload ctx wDest amt = do
        addrs <- listAddresses @n ctx wDest
        let destination = (addrs !! 1) ^. #id
        return $ Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }]
            }|]

    -- Like mkTxPayload, except that assets are included in the payment.
    -- Asset amounts are specified by ((PolicyId Hex, AssetName Hex), amount).
    mkTxPayloadMA
        :: forall l m.
            ( DecodeAddress l
            , DecodeStakeAddress l
            , EncodeAddress l
            , MonadUnliftIO m
            )
        => (ApiT Address, Proxy l)
        -> Natural
        -> [((Text, Text), Natural)]
        -> m Payload
    mkTxPayloadMA destination coin val = do
        let assetJson ((pid, name), q) = [json|{
                    "policy_id": #{pid},
                    "asset_name": #{name},
                    "quantity": #{q}
                }|]
        return $ Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{coin},
                        "unit": "lovelace"
                    },
                    "assets": #{map assetJson val}
                }]
            }|]

    serializedPlutusTx :: Text
    serializedPlutusTx =
        "84a60081825820888963613d2bb4c5c55cee335f724624cbc54b185eca\
        \a2fb1eb07545ed5db421010d80018002000e800b58201dd28c3485f707\
        \dd5fb5db3ef96c5ed723c05ce5ef5439b0cc291f8a8ac6531ca3038159\
        \1b6f591b6c010000332332233322232323232332232333222333222333\
        \3333322222222332233333222223332223333222233223322332233223\
        \3223332223322332233223322323232323232323232323232323232323\
        \2323232323232323232323232323232323232323232323232323232323\
        \2323232323232323232323232323232323232323232323232300112001\
        \23355002307812001307812001112222223335306433300433503a0060\
        \0333503a00500230070012088012350810149821c048c8c8c8c8c8c8cc\
        \cd5ca99b8935573e600a24002900011801090009180389000843809198\
        \26180109000980189000918039aba230031200123043357446ae8cc008\
        \480048d55d018010900091ba900323507b4988c8c8c8c8c8c8c8c8c8c8\
        \c8c8c8c8c8c8c8c8c8c8c8c8cccd5ca99b8935573e602a240029000118\
        \01090009180b8900084b009199999999982e9801090009801890009802\
        \0900098028900098030900098038900098040900098048900098050900\
        \0980589000919a822980b890009aba23013120012335044301e1200135\
        \744602224002466a086604e240026ae88c03c480048cd4108c09848004\
        \d5d1180689000919a8209817890009aba2300b12001233504033550413\
        \0321200130311200135744601224002466a072606c240026ae88c01c48\
        \0048cd40f8c0f448004d5d1180289000919a81e99aa81f182289000982\
        \4090009aba23003120012304f357446ae8cc008480048d5d1980109000\
        \91aba330021200123574660042400246ae8cc008480048d5d198010900\
        \091aba330021200123574660042400246ae8cc008480048d55d0180109\
        \00091ba900323507a4988ccd41d800c0180088c8c8c8c8c8c8cccd5ca9\
        \9b8935573e600a24002900011801090009180389000842009198269801\
        \09000980189000918229aba23003120012300d357446ae8cc008480048\
        \d55d018010900091ba90032350784988d4c11400488cdd22400066aed0\
        \c010008cd5da18068009bb24988d4c18400488cdd22400066aed0c0100\
        \08cd5da1ba7001376493119ba448000cd5da1ba800137649311999999a\
        \baf2323232323232323333572a66e24d55cf980389000a400046004240\
        \024a100021040246666aaed494200048c00c48004c01848004208048cc\
        \cd55da9283f91801890009802090008408091999aabb52300312001250\
        \7e35746600624002100024666a6a0f6600e6ae88c00c4800488ccd4d41\
        \f4c048d5d1180209000911a84080998260020011283f840809283e83f9\
        \1aba3300212001235574060042400246ea400c941e0941e0941e0941e0\
        \0041e88ccccccd5d79191919191919191999ab95337126aae7cc01c480\
        \05200023002120012507f08101233335576a4a0fe4600624002600c240\
        \021020246666aaed4941f88c00c48004c01048004200048cccd55da918\
        \01890009283e9aba330031200107f233353507a3007357446006240024\
        \4666a6a0f860526ae88c0104800488d420004cc1a0010008941f820004\
        \941f01f88d5d198010900091aaba0300212001237520064a0ee4a0ee4a\
        \0ee4a0ee0020f246666666aebc8c8c8c8c8c8cccd5ca99b8935573e600\
        \a24002900011801090009283e03f11999aabb52507c230031200130041\
        \200107e233335576a46006240024a0f66ae8cc00c480041f48ccd4d41e\
        \0c0d8d5d1180109000911a83e0011283d03e11aaba0300212001237520\
        \064a0ec4a0ec4a0ec4a0ec0020f04666a0dc00a0100044646464646464\
        \64646666ae54cdc49aab9f300712001480008c008480048c024480041f\
        \88ccc124c00848004c00c48004c010480048c024d5d1180289000919a8\
        \161808090009aba23003120012335008303312001357446ae8cc008480\
        \048d5d198010900091aaba03002120012375200646a0e0931191919191\
        \9191999ab95337126aae7cc0144800520002300212001230071200107b\
        \23304d3002120013003120012303b35744600624002466a00c60362400\
        \26ae88d5d198010900091aaba03002120012375200646a0de930911919\
        \191919191999ab953371260082400290001180109000918020900083d9\
        \1a83c980109000919a8168039aba235574060082400246666ae54cdc49\
        \80109000a40044a0ee4600a240020f246aae7cc008480048dd480191a8\
        \37a4c46a607a00244466e912000335768600a00666aed0cd403cc02848\
        \004008cd5da19a8031817090008009bb24988d4c10c00488cdd2240006\
        \6aed0c060008cd5da19a802180b090008009bb2498488ccd4d41b80048\
        \8cdd22400066aed0cd40a4010008dd924c466e91200237649303911999\
        \999abaf23232323232323232323333572a66e24d55cf980489000a4000\
        \46004240024a0f00f446666aaed4941e08c00c48004c020480041e88cc\
        \cd55da9283b918018900098030900083c91999aabb5250762300312001\
        \300412001078233335576a46006240024a0ea6ae8cc00c480041dc8ccd\
        \4d41c8c020d5d118020900091199a9a83a19a8099806090009aba23005\
        \120012233353507633500d30321200135744600c24002446a0f466608e\
        \00c0080044a0f00f44a0ec0f04a0e80ec46ae8cc008480048d5d198010\
        \900091aaba0300212001237520064a0dc4a0dc4a0dc4a0dc0020e04666\
        \6666aebc8c8c8c8c8c8c8c8cccd5ca99b8935573e600e2400290001180\
        \1090009283a83b91999aabb52507523003120013006120010772333355\
        \76a4a0e846006240026008240020ec46666aaed48c00c48004941ccd5d\
        \198018900083a9199a9a838180d1aba230031200122333535072335009\
        \30181200135744600824002446a0ec660940080044a0e80ec4a0e40e84\
        \6ae8cc008480048d55d018010900091ba90032506d2506d2506d2506d0\
        \0106f1223333333575e46464646464646464646666ae54cdc498028900\
        \0a400046004240024600a240020f246666aaed4941dc8c00c48004c01c\
        \480041e48cccd55da91801890009283b1aba3300612001078233353507\
        \333502e00a35744600a24002446a0ee6a0ee0044a0ea0ee46666ae54cd\
        \c4980109000a400446008240024a0e80ec46aae7cc010480048cccd55d\
        \a91802890009283918010900083a11aaba03002120012375200846a0de\
        \a0dc4a0da4a0da4a0da4a0da0020de466aa03c60142400260042400246\
        \6aa006600424002601a240024666a0c404804aeb44488ccd4188cd5400\
        \c008004cd54014008004cd5401c0080044488d400ccd5406c008004488\
        \c8c8dd318008019a80090008918009aa83591199a9a81c00091bb24988\
        \88cd5da19a812004001980280103608911a80199aa80c8010008911919\
        \1999999abaf25067250672300237560084a0ce4a0ce0060d26a0024002\
        \2460026aa0d244646666aaed48c008480048ccd4d41a0cd408c01cd5d1\
        \00191199a9a83518031aba3005223506e33503d0040022506c06e2506a\
        \06c00206b235069503911223501633550170020012333505b01d01e75a\
        \4666a0b4004006044466666666a60920024466e912002335768600e004\
        \6ec92622233748900219abb430080033357686ea0008dd924c4466e912\
        \000335768600e0046ec92623374890051bb24988cdd2240186ec926222\
        \33748900319abb4375000666aed0dd40011bb2498888cdd22401066aed\
        \0dd400199abb4374e0046ec92606223333333575e46464646464646464\
        \64646464646464646464646464646464646464646464646464646666ae\
        \54cdc4980e89000a4018460042400246008240021080246666aaed48c0\
        \0c480049420804c08448004210048d4204041b48cccd5ca99b89301b12\
        \001480288c00c480048c01448004208048cccd55da91801890009283f9\
        \80f090008408091a83f03591999ab95337126030240029004118018900\
        \0918040900083f91999aabb52507c2300312001301b1200107e2333355\
        \76a4a0f646006240026008240020fa46666aaed48c00c48004941e8d5d\
        \198018900083e1199a9a83b981a9aba230181200122333535079302635\
        \744600824002446a0fa660ce0080044a0f60fa4a0f20f646ae8cc05c48\
        \0048cccd5ca99b89301212001480188c00c480048c020480041e48cccd\
        \55da9283b1180189000980a8900083c11999aabb525075230031200130\
        \0412001077233335576a46006240024a0e86ae8cc00c480041d88ccd4d\
        \41c4c0bcd5d118090900091199a9a83998189aba230041200122350773\
        \3062004002250750772507307523574660222400246666ae54cdc49806\
        \09000a4008460062400246010240020e646666aaed4941c08c00c48004\
        \c03c480041c88cccd55da92837918018900098020900083891999aabb5\
        \23003120012506e357466006240020e04666a6a0d660286ae88c030480\
        \0488ccd4d41b4c0acd5d1180209000911a838998300020011283783892\
        \83683791aba3300b1200123333572a66e24c0184800520022300312001\
        \230061200106d233335576a4a0d446006240026012240020d846666aae\
        \d48c00c48004941a4d5d19804090008359199a9a83318079aba2300712\
        \001223506a305a0022506806a23333572a66e24c008480052000230041\
        \200125067069235573e600c2400246666aaed4941948c00c48004c0104\
        \800419c8cccd55da9180189000928321aba33003120010662333535061\
        \300a35744600424002446a0ca60a60044a0c60ca46aae80c008480048d\
        \d48019282f9282f9282f9282f8008309199a82b804004bac2333505600\
        \200401f2335304d001233748900019abb4300300137649311119ba4480\
        \08cd5da1ba70033357686e9c008cd5da1ba7001376493119a982780091\
        \9ba448000cd5da1ba8001376493119ba448008cd5da1ba800137649311\
        \999999abaf232323232323232323232323232323333572a66e24c02448\
        \00520022300212001230091200106b233335576a4a0d24600624002601\
        \a240020d646666aaed4941a08c00c48004c018480041a88cccd55da928\
        \33918018900098020900083491999aabb5230031200125066357466006\
        \240020d04666a6a0c660206ae88c0244800488ccd4d4194c048d5d1180\
        \28900091199a9a833980a1aba2300612001223506b33305d0060040022\
        \506906b250670692506506723574660042400246ae8cc01c480048cccd\
        \5ca99b89300212001480008c01048004941881908d55cf980309000919\
        \99aabb5250602300312001300412001062233335576a46006240024a0b\
        \e6ae8cc00c480041848ccd4d4170c018d5d1180109000911a830182980\
        \11282f03011aaba0300212001237520064a0b44a0b44a0b44a0b40020b\
        \846666666aebc8c8c8c8c8c8c8c8c8c8c8cccd5ca99b89300512001480\
        \088c008480048c014480041988cccd55da928321180189000980489000\
        \83311999aabb5230031200125063357466010240020ca4666a6a0c0603\
        \c6ae88c01c4800488d4190c164008941881908cccd5ca99b8930021200\
        \1480008c010480049418418c8d55cf98030900091999aabb52505f2300\
        \312001300412001061233335576a46006240024a0bc6ae8cc00c480041\
        \808ccd4d416cc064d5d1180109000911a82f982a8011282e82f91aaba0\
        \300212001237520064a0b24a0b24a0b24a0b20020b64666a0a2004006e\
        \b08dd380091999999abaf25056250562505623505737580044a0ac0020\
        \b02446464646464646666ae54cdc49aab9f300512001480008c0084800\
        \48c01c480041788cd40c8c00848004c00c480048cd402001cd5d118018\
        \9000919a8040031aba23574660042400246aae80c008480048dd480191\
        \a82924c2446464646464646666ae54cdc49aab9f300512001480008c00\
        \8480048c01c480041748cd40d4c00848004c00c480048cd402401cd5d1\
        \180189000918049aba23574660042400246aae80c008480048dd480191\
        \a828a4c2446464646464646666ae54cdc49aab9f300512001480008c00\
        \8480048c01c480041708cd40c8c00848004c00c480048cd402001cd5d1\
        \180189000918041aba23574660042400246aae80c008480048dd480191\
        \a82824c244646464646464646666ae54cdc4980289000a40084a06e460\
        \04240020b846666ae54cdc4980289000a400446006240024600a240020\
        \b846a06e600424002466a01a00e6ae88d55d018020900091999ab95337\
        \1260042400290001281a918028900082c91aab9f300212001237520064\
        \6a09e9311919191999ab95337126004240029001101b118010900082b1\
        \1999ab95337126004240029000101a918020900082b11aab9f37520064\
        \6a09a931199a8248058063ad1223232300137560066a00240022460026\
        \aa0a8446666aaed4940908cd408ccd4024018d5d100118019aba300200\
        \1055112233350483355005002001335500700200133550030020011122\
        \23232323232323333572a66e24d55cf980289000a40004600424002460\
        \0e240020ae466aa04e600424002600624002466a0120106ae88c00c480\
        \048cd4020018d5d11aba3300212001235574060042400246ea400c8d41\
        \2d26123535044001222001112223535501e0012233748900019abb4335\
        \00600500233576866a00c0080026ec9261235350420012220031122233\
        \33333575e4646464646464646666ae54cdc49aab9f300712001480008c\
        \008480049414c1548cccd55da92829918018900098030900082a91999a\
        \abb5250522300312001300412001054233335576a46006240024a0a26a\
        \e8cc00c4800414c8ccd4d4138cd4024020d5d118018900091199a9a828\
        \19a8058049aba230041200122350543355026004002250520542505005\
        \223574660042400246aae80c008480048dd48019282592825928259282\
        \5800826891a9a8200009110011199a820001001bad2375000246666666\
        \aebc941149411494114941148d4118dd68010008239199a81ea8012822\
        \001890009000919191919191919191919191919191999ab95337126014\
        \240029003118010900091802090008289181a980109000918079aba235\
        \574060182400246666ae54cdc4980409000a400846006240024600a240\
        \0209e460606004240024601a6ae88d55d018048900091999ab95337126\
        \00a2400290011180189000918028900082611816180109000918061aba\
        \2355740600c2400246666ae54cdc4980109000a400046008240024600e\
        \2400209246aae7cc010480048c0a8c008480048dd69aba235574060042\
        \400246ea400c8d40f52623232323232323232323232323232323232323\
        \23232323232323333572a66e24c05448005200c2046230021200105b23\
        \333572a66e24c05448005200a2047230031200105b23333572a66e24c0\
        \504800520082300312001230071200105a233041300212001300312001\
        \2375a6ae88c00c480048dd61aba23574660042400246aae80c04c48004\
        \8cccd5ca99b89300f12001480188c00c480048c01c480041548cc0f4c0\
        \0848004c00c480048dd69aba23003120012375a6ae88d5d19801090009\
        \1aaba0300e1200123333572a66e24c0284800520042300312001230071\
        \200105023303c3002120013003120012300e3574460062400246eb4d5d\
        \11aba3300212001235574060122400246666ae54cdc4980289000a4004\
        \46006240024600a2400209646070600424002460126ae88d55d0180309\
        \00091999ab953371260042400290001180209000918038900082411aab\
        \9f3004120012303230021200123005357446aae80c008480048dd48019\
        \1a81e24c46464646464646464646464646666ae54cdc4980409000a400\
        \44600424002460102400209a466607a600424002600624002600824002\
        \46eb0d5d118028900091bac3574460062400246eb0d5d11aba33002120\
        \0123574660042400246aae80c018480048cccd5ca99b89300212001480\
        \008c010480048c01c4800411c8d55cf9802090009181b1801090009180\
        \29aba235574060042400246ea400c8d40ed26232323232323232323333\
        \572a66e24c010480052002230021200123004120010482303b30021200\
        \12375a6ae88d55d018030900091999ab95337126004240029000118020\
        \9000918038900082311aab9f300412001230383002120012375a6ae88d\
        \55d018010900091ba900323503a4988c8c8c8c8c8c8cccd5ca99b89355\
        \73e600a240029000118010900091803890008229198159801090009801\
        \89000918039aba2300312001237586ae88d5d198010900091aaba03002\
        \120012375200646a0729311919191999ab95337126aae7cc0084800520\
        \00230021200123004120010412375a6ae88d55d018010900091ba90032\
        \350384988848cc00400c0088004888888888848cccccccccc00402c028\
        \02402001c01801401000c00880048848cc00400c008800488848ccc004\
        \01000c00880044488008488488cc00401000c48004448848cc00400c00\
        \84480048848cc00400c008800448848cc00400c0084800448848cc0040\
        \0c0084800448848cc00400c00848004484888c00c01044888008448880\
        \04480044880084880048004848888c010014848888c00c014848888c00\
        \8014848888c00401480048848cc00400c0088004848888888c01c02088\
        \48888888cc018024020848888888c014020488888880104888888800c8\
        \848888888cc0080240208848888888cc00402402080048488c00800c88\
        \8488ccc00401401000c80048488c00800c8488c00400c80048ccd400c0\
        \1801c010488848ccc00401000c008480048c8cccd5ca99b8935573e6ea\
        \40092000200b230021200100a2350034984988d4c018004cdd2240006e\
        \c92623333333575e46464646666ae54cdc49aab9f300212001480008c0\
        \0848004940200288cccd55da9180209000928041aaba030021200100a2\
        \375200846a00c0104a0084a0084a0084a00800200c2424460040062244\
        \0022400224002400222464600200244660066460020020040026646664\
        \4466644466666666444444446644666664444466644466664444664466\
        \4466446664446644664466644466446644664466446466446464664464\
        \646464646400244446600a6020002008446a60200044444444444666a6\
        \a04e666aa601e24002a01a4666a606a666ae54cdc800600081b81b11a8\
        \15801128150098019101c101b009099a804a800a80d891199a9a80d801\
        \1100210010031a80090008918009aa81e910891911199a9a80b80091a8\
        \05802911199803090009802801999aa98048900080380280100491a804\
        \801090009000891a9a80180091000891a9a80100091001091091980080\
        \18010900091a9801000910011109198008018011000911111111109199\
        \9999999800805805004804003803002802001801100091091980080180\
        \1100091109199800802001801100088910010910911980080200189000\
        \8891091980080180108900091091980080180110008909118010018891\
        \0008900089109198008018010900089109198008018010900089109198\
        \0080180109000890911180180208911001089110008900089100109100\
        \0900090911118020029091111801802909111180100290911118008029\
        \0009109198008018011000909111111180380411091111111980300480\
        \4109111111180280409111111002091111110019109111111198010048\
        \0411091111111980080480410009091180100191109119980080280200\
        \1900090911801001909118008019000889191800800911980199180080\
        \0801000a451c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1c\
        \f2d40e61a300010481d879800581840000d8798082191a221a019d6038\
        \f5f6"

    -- TODO: This function should really not exist, but instead, it should be
    -- possible to construct a transaction from the API with additional required
    -- signers!
    addRequiredSigners :: SealedTx -> [XPub] -> SealedTx
    addRequiredSigners tx vks =
        case getSealedTxBody tx of
            InAnyCardanoEra AlonzoEra (Cardano.ShelleyTxBody a body b c d e) ->
                let body' = body
                        { Alonzo.reqSignerHashes = Set.fromList $ hashKey <$> vks
                        }
                 in sealedTxFromCardanoBody (Cardano.ShelleyTxBody a body' b c d e)
            _ ->
                tx
      where
        hashKey
            :: forall kd crypto. (Ledger.Crypto crypto)
            => XPub
            -> Ledger.KeyHash kd crypto
        hashKey =
            Ledger.hashKey
            . Ledger.VKey
            . fromJust
            . rawDeserialiseVerKeyDSIGN
            . xpubPublicKey

    fromTextEnvelope cborHex =
        let textEnvelope =
                Cardano.TextEnvelope
                (Cardano.TextEnvelopeType "TxBodyAlonzo")
                ""
                (unsafeFromHex $ T.encodeUtf8 cborHex)

            (Right txBody) =
                Cardano.deserialiseFromTextEnvelope
                (Cardano.AsTxBody Cardano.AsAlonzoEra) textEnvelope

            toCborHexTx txbody =
                T.decodeUtf8 $
                hex $
                Cardano.serialiseToCBOR (Cardano.makeSignedTransaction [] txbody)

        in toCborHexTx txBody
