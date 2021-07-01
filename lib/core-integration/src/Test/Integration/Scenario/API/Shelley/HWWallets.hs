{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.API.Shelley.HWWallets
    ( spec
    ) where

import Prelude

import Cardano.Mnemonic
    ( entropyToMnemonic, genEntropy, mnemonicToText )
import Cardano.Wallet.Api.Types
    ( AddressAmount (..)
    , ApiAddress
    , ApiCoinSelection (..)
    , ApiCoinSelectionOutput (..)
    , ApiFee
    , ApiTransaction
    , ApiUtxoStatistics
    , ApiWallet
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( defaultAddressPoolGap, getAddressPoolGap )
import Cardano.Wallet.Primitive.Types.Address
    ( AddressState (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxMetadata (..) )
import Control.Monad
    ( forM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Resource
    ( runResourceT )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Monoid
    ( Sum (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word64 )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith, describe )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain, shouldSatisfy )
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
    , expectListField
    , expectListSize
    , expectResponseCode
    , expectWalletUTxO
    , fixturePassphrase
    , fixtureWallet
    , fixtureWalletWithMnemonics
    , getFromResponse
    , json
    , listAddresses
    , minUTxOValue
    , postWallet
    , pubKeyFromMnemonics
    , request
    , restoreWalletFromPubKey
    , selectCoins
    , unsafeResponse
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( errMsg403NoRootKey, payloadWith, updateNamePayload, updatePassPayload )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Foldable as F
import qualified Data.HashSet as Set
import qualified Data.List.NonEmpty as NE
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall n.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    ) => SpecWith Context
spec = describe "SHELLEY_HW_WALLETS" $ do
    it "HW_WALLETS_01 - Restoration from account public key preserves funds" $ \ctx -> runResourceT $ do
        wSrc <- fixtureWallet ctx
        -- create wallet
        mnemonics <- liftIO $ mnemonicToText @15 . entropyToMnemonic <$> genEntropy
        let wName = "!st created"
        let payldCrt = payloadWith wName mnemonics
        rInit <- postWallet ctx payldCrt
        verify rInit
            [ expectResponseCode HTTP.status201
            , expectField (#balance . #available) (`shouldBe` Quantity 0)
            , expectField (#balance . #total) (`shouldBe` Quantity 0)
            ]

        --send funds
        let wDest = getFromResponse id rInit
        addrs <- listAddresses @n ctx wDest
        let destination = (addrs !! 1) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{minUTxOValue},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]
        rTrans <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload
        expectResponseCode HTTP.status202 rTrans

        eventually "Wallet balance is as expected" $ do
            rGet <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            verify rGet
                [ expectField
                        (#balance . #total) (`shouldBe` Quantity minUTxOValue)
                , expectField
                        (#balance . #available) (`shouldBe` Quantity minUTxOValue)
                ]

        -- delete wallet
        rDel <-
            request @ApiWallet ctx (Link.deleteWallet @'Shelley wDest) Default Empty
        expectResponseCode HTTP.status204 rDel

        -- restore from account public key and make sure funds are there
        let accXPub = pubKeyFromMnemonics mnemonics
        wDest' <- restoreWalletFromPubKey @ApiWallet @'Shelley ctx accXPub restoredWalletName

        eventually "Balance of restored wallet is as expected" $ do
            rGet <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest') Default Empty
            verify rGet
                [ expectField
                        (#balance . #total) (`shouldBe` Quantity minUTxOValue)
                , expectField
                        (#balance . #available) (`shouldBe` Quantity minUTxOValue)
                ]

    describe "HW_WALLETS_03 - Cannot do operations requiring private key" $ do
        it "Cannot send tx" $ \ctx -> runResourceT $ do
            (w, mnemonics) <- fixtureWalletWithMnemonics (Proxy @"shelley") ctx
            let pubKey = pubKeyFromMnemonics mnemonics
            r <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
            expectResponseCode HTTP.status204 r

            wSrc <- restoreWalletFromPubKey @ApiWallet @'Shelley ctx pubKey restoredWalletName
            wDest <- emptyWallet ctx

            addrs <- listAddresses @n ctx wDest
            let destination = (addrs !! 1) ^. #id
            let payload = Json [json|{
                    "payments": [{
                        "address": #{destination},
                        "amount": {
                            "quantity": #{minUTxOValue},
                            "unit": "lovelace"
                        }
                    }],
                    "passphrase": "cardano-wallet"
                }|]
            rTrans <- request @(ApiTransaction n) ctx
                (Link.createTransaction @'Shelley wSrc) Default payload
            expectResponseCode HTTP.status403 rTrans
            expectErrorMessage (errMsg403NoRootKey $ wSrc ^. walletId) rTrans

        it "Cannot update pass" $ \ctx -> runResourceT $ do
            mnemonics <- liftIO $ mnemonicToText @15 . entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wk <- restoreWalletFromPubKey @ApiWallet @'Shelley ctx pubKey restoredWalletName

            -- cannot update pass
            let payload = updatePassPayload fixturePassphrase "new-wallet-passphrase"
            rup <- request @ApiWallet ctx
                (Link.putWalletPassphrase @'Shelley wk) Default payload
            expectResponseCode HTTP.status403 rup
            expectErrorMessage (errMsg403NoRootKey $ wk ^. walletId) rup

    describe "HW_WALLETS_04 - Can manage HW wallet the same way as others" $ do
        it "Can update name" $ \ctx -> runResourceT $ do
            mnemonics <- liftIO $ mnemonicToText @15 . entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wk <- restoreWalletFromPubKey @ApiWallet @'Shelley ctx pubKey restoredWalletName

            -- cannot update wallet name
            let newName = "new name"
            let payload = updateNamePayload newName
            rup <- request @ApiWallet ctx (Link.putWallet @'Shelley wk) Default payload
            expectResponseCode HTTP.status200 rup

            rGet <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wk) Default Empty
            expectField
                (#name . #getApiT . #getWalletName)
                (`shouldBe` newName)
                rGet

        it "Can get tx fee" $ \ctx -> runResourceT $ do
            (w, mnemonics) <- fixtureWalletWithMnemonics (Proxy @"shelley") ctx
            let pubKey = pubKeyFromMnemonics mnemonics
            r <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
            expectResponseCode HTTP.status204 r

            wSrc <- restoreWalletFromPubKey @ApiWallet @'Shelley ctx pubKey restoredWalletName
            wDest <- emptyWallet ctx

            addrs <- listAddresses @n ctx wDest
            let destination = (addrs !! 1) ^. #id
            let payload = Json [json|{
                    "payments": [{
                        "address": #{destination},
                        "amount": {
                            "quantity": #{minUTxOValue},
                            "unit": "lovelace"
                        }
                    }]
                }|]

            rFee <- request @ApiFee ctx
                (Link.getTransactionFee @'Shelley wSrc) Default payload
            expectResponseCode HTTP.status202 rFee

        it "Can delete" $ \ctx -> runResourceT $ do
            mnemonics <- liftIO $ mnemonicToText @15 . entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wPub <- restoreWalletFromPubKey @ApiWallet @'Shelley ctx pubKey restoredWalletName
            r <- request @ApiWallet ctx
                (Link.deleteWallet @'Shelley wPub) Default Empty
            expectResponseCode HTTP.status204 r

        it "Can see utxo" $ \ctx -> runResourceT $ do
            mnemonics <- liftIO $ mnemonicToText @15 . entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wPub <- restoreWalletFromPubKey @ApiWallet @'Shelley ctx pubKey restoredWalletName
            rStat <- request @ApiUtxoStatistics ctx
                (Link.getUTxOsStatistics @'Shelley wPub) Default Empty
            expectResponseCode HTTP.status200 rStat
            expectWalletUTxO [] (snd rStat)

        it "Can list addresses" $ \ctx -> runResourceT $ do
            mnemonics <- liftIO $ mnemonicToText @15 . entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wPub <- restoreWalletFromPubKey @ApiWallet @'Shelley ctx pubKey restoredWalletName

            let g = fromIntegral $ getAddressPoolGap defaultAddressPoolGap
            r <- request @[ApiAddress n] ctx
                (Link.listAddresses @'Shelley wPub) Default Empty
            expectResponseCode HTTP.status200 r
            expectListSize g r
            forM_ [0..(g-1)] $ \addrNum -> do
                expectListField addrNum (#state . #getApiT) (`shouldBe` Unused) r

        it "Can have address pool gap" $ \ctx -> runResourceT $ do
            mnemonics <- liftIO $ mnemonicToText @15 . entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            let addrPoolGap = 55 --arbitraty but known
            let payloadRestore = Json [json| {
                    "name": #{restoredWalletName},
                    "account_public_key": #{pubKey},
                    "address_pool_gap": #{addrPoolGap}
                }|]
            rRestore <- postWallet ctx payloadRestore
            expectResponseCode HTTP.status201 rRestore

            let wPub = getFromResponse id rRestore

            r <- request @[ApiAddress n] ctx
                (Link.listAddresses @'Shelley wPub) Default Empty
            expectResponseCode HTTP.status200 r
            expectListSize addrPoolGap r
            forM_ [0..(addrPoolGap-1)] $ \addrNum -> do
                expectListField addrNum (#state . #getApiT) (`shouldBe` Unused) r

        it "Can list transactions" $ \ctx -> runResourceT $ do
            mnemonics <- liftIO $ mnemonicToText @15 . entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wPub <- restoreWalletFromPubKey @ApiWallet @'Shelley ctx pubKey restoredWalletName

            rt <- request @([ApiTransaction n]) ctx
                (Link.listTransactions @'Shelley wPub) Default Empty
            expectResponseCode HTTP.status200 rt
            expectListSize 0 rt

        describe "Can create a coin selection" $
            mapM_ spec_selectCoins
                [ ( "without metadata"
                  , Nothing
                  , Quantity 130_500
                  )
                ]

    describe "HW_WALLETS_05 - Wallet from pubKey is available" $ do
        it "Can get wallet" $ \ctx -> runResourceT $ do
            mnemonics <- liftIO $ mnemonicToText @15 . entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wPub <- restoreWalletFromPubKey @ApiWallet @'Shelley ctx pubKey restoredWalletName
            rGet <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wPub) Default Empty
            expectField
                (#name . #getApiT . #getWalletName)
                (`shouldBe` restoredWalletName)
                rGet

        it "Can list wallet" $ \ctx -> runResourceT $ do
            mnemonics <- liftIO $ mnemonicToText @15 . entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            w <- restoreWalletFromPubKey @ApiWallet @'Shelley ctx pubKey restoredWalletName
            wids <- map (view #id) . unsafeResponse <$> request @[ApiWallet] ctx
                (Link.listWallets @'Shelley) Default Empty
            liftIO $ wids `shouldContain` [view #id w]

        it "The same account and mnemonic wallet can live side-by-side" $ \ctx -> runResourceT $ do
            mnemonics <- liftIO $ mnemonicToText @15 . entropyToMnemonic <$> genEntropy

            -- create mnemonic wallet
            let mnemonicWalletName = "Mnemonic wallet"
            let payldCrt = payloadWith mnemonicWalletName mnemonics
            r1' <- postWallet ctx payldCrt
            expectResponseCode HTTP.status201 r1'

            -- create from account public key
            let accXPub = pubKeyFromMnemonics mnemonics
            r2' <- restoreWalletFromPubKey @ApiWallet @'Shelley ctx accXPub restoredWalletName

            r1 <- request @ApiWallet ctx (Link.getWallet @'Shelley (getFromResponse id r1')) Default Empty
            r2 <- request @ApiWallet ctx (Link.getWallet @'Shelley r2') Default Empty

            -- both wallets are available
            verify r1
                [ expectField (#name . #getApiT . #getWalletName)
                    (`shouldBe` mnemonicWalletName)
                ]
            verify r2
                [ expectField
                    (#name . #getApiT . #getWalletName)
                    (`shouldBe` restoredWalletName)
                ]
  where
    spec_selectCoins
        :: (String, Maybe TxMetadata, Quantity "lovelace" Word64)
        -> SpecWith Context
    spec_selectCoins (testName, _mTxMetadata, Quantity expectedFee) =
        it testName $ \ctx -> runResourceT $ do
            (w, mnemonics) <- fixtureWalletWithMnemonics (Proxy @"shelley") ctx
            let pubKey = pubKeyFromMnemonics mnemonics
            r <- request
                @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
            expectResponseCode HTTP.status204 r
            source <- restoreWalletFromPubKey
                @ApiWallet @'Shelley ctx pubKey restoredWalletName
            target <- emptyWallet ctx
            let paymentCount = 1
            targetAddresses <- take paymentCount .
                fmap (view #id) <$> listAddresses @n ctx target
            let targetAmounts = take paymentCount $
                    Quantity <$> [minUTxOValue ..]
            let targetAssets = repeat mempty
            let payments = NE.fromList $ map ($ mempty) $
                    zipWith AddressAmount targetAddresses targetAmounts
            let outputs = zipWith3 ApiCoinSelectionOutput
                    targetAddresses targetAmounts targetAssets
            coinSelectionResponse <-
                selectCoins @n @'Shelley ctx source payments
            verify coinSelectionResponse
                [ expectResponseCode HTTP.status200
                , expectField #inputs
                    (`shouldSatisfy` (not . null))
                , expectField #outputs
                    (`shouldSatisfy` ((Set.fromList outputs ==) . Set.fromList))
                , expectField #change
                    (`shouldSatisfy` (not . null))
                ]
            let apiCoinSelection =
                    getFromResponse Prelude.id coinSelectionResponse
            let fee = computeApiCoinSelectionFee apiCoinSelection
            fee `shouldBe` Coin expectedFee

    restoredWalletName :: Text
    restoredWalletName = "Wallet from pub key"

computeApiCoinSelectionFee :: ApiCoinSelection n -> Coin
computeApiCoinSelectionFee selection
    | feeIsValid =
        Coin $ fromIntegral fee
    | otherwise =
        error $ unlines
            [ "Unable to compute fee of ApiCoinSelection:"
            , "fee:"
            , show fee
            , "balanceOfInputs:"
            , show balanceOfInputs
            , "balanceOfOutputs:"
            , show balanceOfOutputs
            , "balanceOfChange:"
            , show balanceOfChange
            , "balanceOfRewardWithdrawals"
            , show balanceOfRewardWithdrawals
            , "balanceOfDeposits"
            , show balanceOfDeposits
            ]
  where
    fee :: Integer
    fee
        = balanceOfInputs
        + balanceOfRewardWithdrawals
        - balanceOfOutputs
        - balanceOfChange
        - balanceOfDeposits
    feeIsValid :: Bool
    feeIsValid = (&&)
        (fee >= fromIntegral (unCoin (minBound :: Coin)))
        (fee <= fromIntegral (unCoin (maxBound :: Coin)))
    balanceOfInputs
        = selection
        & view #inputs
        & F.foldMap (Sum . quantityToInteger . view #amount)
        & getSum
    balanceOfOutputs
        = selection
        & view #outputs
        & F.foldMap (Sum . quantityToInteger . view #amount)
        & getSum
    balanceOfChange
        = selection
        & view #change
        & F.foldMap (Sum . quantityToInteger . view #amount)
        & getSum
    balanceOfRewardWithdrawals
        = selection
        & view #withdrawals
        & F.foldMap (Sum . quantityToInteger . view #amount)
        & getSum
    balanceOfDeposits
        = selection
        & view #deposits
        & F.foldMap (Sum . quantityToInteger)
        & getSum

quantityToInteger :: Quantity "lovelace" Natural -> Integer
quantityToInteger (Quantity n) = fromIntegral n
