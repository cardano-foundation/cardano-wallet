{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Cardano.Wallet.Address.Discovery.Sequential
    ( defaultAddressPoolGap, getAddressPoolGap )
import Cardano.Wallet.Api.Types
    ( ApiAddressWithPath, ApiFee, ApiTransaction, ApiUtxoStatistics, ApiWallet,
    WalletStyle (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( AddressState (..) )
import Cardano.Wallet.Read.NetworkId
    ( HasSNetworkId )
import Control.Monad
    ( forM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Resource
    ( runResourceT )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Test.Hspec
    ( SpecWith, describe )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..), Headers (..), Payload (..), emptyWallet, eventually,
    expectErrorMessage, expectField, expectListField, expectListSize,
    expectResponseCode, expectWalletUTxO, fixturePassphrase, fixtureWallet,
    fixtureWalletWithMnemonics, getFromResponse, json, listAddresses,
    minUTxOValue, postWallet, pubKeyFromMnemonics, request,
    restoreWalletFromPubKey, unsafeResponse, verify, walletId )
import Test.Integration.Framework.TestData
    ( errMsg403NoRootKey, payloadWith, updateNamePayload, updatePassPayload )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Network.HTTP.Types.Status as HTTP

spec
    :: forall n
     . HasSNetworkId n
    => SpecWith Context
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
        let minUTxOValue' = minUTxOValue (_mainEra ctx)
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{minUTxOValue' },
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]
        rTrans <- request @(ApiTransaction n) ctx
            (Link.createTransactionOld @'Shelley wSrc) Default payload
        expectResponseCode HTTP.status202 rTrans

        eventually "Wallet balance is as expected" $ do
            rGet <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            verify rGet
                [ expectField
                        (#balance . #total) (`shouldBe` Quantity minUTxOValue')
                , expectField
                        (#balance . #available) (`shouldBe` Quantity minUTxOValue')
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
                        (#balance . #total) (`shouldBe` Quantity minUTxOValue')
                , expectField
                        (#balance . #available) (`shouldBe` Quantity minUTxOValue')
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
                            "quantity": #{minUTxOValue (_mainEra ctx) },
                            "unit": "lovelace"
                        }
                    }],
                    "passphrase": "cardano-wallet"
                }|]
            rTrans <- request @(ApiTransaction n) ctx
                (Link.createTransactionOld @'Shelley wSrc) Default payload
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
                            "quantity": #{minUTxOValue (_mainEra ctx) },
                            "unit": "lovelace"
                        }
                    }]
                }|]

            rFee <- request @ApiFee ctx
                (Link.getTransactionFeeOld @'Shelley wSrc) Default payload
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
            r <- request @[ApiAddressWithPath n] ctx
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

            r <- request @[ApiAddressWithPath n] ctx
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

        it "Can create a coin selection" $
            pure (pure ())
            -- This is covered in Integration.Scenario.API.Shelley.Transactions.

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
    restoredWalletName :: Text
    restoredWalletName = "Wallet from pub key"
