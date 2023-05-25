{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.API.Byron.HWWallets
    ( spec
    ) where

import Prelude

import Cardano.Mnemonic
    ( Mnemonic
    , SomeMnemonic (..)
    , entropyToMnemonic
    , genEntropy
    , mnemonicToText
    )
import Cardano.Wallet.Address.Derivation
    ( HardDerivation (..), PersistPublicKey (..) )
import Cardano.Wallet.Address.Discovery.Sequential
    ( defaultAddressPoolGap, getAddressPoolGap )
import Cardano.Wallet.Address.Keys.WalletKey
    ( publicKey )
import Cardano.Wallet.Api.Types
    ( AddressAmount (..)
    , ApiAddress (..)
    , ApiAddressWithPath
    , ApiByronWallet
    , ApiCoinSelectionOutput (..)
    , ApiFee
    , ApiTransaction
    , ApiUtxoStatistics
    , WalletStyle (..)
    )
import Cardano.Wallet.Flavor
    ( KeyFlavorS (IcarusKeyS) )
import Cardano.Wallet.Primitive.Types.Address
    ( AddressState (..) )
import Cardano.Wallet.Read.NetworkId
    ( HasSNetworkId (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( encodeAddress )
import Control.Monad
    ( forM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Resource
    ( runResourceT )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Test.Hspec
    ( SpecWith, describe, pendingWith, shouldBe, shouldSatisfy )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , eventually
    , expectErrorMessage
    , expectField
    , expectListField
    , expectListSize
    , expectResponseCode
    , expectWalletUTxO
    , fixtureIcarusWallet
    , fixtureIcarusWalletMws
    , fixturePassphrase
    , getFromResponse
    , icarusAddresses
    , json
    , minUTxOValue
    , postByronWallet
    , request
    , restoreWalletFromPubKey
    , selectCoins
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( errMsg403NoRootKey, updateNamePayload, updatePassPayload )

import qualified Cardano.Wallet.Address.Derivation.Icarus as Icarus
import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.HashSet as Set
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types.Status as HTTP

spec
    :: forall n
     . HasSNetworkId n
    => SpecWith Context
spec = describe "BYRON_HW_WALLETS" $ do
    it "HW_WALLETS_01 - Restoration from account public key preserves funds" $ \ctx -> runResourceT $ do
        wSrc <- fixtureIcarusWallet ctx
        -- create wallet
        mnemonics <- liftIO $ entropyToMnemonic <$> genEntropy
        let mnemonicTxt = mnemonicToText @15 mnemonics
        let payldCrt = Json [json| {
            "name": "!st created",
            "mnemonic_sentence": #{mnemonicTxt},
            "passphrase": #{fixturePassphrase},
            "style": "icarus"
            }|]

        rInit <- postByronWallet ctx payldCrt
        verify rInit
            [ expectResponseCode HTTP.status201
            , expectField (#balance . #available) (`shouldBe` Quantity 0)
            , expectField (#balance . #total) (`shouldBe` Quantity 0)
            ]
        let wDest = getFromResponse id rInit

        --send funds
        let [addr] = take 1 $ icarusAddresses @n mnemonics
        let destination = encodeAddress (sNetworkId @n) addr
        let minUTxOValue' = minUTxOValue (_mainEra ctx)
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{minUTxOValue'},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]
        rTrans <- request @(ApiTransaction n) ctx
            (Link.createTransactionOld @'Byron wSrc) Default payload
        expectResponseCode HTTP.status202 rTrans

        eventually "Wallet balance is as expected" $ do
            rGet <- request @ApiByronWallet ctx
                (Link.getWallet @'Byron wDest) Default Empty
            verify rGet
                [ expectField
                        (#balance . #total) (`shouldBe` Quantity minUTxOValue')
                , expectField
                        (#balance . #available) (`shouldBe` Quantity minUTxOValue')
                ]

        -- delete wallet
        rDel <-
            request @ApiByronWallet ctx (Link.deleteWallet @'Byron wDest) Default Empty
        expectResponseCode HTTP.status204 rDel

        -- restore from account public key and make sure funds are there
        let accXPub = pubKeyFromMnemonics mnemonics
        wDest' <- restoreWalletFromPubKey @ApiByronWallet @'Byron ctx accXPub restoredWalletName

        eventually "Balance of restored wallet is as expected" $ do
            rGet <- request @ApiByronWallet ctx
                (Link.getWallet @'Byron wDest') Default Empty
            verify rGet
                [ expectField
                        (#balance . #total) (`shouldBe` Quantity minUTxOValue')
                , expectField
                        (#balance . #available) (`shouldBe` Quantity minUTxOValue')
                ]

    describe "HW_WALLETS_03 - Cannot do operations requiring private key" $ do
        it "Cannot send tx" $ \ctx -> runResourceT $ do
            (w, mnemonics) <- fixtureIcarusWalletMws ctx
            let pubKey = pubKeyFromMnemonics mnemonics
            r <- request @ApiByronWallet ctx (Link.deleteWallet @'Byron w) Default Empty
            expectResponseCode HTTP.status204 r

            wSrc <- restoreWalletFromPubKey @ApiByronWallet @'Byron ctx pubKey restoredWalletName

            let [addr] = take 1 $ icarusAddresses @n mnemonics
            let destination = encodeAddress (sNetworkId @n) addr
            let minUTxOValue' = minUTxOValue (_mainEra ctx)
            let payload = Json [json|{
                    "payments": [{
                        "address": #{destination},
                        "amount": {
                            "quantity": #{minUTxOValue'},
                            "unit": "lovelace"
                        }
                    }],
                    "passphrase": "cardano-wallet"
                }|]
            rTrans <- request @(ApiTransaction n) ctx
                (Link.createTransactionOld @'Byron wSrc) Default payload
            expectResponseCode HTTP.status403 rTrans
            expectErrorMessage (errMsg403NoRootKey $ wSrc ^. walletId) rTrans

        it "Cannot update pass" $ \ctx -> runResourceT $ do
            mnemonics <- liftIO $ entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wk <- restoreWalletFromPubKey @ApiByronWallet @'Byron ctx pubKey restoredWalletName

            -- cannot update pass
            let payload = updatePassPayload fixturePassphrase "new-wallet-passphrase"
            rup <- request @ApiByronWallet ctx
                (Link.putWalletPassphrase @'Byron wk) Default payload
            expectResponseCode HTTP.status403 rup
            expectErrorMessage (errMsg403NoRootKey $ wk ^. walletId) rup

    describe "HW_WALLETS_04 - Can manage HW wallet the same way as others" $ do
        it "Can update name" $ \ctx -> runResourceT $ do
            mnemonics <- liftIO $ entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wk <- restoreWalletFromPubKey @ApiByronWallet @'Byron ctx pubKey restoredWalletName

            -- cannot update wallet name
            let newName = "new name"
            let payload = updateNamePayload newName
            rup <- request @ApiByronWallet ctx (Link.putWallet @'Byron wk) Default payload
            expectResponseCode HTTP.status200 rup

            rGet <- request @ApiByronWallet ctx
                (Link.getWallet @'Byron wk) Default Empty
            expectField
                (#name . #getApiT . #getWalletName)
                (`shouldBe` newName)
                rGet

        it "Can get tx fee" $ \ctx -> runResourceT $ do
            (w, mnemonics) <- fixtureIcarusWalletMws ctx
            let pubKey = pubKeyFromMnemonics mnemonics
            r <- request @ApiByronWallet ctx (Link.deleteWallet @'Byron w) Default Empty
            expectResponseCode HTTP.status204 r

            wSrc <- restoreWalletFromPubKey @ApiByronWallet @'Byron ctx pubKey restoredWalletName

            let [addr] = take 1 $ icarusAddresses @n mnemonics
            let destination = encodeAddress (sNetworkId @n) addr
            let minUTxOValue' = minUTxOValue (_mainEra ctx)
            let payload = Json [json|{
                    "payments": [{
                        "address": #{destination},
                        "amount": {
                            "quantity": #{minUTxOValue'},
                            "unit": "lovelace"
                        }
                    }]
                }|]

            rFee <- request @ApiFee ctx
                (Link.getTransactionFeeOld @'Byron wSrc) Default payload
            expectResponseCode HTTP.status202 rFee

        it "Can delete" $ \ctx -> runResourceT $ do
            mnemonics <- liftIO $ entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wPub <- restoreWalletFromPubKey @ApiByronWallet @'Byron ctx pubKey restoredWalletName
            r <- request @ApiByronWallet ctx
                (Link.deleteWallet @'Byron wPub) Default Empty
            expectResponseCode HTTP.status204 r

        it "Can see utxo" $ \ctx -> runResourceT $ do
            mnemonics <- liftIO $ entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wPub <- restoreWalletFromPubKey @ApiByronWallet @'Byron ctx pubKey restoredWalletName
            rStat <- request @ApiUtxoStatistics ctx
                (Link.getUTxOsStatistics @'Byron wPub) Default Empty
            expectResponseCode HTTP.status200 rStat
            expectWalletUTxO [] (snd rStat)

        it "Can list addresses" $ \ctx -> runResourceT $ do
            mnemonics <- liftIO $ entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wPub <- restoreWalletFromPubKey @ApiByronWallet @'Byron ctx pubKey restoredWalletName

            let g = fromIntegral $ getAddressPoolGap defaultAddressPoolGap
            r <- request @[ApiAddressWithPath n] ctx
                (Link.listAddresses @'Byron wPub) Default Empty
            expectResponseCode HTTP.status200 r
            expectListSize g r
            forM_ [0..(g-1)] $ \addrNum -> do
                expectListField addrNum (#state . #getApiT) (`shouldBe` Unused) r

        it "Can have address pool gap" $ \ctx -> runResourceT $ do
            mnemonics <- liftIO $ entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            let addrPoolGap = 55 --arbitraty but known
            let payloadRestore = Json [json| {
                    "name": #{restoredWalletName},
                    "account_public_key": #{pubKey},
                    "address_pool_gap": #{addrPoolGap}
                }|]
            rRestore <- postByronWallet ctx payloadRestore
            expectResponseCode HTTP.status201 rRestore

            let wPub = getFromResponse id rRestore

            r <- request @[ApiAddressWithPath n] ctx
                (Link.listAddresses @'Byron wPub) Default Empty
            expectResponseCode HTTP.status200 r
            expectListSize addrPoolGap r
            forM_ [0..(addrPoolGap-1)] $ \addrNum -> do
                expectListField addrNum (#state . #getApiT) (`shouldBe` Unused) r

        it "Can list transactions" $ \ctx -> runResourceT $ do
            mnemonics <- liftIO $ entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wPub <- restoreWalletFromPubKey @ApiByronWallet @'Byron ctx pubKey restoredWalletName

            rt <- request @([ApiTransaction n]) ctx
                (Link.listTransactions @'Byron wPub) Default Empty
            expectResponseCode HTTP.status200 rt
            expectListSize 0 rt

        it "Can get coin selection" $ \ctx -> runResourceT $ do
            (w, mnemonics) <- fixtureIcarusWalletMws ctx
            let pubKey = pubKeyFromMnemonics mnemonics
            r <- request
                @ApiByronWallet ctx (Link.deleteWallet @'Byron w) Default Empty
            expectResponseCode HTTP.status204 r

            source <- restoreWalletFromPubKey
                @ApiByronWallet @'Byron ctx pubKey restoredWalletName
            let paymentCount = 4
            let targetAddresses = take paymentCount $
                    ApiAddress <$>
                    icarusAddresses @n mnemonics
            let minUTxOValue' = minUTxOValue (_mainEra ctx)
            let targetAmounts = take paymentCount $
                    Quantity <$> [minUTxOValue' ..]
            let targetAssets = repeat mempty
            let payments = NE.fromList $ map ($ mempty) $
                    zipWith AddressAmount targetAddresses targetAmounts
            let outputs = zipWith3 ApiCoinSelectionOutput
                    targetAddresses targetAmounts targetAssets
            selectCoins @n @'Byron ctx source payments >>= flip verify
                [ expectResponseCode HTTP.status200
                , expectField #inputs
                    (`shouldSatisfy` (not . null))
                , expectField #outputs
                    (`shouldSatisfy` ((Set.fromList outputs ==) . Set.fromList))
                , expectField #change
                    (`shouldSatisfy` (not . null))
                ]

    describe "HW_WALLETS_05 - Wallet from pubKey is available" $ do
        it "Can get wallet" $ \ctx -> runResourceT $ do
            mnemonics <- liftIO $ entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wPub <- restoreWalletFromPubKey @ApiByronWallet @'Byron ctx pubKey restoredWalletName
            rGet <- request @ApiByronWallet ctx
                (Link.getWallet @'Byron wPub) Default Empty
            expectField
                (#name . #getApiT . #getWalletName)
                (`shouldBe` restoredWalletName)
                rGet

        it "Can list wallet" $ \ctx -> runResourceT $ do
            liftIO $ pendingWith "TODO: appears to be flaky from time to time."
            mnemonics <- liftIO $ entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            _ <- restoreWalletFromPubKey @ApiByronWallet @'Byron ctx pubKey restoredWalletName
            rl <- request @[ApiByronWallet] ctx
                (Link.listWallets @'Byron) Default Empty
            expectListField 0
                (#name . #getApiT . #getWalletName)
                (`shouldBe` restoredWalletName)
                rl

        it "The same account and mnemonic wallet can live side-by-side" $ \ctx -> runResourceT $ do
            liftIO $ pendingWith "TODO: appears to flaky from time to time."
            mnemonics <- liftIO $ entropyToMnemonic <$> genEntropy
            let mnemonicsTxt = mnemonicToText @15 mnemonics

            -- create mnemonic wallet
            let mnemonicWalletName = "Mnemonic wallet"
            let payldCrt = Json [json| {
                "name": #{mnemonicWalletName},
                "mnemonic_sentence": #{mnemonicsTxt},
                "passphrase": #{fixturePassphrase},
                "style": "icarus"
                }|]
            r <- postByronWallet ctx payldCrt
            expectResponseCode HTTP.status201 r

            -- create from account public key
            let accXPub = pubKeyFromMnemonics mnemonics
            _ <- restoreWalletFromPubKey @ApiByronWallet @'Byron ctx accXPub restoredWalletName

            -- both wallets are available
            rl <- request @[ApiByronWallet] ctx
                (Link.listWallets @'Byron) Default Empty
            verify rl
                [ expectListField 0
                    (#name . #getApiT . #getWalletName)
                    (`shouldBe` mnemonicWalletName)
                , expectListField 1
                    (#name . #getApiT . #getWalletName)
                    (`shouldBe` restoredWalletName)
                ]
 where
     restoredWalletName :: Text
     restoredWalletName = "Wallet from pub key"

-- Helper for HWWallets, getting pubKey from mnemonic sentence
pubKeyFromMnemonics :: Mnemonic 15 -> Text
pubKeyFromMnemonics seed =
    T.decodeUtf8 $ serializeXPub $ publicKey IcarusKeyS $
        deriveAccountPrivateKey mempty rootXPrv minBound
  where
    rootXPrv = Icarus.generateKeyFromSeed (SomeMnemonic seed) mempty
