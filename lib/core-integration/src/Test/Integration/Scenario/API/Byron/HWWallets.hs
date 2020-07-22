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
import Cardano.Wallet.Api.Types
    ( AddressAmount (..)
    , ApiAddress
    , ApiByronWallet
    , ApiFee
    , ApiT (..)
    , ApiTransaction
    , ApiUtxoStatistics
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( HardDerivation (..)
    , PaymentAddress
    , PersistPublicKey (..)
    , WalletKey (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( defaultAddressPoolGap, getAddressPoolGap )
import Cardano.Wallet.Primitive.Types
    ( AddressState (..) )
import Control.Monad
    ( forM_ )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.List.NonEmpty
    ( NonEmpty ((:|)) )
import Data.Proxy
    ( Proxy (..) )
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
    , request
    , restoreWalletFromPubKey
    , selectCoins
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( errMsg403NoRootKey, updateNamePayload, updatePassPayload )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Cardano.Wallet.Primitive.AddressDerivation.Icarus as Icarus
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types.Status as HTTP


spec :: forall n t.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    , PaymentAddress n IcarusKey
    ) => SpecWith (Context t)
spec = do
    it "HW_WALLETS_01 - Restoration from account public key preserves funds" $ \ctx -> do
        wSrc <- fixtureIcarusWallet ctx
        -- create wallet
        mnemonics <- entropyToMnemonic <$> genEntropy
        let mnemonicTxt = mnemonicToText @15 mnemonics
        let payldCrt = Json [json| {
            "name": "!st created",
            "mnemonic_sentence": #{mnemonicTxt},
            "passphrase": #{fixturePassphrase},
            "style": "icarus"
            }|]
        rInit <- request @ApiByronWallet ctx (Link.postWallet @'Byron) Default payldCrt
        verify rInit
            [ expectResponseCode @IO HTTP.status201
            , expectField (#balance . #available) (`shouldBe` Quantity 0)
            , expectField (#balance . #total) (`shouldBe` Quantity 0)
            ]
        let wDest = getFromResponse id rInit

        --send funds
        let [addr] = take 1 $ icarusAddresses @n mnemonics
        let destination = encodeAddress @n addr
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
            (Link.createTransaction @'Byron wSrc) Default payload
        expectResponseCode @IO HTTP.status202 rTrans

        eventually "Wallet balance is as expected" $ do
            rGet <- request @ApiByronWallet ctx
                (Link.getWallet @'Byron wDest) Default Empty
            verify rGet
                [ expectField
                        (#balance . #total) (`shouldBe` Quantity minUTxOValue)
                , expectField
                        (#balance . #available) (`shouldBe` Quantity minUTxOValue)
                ]

        -- delete wallet
        rDel <-
            request @ApiByronWallet ctx (Link.deleteWallet @'Byron wDest) Default Empty
        expectResponseCode @IO HTTP.status204 rDel

        -- restore from account public key and make sure funds are there
        let accXPub = pubKeyFromMnemonics mnemonics
        wDest' <- restoreWalletFromPubKey @ApiByronWallet @'Byron ctx accXPub restoredWalletName

        eventually "Balance of restored wallet is as expected" $ do
            rGet <- request @ApiByronWallet ctx
                (Link.getWallet @'Byron wDest') Default Empty
            verify rGet
                [ expectField
                        (#balance . #total) (`shouldBe` Quantity minUTxOValue)
                , expectField
                        (#balance . #available) (`shouldBe` Quantity minUTxOValue)
                ]

    describe "HW_WALLETS_03 - Cannot do operations requiring private key" $ do
        it "Cannot send tx" $ \ctx -> do
            (w, mnemonics) <- fixtureIcarusWalletMws ctx
            let pubKey = pubKeyFromMnemonics mnemonics
            r <- request @ApiByronWallet ctx (Link.deleteWallet @'Byron w) Default Empty
            expectResponseCode @IO HTTP.status204 r

            wSrc <- restoreWalletFromPubKey @ApiByronWallet @'Byron ctx pubKey restoredWalletName

            let [addr] = take 1 $ icarusAddresses @n mnemonics
            let destination = encodeAddress @n addr
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
                (Link.createTransaction @'Byron wSrc) Default payload
            expectResponseCode @IO HTTP.status403 rTrans
            expectErrorMessage (errMsg403NoRootKey $ wSrc ^. walletId) rTrans

        it "Cannot update pass" $ \ctx -> do
            mnemonics <- entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wk <- restoreWalletFromPubKey @ApiByronWallet @'Byron ctx pubKey restoredWalletName

            -- cannot update pass
            let payload = updatePassPayload fixturePassphrase "new-wallet-passphrase"
            rup <- request @ApiByronWallet ctx
                (Link.putWalletPassphrase @'Byron wk) Default payload
            expectResponseCode @IO HTTP.status403 rup
            expectErrorMessage (errMsg403NoRootKey $ wk ^. walletId) rup

    describe "HW_WALLETS_04 - Can manage HW wallet the same way as others" $ do
        it "Can update name" $ \ctx -> do
            mnemonics <- entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wk <- restoreWalletFromPubKey @ApiByronWallet @'Byron ctx pubKey restoredWalletName

            -- cannot update wallet name
            let newName = "new name"
            let payload = updateNamePayload newName
            rup <- request @ApiByronWallet ctx (Link.putWallet @'Byron wk) Default payload
            expectResponseCode @IO HTTP.status200 rup

            rGet <- request @ApiByronWallet ctx
                (Link.getWallet @'Byron wk) Default Empty
            expectField
                (#name . #getApiT . #getWalletName)
                (`shouldBe` newName)
                rGet

        it "Can get tx fee" $ \ctx -> do
            (w, mnemonics) <- fixtureIcarusWalletMws ctx
            let pubKey = pubKeyFromMnemonics mnemonics
            r <- request @ApiByronWallet ctx (Link.deleteWallet @'Byron w) Default Empty
            expectResponseCode @IO HTTP.status204 r

            wSrc <- restoreWalletFromPubKey @ApiByronWallet @'Byron ctx pubKey restoredWalletName

            let [addr] = take 1 $ icarusAddresses @n mnemonics
            let destination = encodeAddress @n addr
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
                (Link.getTransactionFee @'Byron wSrc) Default payload
            expectResponseCode @IO HTTP.status202 rFee

        it "Can delete" $ \ctx -> do
            mnemonics <- entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wPub <- restoreWalletFromPubKey @ApiByronWallet @'Byron ctx pubKey restoredWalletName
            r <- request @ApiByronWallet ctx
                (Link.deleteWallet @'Byron wPub) Default Empty
            expectResponseCode @IO HTTP.status204 r

        it "Can see utxo" $ \ctx -> do
            mnemonics <- entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wPub <- restoreWalletFromPubKey @ApiByronWallet @'Byron ctx pubKey restoredWalletName
            rStat <- request @ApiUtxoStatistics ctx
                (Link.getUTxOsStatistics @'Byron wPub) Default Empty
            expectResponseCode @IO HTTP.status200 rStat
            expectWalletUTxO [] (snd rStat)

        it "Can list addresses" $ \ctx -> do
            mnemonics <- entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wPub <- restoreWalletFromPubKey @ApiByronWallet @'Byron ctx pubKey restoredWalletName

            let g = fromIntegral $ getAddressPoolGap defaultAddressPoolGap
            r <- request @[ApiAddress n] ctx
                (Link.listAddresses @'Byron wPub) Default Empty
            expectResponseCode @IO HTTP.status200 r
            expectListSize g r
            forM_ [0..(g-1)] $ \addrNum -> do
                expectListField addrNum (#state . #getApiT) (`shouldBe` Unused) r

        it "Can have address pool gap" $ \ctx -> do
            mnemonics <- entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            let addrPoolGap = 55 --arbitraty but known
            let payloadRestore = Json [json| {
                    "name": #{restoredWalletName},
                    "account_public_key": #{pubKey},
                    "address_pool_gap": #{addrPoolGap}
                }|]
            rRestore <- request @ApiByronWallet ctx (Link.postWallet @'Byron)
                    Default payloadRestore
            expectResponseCode @IO HTTP.status201 rRestore

            let wPub = getFromResponse id rRestore

            r <- request @[ApiAddress n] ctx
                (Link.listAddresses @'Byron wPub) Default Empty
            expectResponseCode @IO HTTP.status200 r
            expectListSize addrPoolGap r
            forM_ [0..(addrPoolGap-1)] $ \addrNum -> do
                expectListField addrNum (#state . #getApiT) (`shouldBe` Unused) r

        it "Can list transactions" $ \ctx -> do
            mnemonics <- entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wPub <- restoreWalletFromPubKey @ApiByronWallet @'Byron ctx pubKey restoredWalletName

            rt <- request @([ApiTransaction n]) ctx
                (Link.listTransactions @'Byron wPub) Default Empty
            expectResponseCode HTTP.status200 rt
            expectListSize 0 rt

        it "Can get coin selection" $ \ctx -> do
            (w, mnemonics) <- fixtureIcarusWalletMws ctx
            let pubKey = pubKeyFromMnemonics mnemonics
            r <- request @ApiByronWallet ctx (Link.deleteWallet @'Byron w) Default Empty
            expectResponseCode @IO HTTP.status204 r

            source <- restoreWalletFromPubKey @ApiByronWallet @'Byron ctx pubKey restoredWalletName
            let [addr] = take 1 $ icarusAddresses @n mnemonics

            let amount = Quantity 1
            let payment = AddressAmount (ApiT addr, Proxy @n) amount
            selectCoins @n @'Byron ctx source (payment :| []) >>= flip verify
                [ expectResponseCode HTTP.status200
                , expectField #inputs (`shouldSatisfy` (not . null))
                , expectField #outputs (`shouldSatisfy` ((> 1) . length))
                , expectField #outputs (`shouldSatisfy` (payment `elem`))
                ]

    describe "HW_WALLETS_05 - Wallet from pubKey is available" $ do
        it "Can get wallet" $ \ctx -> do
            mnemonics <- entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wPub <- restoreWalletFromPubKey @ApiByronWallet @'Byron ctx pubKey restoredWalletName
            rGet <- request @ApiByronWallet ctx
                (Link.getWallet @'Byron wPub) Default Empty
            expectField
                (#name . #getApiT . #getWalletName)
                (`shouldBe` restoredWalletName)
                rGet

        it "Can list wallet" $ \ctx -> do
            pendingWith "TODO: appears to be flaky from time to time."
            mnemonics <- entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            _ <- restoreWalletFromPubKey @ApiByronWallet @'Byron ctx pubKey restoredWalletName
            rl <- request @[ApiByronWallet] ctx
                (Link.listWallets @'Byron) Default Empty
            expectListField 0
                (#name . #getApiT . #getWalletName)
                (`shouldBe` restoredWalletName)
                rl

        it "The same account and mnemonic wallet can live side-by-side" $ \ctx -> do
            pendingWith "TODO: appears to flaky from time to time."
            mnemonics <- entropyToMnemonic <$> genEntropy
            let mnemonicsTxt = mnemonicToText @15 mnemonics

            -- create mnemonic wallet
            let mnemonicWalletName = "Mnemonic wallet"
            let payldCrt = Json [json| {
                "name": #{mnemonicWalletName},
                "mnemonic_sentence": #{mnemonicsTxt},
                "passphrase": #{fixturePassphrase},
                "style": "icarus"
                }|]
            r <- request @ApiByronWallet ctx (Link.postWallet @'Byron) Default payldCrt
            expectResponseCode @IO HTTP.status201 r

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
    T.decodeUtf8 $ serializeXPub $ publicKey $
        deriveAccountPrivateKey mempty rootXPrv minBound
  where
    rootXPrv = Icarus.generateKeyFromSeed (SomeMnemonic seed) mempty
