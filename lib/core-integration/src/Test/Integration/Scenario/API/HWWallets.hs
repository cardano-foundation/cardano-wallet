{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.API.HWWallets
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( AddressAmount (..)
    , ApiAddress
    , ApiFee
    , ApiStakePool
    , ApiTransaction
    , ApiUtxoStatistics
    , ApiWallet
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( FromMnemonic (..)
    , HardDerivation (..)
    , NetworkDiscriminant (..)
    , PersistPublicKey (..)
    , WalletKey (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( generateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( defaultAddressPoolGap, getAddressPoolGap )
import Cardano.Wallet.Primitive.Mnemonic
    ( entropyToMnemonic, genEntropy, mnemonicToText )
import Cardano.Wallet.Primitive.Types
    ( AddressState (..), SyncProgress (..) )
import Control.Monad
    ( forM_ )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.List.NonEmpty
    ( NonEmpty ((:|)) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Test.Hspec
    ( SpecWith, describe, it, shouldBe, shouldSatisfy )
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
    , getSlotParams
    , joinStakePool
    , json
    , listAddresses
    , mkEpochInfo
    , notDelegating
    , quitStakePool
    , request
    , selectCoins
    , unsafeRequest
    , verify
    , waitAllTxsInLedger
    , waitForNextEpoch
    , walletId
    )
import Test.Integration.Framework.TestData
    ( errMsg403NoRootKey, payloadWith, updateNamePayload, updatePassPayload )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types.Status as HTTP


spec :: forall t n. (n ~ 'Testnet) => SpecWith (Context t)
spec = do
    it "HW_WALLETS_01 - Restoration from account public key preserves funds" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        -- create wallet
        mnemonics <- mnemonicToText @15 . entropyToMnemonic <$> genEntropy
        let wName = "!st created"
        let payldCrt = payloadWith wName mnemonics
        rInit <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default payldCrt
        verify rInit
            [ expectResponseCode @IO HTTP.status201
            , expectField (#balance . #getApiT . #available) (`shouldBe` Quantity 0)
            , expectField (#balance . #getApiT . #total) (`shouldBe` Quantity 0)
            ]

        --send funds
        let wDest = getFromResponse id rInit
        addrs <- listAddresses ctx wDest
        let destination = (addrs !! 1) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": 1,
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]
        rTrans <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload
        expectResponseCode @IO HTTP.status202 rTrans

        eventually "Wallet balance is as expected" $ do
            rGet <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            verify rGet
                [ expectField
                        (#balance . #getApiT . #total) (`shouldBe` Quantity 1)
                , expectField
                        (#balance . #getApiT . #available) (`shouldBe` Quantity 1)
                ]

        -- delete wallet
        rDel <-
            request @ApiWallet ctx (Link.deleteWallet @'Shelley wDest) Default Empty
        expectResponseCode @IO HTTP.status204 rDel

        -- restore from account public key and make sure funds are there
        let accXPub = pubKeyFromMnemonics mnemonics
        wDest' <- restoreWalletFromPubKey ctx accXPub

        eventually "Balance of restored wallet is as expected" $ do
            rGet <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest') Default Empty
            verify rGet
                [ expectField
                        (#balance . #getApiT . #total) (`shouldBe` Quantity 1)
                , expectField
                        (#balance . #getApiT . #available) (`shouldBe` Quantity 1)
                ]

    it "HW_WALLETS_02 - Restoration from account public key preserves delegation\
        \ but I cannot quit" $ \ctx -> do
        -- create wallet and get acc pub key from mnemonics
        (w, mnemonics) <- fixtureWalletWithMnemonics ctx
        let accPub = pubKeyFromMnemonics mnemonics

        --make sure you are at the beginning of the epoch
        waitForNextEpoch ctx
        (currentEpoch, sp) <- getSlotParams ctx

        -- join stake pool
        (_, p:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
        r <- joinStakePool ctx (p ^. #id) (w, fixturePassphrase)
        expectResponseCode @IO HTTP.status202 r
        waitAllTxsInLedger ctx w
        let expectedDelegation =
                [ expectField #delegation
                    (`shouldBe` notDelegating
                        [ (Just (p ^. #id), mkEpochInfo (currentEpoch + 2) sp)
                        ]
                    )
                ]
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            expectedDelegation

        -- delete wallet
        rDel <-
            request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
        expectResponseCode @IO HTTP.status204 rDel

        -- restore from pub key and make sure delegation preserved
        wRestored <- restoreWalletFromPubKey ctx accPub
        request @ApiWallet ctx (Link.getWallet @'Shelley wRestored) Default Empty >>= flip verify
            expectedDelegation

        -- cannot quit stake pool
        rQuit <- quitStakePool ctx (wRestored, fixturePassphrase)
        expectResponseCode @IO HTTP.status403 rQuit
        expectErrorMessage (errMsg403NoRootKey $ wRestored ^. walletId) rQuit

    describe "HW_WALLETS_03 - Cannot do operations requiring private key" $ do
        it "Cannot send tx" $ \ctx -> do
            (w, mnemonics) <- fixtureWalletWithMnemonics ctx
            let pubKey = pubKeyFromMnemonics mnemonics
            r <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
            expectResponseCode @IO HTTP.status204 r

            wSrc <- restoreWalletFromPubKey ctx pubKey
            wDest <- emptyWallet ctx

            addrs <- listAddresses ctx wDest
            let destination = (addrs !! 1) ^. #id
            let payload = Json [json|{
                    "payments": [{
                        "address": #{destination},
                        "amount": {
                            "quantity": 1,
                            "unit": "lovelace"
                        }
                    }],
                    "passphrase": "cardano-wallet"
                }|]
            rTrans <- request @(ApiTransaction n) ctx
                (Link.createTransaction @'Shelley wSrc) Default payload
            expectResponseCode @IO HTTP.status403 rTrans
            expectErrorMessage (errMsg403NoRootKey $ wSrc ^. walletId) rTrans

        it "Cannot join SP" $ \ctx -> do
            (w, mnemonics) <- fixtureWalletWithMnemonics ctx
            let pubKey = pubKeyFromMnemonics mnemonics
            r <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
            expectResponseCode @IO HTTP.status204 r

            wk <- restoreWalletFromPubKey ctx pubKey
            -- cannot join stake pool
            (_, p:_) <- eventually "Stake pools are listed" $
                unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
            rJoin <- joinStakePool ctx (p ^. #id) (wk, fixturePassphrase)
            expectResponseCode @IO HTTP.status403 rJoin
            expectErrorMessage (errMsg403NoRootKey $ wk ^. walletId) rJoin

        it "Cannot update pass" $ \ctx -> do
            mnemonics <- mnemonicToText @15 . entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wk <- restoreWalletFromPubKey ctx pubKey

            -- cannot update pass
            let payload = updatePassPayload fixturePassphrase "new-wallet-passphrase"
            rup <- request @ApiWallet ctx
                (Link.putWalletPassphrase wk) Default payload
            expectResponseCode @IO HTTP.status403 rup
            expectErrorMessage (errMsg403NoRootKey $ wk ^. walletId) rup

    describe "HW_WALLETS_04 - Can manage HW wallet the same way as others" $ do
        it "Can update name" $ \ctx -> do
            mnemonics <- mnemonicToText @15 . entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wk <- restoreWalletFromPubKey ctx pubKey

            -- cannot update wallet name
            let newName = "new name"
            let payload = updateNamePayload newName
            rup <- request @ApiWallet ctx (Link.putWallet wk) Default payload
            expectResponseCode @IO HTTP.status200 rup

            rGet <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wk) Default Empty
            expectField
                (#name . #getApiT . #getWalletName)
                (`shouldBe` newName)
                rGet

        it "Can get tx fee" $ \ctx -> do
            (w, mnemonics) <- fixtureWalletWithMnemonics ctx
            let pubKey = pubKeyFromMnemonics mnemonics
            r <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
            expectResponseCode @IO HTTP.status204 r

            wSrc <- restoreWalletFromPubKey ctx pubKey
            wDest <- emptyWallet ctx

            addrs <- listAddresses ctx wDest
            let destination = (addrs !! 1) ^. #id
            let payload = Json [json|{
                    "payments": [{
                        "address": #{destination},
                        "amount": {
                            "quantity": 1,
                            "unit": "lovelace"
                        }
                    }]
                }|]

            rFee <- request @ApiFee ctx
                (Link.getTransactionFee @'Shelley wSrc) Default payload
            expectResponseCode @IO HTTP.status202 rFee

        it "Can delete" $ \ctx -> do
            mnemonics <- mnemonicToText @15 . entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wPub <- restoreWalletFromPubKey ctx pubKey
            r <- request @ApiWallet ctx
                (Link.deleteWallet @'Shelley wPub) Default Empty
            expectResponseCode @IO HTTP.status204 r

        it "Can see utxo" $ \ctx -> do
            mnemonics <- mnemonicToText @15 . entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wPub <- restoreWalletFromPubKey ctx pubKey
            rStat <- request @ApiUtxoStatistics ctx
                (Link.getUTxOsStatistics wPub) Default Empty
            expectResponseCode @IO HTTP.status200 rStat
            expectWalletUTxO [] (snd rStat)

        it "Can list addresses" $ \ctx -> do
            mnemonics <- mnemonicToText @15 . entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wPub <- restoreWalletFromPubKey ctx pubKey

            let g = fromIntegral $ getAddressPoolGap defaultAddressPoolGap
            r <- request @[ApiAddress n] ctx
                (Link.listAddresses wPub) Default Empty
            expectResponseCode @IO HTTP.status200 r
            expectListSize g r
            forM_ [0..(g-1)] $ \addrNum -> do
                expectListField addrNum (#state . #getApiT) (`shouldBe` Unused) r

        it "Can have address pool gap" $ \ctx -> do
            mnemonics <- mnemonicToText @15 . entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            let addrPoolGap = 55 --arbitraty but known
            let payloadRestore = Json [json| {
                    "name": #{restoredWalletName},
                    "account_public_key": #{pubKey},
                    "address_pool_gap": #{addrPoolGap}
                }|]
            rRestore <- request @ApiWallet ctx (Link.postWallet @'Shelley)
                    Default payloadRestore
            expectResponseCode @IO HTTP.status201 rRestore

            let wPub = getFromResponse id rRestore

            r <- request @[ApiAddress n] ctx
                (Link.listAddresses wPub) Default Empty
            expectResponseCode @IO HTTP.status200 r
            expectListSize addrPoolGap r
            forM_ [0..(addrPoolGap-1)] $ \addrNum -> do
                expectListField addrNum (#state . #getApiT) (`shouldBe` Unused) r

        it "Can list transactions" $ \ctx -> do
            mnemonics <- mnemonicToText @15 . entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wPub <- restoreWalletFromPubKey ctx pubKey

            rt <- request @([ApiTransaction n]) ctx
                (Link.listTransactions @'Shelley wPub) Default Empty
            expectResponseCode HTTP.status200 rt
            expectListSize 0 rt

        it "Can force resync" $ \ctx -> do
            mnemonics <- mnemonicToText @15 . entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wPub <- restoreWalletFromPubKey ctx pubKey

            let payload = Json [json|{ "epoch_number": 0, "slot_number": 0 }|]
            r <- request @ApiWallet ctx (Link.forceResyncWallet @'Shelley wPub)
                    Default payload
            expectResponseCode @IO HTTP.status204 r

        it "Can get coin selection" $ \ctx -> do
            (w, mnemonics) <- fixtureWalletWithMnemonics ctx
            let pubKey = pubKeyFromMnemonics mnemonics
            r <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
            expectResponseCode @IO HTTP.status204 r

            source <- restoreWalletFromPubKey ctx pubKey
            target <- emptyWallet ctx
            targetAddress : _ <- fmap (view #id) <$> listAddresses ctx target

            let amount = Quantity 1
            let payment = AddressAmount targetAddress amount
            selectCoins ctx source (payment :| []) >>= flip verify
                [ expectResponseCode HTTP.status200
                , expectField #inputs (`shouldSatisfy` (not . null))
                , expectField #outputs (`shouldSatisfy` ((> 1) . length))
                , expectField #outputs (`shouldSatisfy` (payment `elem`))
                ]

    describe "HW_WALLETS_05 - Wallet from pubKey is available" $ do
        it "Can get wallet" $ \ctx -> do
            mnemonics <- mnemonicToText @15 . entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            wPub <- restoreWalletFromPubKey ctx pubKey
            rGet <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wPub) Default Empty
            expectField
                (#name . #getApiT . #getWalletName)
                (`shouldBe` restoredWalletName)
                rGet

        it "Can list wallet" $ \ctx -> do
            mnemonics <- mnemonicToText @15 . entropyToMnemonic <$> genEntropy
            let pubKey = pubKeyFromMnemonics mnemonics
            _ <- restoreWalletFromPubKey ctx pubKey
            rl <- request @[ApiWallet] ctx
                (Link.listWallets @'Shelley) Default Empty
            expectListField 0
                (#name . #getApiT . #getWalletName)
                (`shouldBe` restoredWalletName)
                rl

        it "The same account and mnemonic wallet can live side-by-side" $ \ctx -> do
            mnemonics <- mnemonicToText @15 . entropyToMnemonic <$> genEntropy

            -- create mnemonic wallet
            let mnemonicWalletName = "Mnemonic wallet"
            let payldCrt = payloadWith mnemonicWalletName mnemonics
            r <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default payldCrt
            expectResponseCode @IO HTTP.status201 r

            -- create from account public key
            let accXPub = pubKeyFromMnemonics mnemonics
            _ <- restoreWalletFromPubKey ctx accXPub

            -- both wallets are available
            rl <- request @[ApiWallet] ctx
                (Link.listWallets @'Shelley) Default Empty
            verify rl
                [ expectListField 0
                    (#name . #getApiT . #getWalletName)
                    (`shouldBe` mnemonicWalletName)
                , expectListField 1
                    (#name . #getApiT . #getWalletName)
                    (`shouldBe` restoredWalletName)
                ]

 where
     pubKeyFromMnemonics :: [Text] -> Text
     pubKeyFromMnemonics mnemonics =
         T.decodeUtf8 $ serializeXPub $ publicKey
            $ deriveAccountPrivateKey mempty rootXPrv minBound
      where
          (Right seed) = fromMnemonic @'[15] mnemonics
          rootXPrv = generateKeyFromSeed (seed, Nothing) mempty

     restoredWalletName :: Text
     restoredWalletName = "Wallet from pub key"

     restoreWalletFromPubKey :: Context t -> Text -> IO ApiWallet
     restoreWalletFromPubKey ctx pubKey = do
         let payloadRestore = Json [json| {
                 "name": #{restoredWalletName},
                 "account_public_key": #{pubKey}
             }|]
         r <- request @ApiWallet ctx (Link.postWallet @'Shelley)
                Default payloadRestore
         expectResponseCode @IO HTTP.status201 r
         let wid = getFromResponse id r
         eventually "restoreWalletFromPubKey: wallet is 100% synced " $ do
             rg <- request @ApiWallet ctx
                 (Link.getWallet @'Shelley wid) Default Empty
             expectField (#state . #getApiT) (`shouldBe` Ready) rg
         return wid
