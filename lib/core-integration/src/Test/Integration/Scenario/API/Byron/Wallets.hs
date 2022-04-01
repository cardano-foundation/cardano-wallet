{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.API.Byron.Wallets
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiByronWallet
    , ApiUtxoStatistics
    , ApiWalletDiscovery (..)
    , ApiWalletUtxoSnapshot
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( PaymentAddress )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.Passphrase
    ( PassphraseMaxLength (..), PassphraseMinLength (..) )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..) )
import Control.Monad
    ( forM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Resource
    ( runResourceT )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Maybe
    ( isJust, isNothing )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Test.Hspec
    ( SpecWith, describe, runIO )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldNotBe, shouldSatisfy )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , MnemonicLength (..)
    , Payload (..)
    , emptyByronWalletFromXPrvWith
    , emptyByronWalletWith
    , emptyIcarusWallet
    , emptyRandomWallet
    , emptyRandomWalletWithPasswd
    , eventually
    , expectErrorMessage
    , expectField
    , expectListField
    , expectListSize
    , expectResponseCode
    , expectWalletUTxO
    , fixtureIcarusWallet
    , fixtureMultiAssetIcarusWallet
    , fixtureMultiAssetRandomWallet
    , fixturePassphrase
    , fixturePassphraseEncrypted
    , fixtureRandomWallet
    , genMnemonics
    , getFromResponse
    , json
    , listFilteredByronWallets
    , postByronWallet
    , request
    , rootPrvKeyFromMnemonics
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( arabicWalletName
    , errMsg400NumberOfWords
    , errMsg403WrongPass
    , errMsg404NoWallet
    , kanjiWalletName
    , polishWalletName
    , russianWalletName
    , updateEmptyPassPayload
    , updatePassPayload
    , wildcardsWalletName
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall n.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    , PaymentAddress n IcarusKey
    , PaymentAddress n ByronKey
    ) => SpecWith Context
spec = describe "BYRON_WALLETS" $ do
    it "BYRON_GET_04, DELETE_01 - Deleted wallet is not available" $ \ctx -> runResourceT $ do
        w <- emptyRandomWallet ctx
        _ <- request @ApiByronWallet ctx (Link.deleteWallet @'Byron w) Default Empty
        rg <- request @ApiByronWallet ctx (Link.getWallet @'Byron w) Default Empty
        expectResponseCode HTTP.status404 rg
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) rg

    it "BYRON_LIST_01 - Byron Wallets are listed from oldest to newest" $
        \ctx -> runResourceT $ do
            m1 <- liftIO $ genMnemonics M12
            m2 <- liftIO $ genMnemonics M12
            m3 <- liftIO $ genMnemonics M12
            r1 <- emptyByronWalletWith ctx "random" ("b1", m1, fixturePassphrase)
            r2 <- emptyByronWalletWith ctx "random" ("b2", m2, fixturePassphrase)
            r3 <- emptyByronWalletWith ctx "random" ("b3", m3, fixturePassphrase)

            let wids = Set.fromList $ map (view walletId) [r1,r2,r3]
            rl <- listFilteredByronWallets wids ctx
            verify rl
                [ expectResponseCode HTTP.status200
                , expectListSize 3
                , expectListField 0
                        (#name . #getApiT . #getWalletName) (`shouldBe` "b1")
                , expectListField 1
                        (#name . #getApiT . #getWalletName) (`shouldBe` "b2")
                , expectListField 2
                        (#name . #getApiT . #getWalletName) (`shouldBe` "b3")
                ]

    it "BYRON_LIST_01 - Interleave of Icarus and Random wallets" $ \ctx -> runResourceT $ do
        let pwd = fixturePassphrase
        r1 <- liftIO (genMnemonics M15) >>= \m -> (emptyByronWalletWith ctx "icarus" ("ica1", m, pwd))
        r2 <- liftIO (genMnemonics M12) >>= \m -> (emptyByronWalletWith ctx "random" ("rnd2", m, pwd))
        r3 <- liftIO (genMnemonics M15) >>= \m -> (emptyByronWalletWith ctx "icarus" ("ica3", m, pwd))
        let wids = Set.fromList $ map (view walletId) [r1,r2,r3]
        rl <- listFilteredByronWallets wids ctx
        verify rl
            [ expectResponseCode HTTP.status200
            , expectListSize 3
            , expectListField 0
                (#name . #getApiT . #getWalletName) (`shouldBe` "ica1")
            , expectListField 1
                (#name . #getApiT . #getWalletName) (`shouldBe` "rnd2")
            , expectListField 2
                (#name . #getApiT . #getWalletName) (`shouldBe` "ica3")
            ]

    describe "BYRON_RESTORE_01, GET_01, LIST_01 - Restore a wallet" $ do
        let scenarioSuccess style mnemonic ctx = runResourceT $ do
                let name = "Empty Byron Wallet"
                let payload = Json [json| {
                        "name": #{name},
                        "mnemonic_sentence": #{mnemonic},
                        "passphrase": #{fixturePassphrase},
                        "style": #{style}
                    }|]
                let discovery =
                        if style == "random"
                        then DiscoveryRandom
                        else DiscoverySequential
                let expectations =
                            [ expectField (#name . #getApiT . #getWalletName) (`shouldBe` name)
                            , expectField (#balance . #available) (`shouldBe` Quantity 0)
                            , expectField (#balance . #total) (`shouldBe` Quantity 0)
                            , expectField #passphrase (`shouldNotBe` Nothing)
                            , expectField #discovery (`shouldBe` discovery)
                            ]
                -- create
                r <- postByronWallet ctx payload
                liftIO $ verify r expectations
                let w = getFromResponse id r

                eventually "wallet is available and ready" $ do
                    -- get
                    rg <- request @ApiByronWallet ctx
                        (Link.getWallet @'Byron w) Default Empty
                    liftIO $ verify rg $
                        (expectField (#state . #getApiT) (`shouldBe` Ready)) : expectations
                    -- list
                    let wid = getFromResponse walletId rg
                    rl <- listFilteredByronWallets (Set.singleton wid) ctx
                    verify rl
                        [ expectResponseCode HTTP.status200
                        , expectListSize 1
                        , expectListField 0
                                (#name . #getApiT . #getWalletName) (`shouldBe` name)
                        , expectListField 0
                                (#balance . #available) (`shouldBe` Quantity 0)
                        , expectListField 0
                                (#state . #getApiT) (`shouldBe` Ready)
                        , expectListField 0
                                (#balance . #total) (`shouldBe` Quantity 0)
                        ]

        let scenarioFailure style mnemonic ctx = runResourceT $ do
                let payload = Json [json| {
                        "name": "Empty Byron Wallet",
                        "mnemonic_sentence": #{mnemonic},
                        "passphrase": #{fixturePassphrase},
                        "style": #{style}
                    }|]
                r <- postByronWallet ctx payload
                verify r
                    [ expectResponseCode HTTP.status400
                    , expectErrorMessage errMsg400NumberOfWords
                    ]

        let it' style genMnemonicIO test = do
                mnemonic <- runIO genMnemonicIO
                flip it (test style mnemonic) $ unwords
                    [ style
                    , show (length mnemonic)
                    , "words"
                    ]

        it' "random" (liftIO $ genMnemonics M9)  scenarioFailure -- ❌
        it' "random" (liftIO $ genMnemonics M12) scenarioSuccess -- ✔️
        it' "random" (liftIO $ genMnemonics M15) scenarioSuccess -- ✔️
        it' "random" (liftIO $ genMnemonics M18) scenarioSuccess -- ✔️
        it' "random" (liftIO $ genMnemonics M21) scenarioSuccess -- ✔️
        it' "random" (liftIO $ genMnemonics M24) scenarioSuccess -- ✔️

        it' "icarus" (liftIO $ genMnemonics M9)  scenarioFailure -- ❌
        it' "icarus" (liftIO $ genMnemonics M12) scenarioSuccess -- ✔️
        it' "icarus" (liftIO $ genMnemonics M15) scenarioSuccess -- ✔️
        it' "icarus" (liftIO $ genMnemonics M18) scenarioSuccess -- ✔️
        it' "icarus" (liftIO $ genMnemonics M21) scenarioSuccess -- ✔️
        it' "icarus" (liftIO $ genMnemonics M24) scenarioSuccess -- ✔️

        it' "trezor" (liftIO $ genMnemonics M9)  scenarioFailure -- ❌
        it' "trezor" (liftIO $ genMnemonics M12) scenarioSuccess -- ✔️
        it' "trezor" (liftIO $ genMnemonics M15) scenarioSuccess -- ✔️
        it' "trezor" (liftIO $ genMnemonics M18) scenarioSuccess -- ✔️
        it' "trezor" (liftIO $ genMnemonics M21) scenarioSuccess -- ✔️
        it' "trezor" (liftIO $ genMnemonics M24) scenarioSuccess -- ✔️

        it' "ledger" (liftIO $ genMnemonics M9)  scenarioFailure -- ❌
        it' "ledger" (liftIO $ genMnemonics M12) scenarioSuccess -- ✔️
        it' "ledger" (liftIO $ genMnemonics M15) scenarioSuccess -- ✔️
        it' "ledger" (liftIO $ genMnemonics M18) scenarioSuccess -- ✔️
        it' "ledger" (liftIO $ genMnemonics M21) scenarioSuccess -- ✔️
        it' "ledger" (liftIO $ genMnemonics M24) scenarioSuccess -- ✔️

    it "BYRON_RESTORE_02 - One can restore previously deleted wallet" $
        \ctx -> runResourceT $ do
            m <- liftIO $ genMnemonics M12
            w <- emptyByronWalletWith ctx "random"
                ("Byron Wallet", m, fixturePassphrase)
            rd <- request
                @ApiByronWallet ctx (Link.deleteWallet @'Byron w) Default Empty
            expectResponseCode HTTP.status204 rd
            wr <- emptyByronWalletWith ctx "random"
                ("Byron Wallet2", m, "Secure Pa33phrase")
            w ^. walletId `shouldBe` wr ^. walletId

    it "BYRON_RESTORE_03 - Cannot restore wallet that exists" $ \ctx -> runResourceT $ do
        mnemonic <- liftIO $ genMnemonics M12
        let payload = Json [json| {
                "name": "Some Byron Wallet",
                "mnemonic_sentence": #{mnemonic},
                "passphrase": #{fixturePassphrase},
                "style": "random"
                } |]
        r1 <- postByronWallet ctx payload
        expectResponseCode HTTP.status201 r1

        r2 <- postByronWallet ctx payload
        verify r2
            [ expectResponseCode HTTP.status409
            , expectErrorMessage ("This operation would yield a wallet with the\
                 \ following id: " ++ T.unpack (getFromResponse walletId r1) ++
                 " However, I already know of a wallet with this id.")
            ]

    describe "BYRON_RESTORE_06 - Passphrase" $ do
        let minLength = passphraseMinLength (Proxy @"user")
        let maxLength = passphraseMaxLength (Proxy @"user")
        let matrix =
                [ ( show minLength ++ " char long"
                  , T.pack (replicate minLength 'ź')
                  , [ expectResponseCode HTTP.status201 ]
                  )
                , ( show maxLength ++ " char long"
                  , T.pack (replicate maxLength 'ą')
                  , [ expectResponseCode HTTP.status201 ]
                  )
                , ( "Russian passphrase", russianWalletName
                  , [ expectResponseCode HTTP.status201 ]
                  )
                , ( "Polish passphrase", polishWalletName
                  , [ expectResponseCode HTTP.status201 ]
                  )
                , ( "Kanji passphrase", kanjiWalletName
                  , [ expectResponseCode HTTP.status201 ]
                  )
                , ( "Arabic passphrase", arabicWalletName
                  , [ expectResponseCode HTTP.status201 ]
                  )
                , ( "Wildcards passphrase", wildcardsWalletName
                  , [ expectResponseCode HTTP.status201 ]
                  )
                ]
        forM_ matrix $ \(title, passphrase, expectations) -> it title $
            \ctx -> runResourceT $ do
                mnemonics12 <- liftIO $ genMnemonics M12
                let payload = Json [json| {
                        "name": "Secure Wallet",
                        "mnemonic_sentence": #{mnemonics12},
                        "passphrase": #{passphrase},
                        "style": "random"
                        } |]
                r <- postByronWallet ctx payload
                verify r expectations

    it "BYRON_UPDATE_NAME_01 - Update names of wallets" $ \ctx ->
        forM_ [ (emptyRandomWallet ctx, "Random Wallet")
              , (emptyIcarusWallet ctx, "Icarus Wallet")
              ] $
        \(emptyByronWallet, wName) -> runResourceT $ do
            w <- emptyByronWallet
            r1 <- request @ApiByronWallet ctx
                  (Link.getWallet @'Byron w) Default Empty
            verify r1
                [ expectField (#name . #getApiT . #getWalletName) (`shouldBe` wName) ]
            let updatedName = "new wallet 1"
            let payload = Json [json| {
                    "name": #{updatedName}
                    } |]
            r2 <- request @ApiByronWallet ctx
                  (Link.putWallet @'Byron w) Default payload
            verify r2
                [ expectResponseCode HTTP.status200
                , expectField (#name . #getApiT . #getWalletName) (`shouldBe` updatedName)
                ]

    it "BYRON_UPDATE_NAME_02 - Update names of wallets from Xprv" $ \ctx -> runResourceT $ do
        -- Wallet from XPRV
        let wName = "Byron Wallet from XPRV"
        mnemonics <- liftIO $ genMnemonics M12
        let rootXPrv = rootPrvKeyFromMnemonics mnemonics fixturePassphrase
        w <- emptyByronWalletFromXPrvWith ctx "random"
            (wName, rootXPrv, fixturePassphraseEncrypted)

        r1 <- request @ApiByronWallet ctx
              (Link.getWallet @'Byron w) Default Empty
        verify r1
            [ expectField (#name . #getApiT . #getWalletName) (`shouldBe` wName) ]

        -- verify you can update name
        let updatedName = "new wallet 1"
        let payload = Json [json| {
                "name": #{updatedName}
                } |]
        r2 <- request @ApiByronWallet ctx
              (Link.putWallet @'Byron w) Default payload
        verify r2
            [ expectResponseCode HTTP.status200
            , expectField (#name . #getApiT . #getWalletName) (`shouldBe` updatedName)
            ]

    it "BYRON_UTXO_01 - Wallet's inactivity is reflected in utxo" $ \ctx ->
        forM_ [ emptyRandomWallet, emptyIcarusWallet ] $ \emptyByronWallet -> runResourceT $ do
        w <- emptyByronWallet ctx
        rStat <- request @ApiUtxoStatistics ctx
                 (Link.getUTxOsStatistics @'Byron w) Default Empty
        expectResponseCode HTTP.status200 rStat
        expectWalletUTxO [] (snd rStat)

    it "BYRON_WALLET_UTXO_SNAPSHOT_01 - \
        \Can generate UTxO snapshot of empty wallet" $
        \ctx -> do
            let emptyByronWallets =
                  [ emptyRandomWallet
                  , emptyIcarusWallet
                  ]
            forM_ emptyByronWallets $ \emptyByronWallet -> runResourceT $ do
                w <- emptyByronWallet ctx
                rSnap <- request @ApiWalletUtxoSnapshot ctx
                  (Link.getWalletUtxoSnapshot @'Byron w) Default Empty
                expectResponseCode HTTP.status200 rSnap
                expectField #entries (`shouldBe` []) rSnap

    it "BYRON_WALLET_UTXO_SNAPSHOT_02 - \
        \Can generate UTxO snapshot of pure-ada wallet" $
        \ctx -> do
            let fixtureWallets =
                  [ fixtureRandomWallet
                  , fixtureIcarusWallet
                  ]
            forM_ fixtureWallets $ \fixtureWallet -> runResourceT $ do
                w <- fixtureWallet ctx
                rSnap <- request @ApiWalletUtxoSnapshot ctx
                    (Link.getWalletUtxoSnapshot @'Byron w) Default Empty
                expectResponseCode HTTP.status200 rSnap
                let entries = getFromResponse #entries rSnap
                length entries `shouldBe` 10

    it "BYRON_WALLET_UTXO_SNAPSHOT_03 - \
        \Can generate UTxO snapshot of multi-asset wallet" $
        \ctx -> do
            let fixtureWallets =
                    [ fixtureMultiAssetRandomWallet @n
                    , fixtureMultiAssetIcarusWallet @n
                    ]
            forM_ fixtureWallets $ \fixtureWallet -> runResourceT $ do
                w <- fixtureWallet ctx
                rSnap <- request @ApiWalletUtxoSnapshot ctx
                    (Link.getWalletUtxoSnapshot @'Byron w) Default Empty
                expectResponseCode HTTP.status200 rSnap
                let entries = getFromResponse #entries rSnap
                length entries `shouldBe` 11

    it "BYRON_UPDATE_PASS_01 - change passphrase" $ \ctx ->
        forM_ [ emptyRandomWallet, emptyIcarusWallet ] $ \emptyByronWallet -> runResourceT $ do
        w <- emptyByronWallet ctx
        request @ApiByronWallet ctx (Link.getWallet @'Byron w) Default Empty
            >>= flip verify [ expectField #passphrase (`shouldSatisfy` isJust) ]

        let payload = updatePassPayload fixturePassphrase "New Secure Passphrase"
        r <- request @ApiByronWallet ctx
            (Link.putWalletPassphrase @'Byron w) Default payload
        verify r
            [ expectResponseCode HTTP.status204
            ]

    it "BYRON_UPDATE_PASS_02 - Old passphrase incorrect" $ \ctx ->
        forM_ [ emptyRandomWallet, emptyIcarusWallet ] $ \emptyByronWallet -> runResourceT $ do
        w <- emptyByronWallet ctx
        request @ApiByronWallet ctx (Link.getWallet @'Byron w) Default Empty
            >>= flip verify [ expectField #passphrase (`shouldSatisfy` isJust) ]
        let payload = updatePassPayload "incorrect-passphrase" "whatever-pass"
        r <- request @ApiByronWallet ctx
            (Link.putWalletPassphrase @'Byron w) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403WrongPass
            ]

    it "BYRON_UPDATE_PASS_03 - Updating passphrase with no password wallets" $ \ctx -> runResourceT $ do
        w <- emptyRandomWalletWithPasswd ctx ""
        request @ApiByronWallet ctx (Link.getWallet @'Byron w) Default Empty
            >>= flip verify [ expectField #passphrase (`shouldSatisfy` isNothing) ]
        let payload = updateEmptyPassPayload "correct-password"
        r <- request @ApiByronWallet ctx
            (Link.putWalletPassphrase @'Byron w) Default payload
        verify r
            [ expectResponseCode HTTP.status204
            ]

    it "BYRON_UPDATE_PASS_04a - Updating passphrase with no password wallets" $ \ctx -> runResourceT $ do
        w <- emptyRandomWalletWithPasswd ctx ""
        request @ApiByronWallet ctx (Link.getWallet @'Byron w) Default Empty
            >>= flip verify [ expectField #passphrase (`shouldSatisfy` isNothing) ]
        let payload = updatePassPayload "" "correct-password"
        r <- request @ApiByronWallet ctx
            (Link.putWalletPassphrase @'Byron w) Default payload
        verify r
            [ expectResponseCode HTTP.status204
            ]

    it "BYRON_UPDATE_PASS_04b - Regression test" $ \ctx -> runResourceT $ do
        let key = "38e8de9c583441213fe34eecc4e28265267466877ba4048e3ab1fa99563\
                  \66947aefaf5ba9779db67eead7fc9cd1354b994a5d8d9cd40ab874bfeb1\
                  \b33649280cd33651377731e0e59e0233425a55257782c5adaa768da0567\
                  \f43c1c6c0c18766ed0a547bb34eb472c120b170a8640279832ddf180028\
                  \87f03c15dea59705422d"
        let pwd = "31347c387c317c574342652b796362417576356c2b4258676a344a314c6\
                  \343675375414c2f5653393661364e576a2b7550766655513d3d7c6f7846\
                  \36654939734151444e6f38395147747366324e653937426338372b484b6\
                  \b4137756772752f5970673d"
        w <- emptyByronWalletFromXPrvWith ctx "random" ("Random Wallet", key, pwd)
        let payload = updatePassPayload "" "correct-password"
        r <- request @ApiByronWallet ctx
            (Link.putWalletPassphrase @'Byron w) Default payload
        verify r
            [ expectResponseCode HTTP.status204
            ]

    it "BYRON_UPDATE_PASS_07 - Updating passphrase with short password wallets" $ \ctx -> runResourceT $ do
        w <- emptyRandomWalletWithPasswd ctx "cos"
        request @ApiByronWallet ctx (Link.getWallet @'Byron w) Default Empty
            >>= flip verify [ expectField #passphrase (`shouldSatisfy` isJust) ]
        let payload = updatePassPayload "cos" "correct-password"
        r <- request @ApiByronWallet ctx
            (Link.putWalletPassphrase @'Byron w) Default payload
        verify r
            [ expectResponseCode HTTP.status204
            ]
