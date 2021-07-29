{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.CLI.Byron.Wallets
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiByronWallet, ApiUtxoStatistics, DecodeAddress )
import Cardano.Wallet.Primitive.AddressDerivation
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
    ( isJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import System.Command
    ( Exit (..), Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith, describe, it, runIO )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain, shouldNotBe, shouldSatisfy )
import Test.Integration.Framework.DSL
    ( Context (..)
    , MnemonicLength (..)
    , createWalletViaCLI
    , deleteWalletViaCLI
    , emptyIcarusWallet
    , emptyRandomWallet
    , eventually
    , expectCliField
    , expectCliListField
    , expectValidJSON
    , expectWalletUTxO
    , fixturePassphrase
    , genMnemonics
    , getWalletUtxoStatisticsViaCLI
    , getWalletViaCLI
    , listWalletsViaCLI
    , updateWalletNameViaCLI
    , updateWalletPassphraseViaCLI
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( arabicWalletName
    , cmdOk
    , errMsg400NumberOfWords
    , errMsg403WrongPass
    , errMsg404NoWallet
    , errMsg409WalletExists
    , russianWalletName
    , wildcardsWalletName
    )

import qualified Data.Text as T

spec :: forall n.
    ( DecodeAddress n
    ) => SpecWith Context
spec = describe "BYRON_CLI_WALLETS" $ do

    describe "CLI_BYRON_GET_04, CLI_BYRON_DELETE_01, BYRON_RESTORE_02, BYRON_RESTORE_03 -\
        \ Deleted wallet is not available, but can be restored" $ do
        let matrix = [ ("random", genMnemonics M12)
                     , ("icarus", genMnemonics M15)
                     ]
        forM_ matrix $ \(style, genM) -> it style $ \ctx -> runResourceT $ do
            mnemonic <- liftIO genM
            let args =
                    [ "Name of the wallet"
                    , "--wallet-style", style
                    ]
            --create
            (c, out, err) <- createWalletViaCLI @_ @IO ctx
                        args (unwords $ T.unpack <$> mnemonic)
                        "\n" "secure-passphrase"
            T.unpack err `shouldContain` cmdOk
            c `shouldBe` ExitSuccess
            j <- expectValidJSON (Proxy @ApiByronWallet) out
            let wid = T.unpack $ j ^. walletId
            --delete
            (Exit cd, Stdout outd, Stderr errd) <- deleteWalletViaCLI ctx wid
            outd`shouldBe` "\n"
            cd `shouldBe` ExitSuccess
            errd `shouldContain` cmdOk
            --not available
            (Exit c2, Stdout out2, Stderr err2) <- getWalletViaCLI ctx wid
            out2 `shouldBe` mempty
            c2 `shouldBe` ExitFailure 1
            err2 `shouldContain` errMsg404NoWallet (T.pack wid)
            --re-create
            (c3, out3, err3) <- createWalletViaCLI ctx
                        args (unwords $ T.unpack <$> mnemonic)
                        "\n" "secure-passphrase-restored"
            c3 `shouldBe` ExitSuccess
            T.unpack err3 `shouldContain` cmdOk
            jr <- expectValidJSON (Proxy @ApiByronWallet) out3
            verify jr [ expectCliField walletId (`shouldBe` T.pack wid) ]
            --re-create again? No!
            (c4, out4, err4) <- createWalletViaCLI ctx
                        args (unwords $ T.unpack <$> mnemonic)
                        "\n" "secure-passphrase-restored-again"
            c4 `shouldBe` ExitFailure 1
            T.unpack err4 `shouldContain` (errMsg409WalletExists wid)
            out4 `shouldBe` mempty

    describe "CLI_BYRON_RESTORE_01, CLI_BYRON_GET_01, CLI_BYRON_LIST_01 -\
        \Restore a wallet" $ do
        let scenarioSuccess style mnemonic ctx = runResourceT @IO $ do
                let name = "Name of the wallet"
                let args =
                        [ name
                        , "--wallet-style", style
                        ]
                let expectations =
                        [ expectCliField (#name . #getApiT . #getWalletName)
                            (`shouldBe` (T.pack name))
                        , expectCliField (#balance .  #available)
                            (`shouldBe` Quantity 0)
                        , expectCliField (#balance .  #total)
                            (`shouldBe` Quantity 0)
                        , expectCliField #passphrase (`shouldNotBe` Nothing)
                        ]
                -- create
                (c, out, err) <- createWalletViaCLI ctx
                            args (unwords $ T.unpack <$> mnemonic)
                            "\n" "secure-passphrase"
                T.unpack err `shouldContain` cmdOk
                c `shouldBe` ExitSuccess
                j <- expectValidJSON (Proxy @ApiByronWallet) out
                liftIO $ verify j expectations
                let wid = T.unpack $ j ^. walletId

                eventually "wallet is available and ready" $ do
                    -- get
                    (Exit c2, Stdout out2, Stderr err2) <- getWalletViaCLI ctx wid
                    c2 `shouldBe` ExitSuccess
                    err2 `shouldContain` cmdOk
                    jg <- expectValidJSON (Proxy @ApiByronWallet) out2
                    verify jg $
                        (expectCliField (#state . #getApiT) (`shouldBe` Ready)) : expectations
                    -- list
                    (Exit c3, Stdout out3, Stderr err3) <- listWalletsViaCLI ctx
                    c3 `shouldBe` ExitSuccess
                    err3 `shouldBe` cmdOk
                    jl <- expectValidJSON (Proxy @[ApiByronWallet]) out3
                    length jl `shouldBe` 1
                    expectCliListField 0 walletId (`shouldBe` T.pack wid) jl

        let scenarioFailure style mnemonic ctx = runResourceT @IO $ do
                let args =
                        [ "The wallet that didn't exist"
                        , "--wallet-style", style
                        ]
                (c, out, err) <- createWalletViaCLI ctx
                            args (unwords $ T.unpack <$> mnemonic)
                            "\n" "secure-passphrase"
                T.unpack err `shouldContain` errMsg400NumberOfWords
                c `shouldBe` ExitFailure 1
                out `shouldBe` mempty

        let it' style genMnemonicIO test = do
                mnemonic <- runIO genMnemonicIO
                flip it (test style mnemonic) $ unwords
                    [ style
                    , show (length mnemonic)
                    , "words"
                    ]

        it' "random" (genMnemonics M9)  scenarioFailure -- ❌
        it' "random" (genMnemonics M12) scenarioSuccess -- ✔️
        it' "random" (genMnemonics M15) scenarioSuccess -- ✔️
        it' "random" (genMnemonics M18) scenarioSuccess -- ✔️
        it' "random" (genMnemonics M21) scenarioSuccess -- ✔️
        it' "random" (genMnemonics M24) scenarioSuccess -- ✔️

        it' "icarus" (genMnemonics M9)  scenarioFailure -- ❌
        it' "icarus" (genMnemonics M12) scenarioSuccess -- ✔️
        it' "icarus" (genMnemonics M15) scenarioSuccess -- ✔️
        it' "icarus" (genMnemonics M18) scenarioSuccess -- ✔️
        it' "icarus" (genMnemonics M21) scenarioSuccess -- ✔️
        it' "icarus" (genMnemonics M24) scenarioSuccess -- ✔️

        it' "trezor" (genMnemonics M9)  scenarioFailure -- ❌
        it' "trezor" (genMnemonics M12) scenarioSuccess -- ✔️
        it' "trezor" (genMnemonics M15) scenarioSuccess -- ✔️
        it' "trezor" (genMnemonics M18) scenarioSuccess -- ✔️
        it' "trezor" (genMnemonics M21) scenarioSuccess -- ✔️
        it' "trezor" (genMnemonics M24) scenarioSuccess -- ✔️

        it' "ledger" (genMnemonics M9)  scenarioFailure -- ❌
        it' "ledger" (genMnemonics M12) scenarioSuccess -- ✔️
        it' "ledger" (genMnemonics M15) scenarioSuccess -- ✔️
        it' "ledger" (genMnemonics M18) scenarioSuccess -- ✔️
        it' "ledger" (genMnemonics M21) scenarioSuccess -- ✔️
        it' "ledger" (genMnemonics M24) scenarioSuccess -- ✔️

    describe "CLI_BYRON_RESTORE_06 - Passphrase" $ do
        let minLength = passphraseMinLength (Proxy @"user")
        let maxLength = passphraseMaxLength (Proxy @"user")
        let matrix =
                [ ( show minLength ++ " char long"
                  , T.pack (replicate minLength 'ź')
                  )
                , ( show maxLength ++ " char long"
                  , T.pack (replicate maxLength 'ą')
                  )
                , ( "Russian passphrase", russianWalletName )
                , ( "Arabic passphrase", arabicWalletName )
                , ( "Wildcards passphrase", wildcardsWalletName )
                ]
        forM_ matrix $ \(title, passphrase) -> it title $
            \ctx -> runResourceT @IO $ do
                let args =
                        [ "Name of the wallet"
                        , "--wallet-style", "random"
                        ]
                mnemonic <- liftIO $ genMnemonics M12
                (c, out, err) <- createWalletViaCLI ctx
                            args (unwords $ T.unpack <$> mnemonic)
                            "\n" (T.unpack passphrase)
                T.unpack err `shouldContain` cmdOk
                _ <- expectValidJSON (Proxy @ApiByronWallet) out
                c `shouldBe` ExitSuccess

    it "CLI_BYRON_UPDATE_NAME_01 - Update names of wallets" $ \ctx ->
        forM_ [ emptyRandomWallet, emptyIcarusWallet ] $
            \emptyByronWallet -> runResourceT @IO $ do
            wid <- fmap (T.unpack . view walletId) (emptyByronWallet ctx)
            let updatedName = "Name is updated"
            (Exit c, Stdout out, Stderr err) <-
                updateWalletNameViaCLI ctx [wid, updatedName]
            c `shouldBe` ExitSuccess
            err `shouldBe` cmdOk
            ju <- expectValidJSON (Proxy @ApiByronWallet) out
            expectCliField
                (#name . #getApiT . #getWalletName)
                (`shouldBe` T.pack updatedName) ju

    it "CLI_BYRON_UPDATE_NAME_02 - When updated name too long" $ \ctx ->
        forM_ [ emptyRandomWallet, emptyIcarusWallet ] $
            \emptyByronWallet -> runResourceT @IO $ do
            wid <- fmap (T.unpack . view walletId) (emptyByronWallet ctx)
            let updatedName = replicate 500 'o'
            (Exit c, Stdout out, Stderr err) <-
                updateWalletNameViaCLI ctx [wid, updatedName]
            c `shouldBe` ExitFailure 1
            err `shouldContain` "name is too long: expected at most 255 characters"
            out `shouldBe` mempty

    it "CLI_BYRON_UTXO_01 - Wallet's inactivity is reflected in utxo" $ \ctx ->
        forM_ [ emptyRandomWallet, emptyIcarusWallet ]
        $ \emptyByronWallet -> runResourceT @IO $ do
            wid <- fmap (T.unpack . view walletId) (emptyByronWallet ctx)
            (Exit c, Stdout o, Stderr e) <- getWalletUtxoStatisticsViaCLI ctx wid
            c `shouldBe` ExitSuccess
            e `shouldBe` cmdOk
            utxoStats <- expectValidJSON (Proxy @ApiUtxoStatistics) o
            expectWalletUTxO [] (Right utxoStats)

    it "CLI_BYRON_UPDATE_PASS_01 - change passphrase" $ \ctx ->
        forM_ [ emptyRandomWallet, emptyIcarusWallet ] $
        \emptyByronWallet -> runResourceT @IO $ do
            wid <- fmap (T.unpack . view walletId) (emptyByronWallet ctx)
            Stdout out <- getWalletViaCLI ctx wid
            expectValidJSON (Proxy @ApiByronWallet) out
                >>= flip verify [ expectCliField #passphrase (`shouldSatisfy` isJust) ]
            let oldPass = T.unpack fixturePassphrase
            let newPass = "cardano-wallet-new-pass"
            (c, o, e) <-
                updateWalletPassphraseViaCLI ctx wid oldPass newPass newPass
            c `shouldBe` ExitSuccess
            o `shouldBe` "\n"
            T.unpack e `shouldContain` cmdOk

    it "CLI_BYRON_UPDATE_PASS_02 - Old passphrase incorrect" $ \ctx ->
        forM_ [ emptyRandomWallet, emptyIcarusWallet ] $
        \emptyByronWallet -> runResourceT @IO $ do
            wid <- fmap (T.unpack . view walletId) (emptyByronWallet ctx)
            Stdout out <- getWalletViaCLI ctx wid
            expectValidJSON (Proxy @ApiByronWallet) out
                >>= flip verify [ expectCliField #passphrase (`shouldSatisfy` isJust) ]
            let oldPass = "incorrect-passphrase"
            let newPass = "cardano-wallet-new-pass"
            (c, o, e) <-
                updateWalletPassphraseViaCLI ctx wid oldPass newPass newPass
            c `shouldBe` ExitFailure 1
            o `shouldBe` mempty
            T.unpack e `shouldContain` errMsg403WrongPass

    describe "CLI_BYRON_UPDATE_PASS_03 - Pass length incorrect" $ do
        let minLength = passphraseMinLength (Proxy @"user")
        let maxLength = passphraseMaxLength (Proxy @"user")
        let passTooShort = replicate (minLength - 1) 'o'
        let errMsgTooShort = "passphrase is too short: expected at least 10 characters"
        let passTooLong = replicate (maxLength + 1) 'o'
        let errMsgTooLong = "passphrase is too long: expected at most 255 characters"
        let passOK = T.unpack fixturePassphrase

        let matrix = [ ("old pass too short", passTooShort, passOK, errMsgTooShort)
                     , ("old pass too long", passTooLong, passOK, errMsgTooLong)
                     , ("new pass too short", passOK, passTooShort, errMsgTooShort)
                     , ("new pass too long", passOK, passTooLong, errMsgTooLong)
                     ]
        forM_ matrix $ \(title, oldPass, newPass, errMsg) -> it title $ \ctx -> runResourceT @IO $ do
            forM_ [ emptyRandomWallet, emptyIcarusWallet ] $ \emptyByronWallet -> do
                wid <- fmap (T.unpack . view walletId) (emptyByronWallet ctx)
                Stdout out <- getWalletViaCLI ctx wid
                expectValidJSON (Proxy @ApiByronWallet) out
                    >>= flip verify [ expectCliField #passphrase (`shouldSatisfy` isJust) ]
                (c, o, e) <-
                    updateWalletPassphraseViaCLI ctx wid oldPass newPass newPass
                T.unpack e `shouldContain` errMsg
                c `shouldBe` ExitFailure 1
                o `shouldBe` mempty
