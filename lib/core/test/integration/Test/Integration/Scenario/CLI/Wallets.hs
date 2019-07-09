{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.CLI.Wallets
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiTransaction, ApiWallet, getApiT )
import Cardano.Wallet.Primitive.Types
    ( DecodeAddress (..)
    , EncodeAddress (..)
    , WalletDelegation (..)
    , WalletState (..)
    , encodeAddress
    , walletNameMaxLength
    , walletNameMinLength
    )
import Control.Monad
    ( forM_ )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import System.Command
    ( Exit (..), Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain, shouldNotContain )
import Test.Integration.Framework.DSL
    ( Context (..)
    , KnownCommand
    , addressPoolGap
    , balanceAvailable
    , balanceTotal
    , createWalletViaCLI
    , delegation
    , deleteWalletViaCLI
    , emptyWallet
    , emptyWalletWith
    , expectCliFieldEqual
    , expectCliFieldNotEqual
    , expectCliListItemFieldEqual
    , expectEventually'
    , expectValidJSON
    , fixtureWallet
    , generateMnemonicsViaCLI
    , getWalletViaCLI
    , listAddresses
    , listWalletsViaCLI
    , passphraseLastUpdate
    , postTransactionViaCLI
    , state
    , updateWalletNameViaCLI
    , verify
    , walletId
    , walletName
    )
import Test.Integration.Framework.TestData
    ( addressPoolGapMax
    , addressPoolGapMin
    , cmdOk
    , falseWalletIds
    , passphraseMaxLength
    , passphraseMinLength
    )

import qualified Data.Text as T

spec
    :: forall t. (EncodeAddress t, DecodeAddress t, KnownCommand t)
    => SpecWith (Context t)
spec = do
    it "WALLETS_CREATE_01,08 - Can create a wallet" $ \ctx -> do
        Stdout m <- generateMnemonicsViaCLI @t []
        (c, out, err) <- createWalletViaCLI @t ctx ["n"] m "\n" "secure-passphrase"
        c `shouldBe` ExitSuccess
        T.unpack err `shouldContain` cmdOk
        j <- expectValidJSON (Proxy @ApiWallet) out
        verify j
            [ expectCliFieldEqual walletName "n"
            , expectCliFieldEqual addressPoolGap 20
            , expectCliFieldEqual balanceAvailable 0
            , expectCliFieldEqual balanceTotal 0
            , expectEventually' ctx state Ready
            , expectCliFieldEqual delegation (NotDelegating)
            , expectCliFieldNotEqual passphraseLastUpdate Nothing
            ]

    it "WALLETS_CREATE_02 - Restored wallet preserves funds" $ \ctx -> do
        wSrc <- fixtureWallet ctx

        -- create a wallet
        Stdout m <- generateMnemonicsViaCLI @t []
        (c1, o1, e1) <- createWalletViaCLI @t ctx ["n"] m "\n" "secure-passphrase"
        c1 `shouldBe` ExitSuccess
        T.unpack e1 `shouldContain` cmdOk
        wDest <- expectValidJSON (Proxy @ApiWallet) o1
        verify wDest
            [ expectCliFieldEqual balanceAvailable 0
            , expectCliFieldEqual balanceTotal 0
            ]

        --send transaction to the wallet
        let amount = 11
        addrs:_ <- listAddresses ctx wDest
        let addr =
                encodeAddress (Proxy @t) (getApiT $ fst $ addrs ^. #id)
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", T.pack (show amount) <> "@" <> addr
                ]

        (cp, op, ep) <- postTransactionViaCLI @t ctx "cardano-wallet" args
        T.unpack ep `shouldContain` cmdOk
        _ <- expectValidJSON (Proxy @(ApiTransaction t)) op
        cp `shouldBe` ExitSuccess

        expectEventually' ctx balanceAvailable amount wDest
        expectEventually' ctx balanceTotal amount wDest

        -- delete wallet
        Exit cd <- deleteWalletViaCLI @t ctx $ T.unpack (wDest ^. walletId)
        cd `shouldBe` ExitSuccess

        -- restore wallet
        (c2, o2, e2) <- createWalletViaCLI @t ctx ["n"] m "\n" "secure-passphraseX"
        c2 `shouldBe` ExitSuccess
        T.unpack e2 `shouldContain` cmdOk
        wRestored <- expectValidJSON (Proxy @ApiWallet) o2
        verify wRestored
            [ expectEventually' ctx state Ready
            , expectCliFieldEqual walletId (wDest ^. walletId)
            ]

        -- make sure funds are there
        Stdout o3 <- getWalletViaCLI @t ctx $ T.unpack (wRestored ^. walletId)
        justRestored <- expectValidJSON (Proxy @ApiWallet) o3
        verify justRestored
            [ expectCliFieldEqual balanceAvailable amount
            , expectCliFieldEqual balanceTotal amount
            , expectCliFieldEqual walletId (wDest ^. walletId)
            ]

    it "WALLETS_CREATE_03 - Cannot create wallet that exists" $ \ctx -> do
        Stdout m1 <- generateMnemonicsViaCLI @t ["--size", "24"]
        Stdout m2 <- generateMnemonicsViaCLI @t ["--size", "12"]

        (c1, o1, e1) <- createWalletViaCLI @t ctx ["n1"] m1 m2 "secure-passphrase"
        c1 `shouldBe` ExitSuccess
        T.unpack e1 `shouldContain` cmdOk
        j <- expectValidJSON (Proxy @ApiWallet) o1
        expectCliFieldEqual walletName "n1" j

        (c2, o2, e2) <- createWalletViaCLI @t ctx ["n2"] m1 m2 "Xsecure-passphraseX"
        c2 `shouldBe` ExitFailure 1
        o2 `shouldBe` ""
        T.unpack e2 `shouldContain` "This operation would yield a wallet with \
            \the following id: " ++ T.unpack (j ^. walletId) ++ " \
            \However, I already know of a wallet with this id."

    describe "WALLETS_CREATE_04 - Wallet names" $ do
        forM_ walletNames $ \(title, n) -> it title $ \ctx -> do
            Stdout m <- generateMnemonicsViaCLI @t ["--size", "18"]
            (c, o, e) <- createWalletViaCLI @t ctx [n] m "\n" "secure-passphrase"
            c `shouldBe` ExitSuccess
            T.unpack e `shouldContain` cmdOk
            j <- expectValidJSON (Proxy @ApiWallet) o
            expectCliFieldEqual walletName (T.pack n) j

    it "WALLETS_CREATE_04 - Cannot create wallet when name exceeds length" $ \ctx -> do
        let n = replicate (walletNameMaxLength + 1) 'ą'
        Stdout m <- generateMnemonicsViaCLI @t ["--size", "18"]

        (c, o, e) <- createWalletViaCLI @t ctx [n] m "\n" "secure-passphrase"
        c `shouldBe` ExitFailure 1
        T.unpack e `shouldContain`
            "name is too long: expected at most 255 characters"
        o `shouldBe` ""

    describe "WALLETS_CREATE_05 - Can create wallet with different mnemonic sizes" $ do
        forM_ ["15", "18", "21", "24"] $ \(size) -> it size $ \ctx -> do
            let name = "Wallet created via CLI "
            Stdout mnemonics <- generateMnemonicsViaCLI @t ["--size", size]
            let pwd = "Secure passphrase"
            (c, out, err) <-
                createWalletViaCLI @t ctx [name] mnemonics "\n" pwd
            c `shouldBe` ExitSuccess
            T.unpack err `shouldContain` cmdOk
            j <- expectValidJSON (Proxy @ApiWallet) out
            expectCliFieldEqual walletName (T.pack name) j

    describe "WALLETS_CREATE_05 - Can't create wallet with wrong size of mnemonic" $ do
        forM_ ["9", "12"] $ \(size) -> it size $ \ctx -> do
            let name = "Wallet created via CLI"
            Stdout m1 <- generateMnemonicsViaCLI @t ["--size", size]
            Stdout m2 <- generateMnemonicsViaCLI @t ["--size", size]
            let pwd = "Secure passphrase"
            (c, out, err) <-
                createWalletViaCLI @t ctx [name] m1 m2 pwd
            c `shouldBe` (ExitFailure 1)
            out `shouldBe` ""
            T.unpack err `shouldContain` "Invalid number of words: 15, 18, 21\
                \ or 24 words are expected."
            (Stdout o, Stderr e) <- listWalletsViaCLI @t ctx
            o `shouldBe` "[]\n"
            e `shouldBe` cmdOk

    describe "WALLETS_CREATE_06 - Can create wallet with different mnemonic snd factor sizes" $ do
        forM_ ["9", "12"] $ \(size) -> it size $ \ctx -> do
            let name = "Wallet created via CLI"
            Stdout m1 <- generateMnemonicsViaCLI @t []
            Stdout m2 <- generateMnemonicsViaCLI @t ["--size", size]
            let pwd = "Secure passphrase"
            (c, out, err) <- createWalletViaCLI @t ctx [name] m1 m2 pwd
            c `shouldBe` ExitSuccess
            T.unpack err `shouldContain` cmdOk
            j <- expectValidJSON (Proxy @ApiWallet) out
            expectCliFieldEqual walletName (T.pack name) j

    describe "WALLETS_CREATE_06 - Can't create wallet with wrong size of mnemonic snd factor" $ do
        forM_ ["15", "18", "21", "24"] $ \(size) -> it size $ \ctx -> do
            let name = "Wallet created via CLI"
            Stdout m1 <- generateMnemonicsViaCLI @t ["--size", size]
            Stdout m2 <- generateMnemonicsViaCLI @t ["--size", size]
            let pwd = "Secure passphrase"
            (c, out, err) <- createWalletViaCLI @t ctx [name] m1 m2 pwd
            c `shouldBe` (ExitFailure 1)
            out `shouldBe` ""
            T.unpack err `shouldContain` "Invalid number of words: 9 or 12\
                \ words are expected."
            (Stdout o, Stderr e) <- listWalletsViaCLI @t ctx
            o `shouldBe` "[]\n"
            e `shouldBe` cmdOk

    describe "WALLETS_CREATE_07 - Passphrase is valid" $ do
        let passphraseMax = replicate passphraseMaxLength 'ą'
        let passBelowMax = replicate ( passphraseMaxLength - 1 ) 'ć'
        let passphraseMin = replicate passphraseMinLength 'ń'
        let passAboveMin = replicate ( passphraseMinLength + 1 ) 'ę'
        let matrix =
                [ ( "Pass min", passphraseMin )
                , ( "Pass max", passphraseMax )
                , ( "Pass max - 1", passBelowMax )
                , ( "Pass min + 1", passAboveMin )
                , ( "Russian", "АаБбВвГгДдЕеЁёЖжЗз")
                , ( "Polish", "aąbcćdeęfghijklłmnoóp" )
                , ( "Kanji", "亜哀挨愛曖悪握圧扱宛嵐")
                ]
        forM_ matrix $ \(title, pass) -> it title $ \ctx -> do
            Stdout m <- generateMnemonicsViaCLI @t []
            (c, o, e) <- createWalletViaCLI @t ctx ["Wallet name"] m "\n" pass
            c `shouldBe` ExitSuccess
            T.unpack e `shouldContain` cmdOk
            j <- expectValidJSON (Proxy @ApiWallet) o
            expectCliFieldNotEqual passphraseLastUpdate Nothing j

    describe "WALLETS_CREATE_07 - When passphrase is invalid" $ do
        let pasAboveMax = replicate (passphraseMaxLength + 1) 'ą'
        let passBelowMin = replicate (passphraseMinLength - 1) 'ń'
        let passMinWarn = "passphrase is too short: expected at \
            \least " ++ show passphraseMinLength ++ " characters"
        let passMaxWarn = "passphrase is too long: expected at \
            \most " ++ show passphraseMaxLength ++ " characters"

        let matrix =
                [ ( "Pass below min length", passBelowMin, passMinWarn )
                , ( "Pass above max length", pasAboveMax, passMaxWarn )
                ]
        forM_ matrix $ \(title, pass, warn) -> it title $ \ctx -> do
            Stdout m <- generateMnemonicsViaCLI @t []
            (c, o, e) <- createWalletViaCLI @t ctx ["Wallet name"] m "\n" pass
            c `shouldBe` ExitFailure 1
            o `shouldBe` ""
            T.unpack e `shouldContain` warn

    describe "WALLETS_CREATE_08 - --address-pool-gap values" $ do
        let expectsOk c o e gap = do
                c `shouldBe` ExitSuccess
                T.unpack e `shouldContain` cmdOk
                j <- expectValidJSON (Proxy @ApiWallet) o
                expectCliFieldEqual addressPoolGap (read gap :: Int) j

        let expectsErr c o e gap = do
                c `shouldBe` ExitFailure 1
                o `shouldNotContain` gap
                T.unpack e `shouldContain`
                    "An address pool gap must be a natural number between "
                    ++ show addressPoolGapMin ++ " and "
                    ++ show addressPoolGapMax ++ "."

        let matrix =
                [ ( "Gap max", show addressPoolGapMax, expectsOk )
                , ( "Gap min", show addressPoolGapMin, expectsOk )
                , ( "Gap max - 1", show (addressPoolGapMax - 1), expectsOk )
                , ( "Gap min + 1", show (addressPoolGapMin + 1), expectsOk )
                , ( "Gap max + 1 -> fail", show (addressPoolGapMax + 1), expectsErr )
                , ( "Gap min - 1 -> fail", show (addressPoolGapMin - 1), expectsErr )
                , ( "-1000 -> fail", "-1000", expectsErr )
                , ( "0 -> fail", "0", expectsErr )
                , ( "10.5 -> fail", "10.5", expectsErr )
                , ( "arbitraty string -> fail", "string", expectsErr )
                ]

        forM_ matrix $ \(title, gap, expects) -> it title $ \ctx -> do
            Stdout m <- generateMnemonicsViaCLI @t []
            (c, o, e) <- createWalletViaCLI @t ctx ["n", "--address-pool-gap", gap]
                            m "\n" "secure-passphraze"
            expects c o e gap

    it "WALLETS_GET_01 - Can get a wallet" $ \ctx -> do
        walId <- emptyWallet' ctx
        (Exit c, Stdout out, Stderr err) <- getWalletViaCLI @t ctx walId
        c `shouldBe` ExitSuccess
        err `shouldBe` cmdOk
        j <- expectValidJSON (Proxy @ApiWallet) out
        expectCliFieldEqual walletName "Empty Wallet" j

    describe "WALLETS_GET_03,04 - Cannot get wallets with false ids" $ do
        forM_ falseWalletIds $ \(title, walId) -> it title $ \ctx -> do
            (Exit c, Stdout out, Stderr err) <- getWalletViaCLI @t ctx walId
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1
            if (title == "40 chars hex") then
                err `shouldContain`
                    "I couldn't find a wallet with the given id:\
                    \ 1111111111111111111111111111111111111111\n"
            else
                err `shouldContain`
                    "wallet id should be an hex-encoded string of\
                    \ 40 characters\n"

    it "WALLETS_LIST_01 - Can list wallets" $ \ctx -> do
        let name = "Wallet to be listed"
        w1 <- emptyWalletWith' ctx (name, "secure-passphrase", 21)
        _ <- emptyWallet' ctx
        (Exit c, Stdout out, Stderr err) <- listWalletsViaCLI @t ctx
        c `shouldBe` ExitSuccess
        err `shouldBe` cmdOk
        j <- expectValidJSON (Proxy @[ApiWallet]) out
        length j `shouldBe` 2
        verify j
            [ expectCliListItemFieldEqual 0 walletName name
            , expectCliListItemFieldEqual 0 addressPoolGap 21
            , expectCliListItemFieldEqual 0 balanceAvailable 0
            , expectCliListItemFieldEqual 0 balanceTotal 0
            , expectCliListItemFieldEqual 0 delegation (NotDelegating)
            , expectCliListItemFieldEqual 0 walletId (T.pack w1)
            ]

    it "WALLETS_LIST_01 - Wallets are listed from oldest to newest" $ \ctx -> do
        w1 <- emptyWalletWith' ctx ("1", "secure-passphrase", 20)
        w2 <- emptyWalletWith' ctx ("2", "secure-passphrase", 20)
        w3 <- emptyWalletWith' ctx ("3", "secure-passphrase", 20)
        (Exit c, Stdout out, Stderr err) <- listWalletsViaCLI @t ctx
        c `shouldBe` ExitSuccess
        err `shouldBe` cmdOk
        j <- expectValidJSON (Proxy @[ApiWallet]) out
        length j `shouldBe` 3
        verify j
            [ expectCliListItemFieldEqual 0 walletId (T.pack w1)
            , expectCliListItemFieldEqual 1 walletId (T.pack w2)
            , expectCliListItemFieldEqual 2 walletId (T.pack w3)
            ]

    describe "WALLETS_UPDATE_NAME_01,02 - Can update wallet name" $ do
        forM_ walletNames $ \(title, n) -> it title $ \ctx -> do
            wid <- emptyWallet' ctx
            let args = [wid, n]
            (Exit c, Stdout out, Stderr err) <-
                updateWalletNameViaCLI @t ctx args
            c `shouldBe` ExitSuccess
            err `shouldBe` cmdOk
            j <- expectValidJSON (Proxy @ApiWallet) out
            expectCliFieldEqual walletName (T.pack n) j

    it "WALLETS_DELETE_01, WALLETS_LIST_02 - Can delete wallet" $ \ctx -> do
        walId <- emptyWallet' ctx
        (Exit c, Stdout out, Stderr err) <- deleteWalletViaCLI @t ctx walId
        err `shouldBe` cmdOk
        c `shouldBe` ExitSuccess
        out `shouldBe` "\n"
        (Stdout o, Stderr e) <- listWalletsViaCLI @t ctx
        o `shouldBe` "[]\n"
        e `shouldBe` cmdOk

emptyWallet' :: Context t -> IO String
emptyWallet' = fmap (T.unpack . view walletId) . emptyWallet

emptyWalletWith' :: Context t -> (Text, Text, Int) -> IO String
emptyWalletWith' ctx (name, pass, pg) =
    fmap (T.unpack . view walletId) (emptyWalletWith ctx (name, pass, pg))

walletNames :: [(String, String)]
walletNames =
        [ ( "Name min", replicate walletNameMinLength 'ź' )
        , ( "Name max", replicate walletNameMaxLength 'ą' )
        , ( "Name max - 1", replicate ( walletNameMaxLength - 1 ) 'ą' )
        , ( "Name max + 1", replicate ( walletNameMinLength + 1 ) 'ź' )
        , ( "Single space", " " )
        , ( "Russian", "АаБбВвГгДдЕеЁёЖжЗз")
        -- these names give error in automated tests which cannot be reproduced
        -- by hand:
        , ( "Polish", "aąbcćdeęfghijklłmnoóp" )
        , ( "Kanji", "亜哀挨愛曖悪握圧扱宛嵐")
        ]
