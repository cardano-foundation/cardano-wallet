{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.API.ByronWallets
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiByronWallet
    , ApiByronWalletMigrationInfo (..)
    , ApiTransaction
    , ApiWallet
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Mnemonic
    ( entropyToMnemonic, genEntropy, mnemonicToText )
import Cardano.Wallet.Primitive.Types
    ( SyncProgress (..), walletNameMaxLength, walletNameMinLength )
import Control.Monad
    ( forM_ )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Maybe
    ( mapMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word64 )
-- import Numeric.Natural
--     ( Natural )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , amount
    , balanceAvailable
    , balanceTotal
    , calculateByronMigrationCostEp
    , deleteByronWalletEp
    , deleteWalletEp
    , emptyByronWallet
    , emptyByronWalletWith
    , emptyWallet
    , emptyWalletWith
    , expectErrorMessage
    , expectEventually
    , expectEventually'
    , expectFieldEqual
    , expectFieldNotEqual
    , expectFieldSatisfy
    , expectListItemFieldEqual
    , expectListSizeEqual
    , expectResponseCode
    , fixtureByronWalletWith
    , getByronWalletEp
    , getFromResponse
    , getWalletEp
    , json
    , listByronWalletsEp
    , listWalletsEp
    , migrateByronWalletEp
    , passphraseLastUpdate
    , postByronWalletEp
    , request
    , state
    , verify
    , walletId
    , walletName
    )
import Test.Integration.Framework.TestData
    ( arabicWalletName
    , errMsg404NoEndpoint
    , errMsg404NoWallet
    , errMsg405
    , errMsg406
    , errMsg415
    , falseWalletIds
    , frenchMnemonics12
    , getHeaderCases
    , invalidMnemonics12
    , japaneseMnemonics12
    , kanjiWalletName
    , mnemonics12
    , mnemonics15
    , mnemonics18
    , mnemonics21
    , mnemonics24
    , mnemonics3
    , mnemonics6
    , mnemonics9
    , passphraseMaxLength
    , passphraseMinLength
    , polishWalletName
    , russianWalletName
    , specMnemonicByron
    , wildcardsWalletName
    )

import qualified Cardano.Wallet.Api.Types as ApiTypes
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall t n. (n ~ 'Testnet) => SpecWith (Context t)
spec = do

    -- Compute the fee associated with an API transaction.
    let apiTransactionFee :: ApiTransaction n -> Word64
        apiTransactionFee t =
            inputBalance t - outputBalance t
          where
            inputBalance = fromIntegral
                . sum
                . fmap (view amount)
                . mapMaybe ApiTypes.source
                . ApiTypes.inputs
            outputBalance = fromIntegral
                . sum
                . fmap (view amount)
                . ApiTypes.outputs

    it "BYRON_MIGRATE_01 - \
        \migrating an empty wallet should generate a fee of zero."
        $ \ctx -> do
            w <- emptyByronWallet ctx
            r@(_, Right (ApiByronWalletMigrationInfo fee)) <-
                request ctx (calculateByronMigrationCostEp w) Default Empty
            expectResponseCode @IO HTTP.status200 r
            fee `shouldBe` Quantity 0

    it "BYRON_MIGRATE_02 - \
        \migrating an empty wallet should not generate transactions."
        $ \ctx -> do
            mnemonic <- genMnemonics
            sourceWallet <- emptyByronWalletWith
                ctx ("byron-wallet-name", mnemonic, "Secure Passphrase")
            targetWallet <- emptyWallet ctx
            let payload = Json [json| {
                    "passphrase": "Secure Passphrase"
                }|]
            r@(_, Right transactions) <- request @[ApiTransaction n] ctx
                (migrateByronWalletEp sourceWallet targetWallet) Default payload
            expectResponseCode @IO HTTP.status202 r
            transactions `shouldBe` []

    it "BYRON_MIGRATE_03 - \
        \actual fee for migration is the same as the predicted fee."
        $ \ctx -> do
            -- Restore a Byron wallet with funds.
            let name = "test byron wallet"
            let passphrase = "test passphrase"
            sourceWallet <- fixtureByronWalletWith name passphrase ctx

            -- Request a migration fee prediction.
            let ep0 = (calculateByronMigrationCostEp sourceWallet)
            r0 <- request @ApiByronWalletMigrationInfo ctx ep0 Default Empty
            verify r0
                [ expectResponseCode @IO HTTP.status200
                , expectFieldSatisfy amount (> 0)
                ]

            -- Perform the migration.
            targetWallet <- emptyWallet ctx
            let payload = Json [aesonQQ|{"passphrase": #{passphrase}}|]
            let ep1 = migrateByronWalletEp sourceWallet targetWallet
            r1 <- request @[ApiTransaction n] ctx ep1 Default payload
            verify r1
                [ expectResponseCode @IO HTTP.status202
                , expectFieldSatisfy id (not . null)
                ]

            -- Verify that the fee prediction was correct.
            let actualFee = fromIntegral $ sum $ apiTransactionFee
                    <$> getFromResponse id r1
            let predictedFee =
                    getFromResponse amount r0
            actualFee `shouldBe` predictedFee

    it "BYRON_MIGRATE_04 - \
        \a migration operation removes all funds from the source wallet."
        $ \ctx -> do

            -- Restore a Byron wallet with funds, to act as a source wallet:
            let sourceWalletName = "source wallet"
            let sourceWalletPass = "source wallet passphrase"
            sourceWallet <-
                fixtureByronWalletWith sourceWalletName sourceWalletPass ctx

            -- Verify that the source wallet has funds available:
            r0 <- request @ApiByronWallet ctx
                (getByronWalletEp sourceWallet) Default Empty
            verify r0
                [ expectResponseCode @IO HTTP.status200
                , expectFieldSatisfy balanceAvailable (> 0)
                ]

            -- Perform a migration from the source wallet to a target wallet:
            targetWallet <- emptyWallet ctx
            r1 <- request @[ApiTransaction n] ctx
                (migrateByronWalletEp sourceWallet targetWallet )
                Default
                (Json [aesonQQ|{"passphrase": #{sourceWalletPass}}|])
            verify r1
                [ expectResponseCode @IO HTTP.status202
                , expectFieldSatisfy id (not . null)
                ]

            -- Verify that the source wallet is still listable:
            r2 <- request @[ApiByronWallet] ctx
                listByronWalletsEp Default Empty
            verify r2
                [ expectResponseCode @IO HTTP.status200
                , expectListSizeEqual 1
                , expectListItemFieldEqual 0 walletName sourceWalletName
                ]

            -- Verify that the source wallet has no funds available:
            r3 <- request @ApiByronWallet ctx
                (getByronWalletEp sourceWallet) Default Empty
            verify r3
                [ expectResponseCode @IO HTTP.status200
                , expectFieldSatisfy balanceAvailable (== 0)
                ]

    it "BYRON_MIGRATE_05 - \
        \after a migration operation successfully completes, the correct \
        \amount eventually becomes available in the target wallet."
        $ \ctx -> do

            -- Restore a Byron wallet with funds, to act as a source wallet:
            let sourceWalletName = "source wallet"
            let sourceWalletPass = "source wallet passphrase"
            sourceWallet <-
                fixtureByronWalletWith sourceWalletName sourceWalletPass ctx

            -- Verify that the source wallet has funds available:
            r0 <- request @ApiByronWallet ctx
                (getByronWalletEp sourceWallet) Default Empty
            verify r0
                [ expectResponseCode @IO HTTP.status200
                , expectFieldSatisfy balanceAvailable (> 0)
                ]
            let originalBalance = getFromResponse balanceAvailable r0

            -- Create an empty target wallet:
            targetWallet <- emptyWallet ctx

            -- Verify that the target wallet has no funds available:
            r1 <- request @ApiWallet ctx
                (getWalletEp targetWallet) Default Empty
            verify r1
                [ expectResponseCode @IO HTTP.status200
                , expectFieldSatisfy balanceAvailable (== 0)
                ]

            -- Calculate the expected migration fee:
            r2 <- request @ApiByronWalletMigrationInfo ctx
                (calculateByronMigrationCostEp sourceWallet) Default Empty
            verify r2
                [ expectResponseCode @IO HTTP.status200
                , expectFieldSatisfy amount (> 0)
                ]
            let expectedFee = getFromResponse amount r2

            -- Perform a migration from the source wallet to the target wallet:
            r3 <- request @[ApiTransaction n] ctx
                (migrateByronWalletEp sourceWallet targetWallet)
                Default
                (Json [aesonQQ|{"passphrase": #{sourceWalletPass}}|])
            verify r3
                [ expectResponseCode @IO HTTP.status202
                , expectFieldSatisfy id (not . null)
                ]

            -- Check that funds become available in the target wallet:
            let expectedBalance = originalBalance - expectedFee
            expectEventually'
                ctx getWalletEp balanceTotal expectedBalance targetWallet

    describe "BYRON_MIGRATE_06 - non-existing wallets" $  do
        forM_ (take 1 falseWalletIds) $ \(desc, walId) -> it desc $ \ctx -> do
            let endpoint = "v2/byron-wallets/" <> T.pack walId <> "/migrations"
            rg <- request @ApiByronWallet ctx ("GET", endpoint) Default Empty
            expectResponseCode @IO HTTP.status404 rg

        forM_ (drop 1 falseWalletIds) $ \(desc, walId) -> it desc $ \ctx -> do
            let endpoint = "v2/byron-wallets/" <> T.pack walId <> "/migrations"
            rg <- request @ApiByronWallet ctx ("GET", endpoint) Default Empty
            expectResponseCode @IO HTTP.status404 rg
            expectErrorMessage errMsg404NoEndpoint rg

    it "BYRON_GET_02 - Byron ep does not show Shelley wallet" $ \ctx -> do
        w <- emptyWallet ctx
        let wid = w ^. walletId
        let ep  = ( "GET", "v2/byron-wallets/" <> wid )
        r <- request @ApiByronWallet ctx ep Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet wid) r

    it "BYRON_GET_03 - Shelley ep does not show Byron wallet" $ \ctx -> do
        w <- emptyByronWallet ctx
        let wid = w ^. walletId
        let ep  = ( "GET", "v2/wallets/" <> wid )
        r <- request @ApiWallet ctx ep Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet wid) r

    it "BYRON_GET_04, DELETE_01 - Deleted wallet is not available" $ \ctx -> do
        w <- emptyByronWallet ctx
        _ <- request @ApiByronWallet ctx (deleteByronWalletEp w) Default Empty
        rg <- request @ApiByronWallet ctx (getByronWalletEp w) Default Empty
        expectResponseCode @IO HTTP.status404 rg
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) rg

    describe "BYRON_GET_05,06 - non-existing wallets" $  do
        forM_ falseWalletIds $ \(desc, walId) -> it desc $ \ctx -> do
            let endpoint = "v2/byron-wallets/" <> T.pack walId
            rg <- request @ApiByronWallet ctx ("GET", endpoint) Default Empty
            expectResponseCode @IO HTTP.status404 rg
            if (desc == valid40CharHexDesc) then do
                expectErrorMessage (errMsg404NoWallet $ T.pack walId) rg
            else
                expectErrorMessage errMsg404NoEndpoint rg

    describe "BYRON_GET_07 - v2/byron-wallets/<wid> - Methods Not Allowed" $ do
        let matrix = ["PUT", "POST", "CONNECT", "TRACE", "OPTIONS"]
        forM_ matrix $ \method -> it (show method) $ \ctx -> do
            w <- emptyByronWallet ctx
            let ep = "v2/byron-wallets/" <> w ^. walletId
            r <- request @ApiByronWallet ctx (method, ep) Default Empty
            expectResponseCode @IO HTTP.status405 r
            expectErrorMessage errMsg405 r

    describe "BYRON_GET_07 - v2/byron-wallets/<wid> - HTTP headers" $ do
        forM_ (getHeaderCases HTTP.status200)
            $ \(title, headers, expec) -> it title $ \ctx -> do
            w <- emptyByronWallet ctx
            rg <- request @ApiWallet ctx (getByronWalletEp w) headers Empty
            verify rg expec

    it "BYRON_LIST_01 - Byron Wallets are listed from oldest to newest" $
        \ctx -> do
            m1 <- genMnemonics
            m2 <- genMnemonics
            m3 <- genMnemonics
            _ <- emptyByronWalletWith ctx ("b1", m1, "Secure Passphrase")
            _ <- emptyByronWalletWith ctx ("b2", m2, "Secure Passphrase")
            _ <- emptyByronWalletWith ctx ("b3", m3, "Secure Passphrase")

            rl <- request @[ApiByronWallet] ctx listByronWalletsEp Default Empty
            verify rl
                [ expectResponseCode @IO HTTP.status200
                , expectListSizeEqual 3
                , expectListItemFieldEqual 0 walletName "b1"
                , expectListItemFieldEqual 1 walletName "b2"
                , expectListItemFieldEqual 2 walletName "b3"
                ]

    it "BYRON_LIST_02,03 -\
        \ Byron wallets listed only via Byron endpoints \\\
        \ Shelley wallets listed only via new endpoints" $ \ctx -> do
        m1 <- genMnemonics
        m2 <- genMnemonics
        m3 <- genMnemonics
        _ <- emptyByronWalletWith ctx ("byron1", m1, "Secure Passphrase")
        _ <- emptyByronWalletWith ctx ("byron2", m2, "Secure Passphrase")
        _ <- emptyByronWalletWith ctx ("byron3", m3, "Secure Passphrase")

        _ <- emptyWalletWith ctx ("shelley1", "Secure Passphrase", 20)
        _ <- emptyWalletWith ctx ("shelley2", "Secure Passphrase", 20)
        _ <- emptyWalletWith ctx ("shelley3", "Secure Passphrase", 20)

        --list only byron
        rl <- request @[ApiByronWallet] ctx listByronWalletsEp Default Empty
        verify rl
            [ expectResponseCode @IO HTTP.status200
            , expectListSizeEqual 3
            , expectListItemFieldEqual 0 walletName "byron1"
            , expectListItemFieldEqual 1 walletName "byron2"
            , expectListItemFieldEqual 2 walletName "byron3"
            ]
        --list only shelley
        rl2 <- request @[ApiWallet] ctx listWalletsEp Default Empty
        verify rl2
            [ expectResponseCode @IO HTTP.status200
            , expectListSizeEqual 3
            , expectListItemFieldEqual 0 walletName "shelley1"
            , expectListItemFieldEqual 1 walletName "shelley2"
            , expectListItemFieldEqual 2 walletName "shelley3"
            ]

    it "BYRON_LIST_04, DELETE_01 -\
        \ Deleted wallets cannot be listed" $ \ctx -> do
        m1 <- genMnemonics
        m2 <- genMnemonics
        m3 <- genMnemonics
        _ <- emptyByronWalletWith ctx ("byron1", m1, "Secure Passphrase")
        wb2 <- emptyByronWalletWith ctx ("byron2", m2, "Secure Passphrase")
        _ <- emptyByronWalletWith ctx ("byron3", m3, "Secure Passphrase")

        _ <- emptyWalletWith ctx ("shelley1", "Secure Passphrase", 20)
        _ <- emptyWalletWith ctx ("shelley2", "Secure Passphrase", 20)
        ws3 <- emptyWalletWith ctx ("shelley3", "Secure Passphrase", 20)

        -- delete
        _ <- request @ApiByronWallet ctx (deleteByronWalletEp wb2) Default Empty
        _ <- request @ApiWallet ctx (deleteWalletEp ws3) Default Empty

        --list only byron
        rdl <- request @[ApiByronWallet] ctx listByronWalletsEp Default Empty
        verify rdl
            [ expectResponseCode @IO HTTP.status200
            , expectListSizeEqual 2
            , expectListItemFieldEqual 0 walletName "byron1"
            , expectListItemFieldEqual 1 walletName "byron3"
            ]
        --list only shelley
        rdl2 <- request @[ApiWallet] ctx listWalletsEp Default Empty
        verify rdl2
            [ expectResponseCode @IO HTTP.status200
            , expectListSizeEqual 2
            , expectListItemFieldEqual 0 walletName "shelley1"
            , expectListItemFieldEqual 1 walletName "shelley2"
            ]

    describe "BYRON_LIST_05 - HTTP headers" $ do
        forM_ (getHeaderCases HTTP.status200)
            $ \(title, headers, expec) -> it title $ \ctx -> do
            w <- emptyByronWallet ctx
            rl <- request @ApiWallet ctx (getByronWalletEp w) headers Empty
            verify rl expec

    it "BYRON_DELETE_02 - Byron ep does not delete Shelley wallet" $ \ctx -> do
        w <- emptyWallet ctx
        let wid = w ^. walletId
        let ep  = ( "DELETE", "v2/byron-wallets/" <> wid )
        r <- request @ApiByronWallet ctx ep Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet wid) r

    it "BYRON_DELETE_03 - Shelley ep does not delete Byron wallet" $ \ctx -> do
        w <- emptyByronWallet ctx
        let wid = w ^. walletId
        let ep  = ( "DELETE", "v2/wallets/" <> wid )
        r <- request @ApiWallet ctx ep Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet wid) r

    describe "BYRON_DELETE_04 - non-existing wallets" $  do
        forM_ falseWalletIds $ \(desc, walId) -> it desc $ \ctx -> do
            let endpoint = "v2/byron-wallets/" <> T.pack walId
            rg <- request @ApiByronWallet ctx ("DELETE", endpoint) Default Empty
            expectResponseCode @IO HTTP.status404 rg
            if (desc == valid40CharHexDesc) then do
                expectErrorMessage (errMsg404NoWallet $ T.pack walId) rg
            else do
                expectErrorMessage errMsg404NoEndpoint rg

    describe "BYRON_DELETE_05 - HTTP headers" $ do
        forM_ (getHeaderCases HTTP.status204)
            $ \(title, headers, expectations) -> it title $ \ctx -> do
            w <- emptyByronWallet ctx
            rl <- request
                @ApiByronWallet ctx (deleteByronWalletEp w) headers Empty
            verify rl expectations

    it "BYRON_RESTORE_01, GET_01, LIST_01 - Restore a wallet" $ \ctx -> do
        mnemonic <- genMnemonics
        let name = "Empty Byron Wallet"
        let payload = Json [json| {
                "name": #{name},
                "mnemonic_sentence": #{mnemonic},
                "passphrase": "Secure Passphrase"
            }|]
        let expectations =
                    [ expectFieldEqual walletName name
                    , expectFieldEqual balanceAvailable 0
                    , expectFieldEqual balanceTotal 0
                    , expectEventually ctx getByronWalletEp state Ready
                    , expectFieldNotEqual passphraseLastUpdate Nothing
                    ]
        -- create
        r <- request @ApiByronWallet ctx postByronWalletEp Default payload
        verify r $ (expectResponseCode @IO HTTP.status202) : expectations
        let w = getFromResponse id r
        -- get
        rg <- request @ApiByronWallet ctx (getByronWalletEp w) Default Empty
        verify rg $ (expectResponseCode @IO HTTP.status200) : expectations
        -- list
        rl <- request @[ApiByronWallet] ctx listByronWalletsEp Default Empty
        verify rl
            [ expectResponseCode @IO HTTP.status200
            , expectListSizeEqual 1
            , expectListItemFieldEqual 0 walletName name
            , expectListItemFieldEqual 0 balanceAvailable 0
            , expectListItemFieldEqual 0 balanceTotal 0
            ]

    it "BYRON_RESTORE_02 - One can restore previously deleted wallet" $
        \ctx -> do
            m <- genMnemonics
            w <- emptyByronWalletWith
                ctx ("Byron Wallet", m, "Secure Passphrase")
            rd <- request
                @ApiByronWallet ctx (deleteByronWalletEp w) Default Empty
            expectResponseCode @IO HTTP.status204 rd
            wr <- emptyByronWalletWith
                ctx ("Byron Wallet2", m, "Secure Pa33phrase")
            w ^. walletId `shouldBe` wr ^. walletId

    it "BYRON_RESTORE_03 - Cannot restore wallet that exists" $ \ctx -> do
        mnemonic <- genMnemonics
        let payload = Json [json| {
                "name": "Some Byron Wallet",
                "mnemonic_sentence": #{mnemonic},
                "passphrase": "Secure Passphrase"
                } |]
        r1 <- request @ApiByronWallet ctx postByronWalletEp Default payload
        expectResponseCode @IO HTTP.status202 r1

        r2 <- request @ApiByronWallet ctx postByronWalletEp Default payload
        verify r2
            [ expectResponseCode @IO HTTP.status409
            , expectErrorMessage ("This operation would yield a wallet with the\
                 \ following id: " ++ T.unpack (getFromResponse walletId r1) ++
                 " However, I already know of a wallet with this id.")
            ]

    describe "BYRON_RESTORE_04 - Wallet name" $ do
        let walNameMax = T.pack (replicate walletNameMaxLength 'ą')
        let matrix =
                [ ( show walletNameMinLength ++ " char long", "1"
                  , [ expectResponseCode @IO HTTP.status202
                    , expectFieldEqual walletName "1"
                    ]
                  )
                , ( show walletNameMaxLength ++ " char long", walNameMax
                  , [ expectResponseCode @IO HTTP.status202
                    , expectFieldEqual walletName walNameMax
                    ]
                  )
                , ( show (walletNameMaxLength + 1) ++ " char long"
                  , T.pack (replicate (walletNameMaxLength + 1) 'ę')
                  , [ expectResponseCode @IO HTTP.status400
                    , expectErrorMessage "name is too long: expected at\
                            \ most 255 characters"
                    ]
                  )
                , ( "Empty name", ""
                   , [ expectResponseCode @IO HTTP.status400
                     , expectErrorMessage "name is too short: expected at\
                            \ least 1 character"
                     ]
                  )
                , ( "Russian name", russianWalletName
                  , [ expectResponseCode @IO HTTP.status202
                    , expectFieldEqual walletName russianWalletName
                    ]
                  )
                , ( "Polish name", polishWalletName
                  , [ expectResponseCode @IO HTTP.status202
                    , expectFieldEqual walletName polishWalletName
                    ]
                  )
                , ( "Kanji name", kanjiWalletName
                  , [ expectResponseCode @IO HTTP.status202
                    , expectFieldEqual walletName kanjiWalletName
                    ]
                  )
                , ( "Arabic name", arabicWalletName
                  , [ expectResponseCode @IO HTTP.status202
                    , expectFieldEqual walletName arabicWalletName
                    ]
                  )
                , ( "Wildcards name", wildcardsWalletName
                  , [ expectResponseCode @IO HTTP.status202
                    , expectFieldEqual walletName wildcardsWalletName
                    ]
                  )
                ]
        forM_ matrix $ \(title, walName, expectations) -> it title $ \ctx -> do
            let payload = Json [json| {
                    "name": #{walName},
                    "mnemonic_sentence": #{mnemonics12},
                    "passphrase": "Secure Passphrase"
                    } |]
            r <- request @ApiByronWallet ctx postByronWalletEp Default payload
            verify r expectations

    it "BYRON_RESTORE_04 - [] as name -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": [],
                "mnemonic_sentence": #{mnemonics12},
                "passphrase": "Secure Passphrase"
                } |]
        r <- request @ApiByronWallet ctx postByronWalletEp Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "expected Text, encountered Array"
            ]

    it "BYRON_RESTORE_04 - Num as name -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": 123,
                "mnemonic_sentence": #{mnemonics12},
                "passphrase": "Secure Passphrase"
                } |]
        r <- request @ApiByronWallet ctx postByronWalletEp Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "expected Text, encountered Number"
            ]

    it "BYRON_RESTORE_04 - Name param missing -> fail" $ \ctx -> do
        let payload = Json [json| {
                "mnemonic_sentence": #{mnemonics12},
                "passphrase": "Secure Passphrase"
                } |]
        r <- request @ApiByronWallet ctx postByronWalletEp Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "key 'name' not present"
            ]

    describe "BYRON_RESTORE_05 - Non 12 word long mnemonic is incorrect" $ do
        forM_ incorrectSizeMnemonics $ \m -> it (show $ length m) $ \ctx -> do
            let payload = Json [json| {
                    "name": "Some Byron Wallet",
                    "mnemonic_sentence": #{m},
                    "passphrase": "Secure Passphrase"
                    } |]
            r <- request @ApiByronWallet ctx postByronWalletEp Default payload
            expectResponseCode @IO HTTP.status400 r
            expectErrorMessage
                "Invalid number of words: 12 words are expected" r

    describe "BYRON_RESTORE_05 - Faulty mnemonics" $ do
        let matrix =
             [ ( "[] as mnemonic_sentence -> fail", []
               , [ expectResponseCode @IO HTTP.status400
                 , expectErrorMessage
                    "Invalid number of words: 12 words are expected"
                 ]
               )
             , ( "specMnemonicSentence -> fail", specMnemonicByron
               , [ expectResponseCode @IO HTTP.status400
                 , expectErrorMessage "Invalid entropy checksum: please \
                      \double-check the last word of your mnemonic sentence."
                 ]
               )
             , ( "invalid mnemonics -> fail", invalidMnemonics12
               , [ expectResponseCode @IO HTTP.status400
                 , expectErrorMessage "Invalid entropy checksum: please \
                      \double-check the last word of your mnemonic sentence."
                 ]
               )
             , ( "Japanese mnemonics -> fail", japaneseMnemonics12
               , [ expectResponseCode @IO HTTP.status400
                 , expectErrorMessage "Found invalid (non-English) word:"
                 ]
               )
             , ( "French mnemonics -> fail"
               , frenchMnemonics12
               , [ expectResponseCode @IO HTTP.status400
                 , expectErrorMessage "Found invalid (non-English) word:"
                 ]
               )

             ]

        forM_ matrix $ \(title, mnemonics, expectations) -> it title $
            \ctx -> do
                let payload = Json [json| {
                        "name": "Byron Wallet bye bye",
                        "mnemonic_sentence": #{mnemonics},
                        "passphrase": "Secure Passphrase"
                        } |]
                r <- request
                    @ApiByronWallet ctx postByronWalletEp Default payload
                verify r expectations

    it "BYRON_RESTORE_05 - String as mnemonic_sentence -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": "ads",
                "mnemonic_sentence": "album execute kingdom dumb trip",
                "passphrase": "Secure Passphrase"
                } |]
        r <- request @ApiByronWallet ctx postByronWalletEp Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "expected [a], encountered String"
            ]

    it "BYRON_RESTORE_05 - Num as mnemonic_sentence -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": "123",
                "mnemonic_sentence": 15,
                "passphrase": "Secure Passphrase"
                } |]
        r <- request @ApiByronWallet ctx postByronWalletEp Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "expected [a], encountered Number"
            ]

    it "BYRON_RESTORE_05 - mnemonic_sentence param missing -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": "A name",
                "passphrase": "Secure Passphrase"
                } |]
        r <- request @ApiByronWallet ctx postByronWalletEp Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "key 'mnemonic_sentence' not present"
            ]

    describe "BYRON_RESTORE_06 - Passphrase" $ do
        let passphraseMax = T.pack (replicate passphraseMaxLength 'ą')
        let matrix =
                [ ( show passphraseMinLength ++ " char long"
                  , T.pack (replicate passphraseMinLength 'ź')
                  , [ expectResponseCode @IO HTTP.status202 ]
                  )
                , ( show (passphraseMinLength - 1) ++ " char long"
                  , T.pack (replicate (passphraseMinLength - 1) 'ż')
                  , [ expectResponseCode @IO HTTP.status400
                    , expectErrorMessage "passphrase is too short: expected at\
                            \ least 10 characters"
                    ]
                  )
                , ( show passphraseMaxLength ++ " char long", passphraseMax
                  , [ expectResponseCode @IO HTTP.status202 ]
                  )
                , ( show (passphraseMaxLength + 1) ++ " char long"
                  , T.pack (replicate (passphraseMaxLength + 1) 'ę')
                  , [ expectResponseCode @IO HTTP.status400
                    , expectErrorMessage "passphrase is too long: expected at\
                            \ most 255 characters"
                    ]
                  )
                , ( "Empty passphrase", ""
                   , [ expectResponseCode @IO HTTP.status400
                     , expectErrorMessage "passphrase is too short: expected at\
                            \ least 10 characters"
                     ]
                  )
                , ( "Russian passphrase", russianWalletName
                  , [ expectResponseCode @IO HTTP.status202 ]
                  )
                , ( "Polish passphrase", polishWalletName
                  , [ expectResponseCode @IO HTTP.status202 ]
                  )
                , ( "Kanji passphrase", kanjiWalletName
                  , [ expectResponseCode @IO HTTP.status202 ]
                  )
                , ( "Arabic passphrase", arabicWalletName
                  , [ expectResponseCode @IO HTTP.status202 ]
                  )
                , ( "Wildcards passphrase", wildcardsWalletName
                  , [ expectResponseCode @IO HTTP.status202 ]
                  )
                ]
        forM_ matrix $ \(title, passphrase, expectations) -> it title $
            \ctx -> do
                let payload = Json [json| {
                        "name": "Secure Wallet",
                        "mnemonic_sentence": #{mnemonics12},
                        "passphrase": #{passphrase}
                        } |]
                r <- request
                    @ApiByronWallet ctx postByronWalletEp Default payload
                verify r expectations

    it "BYRON_RESTORE_06 - [] as passphrase -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": "Secure Wallet",
                "mnemonic_sentence": #{mnemonics12},
                "passphrase": []
                } |]
        r <- request @ApiByronWallet ctx postByronWalletEp Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "expected Text, encountered Array"
            ]

    it "BYRON_RESTORE_06 - Num as passphrase -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": "Secure Wallet",
                "mnemonic_sentence": #{mnemonics12},
                "passphrase": 777
                } |]
        r <- request @ApiByronWallet ctx postByronWalletEp Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "expected Text, encountered Number"
            ]

    it "BYRON_RESTORE_06 - passphrase param missing -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": "Secure Wallet",
                "mnemonic_sentence": #{mnemonics12}
                } |]
        r <- request @ApiByronWallet ctx postByronWalletEp Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "key 'passphrase' not present"
            ]

    describe "BYRON_RESTORE_07 - HTTP headers" $ do
        let matrix =
                 [ ( "No HTTP headers -> 415", None
                   , [ expectResponseCode @IO HTTP.status415
                     , expectErrorMessage errMsg415 ]
                   )
                 , ( "Accept: text/plain -> 406"
                   , Headers
                         [ ("Content-Type", "application/json")
                         , ("Accept", "text/plain") ]
                   , [ expectResponseCode @IO HTTP.status406
                     , expectErrorMessage errMsg406 ]
                   )
                 , ( "No Accept -> 202"
                   , Headers [ ("Content-Type", "application/json") ]
                   , [ expectResponseCode @IO HTTP.status202 ]
                   )
                 , ( "No Content-Type -> 415"
                   , Headers [ ("Accept", "application/json") ]
                   , [ expectResponseCode @IO HTTP.status415
                     , expectErrorMessage errMsg415 ]
                   )
                 , ( "Content-Type: text/plain -> 415"
                   , Headers [ ("Content-Type", "text/plain") ]
                   , [ expectResponseCode @IO HTTP.status415
                     , expectErrorMessage errMsg415 ]
                   )
                 ]
        forM_ matrix $ \(title, headers, expectations) -> it title $ \ctx -> do
            let payload = Json [json| {
                    "name": "Secure Wallet",
                    "mnemonic_sentence": #{mnemonics12},
                    "passphrase": "Secure passphrase"
                    } |]
            r <- request @ApiByronWallet ctx postByronWalletEp headers payload
            verify r expectations

    describe "BYRON_RESTORE_07 - Bad request" $ do
        let matrix =
                [ ( "empty payload", NonJson "" )
                , ( "{} payload", NonJson "{}" )
                , ( "non-json valid payload"
                  , NonJson
                        "{name: wallet,\
                        \ mnemonic_sentence: [pill, hand, ask, useless, asset,\
                        \ rely, above, pipe, embark, game, elder, unaware,\
                        \ nasty, coach, glad],\
                        \ passphrase: 1234567890}"
                  )
                ]

        forM_ matrix $ \(name, nonJson) -> it name $ \ctx -> do
            let payload = nonJson
            r <- request @ApiByronWallet ctx postByronWalletEp Default payload
            expectResponseCode @IO HTTP.status400 r

    describe "BYRON_RESTORE_07, LIST_05 -\
        \ v2/byron-wallets - Methods Not Allowed" $ do
        let matrix = ["PUT", "DELETE", "CONNECT", "TRACE", "OPTIONS"]
        forM_ matrix $ \method -> it (show method) $ \ctx -> do
            r <- request
                @ApiByronWallet ctx (method, "v2/byron-wallets") Default Empty
            expectResponseCode @IO HTTP.status405 r
            expectErrorMessage errMsg405 r
 where
     genMnemonics = mnemonicToText @12 . entropyToMnemonic <$> genEntropy

incorrectSizeMnemonics :: [ [ Text ] ]
incorrectSizeMnemonics =
        [ mnemonics15
        , mnemonics18
        , mnemonics21
        , mnemonics24
        , mnemonics3
        , mnemonics6
        , mnemonics9 ]

valid40CharHexDesc :: String
valid40CharHexDesc = "40 chars hex"
