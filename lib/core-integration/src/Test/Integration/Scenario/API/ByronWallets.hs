{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.API.ByronWallets
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiByronWallet )
import Cardano.Wallet.Primitive.Mnemonic
    ( entropyToMnemonic, genEntropy, mnemonicToText )
import Cardano.Wallet.Primitive.Types
    ( DecodeAddress, EncodeAddress, walletNameMaxLength, walletNameMinLength )
import Control.Monad
    ( forM_ )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Text
    ( Text )
-- import Numeric.Natural
--     ( Natural )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , emptyWallet
    , expectErrorMessage
    , expectFieldEqual
    , expectResponseCode
    , getFromResponse
    , json
    , listByronWalletEp
    , postByronWalletEp
    , request
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

import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall t. (EncodeAddress t, DecodeAddress t) => SpecWith (Context t)
spec = do

    describe "BYRON_ESTIMATE_06 - non-existing wallets" $  do
        forM_ (take 1 falseWalletIds) $ \(desc, walId) -> it desc $ \ctx -> do
            let endpoint = "v2/byron-wallets/" <> T.pack walId <> "/migrations"
            rg <- request @ApiByronWallet ctx ("GET", endpoint) Default Empty
            expectResponseCode @IO HTTP.status501 rg

        forM_ (drop 1 falseWalletIds) $ \(desc, walId) -> it desc $ \ctx -> do
            let endpoint = "v2/byron-wallets/" <> T.pack walId <> "/migrations"
            rg <- request @ApiByronWallet ctx ("GET", endpoint) Default Empty
            expectResponseCode @IO HTTP.status404 rg
            expectErrorMessage errMsg404NoEndpoint rg

    it "BYRON_GET_02 - Byron ep does not show new wallet" $ \ctx -> do
        w <- emptyWallet ctx
        let ep  = ( "GET", "v2/byron-wallets/" <> w ^. walletId )
        r <- request @ApiByronWallet ctx ep Default Empty
        expectResponseCode @IO HTTP.status404 r

    describe "BYRON_GET_06 - non-existing wallets" $  do
        forM_ falseWalletIds $ \(desc, walId) -> it desc $ \ctx -> do
            let endpoint = "v2/byron-wallets/" <> T.pack walId
            rg <- request @ApiByronWallet ctx ("GET", endpoint) Default Empty
            if (desc == valid40CharHexDesc) then do
                expectErrorMessage (errMsg404NoWallet $ T.pack walId) rg
            else do
                expectResponseCode @IO HTTP.status404 rg
                expectErrorMessage errMsg404NoEndpoint rg

    it "BYRON_LIST_02 - Byron ep does not list new wallets" $ \ctx -> do
        _ <- emptyWallet ctx
        r <- request @ApiByronWallet ctx listByronWalletEp Default Empty
        expectResponseCode @IO HTTP.status200 r

    it "BYRON_DELETE_02 - Byron ep does not delete new wallet" $ \ctx -> do
        w <- emptyWallet ctx
        let ep  = ( "DELETE", "v2/byron-wallets/" <> w ^. walletId )
        r <- request @ApiByronWallet ctx ep Default Empty
        expectResponseCode @IO HTTP.status404 r

    describe "BYRON_DELETE_04 - non-existing wallets" $  do
        forM_ falseWalletIds $ \(desc, walId) -> it desc $ \ctx -> do
            let endpoint = "v2/byron-wallets/" <> T.pack walId
            rg <- request @ApiByronWallet ctx ("DELETE", endpoint) Default Empty
            if (desc == valid40CharHexDesc) then do
                expectErrorMessage (errMsg404NoWallet $ T.pack walId) rg
            else do
                expectResponseCode @IO HTTP.status404 rg
                expectErrorMessage errMsg404NoEndpoint rg

    it "BYRON_RESTORE_01 - Restore a wallet" $ \ctx -> do
        mnemonic <- mnemonicToText @12 . entropyToMnemonic <$> genEntropy
        let payload = Json [json| {
                "name": "Empty Byron Wallet",
                "mnemonic_sentence": #{mnemonic},
                "passphrase": "Secure Passphrase"
            }|]
        r <- request @ApiByronWallet ctx postByronWalletEp Default payload
        expectResponseCode @IO HTTP.status202 r

        rg <- request @ApiByronWallet ctx listByronWalletEp Default Empty
        expectResponseCode @IO HTTP.status200 rg

    it "BYRON_RESTORE_02 - One can restore previously deleted wallet" $ \_ -> do
        m <- mnemonicToText @12 . entropyToMnemonic <$> genEntropy
        print m
        -- w <- emptyByronWalletWith ctx ("Byron Wallet", m, "Secure Passphrase")
        --
        -- rd <- request @ApiByronWallet ctx (deleteByronWalletEp w ^. walletId) Default Empty
        -- expectResponseCode @IO HTTP.status204 rd
        --
        -- wr <- emptyByronWalletWith ctx ("Byron Wallet2", m, "Secure Pa33phrase")
        -- w ^. walletId `shouldBe` wr ^. walletId

    it "BYRON_RESTORE_03 - Cannot restore wallet that exists" $ \ctx -> do
        mnemonic <- mnemonicToText @12 . entropyToMnemonic <$> genEntropy
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
            expectErrorMessage "Invalid number of words: 12 words are expected" r

    describe "BYRON_RESTORE_05 - Faulty mnemonics" $ do
        let matrix =
             [ ( "[] as mnemonic_sentence -> fail", []
               , [ expectResponseCode @IO HTTP.status400
                 , expectErrorMessage "Invalid number of words: 12 words are expected"
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

        forM_ matrix $ \(title, mnemonics, expectations) -> it title $ \ctx -> do
            let payload = Json [json| {
                    "name": "Byron Wallet bye bye",
                    "mnemonic_sentence": #{mnemonics},
                    "passphrase": "Secure Passphrase"
                    } |]
            r <- request @ApiByronWallet ctx postByronWalletEp Default payload
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
        forM_ matrix $ \(title, passphrase, expectations) -> it title $ \ctx -> do
            let payload = Json [json| {
                    "name": "Secure Wallet",
                    "mnemonic_sentence": #{mnemonics12},
                    "passphrase": #{passphrase}
                    } |]
            r <- request @ApiByronWallet ctx postByronWalletEp Default payload
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

    describe "BYRON_RESTORE_07 - v2/wallets - Methods Not Allowed" $ do
        let matrix = ["PUT", "DELETE", "CONNECT", "TRACE", "OPTIONS"]
        forM_ matrix $ \method -> it (show method) $ \ctx -> do
            r <- request @ApiByronWallet ctx (method, "v2/byron-wallets") Default Empty
            expectResponseCode @IO HTTP.status405 r
            expectErrorMessage errMsg405 r

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
