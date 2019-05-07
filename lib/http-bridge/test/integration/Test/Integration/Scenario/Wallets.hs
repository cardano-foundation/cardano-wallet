{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}


module Test.Integration.Scenario.Wallets
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiWallet )
import Cardano.Wallet.Primitive.Types
    ( WalletDelegation (..)
    , WalletState (..)
    , walletNameMaxLength
    , walletNameMinLength
    )
import Control.Monad
    ( forM_ )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , addressPoolGap
    , balanceAvailable
    , balanceTotal
    , delegation
    , expectErrorMessage
    , expectFieldEqual
    , expectFieldNotEqual
    , expectListItemFieldEqual
    , expectListSizeEqual
    , expectResponseCode
    , json
    , passphraseLastUpdate
    , request
    , state
    , verify
    , walletId
    , walletName
    )

import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

spec :: SpecWith Context
spec = do
    it "WALLETS_CREATE_01 - Create a wallet" $ \ctx -> do

        let payload = Json [json| {
                "name": "1st Wallet",
                "mnemonic_sentence": #{mnemonics15},
                "mnemonic_second_factor": #{mnemonics12},
                "passphrase": "Secure Passphrase",
                "address_pool_gap": 30
                } |]
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        verify r
            [ expectResponseCode @IO HTTP.status202
            , expectFieldEqual walletName "1st Wallet"
            , expectFieldEqual addressPoolGap 30
            , expectFieldEqual balanceAvailable 0
            , expectFieldEqual balanceTotal 0
            , expectFieldEqual state (Restoring (Quantity minBound))
            , expectFieldEqual delegation (NotDelegating)
            , expectFieldEqual walletId "2cf060fe53e4e0593f145f22b858dfc60676d4ab"
            , expectFieldNotEqual passphraseLastUpdate "2019-04-12 07:57:28.439742724 UTC"
            ]

    it "WALLETS_CREATE_01 - Created a wallet can be listed" $ \ctx -> do

        let payload = Json [json| {
                "name": "Wallet to be listed",
                "mnemonic_sentence": #{mnemonics18},
                "mnemonic_second_factor": #{mnemonics9},
                "passphrase": "Secure Passphrase",
                "address_pool_gap": 20
                } |]
        _ <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        r2 <- request @[ApiWallet] ctx ("GET", "v2/wallets") Default Empty
        verify r2
            [ expectResponseCode @IO HTTP.status200
            , expectListSizeEqual 1
            , expectListItemFieldEqual 0 walletName "Wallet to be listed"
            , expectListItemFieldEqual 0 addressPoolGap 20
            , expectListItemFieldEqual 0 balanceAvailable 0
            , expectListItemFieldEqual 0 balanceTotal 0
            , expectListItemFieldEqual 0 state (Restoring (Quantity minBound))
            , expectListItemFieldEqual 0 delegation (NotDelegating)
            , expectListItemFieldEqual 0 walletId "dfe87fcf0560fb57937a6468ea51e860672fad79"
            ]

    it "WALLETS_CREATE_03 - Cannot create wallet that exists" $ \ctx -> do
        let payload = Json [json| {
                "name": "Some Wallet",
                "mnemonic_sentence": #{mnemonics21},
                "passphrase": "Secure Passphrase"
                } |]
        r1 <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        expectResponseCode @IO HTTP.status202 r1

        r2 <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        expectResponseCode @IO HTTP.status409 r2

    describe "WALLETS_CREATE_04 - Wallet name" $ do
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
                    "mnemonic_sentence": #{mnemonics24},
                    "passphrase": "Secure Passphrase"
                    } |]
            r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
            verify r expectations

    it "WALLETS_CREATE_04 - [] as name -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": [],
                "mnemonic_sentence": #{mnemonics15},
                "passphrase": "Secure Passphrase"
                } |]
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "expected Text, encountered Array"
            ]

    it "WALLETS_CREATE_04 - Num as name -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": 123,
                "mnemonic_sentence": #{mnemonics15},
                "passphrase": "Secure Passphrase"
                } |]
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "expected Text, encountered Number"
            ]

    it "WALLETS_CREATE_04 - Name param missing -> fail" $ \ctx -> do
        let payload = Json [json| {
                "mnemonic_sentence": #{mnemonics15},
                "passphrase": "Secure Passphrase"
                } |]
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "key \"name\" not present"
            ]

    describe "WALLETS_CREATE_05 - Mnemonics" $ do
        let matrix =
             [ ( "[] as mnemonic_sentence -> fail", []
               , [ expectResponseCode @IO HTTP.status400
                 , expectErrorMessage "Invalid number of words: 15, 18, 21 or\
                      \ 24 words are expected."
                 ]
               )
             , ( "specMnemonicSentence -> fail", specMnemonicSentence
               , [ expectResponseCode @IO HTTP.status400
                 , expectErrorMessage "Invalid entropy checksum: please \
                      \double-check the last word of your mnemonic sentence."
                 ]
               )
             , ( "invalid mnemonics -> fail", invalidMnemonics15
               , [ expectResponseCode @IO HTTP.status400
                 , expectErrorMessage "Invalid entropy checksum: please \
                      \double-check the last word of your mnemonic sentence."
                 ]
               )
             , ( "Japanese mnemonics -> fail", japaneseMnemonics15
               , [ expectResponseCode @IO HTTP.status400
                 , expectErrorMessage "Found invalid (non-English) word:"
                 ] -- why only for Japanese?
               )
             , ( "Chinese mnemonics -> fail", chineseMnemonics18
               , [ expectResponseCode @IO HTTP.status400
                 , expectErrorMessage "Found invalid (non-English) word:"
                 ]
               )
             , ( "French mnemonics -> fail"
               , frenchMnemonics21
               , [ expectResponseCode @IO HTTP.status400
                 , expectErrorMessage "Found invalid (non-English) word:"
                 ]
               )
             , ( "3 mnemonic words -> fail" , mnemonics3
               , [ expectResponseCode @IO HTTP.status400
                 , expectErrorMessage "Invalid number of words: 15, 18, 21 or\
                      \ 24 words are expected."
                 ]
               )
             , ( "6 mnemonic words -> fail", mnemonics6
               , [ expectResponseCode @IO HTTP.status400
                 , expectErrorMessage "Invalid number of words: 15, 18, 21 or\
                      \ 24 words are expected."
                 ]
               )
             , ( "9 mnemonic words -> fail", mnemonics9
               , [ expectResponseCode @IO HTTP.status400
                 , expectErrorMessage "Invalid number of words: 15, 18, 21 or\
                      \ 24 words are expected."
                 ]
               )
             , ( "12 mnemonic words -> fail", mnemonics12
               , [ expectResponseCode @IO HTTP.status400
                 , expectErrorMessage "Invalid number of words: 15, 18, 21 or\
                      \ 24 words are expected."
                 ]
               )
             , ( "15 mnemonic words", mnemonics15
               , [ expectResponseCode @IO HTTP.status202
                 , expectFieldEqual walletId "b062e8ccf3685549b6c489a4e94966bc4695b75b"
                 ]
               )
             , ( "18 mnemonic words", mnemonics18
               , [ expectResponseCode @IO HTTP.status202
                 , expectFieldEqual walletId "f52ee0daaefd75a0212d70c9fbe15ee8ada9fc11"
                 ]
               )
             , ( "21 mnemonic words" , mnemonics21
               , [ expectResponseCode @IO HTTP.status202
                 , expectFieldEqual walletId "7e8c1af5ff2218f388a313f9c70f0ff0550277e4"
                 ]
               )
             , ( "24 mnemonic words", mnemonics24
               , [ expectResponseCode @IO HTTP.status202
                 , expectFieldEqual walletId "a6b6625cd2bfc51a296b0933f77020991cc80374"
                 ]
               )
             ]

        forM_ matrix $ \(title, mnemonics, expectations) -> it title $ \ctx -> do
            let payload = Json [json| {
                    "name": "Just a łallet",
                    "mnemonic_sentence": #{mnemonics},
                    "passphrase": "Secure Passphrase"
                    } |]
            r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
            verify r expectations

    it "WALLETS_CREATE_05 - String as mnemonic_sentence -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": "ads",
                "mnemonic_sentence": "album execute kingdom dumb trip all salute busy case bring spell ugly umbrella choice shy",
                "passphrase": "Secure Passphrase"
                } |]
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "expected [a], encountered String"
            ]

    it "WALLETS_CREATE_05 - Num as mnemonic_sentence -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": "123",
                "mnemonic_sentence": 15,
                "passphrase": "Secure Passphrase"
                } |]
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "expected [a], encountered Number"
            ]

    it "WALLETS_CREATE_05 - mnemonic_sentence param missing -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": "A name",
                "passphrase": "Secure Passphrase"
                } |]
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "key \"mnemonic_sentence\" not present"
            ]

    describe "WALLETS_CREATE_06 - Mnemonics second factor" $ do
        let matrix =
                [ ( "[] as mnemonic_second_factor -> fail", []
                   , [ expectResponseCode @IO HTTP.status400
                     , expectErrorMessage "Invalid number of words: 9 or 12\
                            \ words are expected."
                     ]
                   )
                 , ( "specMnemonicSecondFactor -> fail", specMnemonicSecondFactor
                   , [ expectResponseCode @IO HTTP.status400
                     , expectErrorMessage "Invalid entropy checksum: please\
                         \ double-check the last word of your mnemonic sentence."
                     ]
                   )
                 , ( "invalid mnemonics -> fail", invalidMnemonics12
                   , [ expectResponseCode @IO HTTP.status400
                   , expectErrorMessage "Invalid entropy checksum: please\
                       \ double-check the last word of your mnemonic sentence."
                     ]
                   )
                 , ( "Japanese mnemonics -> fail", japaneseMnemonics12
                   , [ expectResponseCode @IO HTTP.status400
                     , expectErrorMessage "Found invalid (non-English) word:"
                     ]
                   )
                 , ( "Chinese mnemonics -> fail", chineseMnemonics9
                   , [ expectResponseCode @IO HTTP.status400
                     , expectErrorMessage "Found invalid (non-English) word:"
                     ]
                   )
                 , ( "French mnemonics -> fail", frenchMnemonics12
                   , [ expectResponseCode @IO HTTP.status400
                     , expectErrorMessage "Found invalid (non-English) word:"
                     ]
                   )
                 , ( "3 mnemonic words -> fail", mnemonics3
                   , [ expectResponseCode @IO HTTP.status400
                     , expectErrorMessage "Invalid number of words: 9 or 12\
                          \ words are expected."
                     ]
                   )
                 , ( "6 mnemonic words -> fail", mnemonics6
                   , [ expectResponseCode @IO HTTP.status400
                     , expectErrorMessage "Invalid number of words: 9 or 12\
                          \ words are expected."
                     ]
                   )
                 , ( "9 mnemonic words", mnemonics9
                   , [ expectResponseCode @IO HTTP.status202
                     , expectFieldEqual walletId "4b1a865e39d1006efb99f538b05ea2343b567108"
                     ]
                   )
                 , ( "12 mnemonic words", mnemonics12
                   , [ expectResponseCode @IO HTTP.status202
                     , expectFieldEqual walletId "2cf060fe53e4e0593f145f22b858dfc60676d4ab"
                     ]
                   )
                 , ( "15 mnemonic words -> fail", mnemonics15
                   , [ expectResponseCode @IO HTTP.status400
                     , expectErrorMessage "Invalid number of words: 9 or 12\
                            \ words are expected."
                     ]
                   )
                , ( "18 mnemonic words -> fail", mnemonics18
                 , [ expectResponseCode @IO HTTP.status400
                   , expectErrorMessage "Invalid number of words: 9 or 12\
                          \ words are expected."
                   ]
                 )
                 , ( "18 mnemonic words -> fail", mnemonics18
                   , [ expectResponseCode @IO HTTP.status400
                     , expectErrorMessage "Invalid number of words: 9 or 12\
                            \ words are expected."
                     ]
                   )
                 , ( "21 mnemonic words -> fail", mnemonics21
                   , [ expectResponseCode @IO HTTP.status400
                     , expectErrorMessage "Invalid number of words: 9 or 12\
                            \ words are expected."
                     ]
                   )
                 , ( "24 mnemonic words -> fail", mnemonics24
                   , [ expectResponseCode @IO HTTP.status400
                     , expectErrorMessage "Invalid number of words: 9 or 12\
                            \ words are expected."
                     ]
                   )
                 ]
        forM_ matrix $ \(title, mnemonics, expectations) -> it title $ \ctx -> do
            let payload = Json [json| {
                    "name": "Just a łallet",
                    "mnemonic_sentence": #{mnemonics15},
                    "mnemonic_second_factor": #{mnemonics},
                    "passphrase": "Secure Passphrase"
                    } |]
            r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
            verify r expectations

    describe "WALLETS_CREATE_07 - Passphrase" $ do
        let passphraseMax = T.pack (replicate passphraseMaxLength 'ą')
        let matrix =
                [ ( show passphraseMinLength ++ " char long"
                  , T.pack (replicate passphraseMinLength 'ź')
                  , [ expectResponseCode @IO HTTP.status202
                    ]
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
                    "mnemonic_sentence": #{mnemonics24},
                    "passphrase": #{passphrase}
                    } |]
            r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
            verify r expectations

    it "WALLETS_CREATE_07 - [] as passphrase -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": "Secure Wallet",
                "mnemonic_sentence": #{mnemonics15},
                "passphrase": []
                } |]
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "expected Text, encountered Array"
            ]

    it "WALLETS_CREATE_07 - Num as passphrase -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": "Secure Wallet",
                "mnemonic_sentence": #{mnemonics15},
                "passphrase": 777
                } |]
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "expected Text, encountered Number"
            ]

    it "WALLETS_CREATE_07 - passphrase param missing -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": "Secure Wallet",
                "mnemonic_sentence": #{mnemonics15}
                } |]
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "key \"passphrase\" not present"
            ]

    describe "WALLETS_CREATE_08 - address_pool_gap" $ do
        let matrix =
                [ ( show addressPoolGapMin, addressPoolGapMin
                  , [ expectResponseCode @IO HTTP.status202
                    , expectFieldEqual addressPoolGap addressPoolGapMin
                    ]
                  )
                , ( show (addressPoolGapMin - 1) ++ " -> fail"
                  , (addressPoolGapMin - 1)
                  , [ expectResponseCode @IO HTTP.status400
                    , expectErrorMessage "An address pool gap must be a natural\
                      \ number between 10 and 100."
                    ]
                  )
                , ( show addressPoolGapMax, addressPoolGapMax
                  , [ expectResponseCode @IO HTTP.status202 ]
                  )
                , ( show (addressPoolGapMax + 1) ++ " -> fail"
                  , addressPoolGapMax + 1
                  , [ expectResponseCode @IO HTTP.status400
                    , expectErrorMessage "An address pool gap must be a natural\
                      \ number between 10 and 100."
                    ]
                  )
                , ( "0 -> fail", 0
                  , [ expectResponseCode @IO HTTP.status400
                    , expectErrorMessage "An address pool gap must be a natural\
                       \ number between 10 and 100."
                    ]
                  )
                , ( "-1 -> fail", -1
                  , [ expectResponseCode @IO HTTP.status400
                    , expectErrorMessage "An address pool gap must be a natural\
                         \ number between 10 and 100."
                    ]
                  )
                , ( "1000 -> fail", 1000
                  , [ expectResponseCode @IO HTTP.status400
                    , expectErrorMessage "An address pool gap must be a natural\
                       \ number between 10 and 100."
                    ]
                  )
                , ( "132323000 -> fail", 132323000
                  , [ expectResponseCode @IO HTTP.status400
                    , expectErrorMessage "An address pool gap must be a natural\
                       \ number between 10 and 100."
                    ]
                  )
                , ( "-1000 -> fail", -1000
                  , [ expectResponseCode @IO HTTP.status400
                    , expectErrorMessage "An address pool gap must be a natural\
                       \ number between 10 and 100."
                    ]
                  )
                , ( "-132323000 -> fail", -132323000
                  , [ expectResponseCode @IO HTTP.status400
                    , expectErrorMessage "An address pool gap must be a natural\
                       \ number between 10 and 100."
                    ]
                  )
                ]
        forM_ matrix $ \(title, addrPoolGap, expectations) -> it title $ \ctx -> do
            let payload = Json [json| {
                    "name": "Secure Wallet",
                    "mnemonic_sentence": #{mnemonics24},
                    "passphrase": "Secure passphrase",
                    "address_pool_gap": #{addrPoolGap}
                    } |]
            r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
            verify r expectations

    it "WALLETS_CREATE_08 - 2.5 as address_pool_gap -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": "Secure Wallet",
                "mnemonic_sentence": #{mnemonics15},
                "passphrase": "Secure passphrase",
                "address_pool_gap": 2.5
                } |]
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "expected Integer, encountered floating number"
            ]


    it "WALLETS_CREATE_08 - -2.5 as address_pool_gap -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": "Secure Wallet",
                "mnemonic_sentence": #{mnemonics15},
                "passphrase": "Secure passphrase",
                "address_pool_gap": -2.5
                } |]
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "expected Integer, encountered floating number"
            ]

    it "WALLETS_CREATE_08 - [] as address_pool_gap -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": "Secure Wallet",
                "mnemonic_sentence": #{mnemonics15},
                "passphrase": "Secure passphrase",
                "address_pool_gap": []
                } |]
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "expected Integer, encountered Array"
            ]

    it "WALLETS_CREATE_08 - String as address_pool_gap -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": "Secure Wallet",
                "mnemonic_sentence": #{mnemonics15},
                "passphrase": "Secure passphrase",
                "address_pool_gap": "30"
                } |]
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "expected Integer, encountered String"
            ]

    it "WALLETS_CREATE_08 - default address_pool_gap" $ \ctx -> do
        let payload = Json [json| {
                "name": "Secure Wallet",
                "mnemonic_sentence": #{mnemonics21},
                "passphrase": "Secure passphrase"
                } |]
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        verify r
            [ expectResponseCode @IO HTTP.status202
            , expectFieldEqual addressPoolGap 20
            ]
 where

    mnemonics3 :: [Text]
    mnemonics3 = ["diamond", "flee", "window"]

    mnemonics6 :: [Text]
    mnemonics6 = ["tornado", "canvas", "peasant", "spike", "enrich", "dilemma"]

    mnemonics9 :: [Text]
    mnemonics9 = ["subway", "tourist", "abstract", "roast", "border", "curious",
        "exercise", "work", "narrow"]

    mnemonics12 :: [Text]
    mnemonics12 = ["agent", "siren", "roof", "water", "giant", "pepper",
        "obtain", "oxygen", "treat", "vessel", "hip", "garlic"]

    mnemonics15 :: [Text]
    mnemonics15 = ["network", "empty", "cause", "mean", "expire", "private",
        "finger", "accident", "session", "problem", "absurd", "banner", "stage",
        "void", "what"]

    mnemonics18 :: [Text]
    mnemonics18 = ["whisper", "control", "diary", "solid", "cattle", "salmon",
        "whale", "slender", "spread", "ice", "shock", "solve", "panel",
        "caution", "upon", "scatter", "broken", "tonight"]

    mnemonics21 :: [Text]
    mnemonics21 = ["click", "puzzle", "athlete", "morning", "fold", "retreat",
        "across", "timber", "essay", "drill", "finger", "erase", "galaxy",
        "spoon", "swift", "eye", "awesome", "shrimp", "depend", "zebra", "token"]

    mnemonics24 :: [Text]
    mnemonics24 = ["decade", "distance", "denial", "jelly", "wash", "sword",
        "olive", "perfect", "jewel", "renew", "wrestle", "cupboard", "record",
        "scale", "pattern", "invite", "other", "fruit", "gloom", "west", "oak",
        "deal", "seek", "hand"]

    invalidMnemonics12 :: [Text]
    invalidMnemonics12 = ["word","word","word","word","word","word","word",
            "word","word","word","word","hill"]

    invalidMnemonics15 :: [Text]
    invalidMnemonics15 = ["word","word","word","word","word","word","word",
        "word","word","word","word","word","word","word","word"]

    specMnemonicSentence :: [Text]
    specMnemonicSentence = ["squirrel", "material", "silly", "twice", "direct",
        "slush", "pistol", "razor", "become", "junk", "kingdom", "flee",
        "squirrel", "silly", "twice"]

    specMnemonicSecondFactor :: [Text]
    specMnemonicSecondFactor = ["squirrel", "material", "silly", "twice",
        "direct", "slush", "pistol", "razor", "become"]

    japaneseMnemonics12 :: [Text]
    japaneseMnemonics12 = ["そうだん",　"ひよう",　"にもつ",　"やさしい",　"きふく",　
        "ねつい",　"だったい",　"けんてい",　"けいろ",　"ざつがく",　"ほうもん",　"すこし"]

    japaneseMnemonics15 :: [Text]
    japaneseMnemonics15 = ["うめる", "せんく", "えんぎ", "はんぺん", "おくりがな",
        "さんち", "きなが", "といれ", "からい", "らくだ", "うえる", "ふめん", "せびろ",
        "られつ", "なにわ"]

    chineseMnemonics9 :: [Text]
    chineseMnemonics9 = ["钢", "看", "磁", "塑", "凤", "魏", "世", "腐", "恶" ]

    chineseMnemonics18 :: [Text]
    chineseMnemonics18 = ["盗", "精", "序", "郎", "赋", "姿", "委", "善", "酵",
        "祥", "赛", "矩", "蜡", "注", "韦", "效", "义", "冻"]

    frenchMnemonics12 :: [Text]
    frenchMnemonics12 = ["palmarès", "supplier", "visuel", "gardien", "adorer",
        "cordage", "notifier", "réglage", "employer", "abandon", "scénario",
        "proverbe"]

    frenchMnemonics21 :: [Text]
    frenchMnemonics21 = ["pliage", "exhorter", "brasier", "chausson", "bloquer",
        "besace", "sorcier", "absurde", "neutron", "forgeron", "geyser",
        "moulin", "cynique", "cloche", "baril", "infliger", "rompre", "typique",
        "renifler", "creuser", "matière"]

    russianWalletName :: Text
    russianWalletName = "АаБбВвГгДдЕеЁёЖжЗз ИиЙйКкЛлМмНнО оПпРрСсТтУуФф ХхЦцЧчШшЩщЪъ ЫыЬьЭэЮюЯяІ ѢѲѴѵѳѣі"

    polishWalletName :: Text
    polishWalletName = "aąbcćdeęfghijklłmnoóprsś\r\ntuvwyzżźAĄBCĆDEĘFGHIJKLŁMNOP\rRSŚTUVWYZŻŹ"

    kanjiWalletName :: Text
    kanjiWalletName = "亜哀挨愛曖悪握圧扱宛嵐安案暗以衣位囲医依委威為畏胃尉異移萎偉椅彙意違維慰\
    \遺緯域育一壱逸茨芋引印因咽姻員院淫陰飲隠韻右宇羽雨唄鬱畝浦運雲永泳英映栄\n営詠影鋭衛易疫益液駅悦越謁\
    \閲円延沿炎怨宴媛援園煙猿遠鉛塩演縁艶汚王凹\r\n央応往押旺欧殴桜翁奥横岡屋億憶臆虞乙俺卸音恩温穏下化火加\
    \可仮何花佳価果河苛科架夏家荷華菓貨渦過嫁暇禍靴寡歌箇稼課蚊牙瓦我画芽賀雅餓介回灰会快戒改怪拐悔海界\
    \皆械絵開階塊楷解潰壊懐諧貝外劾害崖涯街慨蓋該概骸垣柿各角拡革格核殻郭覚較隔閣確獲嚇穫学岳楽額顎掛潟\
    \括活喝渇割葛滑褐轄且株釜鎌刈干刊甘汗缶\r"

    arabicWalletName :: Text
    arabicWalletName = "ثم نفس سقطت وبالتحديد،, جزيرتي باستخدام أن دنو. إذ هنا؟ الستار وتنصيب كان. أهّل ايطاليا، بريطانيا-فرنسا قد أخذ. سليمان، إتفاقية بين ما, يذكر الحدود أي بعد, معاملة بولندا، الإطلاق عل إيو."

    wildcardsWalletName :: Text
    wildcardsWalletName = "`~`!@#$%^&*()_+-=<>,./?;':\"\"'{}[]\\|❤️ 💔 💌 💕 💞 \
    \💓 💗 💖 💘 💝 💟 💜 💛 💚 💙0️⃣ 1️⃣ 2️⃣ 3️⃣ 4️⃣ 5️⃣ 6️⃣ 7️⃣ 8️⃣ 9️⃣ 🔟🇺🇸🇷🇺🇸 🇦🇫🇦🇲🇸"

    passphraseMinLength :: Int
    passphraseMinLength = 10

    passphraseMaxLength :: Int
    passphraseMaxLength = 255

    addressPoolGapMin :: Int
    addressPoolGapMin = 10

    addressPoolGapMax :: Int
    addressPoolGapMax = 100
