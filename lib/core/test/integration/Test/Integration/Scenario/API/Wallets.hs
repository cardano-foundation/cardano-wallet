{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.API.Wallets
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiTransaction, ApiUtxoStatistics, ApiWallet )
import Cardano.Wallet.Primitive.Mnemonic
    ( entropyToMnemonic, genEntropy, mnemonicToText )
import Cardano.Wallet.Primitive.Types
    ( DecodeAddress
    , EncodeAddress
    , WalletDelegation (..)
    , WalletState (..)
    , walletNameMaxLength
    , walletNameMinLength
    )
import Control.Monad
    ( forM_ )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Text
    ( Text )
import Numeric.Natural
    ( Natural )
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
    , deleteWalletEp
    , emptyWallet
    , expectErrorMessage
    , expectEventually
    , expectFieldEqual
    , expectFieldNotEqual
    , expectListItemFieldEqual
    , expectListSizeEqual
    , expectResponseCode
    , expectWalletUTxO
    , fixtureWallet
    , getFromResponse
    , getWalletEp
    , getWalletUtxoEp
    , json
    , listAddresses
    , passphraseLastUpdate
    , postTxEp
    , request
    , state
    , updateWalletPassEp
    , verify
    , walletId
    , walletName
    , (</>)
    )
import Test.Integration.Framework.TestData
    ( addressPoolGapMax
    , addressPoolGapMin
    , arabicWalletName
    , chineseMnemonics18
    , chineseMnemonics9
    , errMsg403WrongPass
    , errMsg404NoEndpoint
    , errMsg404NoRootKey
    , errMsg404NoWallet
    , errMsg405
    , errMsg406
    , errMsg415
    , falseWalletIds
    , frenchMnemonics12
    , frenchMnemonics21
    , invalidMnemonics12
    , invalidMnemonics15
    , japaneseMnemonics12
    , japaneseMnemonics15
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
    , payloadWith
    , polishWalletName
    , russianWalletName
    , simplePayload
    , specMnemonicSecondFactor
    , specMnemonicSentence
    , updateNamePayload
    , updatePassPayload
    , wildcardsWalletName
    )

import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall t. (EncodeAddress t, DecodeAddress t) => SpecWith (Context t)
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
            , expectEventually ctx state Ready
            , expectFieldEqual delegation (NotDelegating)
            , expectFieldEqual walletId "2cf060fe53e4e0593f145f22b858dfc60676d4ab"
            , expectFieldNotEqual passphraseLastUpdate Nothing
            ]

    it "WALLETS_CREATE_02 - Restored wallet preserves funds" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        -- create wallet
        mnemonics <- mnemonicToText @15 . entropyToMnemonic <$> genEntropy
        let payldCrt = payloadWith "!st created" mnemonics
        rInit <- request @ApiWallet ctx ("POST", "v2/wallets") Default payldCrt
        verify rInit
            [ expectResponseCode @IO HTTP.status202
            , expectFieldEqual balanceAvailable 0
            , expectFieldEqual balanceTotal 0
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
        rTrans <- request @(ApiTransaction t) ctx (postTxEp wSrc)
            Default payload
        expectResponseCode @IO HTTP.status202 rTrans

        rGet <- request @ApiWallet ctx (getWalletEp wDest) Default Empty
        verify rGet
            [ expectEventually ctx balanceTotal 1
            , expectEventually ctx balanceAvailable 1
            ]

        -- delete wallet
        rDel <- request @ApiWallet ctx (deleteWalletEp wDest) Default Empty
        expectResponseCode @IO HTTP.status204 rDel

        -- restore and make sure funds are there
        rRestore <- request @ApiWallet ctx ("POST", "v2/wallets") Default payldCrt
        verify rRestore
            [ expectResponseCode @IO HTTP.status202
            , expectEventually ctx balanceAvailable 1
            , expectEventually ctx balanceTotal 1
            ]

    it "WALLETS_CREATE_03,09 - Cannot create wallet that exists" $ \ctx -> do
        let payload = Json [json| {
                "name": "Some Wallet",
                "mnemonic_sentence": #{mnemonics21},
                "passphrase": "Secure Passphrase"
                } |]
        r1 <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        expectResponseCode @IO HTTP.status202 r1

        r2 <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        verify r2
            [ expectResponseCode @IO HTTP.status409
            , expectErrorMessage ("This operation would yield a wallet with the\
                \ following id: " ++ T.unpack (getFromResponse walletId r1) ++
                " However, I already know of a wallet with this id.")
            ]

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
            , expectErrorMessage "key 'name' not present"
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
            , expectErrorMessage "key 'mnemonic_sentence' not present"
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
            , expectErrorMessage "key 'passphrase' not present"
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

    describe "WALLETS_CREATE_09 - HTTP headers" $ do
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
                    "mnemonic_sentence": #{mnemonics21},
                    "passphrase": "Secure passphrase"
                    } |]
            r <- request @ApiWallet ctx ("POST", "v2/wallets") headers payload
            verify r expectations

    describe "WALLETS_CREATE_09 - Bad request" $ do
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
            r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
            expectResponseCode @IO HTTP.status400 r

    describe "WALLETS_CREATE_09, WALLETS_LIST_03 - v2/wallets - Methods Not Allowed" $ do
        let matrix = ["PUT", "DELETE", "CONNECT", "TRACE", "OPTIONS"]
        forM_ matrix $ \method -> it (show method) $ \ctx -> do
            r <- request @ApiWallet ctx (method, "v2/wallets") Default Empty
            expectResponseCode @IO HTTP.status405 r
            expectErrorMessage errMsg405 r

    it "WALLETS_GET_01 - can get wallet detals" $ \ctx -> do
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default simplePayload
        let walId = getFromResponse walletId r

        rg <- request @ApiWallet ctx ("GET", "v2/wallets" </> walId) Default Empty
        verify rg
            [ expectResponseCode @IO HTTP.status200
            , expectFieldEqual walletName "Secure Wallet"
            , expectFieldEqual addressPoolGap 20
            , expectFieldEqual balanceAvailable 0
            , expectFieldEqual balanceTotal 0
            , expectEventually ctx state Ready
            , expectFieldEqual delegation (NotDelegating)
            , expectFieldEqual walletId walId
            , expectFieldNotEqual passphraseLastUpdate Nothing
            ]

    it "WALLETS_GET_02, WALLETS_DELETE_01 - Deleted wallet is not available" $ \ctx -> do
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default simplePayload
        let wid = getFromResponse walletId r
        let endpoint = "v2/wallets" </> wid
        _ <- request @ApiWallet ctx ("DELETE", endpoint) Default Empty

        rg <- request @ApiWallet ctx ("GET", endpoint) Default Empty
        expectResponseCode @IO HTTP.status404 rg
        expectErrorMessage (errMsg404NoWallet wid) rg

    describe "WALLETS_GET_03,04 - non-existing wallets" $  do
        forM_ falseWalletIds $ \(title, walId) -> it title $ \ctx -> do
            let endpoint = "v2/wallets" </> walId
            rg <- request @ApiWallet ctx ("GET", endpoint) Default Empty
            expectResponseCode @IO HTTP.status404 rg
            if (title == "40 chars hex") then
                expectErrorMessage (errMsg404NoWallet $ T.pack walId) rg
            else
                expectErrorMessage errMsg404NoEndpoint rg

    it "WALLETS_GET_03 - 'almost' valid walletId" $ \ctx -> do
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default simplePayload
        let endpoint = "v2/wallets" </> (T.append (getFromResponse walletId r) "0")
        rg <- request @ApiWallet ctx ("GET", endpoint) Default Empty
        expectResponseCode @IO HTTP.status404 rg
        expectErrorMessage errMsg404NoEndpoint rg

    describe "WALLETS_GET_05 - v2/wallets/{id} - Methods Not Allowed" $ do
        let matrix = ["POST", "CONNECT", "TRACE", "OPTIONS"]
        forM_ matrix $ \method -> it (show method) $ \ctx -> do
            w <- emptyWallet ctx
            r <- request @ApiWallet ctx (method, "v2/wallets/" <> w ^. walletId)
                    Default Empty
            expectResponseCode @IO HTTP.status405 r
            expectErrorMessage errMsg405 r

    describe "WALLETS_GET_05 - HTTP headers" $ do
        forM_ getHeaderCases $ \(title, headers, expectations) -> it title $ \ctx -> do
            r <- request @ApiWallet ctx ("POST", "v2/wallets") Default simplePayload
            let endpoint = "v2/wallets" </> (getFromResponse walletId r)
            rg <- request @ApiWallet ctx ("GET", endpoint) headers Empty
            verify rg expectations

    it "WALLETS_LIST_01 - Created a wallet can be listed" $ \ctx -> do
        let payload = Json [json| {
                "name": "Wallet to be listed",
                "mnemonic_sentence": #{mnemonics18},
                "mnemonic_second_factor": #{mnemonics9},
                "passphrase": "Secure Passphrase",
                "address_pool_gap": 20
                } |]
        _ <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        rl <- request @[ApiWallet] ctx ("GET", "v2/wallets") Default Empty
        verify rl
            [ expectResponseCode @IO HTTP.status200
            , expectListSizeEqual 1
            , expectListItemFieldEqual 0 walletName "Wallet to be listed"
            , expectListItemFieldEqual 0 addressPoolGap 20
            , expectListItemFieldEqual 0 balanceAvailable 0
            , expectListItemFieldEqual 0 balanceTotal 0
            , expectListItemFieldEqual 0 delegation (NotDelegating)
            , expectListItemFieldEqual 0 walletId "dfe87fcf0560fb57937a6468ea51e860672fad79"
            ]

    it "WALLETS_LIST_01 - Wallets are listed from oldest to newest" $ \ctx -> do
        let walletDetails = [("1", mnemonics15), ("2", mnemonics18)
                    , ("3", mnemonics21)]
        forM_ walletDetails $ \(name, mnemonics) -> do
            let payload = payloadWith name mnemonics
            request @ApiWallet ctx ("POST", "v2/wallets") Default payload

        rl <- request @[ApiWallet] ctx ("GET", "v2/wallets") Default Empty
        verify rl
            [ expectResponseCode @IO HTTP.status200
            , expectListSizeEqual 3
            , expectListItemFieldEqual 0 walletName "1"
            , expectListItemFieldEqual 1 walletName "2"
            , expectListItemFieldEqual 2 walletName "3"
            ]

    it "WALLETS_LIST_02 - Deleted wallet not listed" $ \ctx -> do
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default simplePayload
        let endpoint = "v2/wallets" </> (getFromResponse walletId r)
        _ <- request @ApiWallet ctx ("DELETE", endpoint) Default Empty
        rl <- request @[ApiWallet] ctx ("GET", "v2/wallets") Default Empty
        verify rl
            [ expectResponseCode @IO HTTP.status200
            , expectListSizeEqual 0
            ]

    describe "WALLETS_LIST_03 - HTTP headers" $ do
        forM_ getHeaderCases $ \(title, headers, expectations) -> it title $ \ctx -> do
            _ <- request @ApiWallet ctx ("POST", "v2/wallets") Default simplePayload
            rl <- request @ApiWallet ctx ("GET", "v2/wallets") headers Empty
            verify rl expectations

    describe "WALLETS_DELETE_02 - non-existing wallets" $  do
        forM_ falseWalletIds $ \(title, walId) -> it title $ \ctx -> do
            let endpoint = "v2/wallets" </> walId
            rg <- request @ApiWallet ctx ("DELETE", endpoint) Default Empty
            expectResponseCode @IO HTTP.status404 rg
            if (title == "40 chars hex") then
                expectErrorMessage (errMsg404NoWallet $ T.pack walId) rg
            else
                expectErrorMessage errMsg404NoEndpoint rg

    it "WALLETS_DELETE_02 - 'almost' valid walletId" $ \ctx -> do
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default simplePayload
        let endpoint = "v2/wallets" </> (T.append (getFromResponse walletId r) "0")
        rg <- request @ApiWallet ctx ("DELETE", endpoint) Default Empty
        expectResponseCode @IO HTTP.status404 rg
        expectErrorMessage errMsg404NoEndpoint rg

    describe "WALLETS_DELETE_03 - HTTP headers" $ do
        let matrix =
                  [ ( "No HTTP headers -> 204", None
                    , [ expectResponseCode @IO HTTP.status204 ] )
                  , ( "Accept: text/plain -> 406"
                    , Headers
                          [ ("Content-Type", "application/json")
                          , ("Accept", "text/plain") ]
                    , [ expectResponseCode @IO HTTP.status406
                      , expectErrorMessage errMsg406 ]
                    )
                  , ( "No Accept -> 204"
                    , Headers [ ("Content-Type", "application/json") ]
                    , [ expectResponseCode @IO HTTP.status204 ]
                    )
                  , ( "No Content-Type -> 204"
                    , Headers [ ("Accept", "application/json") ]
                    , [ expectResponseCode @IO HTTP.status204 ]
                    )
                  , ( "Content-Type: text/plain -> 204"
                    , Headers [ ("Content-Type", "text/plain") ]
                    , [ expectResponseCode @IO HTTP.status204 ]
                    )
                  ]
        forM_ matrix $ \(title, headers, expectations) -> it title $ \ctx -> do
            r <- request @ApiWallet ctx ("POST", "v2/wallets") Default simplePayload
            let endpoint = "v2/wallets" </> (getFromResponse walletId r)
            rl <- request @ApiWallet ctx ("DELETE", endpoint) headers Empty
            verify rl expectations

    it "WALLETS_UPDATE_01 - Updated wallet name is available" $ \ctx -> do

        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default simplePayload
        let passLastUpdateValue = getFromResponse passphraseLastUpdate r
        let newName = updateNamePayload "New great name"
        let walId = getFromResponse walletId r
        let expectations = [ expectResponseCode @IO HTTP.status200
                    , expectFieldEqual walletName "New great name"
                    , expectFieldEqual addressPoolGap 20
                    , expectFieldEqual balanceAvailable 0
                    , expectFieldEqual balanceTotal 0
                    , expectEventually ctx state Ready
                    , expectFieldEqual delegation (NotDelegating)
                    , expectFieldEqual walletId walId
                    , expectFieldEqual passphraseLastUpdate passLastUpdateValue
                    ]
        ru <- request @ApiWallet ctx ("PUT", "v2/wallets" </> walId) Default newName
        verify ru expectations
        rg <- request @ApiWallet ctx ("GET", "v2/wallets" </> walId) Default Empty
        verify rg expectations
        rl <- request @[ApiWallet] ctx ("GET", "v2/wallets") Default Empty
        verify rl
            [ expectResponseCode @IO HTTP.status200
            , expectListSizeEqual 1
            , expectListItemFieldEqual 0 walletName "New great name"
            , expectListItemFieldEqual 0 addressPoolGap 20
            , expectListItemFieldEqual 0 balanceAvailable 0
            , expectListItemFieldEqual 0 balanceTotal 0
            , expectListItemFieldEqual 0 delegation (NotDelegating)
            , expectListItemFieldEqual 0 walletId walId
            , expectListItemFieldEqual 0 passphraseLastUpdate passLastUpdateValue
            ]

    describe "WALLETS_UPDATE_02 - Various names" $ do
        let walNameMax = T.pack (replicate walletNameMaxLength 'ą')
        let matrix =
                [ ( show walletNameMinLength ++ " char long", "1"
                  , [ expectResponseCode @IO HTTP.status200
                    , expectFieldEqual walletName "1"
                    ]
                  )
                , ( show walletNameMaxLength ++ " char long", walNameMax
                  , [ expectResponseCode @IO HTTP.status200
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
                  , [ expectResponseCode @IO HTTP.status200
                    , expectFieldEqual walletName russianWalletName
                    ]
                  )
                , ( "Polish name", polishWalletName
                  , [ expectResponseCode @IO HTTP.status200
                    , expectFieldEqual walletName polishWalletName
                    ]
                  )
                , ( "Kanji name", kanjiWalletName
                  , [ expectResponseCode @IO HTTP.status200
                    , expectFieldEqual walletName kanjiWalletName
                    ]
                  )
                , ( "Arabic name", arabicWalletName
                  , [ expectResponseCode @IO HTTP.status200
                    , expectFieldEqual walletName arabicWalletName
                    ]
                  )
                , ( "Wildcards name", wildcardsWalletName
                  , [ expectResponseCode @IO HTTP.status200
                    , expectFieldEqual walletName wildcardsWalletName
                    ]
                  )
                ]
        forM_ matrix $ \(title, walName, expectations) -> it title $ \ctx -> do
            r <- request @ApiWallet ctx ("POST", "v2/wallets") Default simplePayload
            let newName = updateNamePayload walName
            let endpoint = "v2/wallets" </> (getFromResponse walletId r)
            ru <- request @ApiWallet ctx ("PUT", endpoint) Default newName
            verify ru expectations

    it "WALLETS_UPDATE_02 - [] as name -> fail" $ \ctx -> do
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default simplePayload
        let walId = getFromResponse walletId r
        let payload = Json [json| {
                "name": []
                } |]
        ru <- request @ApiWallet ctx ("PUT", "v2/wallets" </> walId) Default payload
        verify ru
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "expected Text, encountered Array"
            ]

    it "WALLETS_UPDATE_02 - Num as name -> fail" $ \ctx -> do
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default simplePayload
        let walId = getFromResponse walletId r
        let payload = Json [json| {
                "name": 123
                } |]
        ru <- request @ApiWallet ctx ("PUT", "v2/wallets" </> walId) Default payload
        verify ru
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "expected Text, encountered Number"
            ]

    it "WALLETS_UPDATE_02 - Name param missing -> OK" $ \ctx -> do
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default simplePayload
        let walId = getFromResponse walletId r
        let payload = Json [json| {  } |]
        ru <- request @ApiWallet ctx ("PUT", "v2/wallets" </> walId) Default payload
        verify ru
            [ expectResponseCode @IO HTTP.status200
            , expectFieldEqual walletName "Secure Wallet"
            ]

    it "WALLETS_UPDATE_02 - No payload -> fail" $ \ctx -> do
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default simplePayload
        let walId = getFromResponse walletId r
        ru <- request @ApiWallet ctx ("PUT", "v2/wallets" </> walId) Default Empty
        verify ru
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "not enough input"
            ]

    describe "WALLETS_UPDATE_03 - non-existing wallets" $  do
        forM_ falseWalletIds $ \(title, walId) -> it title $ \ctx -> do
            let newName = updateNamePayload "new name"
            let endpoint = "v2/wallets" </> walId
            ru <- request @ApiWallet ctx ("PUT", endpoint) Default newName
            expectResponseCode @IO HTTP.status404 ru
            if (title == "40 chars hex") then
                expectErrorMessage (errMsg404NoWallet $ T.pack walId) ru
            else
                expectErrorMessage errMsg404NoEndpoint ru

    it "WALLETS_UPDATE_03 - 'almost' valid walletId" $ \ctx -> do
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default simplePayload
        let newName = updateNamePayload "new name"
        let endpoint = "v2/wallets" </> (T.append (getFromResponse walletId r) "0")
        ru <- request @ApiWallet ctx ("PUT", endpoint) Default newName
        expectResponseCode @IO HTTP.status404 ru
        expectErrorMessage errMsg404NoEndpoint ru

    it "WALLETS_UPDATE_03 - Deleted wallet cannot be updated (404)" $ \ctx -> do
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default simplePayload
        let wid = getFromResponse walletId r
        let endpoint = "v2/wallets" </> wid
        _ <- request @ApiWallet ctx ("DELETE", endpoint) Default Empty

        let newName = updateNamePayload "new name"
        ru <- request @ApiWallet ctx ("GET", endpoint) Default newName
        expectResponseCode @IO HTTP.status404 ru
        expectErrorMessage (errMsg404NoWallet wid) ru

    describe "WALLETS_UPDATE_04 - HTTP headers" $ do
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
                  , ( "No Accept -> 200"
                    , Headers [ ("Content-Type", "application/json") ]
                    , [ expectResponseCode @IO HTTP.status200 ]
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
            r <- request @ApiWallet ctx ("POST", "v2/wallets") Default simplePayload
            let newName = updateNamePayload "new name"
            let endpoint = "v2/wallets" </> (getFromResponse walletId r)
            ru <- request @ApiWallet ctx ("PUT", endpoint) headers newName
            verify ru expectations

    it "WALLETS_UPDATE_PASS_01 - passphaseLastUpdate gets updated" $ \ctx -> do
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default simplePayload
        let payload = updatePassPayload "Secure passphrase" "New passphrase"
        let endpoint = "v2/wallets" </> (getFromResponse walletId r)
                </> ("passphrase" :: Text)
        rup <- request @ApiWallet ctx ("PUT", endpoint) Default payload
        expectResponseCode @IO HTTP.status204 rup

        let getEndpoint = "v2/wallets" </> (getFromResponse walletId r)
        let originalPassUpdateDateTime = getFromResponse passphraseLastUpdate r
        rg <- request @ApiWallet ctx ("GET", getEndpoint) Default Empty
        expectFieldNotEqual passphraseLastUpdate originalPassUpdateDateTime rg

    describe "WALLETS_UPDATE_PASS_02 - New passphrase values" $ do
        let passphraseMax = T.pack (replicate passphraseMaxLength 'ą')
        let matrix =
                [ ( show passphraseMinLength ++ " char long"
                  , T.pack (replicate passphraseMinLength 'ź')
                  , [ expectResponseCode @IO HTTP.status204
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
                  , [ expectResponseCode @IO HTTP.status204 ]
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
                  , [ expectResponseCode @IO HTTP.status204 ]
                  )
                , ( "Polish passphrase", polishWalletName
                  , [ expectResponseCode @IO HTTP.status204 ]
                  )
                , ( "Kanji passphrase", kanjiWalletName
                  , [ expectResponseCode @IO HTTP.status204 ]
                  )
                , ( "Arabic passphrase", arabicWalletName
                  , [ expectResponseCode @IO HTTP.status204 ]
                  )
                , ( "Wildcards passphrase", wildcardsWalletName
                  , [ expectResponseCode @IO HTTP.status204 ]
                  )
                ]
        forM_ matrix $ \(title, passphrase, expectations) -> it title $ \ctx -> do
            r <- request @ApiWallet ctx ("POST", "v2/wallets") Default simplePayload
            let payload = updatePassPayload "Secure passphrase" passphrase
            let endpoint = "v2/wallets" </> (getFromResponse walletId r)
                    </> ("passphrase" :: Text)
            rup <- request @ApiWallet ctx ("PUT", endpoint) Default payload
            verify rup expectations

    describe "WALLETS_UPDATE_PASS_03 - Old passphrase invalid values" $ do
        let matrix =
                [ ( show (passphraseMinLength - 1) ++ " char long"
                  , T.pack (replicate (passphraseMinLength - 1) 'ż')
                  , [ expectResponseCode @IO HTTP.status400
                    , expectErrorMessage "passphrase is too short: expected at\
                            \ least 10 characters" ]
                  )
                , ( show (passphraseMaxLength + 1) ++ " char long"
                  , T.pack (replicate (passphraseMaxLength + 1) 'ę')
                  , [ expectResponseCode @IO HTTP.status400
                    , expectErrorMessage "passphrase is too long: expected at\
                            \ most 255 characters" ]
                  )
                , ( "Empty passphrase", ""
                   , [ expectResponseCode @IO HTTP.status400
                     , expectErrorMessage "passphrase is too short: expected at\
                            \ least 10 characters" ]
                  )
                , ( "Incorrect old pass", "Incorrect passphrase"
                  , [ expectResponseCode @IO HTTP.status403
                    , expectErrorMessage errMsg403WrongPass ]
                  )
                ]
        forM_ matrix $ \(title, passphrase, expectations) -> it title $ \ctx -> do
            r <- request @ApiWallet ctx ("POST", "v2/wallets") Default simplePayload
            let payload = updatePassPayload passphrase "Secure passphrase 2"
            let endpoint = "v2/wallets" </> (getFromResponse walletId r)
                    </> ("passphrase" :: Text)
            rup <- request @ApiWallet ctx ("PUT", endpoint) Default payload
            verify rup expectations

    describe "WALLETS_UPDATE_PASS_03 - Can update pass from pass that's boundary\
    \ value" $ do
        let matrix =
                [ ( show passphraseMinLength ++ " char long"
                  , T.pack (replicate passphraseMinLength 'ź') )
                , ( show passphraseMaxLength ++ " char long"
                  , T.pack (replicate passphraseMaxLength 'ą') )
                , ( "Russian passphrase", russianWalletName )
                , ( "Polish passphrase", polishWalletName )
                , ( "Kanji passphrase", kanjiWalletName )
                , ( "Arabic passphrase", arabicWalletName )
                , ( "Wildcards passphrase", wildcardsWalletName )
                ]
        forM_ matrix $ \(title, oldPass) -> it title $ \ctx -> do
            let createPayload = Json [json| {
                     "name": "Name of the wallet",
                     "mnemonic_sentence": #{mnemonics24},
                     "passphrase": #{oldPass}
                     } |]
            r <- request @ApiWallet ctx ("POST", "v2/wallets") Default createPayload
            let payload = updatePassPayload oldPass
                                (T.pack (replicate passphraseMaxLength '💘'))
            let endpoint = "v2/wallets" </> (getFromResponse walletId r)
                    </> ("passphrase" :: Text)
            rup <- request @ApiWallet ctx ("PUT", endpoint) Default payload
            expectResponseCode @IO HTTP.status204 rup

    describe "WALLETS_UPDATE_PASS_02,03 - invalid payloads" $  do
        let matrix =
                [  ( "[] as new passphrase"
                   , Json [json| {
                        "old_passphrase": "Secure passphrase",
                        "new_passphrase": []
                          } |]
                   , [ expectResponseCode @IO HTTP.status400
                     , expectErrorMessage "expected Text, encountered Array" ]
                   )
                 , ( "[] as old passphrase"
                   , Json [json| {
                       "old_passphrase": [],
                       "new_passphrase": "Secure passphrase"
                         } |]
                   , [ expectResponseCode @IO HTTP.status400
                     , expectErrorMessage "expected Text, encountered Array" ]
                   )
                 , ( "Num as old passphrase"
                   , Json [json| {
                      "old_passphrase": 12345678910,
                      "new_passphrase": "Secure passphrase"
                         } |]
                   , [ expectResponseCode @IO HTTP.status400
                     , expectErrorMessage "expected Text, encountered Number" ]
                   )
                 , ( "Num as new passphrase"
                   , Json [json| {
                      "old_passphrase": "Secure passphrase",
                      "new_passphrase": 12345678910
                         } |]
                   , [ expectResponseCode @IO HTTP.status400
                     , expectErrorMessage "expected Text, encountered Number" ]
                   )
                 , ( "Missing old passphrase"
                   , Json [json| {
                      "new_passphrase": "Secure passphrase"
                         } |]
                   , [ expectResponseCode @IO HTTP.status400
                     , expectErrorMessage "key 'old_passphrase' not present" ]
                   )
                 , ( "Missing new passphrase"
                   , Json [json| {
                      "old_passphrase": "Secure passphrase"
                         } |]
                   , [ expectResponseCode @IO HTTP.status400
                     , expectErrorMessage "key 'new_passphrase' not present" ]
                  )
                ]
        forM_ matrix $ \(title, payload, expectations) -> it title $ \ctx -> do
            r <- request @ApiWallet ctx ("POST", "v2/wallets") Default simplePayload
            let endpoint = "v2/wallets" </> (getFromResponse walletId r)
                    </> ("passphrase" :: Text)
            rup <- request @ApiWallet ctx ("PUT", endpoint) Default payload
            verify rup expectations

    it "WALLETS_UPDATE_PASS_04 - Deleted wallet is not available" $ \ctx -> do
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default simplePayload
        let payload = updatePassPayload "Secure passphrase" "Secure passphrase2"
        let walId = getFromResponse walletId r
        let delEndp = "v2/wallets" </> walId
        _ <- request @ApiWallet ctx ("DELETE", delEndp) Default Empty
        let updEndp = delEndp </> ("passphrase" :: Text)
        rup <- request @ApiWallet ctx ("PUT", updEndp) Default payload
        expectResponseCode @IO HTTP.status404 rup
        expectErrorMessage (errMsg404NoRootKey walId) rup

    describe "WALLETS_UPDATE_PASS_04 - non-existing wallets" $  do
        forM_ falseWalletIds $ \(title, walId) -> it title $ \ctx -> do
            let payload = updatePassPayload "Secure passphrase" "Secure passphrase2"
            let endpoint = "v2/wallets" </> T.pack walId </> ("passphrase" :: Text)
            rup <- request @ApiWallet ctx ("PUT", endpoint) Default payload
            expectResponseCode @IO HTTP.status404 rup
            if (title == "40 chars hex") then
                expectErrorMessage (errMsg404NoRootKey $ T.pack walId) rup
            else
                expectErrorMessage errMsg404NoEndpoint rup

    it "WALLETS_UPDATE_PASS_04 - 'almost' valid walletId" $ \ctx -> do
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default simplePayload
        let payload = updatePassPayload "Secure passphrase" "Secure passphrase2"
        let endpoint =
                "v2/wallets"
                </> (T.append (getFromResponse walletId r) "0")
                </> ("passphrase" :: Text)
        rup <- request @ApiWallet ctx ("PUT", endpoint) Default payload
        expectResponseCode @IO HTTP.status404 rup
        expectErrorMessage errMsg404NoEndpoint rup

    describe "WALLETS_UPDATE_PASS_05,06 - Transaction after updating passphrase" $ do
        let oldPass = "cardano-wallet"
        let newPass = "cardano-wallet2"
        let matrix = [ ("Old passphrase -> fail", oldPass
                       , [ expectResponseCode @IO HTTP.status403
                         , expectErrorMessage errMsg403WrongPass ] )
                     , ("New passphrase -> OK", newPass
                       , [ expectResponseCode @IO HTTP.status202 ] )
                     ]

        forM_ matrix $ \(title, pass, expectations) -> it title $ \ctx -> do
            wSrc <- fixtureWallet ctx
            wDest <- emptyWallet ctx
            let payloadUpdate = updatePassPayload oldPass newPass
            rup <- request @ApiWallet ctx (updateWalletPassEp wSrc) Default payloadUpdate
            expectResponseCode @IO HTTP.status204 rup

            addrs <- listAddresses ctx wDest
            let destination = (addrs !! 1) ^. #id
            let payloadTrans = Json [json|{
                    "payments": [{
                        "address": #{destination},
                        "amount": {
                            "quantity": 1,
                            "unit": "lovelace"
                        }
                    }],
                    "passphrase": #{pass}
                    }|]
            r <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payloadTrans
            verify r expectations

    describe "WALLETS_UPDATE_PASS_07 - HTTP headers" $ do
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
                  , ( "No Accept -> 204"
                    , Headers [ ("Content-Type", "application/json") ]
                    , [ expectResponseCode @IO HTTP.status204 ]
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
            r <- request @ApiWallet ctx ("POST", "v2/wallets") Default simplePayload
            let payload =
                    updatePassPayload "Secure passphrase" "Secure passphrase2"
            let endpoint =
                    "v2/wallets"
                    </> (getFromResponse walletId r)
                    </> ("passphrase" :: Text)
            rup <- request @ApiWallet ctx ("PUT", endpoint) headers payload
            verify rup expectations

    describe "WALLETS_UPDATE_PASS_07 - v2/wallets/{id}/passphrase - Methods Not Allowed" $ do
        let matrix = ["POST", "CONNECT", "TRACE", "OPTIONS", "GET", "DELETE"]
        forM_ matrix $ \method -> it (show method) $ \ctx -> do
            w <- emptyWallet ctx
            let endpoint = "v2/wallets/" <> w ^. walletId <> "/passphrase"
            r <- request @ApiWallet ctx (method, endpoint) Default Empty
            expectResponseCode @IO HTTP.status405 r
            expectErrorMessage errMsg405 r

    it "WALLETS_UTXO_01 - Wallet's inactivity is reflected in utxo" $ \ctx -> do
        w <- emptyWallet ctx
        rStat <- request @ApiUtxoStatistics ctx (getWalletUtxoEp w) Default Empty
        expectResponseCode @IO HTTP.status200 rStat
        expectWalletUTxO [] (snd rStat)

    it "WALLETS_UTXO_02 - Sending and receiving funds updates wallet's utxo." $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx

        --send funds
        addrs <- listAddresses ctx wDest
        let destination = (addrs !! 1) ^. #id
        let coins = [13::Natural, 43, 66, 101, 1339]
        let matrix = zip coins [1..]
        forM_ matrix $ \(c, alreadyAbsorbed) -> do
            let payload = Json [json|{
                    "payments": [{
                        "address": #{destination},
                        "amount": {
                            "quantity": #{c},
                            "unit": "lovelace"
                        }
                    }],
                    "passphrase": "cardano-wallet"
                }|]
            rTrans <- request @(ApiTransaction t) ctx (postTxEp wSrc)
                Default payload
            expectResponseCode @IO HTTP.status202 rTrans

            rGet <- request @ApiWallet ctx (getWalletEp wDest) Default Empty
            let coinsSent = map fromIntegral $ take alreadyAbsorbed coins
            verify rGet
                [ expectEventually ctx balanceTotal (fromIntegral $ sum coinsSent)
                , expectEventually ctx balanceAvailable (fromIntegral $ sum coinsSent)
                ]
            --verify utxo
            rStat1 <- request @ApiUtxoStatistics ctx (getWalletUtxoEp wDest) Default Empty
            expectResponseCode @IO HTTP.status200 rStat1
            expectWalletUTxO coinsSent (snd rStat1)

    describe "WALLETS_UTXO_03 - non-existing wallets" $  do
        forM_ falseWalletIds $ \(title, walId) -> it title $ \ctx -> do
            let endpoint =
                        "v2/wallets/"
                        <> T.pack walId
                        <> "/statistics/utxos" :: Text
            r <- request @ApiUtxoStatistics ctx ("GET", endpoint) Default Empty
            expectResponseCode @IO HTTP.status404 r
            if (title == "40 chars hex") then
                expectErrorMessage (errMsg404NoWallet $ T.pack walId) r
            else
                expectErrorMessage errMsg404NoEndpoint r

    it "WALLETS_UTXO_03 - Deleted wallet is not available for utxo" $ \ctx -> do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx (deleteWalletEp w) Default Empty

        r <- request @ApiUtxoStatistics ctx (getWalletUtxoEp w) Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    it "WALLETS_UTXO_03 - 'almost' valid walletId" $ \ctx -> do
        w <- emptyWallet ctx
        let endpoint =
                    "v2/wallets"
                    <> (T.append (w ^. walletId) "1")
                    <> "/statistics/utxos" :: Text
        r <- request @ApiUtxoStatistics ctx ("GET", endpoint) Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage errMsg404NoEndpoint r

    it "WALLETS_UTXO_03 - 'almost' valid endpoint" $ \ctx -> do
        w <- emptyWallet ctx
        let endpoint =
                    "v2/wallets"
                    <> w ^. walletId
                    <> "/statistics/utxo" :: Text
        r <- request @ApiUtxoStatistics ctx ("GET", endpoint) Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage errMsg404NoEndpoint r

    describe "WALLETS_UTXO_04 - HTTP headers" $ do
        let matrix =
                [ ( "No HTTP headers -> 200"
                  , None
                  , [ expectResponseCode @IO HTTP.status200 ] )
                , ( "Accept: text/plain -> 406"
                  , Headers
                        [ ("Content-Type", "application/json")
                        , ("Accept", "text/plain") ]
                  , [ expectResponseCode @IO HTTP.status406
                    , expectErrorMessage errMsg406 ]
                  )
                , ( "No Accept -> 200"
                  , Headers [ ("Content-Type", "application/json") ]
                  , [ expectResponseCode @IO HTTP.status200 ]
                  )
                , ( "No Content-Type -> 200"
                  , Headers [ ("Accept", "application/json") ]
                  , [ expectResponseCode @IO HTTP.status200 ]
                  )
                , ( "Content-Type: text/plain -> 200"
                  , Headers [ ("Content-Type", "text/plain") ]
                  , [ expectResponseCode @IO HTTP.status200 ]
                  )
                ]
        forM_ matrix $ \(title, headers, expectations) -> it title $ \ctx -> do
            w <- emptyWallet ctx
            r <- request @ApiUtxoStatistics ctx (getWalletUtxoEp w) headers Empty
            verify r expectations

    describe "WALLETS_UTXO_04 - v2/wallets/{id}/statistics/utxos - Methods Not Allowed" $ do
        let matrix = ["POST", "PUT", "CONNECT", "TRACE", "OPTIONS", "DELETE"]
        forM_ matrix $ \method -> it (show method) $ \ctx -> do
            w <- emptyWallet ctx
            let endpoint = "v2/wallets/" <> w ^. walletId <> "/statistics/utxos"
            r <- request @ApiUtxoStatistics ctx (method, endpoint) Default Empty
            expectResponseCode @IO HTTP.status405 r
            expectErrorMessage errMsg405 r
 where
    getHeaderCases =
              [ ( "No HTTP headers -> 200", None
                , [ expectResponseCode @IO HTTP.status200 ] )
              , ( "Accept: text/plain -> 406"
                , Headers
                      [ ("Content-Type", "application/json")
                      , ("Accept", "text/plain") ]
                , [ expectResponseCode @IO HTTP.status406
                  , expectErrorMessage errMsg406 ]
                )
              , ( "No Accept -> 200"
                , Headers [ ("Content-Type", "application/json") ]
                , [ expectResponseCode @IO HTTP.status200 ]
                )
              , ( "No Content-Type -> 200"
                , Headers [ ("Accept", "application/json") ]
                , [ expectResponseCode @IO HTTP.status200 ]
                )
              , ( "Content-Type: text/plain -> 200"
                , Headers [ ("Content-Type", "text/plain") ]
                , [ expectResponseCode @IO HTTP.status200 ]
                )
              ]
