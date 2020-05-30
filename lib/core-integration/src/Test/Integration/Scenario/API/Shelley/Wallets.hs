{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.API.Shelley.Wallets
    ( spec
    ) where

import Prelude

import Cardano.Mnemonic
    ( ConsistentEntropy
    , EntropySize
    , MnemonicWords
    , ValidChecksumSize
    , ValidEntropySize
    , entropyToMnemonic
    , genEntropy
    , mnemonicToText
    )
import Cardano.Wallet.Api.Types
    ( AddressAmount (..)
    , ApiByronWallet
    , ApiCoinSelection
    , ApiNetworkInformation
    , ApiT (..)
    , ApiTransaction
    , ApiUtxoStatistics
    , ApiWallet
    , ApiWalletMigrationInfo (..)
    , DecodeAddress
    , EncodeAddress (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( PassphraseMaxLength (..), PassphraseMinLength (..), PaymentAddress )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap (..) )
import Cardano.Wallet.Primitive.Types
    ( SyncProgress (..), walletNameMaxLength, walletNameMinLength )
import Control.Monad
    ( forM_ )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.List.NonEmpty
    ( NonEmpty ((:|)) )
import Data.Maybe
    ( mapMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( toText )
import Data.Word
    ( Word64 )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith
    , describe
    , it
    , pendingWith
    , runIO
    , shouldBe
    , shouldNotBe
    , shouldSatisfy
    )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , emptyByronWalletWith
    , emptyIcarusWallet
    , emptyRandomWallet
    , emptyWallet
    , emptyWalletWith
    , eventually
    , expectErrorMessage
    , expectField
    , expectListField
    , expectListSize
    , expectResponseCode
    , expectWalletUTxO
    , fixtureIcarusWallet
    , fixturePassphrase
    , fixtureRandomWallet
    , fixtureWallet
    , getFromResponse
    , json
    , listAddresses
    , notDelegating
    , request
    , selectCoins
    , shelleyAddresses
    , unsafeRequest
    , verify
    , walletId
    , (.>)
    , (</>)
    )
import Test.Integration.Framework.TestData
    ( arabicWalletName
    , errMsg400ParseError
    , errMsg403NothingToMigrate
    , errMsg403WrongPass
    , errMsg404NoWallet
    , errMsg406
    , errMsg415
    , kanjiWalletName
    , mnemonics12
    , mnemonics15
    , mnemonics18
    , mnemonics21
    , mnemonics24
    , mnemonics9
    , payloadWith
    , polishWalletName
    , russianWalletName
    , simplePayload
    , updateNamePayload
    , updatePassPayload
    , wildcardsWalletName
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Cardano.Wallet.Api.Types as ApiTypes
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP


spec :: forall n t.
    ( DecodeAddress n
    , EncodeAddress n
    , PaymentAddress n ShelleyKey
    ) => SpecWith (Context t)
spec = do
    it "WALLETS_CREATE_01 - Create a wallet" $ \ctx -> do
        let payload = Json [json| {
                "name": "1st Wallet",
                "mnemonic_sentence": #{mnemonics15},
                "mnemonic_second_factor": #{mnemonics12},
                "passphrase": "Secure Passphrase",
                "address_pool_gap": 30
                } |]
        r <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default payload
        verify r
            [ expectResponseCode @IO HTTP.status201
            , expectField
                    (#name . #getApiT . #getWalletName) (`shouldBe` "1st Wallet")
            , expectField
                    (#addressPoolGap . #getApiT . #getAddressPoolGap) (`shouldBe` 30)
            , expectField (#balance . #getApiT . #available) (`shouldBe` Quantity 0)
            , expectField (#balance . #getApiT . #total) (`shouldBe` Quantity 0)
            , expectField (#balance . #getApiT . #reward) (`shouldBe` Quantity 0)

            , expectField #delegation (`shouldBe` notDelegating [])
            , expectField
                    walletId (`shouldBe` "2cf060fe53e4e0593f145f22b858dfc60676d4ab")
            , expectField #passphrase (`shouldNotBe` Nothing)
            ]
        let wid = getFromResponse id r
        eventually "Wallet state = Ready" $ do
            rg <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wid) Default Empty
            expectField (#state . #getApiT) (`shouldBe` Ready) rg

    describe "OWASP_INJECTION_CREATE_WALLET_01 - \
             \SQL injection when creating a wallet" $  do
        let mnemonics =
                [ "pulp", "ten", "light", "rhythm", "replace"
                , "vessel", "slow", "drift", "kingdom", "amazing"
                , "negative", "join", "auction", "ugly", "symptom"] :: [Text]
        let matrix =
                [ ( "new wallet\",'',''); DROP TABLE \"wallet\"; --"
                  , "new wallet\",'',''); DROP TABLE \"wallet\"; --"
                  )
                , ( "new wallet','ŚεℒℇℂƮ’','ŚεℒℇℂƮ’'); DROP TABLE \"wallet\"; --"
                  , "new wallet','\346\949\8466\8455\8450\430\8217',\
                    \'\346\949\8466\8455\8450\430\8217'); DROP TABLE \"wallet\"; --"
                  ) ]
        forM_ matrix $ \(nameIn, nameOut) -> it nameIn $ \ctx -> do
            let payload = Json [json| {
                    "name": #{nameIn},
                    "mnemonic_sentence": #{mnemonics},
                    "passphrase": "12345678910"
                    } |]
            let postWallet = Link.postWallet @'Shelley
            r <- request @ApiWallet ctx postWallet Default payload
            verify r
                [ expectResponseCode @IO HTTP.status201
                , expectField
                    (#name . #getApiT . #getWalletName) (`shouldBe` nameOut)
                , expectField
                    (#addressPoolGap . #getApiT . #getAddressPoolGap) (`shouldBe` 20)
                , expectField
                    (#balance . #getApiT . #available) (`shouldBe` Quantity 0)
                , expectField
                    (#balance . #getApiT . #total) (`shouldBe` Quantity 0)
                , expectField
                    (#balance . #getApiT . #reward) (`shouldBe` Quantity 0)
                , expectField #delegation (`shouldBe` notDelegating [])
                , expectField walletId
                    (`shouldBe` "135bfb99b9f7a0c702bf8c658cc0d9b1a0d797a2")
                , expectField #passphrase (`shouldNotBe` Nothing)
                ]
            let listWallets = Link.listWallets @'Shelley
            eventually "listed wallet's state = Ready" $ do
                rl <- request @[ApiWallet] ctx listWallets Default Empty
                verify rl
                    [ expectResponseCode @IO HTTP.status200
                    , expectListSize 1
                    , expectListField 0 (#state . #getApiT) (`shouldBe` Ready)
                    ]

    it "WALLETS_CREATE_02 - Restored wallet preserves funds" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        -- create wallet
        mnemonics <- mnemonicToText @15 . entropyToMnemonic <$> genEntropy
        let payldCrt = payloadWith "!st created" mnemonics
        rInit <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default payldCrt
        verify rInit
            [ expectResponseCode @IO HTTP.status201
            , expectField (#balance . #getApiT . #available) (`shouldBe` Quantity 0)
            , expectField (#balance . #getApiT . #total) (`shouldBe` Quantity 0)
            ]

        --send funds
        let wDest = getFromResponse id rInit
        addrs <- listAddresses @n ctx wDest
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
        rDel <- request @ApiWallet ctx (Link.deleteWallet @'Shelley wDest) Default Empty
        expectResponseCode @IO HTTP.status204 rDel

        -- restore and make sure funds are there
        rRestore <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default payldCrt
        expectResponseCode @IO HTTP.status201 rRestore
        eventually "Wallet balance is ok on restored wallet" $ do
            rGet <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            verify rGet
                [ expectField
                        (#balance . #getApiT . #total) (`shouldBe` Quantity 1)
                , expectField
                        (#balance . #getApiT . #available) (`shouldBe` Quantity 1)
                ]

    it "WALLETS_CREATE_03,09 - Cannot create wallet that exists" $ \ctx -> do
        let payload = Json [json| {
                "name": "Some Wallet",
                "mnemonic_sentence": #{mnemonics21},
                "passphrase": "Secure Passphrase"
                } |]
        r1 <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default payload
        expectResponseCode @IO HTTP.status201 r1

        r2 <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default payload
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
                  , [ expectResponseCode @IO HTTP.status201
                    , expectField
                            (#name . #getApiT . #getWalletName) (`shouldBe` "1")
                    ]
                  )
                , ( show walletNameMaxLength ++ " char long", walNameMax
                  , [ expectResponseCode @IO HTTP.status201
                    , expectField
                            (#name . #getApiT . #getWalletName) (`shouldBe` walNameMax)
                    ]
                  )
                , ( "Russian name", russianWalletName
                  , [ expectResponseCode @IO HTTP.status201
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` russianWalletName)
                    ]
                  )
                , ( "Polish name", polishWalletName
                  , [ expectResponseCode @IO HTTP.status201
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` polishWalletName)
                    ]
                  )
                , ( "Kanji name", kanjiWalletName
                  , [ expectResponseCode @IO HTTP.status201
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` kanjiWalletName)
                    ]
                  )
                , ( "Arabic name", arabicWalletName
                  , [ expectResponseCode @IO HTTP.status201
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` arabicWalletName)
                    ]
                  )
                , ( "Wildcards name", wildcardsWalletName
                  , [ expectResponseCode @IO HTTP.status201
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` wildcardsWalletName)
                    ]
                  )
                ]
        forM_ matrix $ \(title, walName, expectations) -> it title $ \ctx -> do
            let payload = Json [json| {
                    "name": #{walName},
                    "mnemonic_sentence": #{mnemonics24},
                    "passphrase": "Secure Passphrase"
                    } |]
            r <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default payload
            verify r expectations

    describe "WALLETS_CREATE_05 - Mnemonics" $ do
        let matrix =
             [ ( "15 mnemonic words", mnemonics15
               , [ expectResponseCode @IO HTTP.status201
                 , expectField walletId
                    (`shouldBe` "b062e8ccf3685549b6c489a4e94966bc4695b75b")
                 ]
               )
             , ( "18 mnemonic words", mnemonics18
               , [ expectResponseCode @IO HTTP.status201
                 , expectField walletId
                    (`shouldBe` "f52ee0daaefd75a0212d70c9fbe15ee8ada9fc11")
                 ]
               )
             , ( "21 mnemonic words" , mnemonics21
               , [ expectResponseCode @IO HTTP.status201
                 , expectField walletId
                    (`shouldBe` "7e8c1af5ff2218f388a313f9c70f0ff0550277e4")
                 ]
               )
             , ( "24 mnemonic words", mnemonics24
               , [ expectResponseCode @IO HTTP.status201
                 , expectField walletId
                    (`shouldBe` "a6b6625cd2bfc51a296b0933f77020991cc80374")
                 ]
               )
             ]

        forM_ matrix $ \(title, mnemonics, expectations) -> it title $ \ctx -> do
            let payload = Json [json| {
                    "name": "Just a łallet",
                    "mnemonic_sentence": #{mnemonics},
                    "passphrase": "Secure Passphrase"
                    } |]
            r <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default payload
            verify r expectations

    describe "WALLETS_CREATE_06 - Mnemonics second factor" $ do
        let matrix =
                 [ ( "9 mnemonic words", mnemonics9
                   , [ expectResponseCode @IO HTTP.status201
                     , expectField walletId
                        (`shouldBe` "4b1a865e39d1006efb99f538b05ea2343b567108")
                     ]
                   )
                 , ( "12 mnemonic words", mnemonics12
                   , [ expectResponseCode @IO HTTP.status201
                     , expectField walletId
                        (`shouldBe` "2cf060fe53e4e0593f145f22b858dfc60676d4ab")
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
            r <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default payload
            verify r expectations

    describe "WALLETS_CREATE_07 - Passphrase" $ do
        let minLength = passphraseMinLength (Proxy @"raw")
        let maxLength = passphraseMaxLength (Proxy @"raw")
        let matrix =
                [ ( show minLength ++ " char long"
                  , T.pack (replicate minLength 'ź')
                  , [ expectResponseCode @IO HTTP.status201
                    ]
                  )
                , ( show maxLength ++ " char long"
                , T.pack (replicate maxLength 'ą')
                  , [ expectResponseCode @IO HTTP.status201 ]
                  )
                , ( "Russian passphrase", russianWalletName
                  , [ expectResponseCode @IO HTTP.status201 ]
                  )
                , ( "Polish passphrase", polishWalletName
                  , [ expectResponseCode @IO HTTP.status201 ]
                  )
                , ( "Kanji passphrase", kanjiWalletName
                  , [ expectResponseCode @IO HTTP.status201 ]
                  )
                , ( "Arabic passphrase", arabicWalletName
                  , [ expectResponseCode @IO HTTP.status201 ]
                  )
                , ( "Wildcards passphrase", wildcardsWalletName
                  , [ expectResponseCode @IO HTTP.status201 ]
                  )
                ]
        forM_ matrix $ \(title, passphrase, expectations) -> it title $ \ctx -> do
            let payload = Json [json| {
                    "name": "Secure Wallet",
                    "mnemonic_sentence": #{mnemonics24},
                    "passphrase": #{passphrase}
                    } |]
            r <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default payload
            verify r expectations

    describe "WALLETS_CREATE_08 - address_pool_gap" $ do
        let addrPoolMin = fromIntegral @_ @Int $ getAddressPoolGap minBound
        let addrPoolMax = fromIntegral @_ @Int $ getAddressPoolGap maxBound
        let matrix =
                [ ( show addrPoolMin
                  , addrPoolMin
                  , [ expectResponseCode @IO HTTP.status201
                    , expectField (#addressPoolGap . #getApiT) (`shouldBe` minBound)
                    ]
                  )
                , ( show addrPoolMax
                  , addrPoolMax
                  , [ expectResponseCode @IO HTTP.status201 ]
                  )
                ]
        forM_ matrix $ \(title, addrPoolGap, expectations) -> it title $ \ctx -> do
            let payload = Json [json| {
                    "name": "Secure Wallet",
                    "mnemonic_sentence": #{mnemonics24},
                    "passphrase": "Secure passphrase",
                    "address_pool_gap": #{addrPoolGap}
                    } |]
            r <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default payload
            verify r expectations

    it "WALLETS_CREATE_08 - default address_pool_gap" $ \ctx -> do
        let payload = Json [json| {
                "name": "Secure Wallet",
                "mnemonic_sentence": #{mnemonics21},
                "passphrase": "Secure passphrase"
                } |]
        r <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default payload
        verify r
            [ expectResponseCode @IO HTTP.status201
            , expectField
                    (#addressPoolGap . #getApiT . #getAddressPoolGap) (`shouldBe` 20)
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
                 , ( "No Accept -> 201"
                   , Headers [ ("Content-Type", "application/json") ]
                   , [ expectResponseCode @IO HTTP.status201 ]
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
            r <- request @ApiWallet ctx (Link.postWallet @'Shelley) headers payload
            verify r expectations

    it "WALLETS_GET_01 - can get wallet details" $ \ctx -> do
        (_, w) <- unsafeRequest @ApiWallet ctx (Link.postWallet @'Shelley) simplePayload

        eventually "I can get all wallet details" $ do
            rg <- request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty
            verify rg
                [ expectResponseCode @IO HTTP.status200
                , expectField
                        (#name . #getApiT . #getWalletName) (`shouldBe` "Secure Wallet")
                , expectField
                        (#addressPoolGap . #getApiT . #getAddressPoolGap) (`shouldBe` 20)
                , expectField
                        (#balance . #getApiT . #available) (`shouldBe` Quantity 0)
                , expectField
                        (#balance . #getApiT . #total) (`shouldBe` Quantity 0)
                , expectField
                        (#balance . #getApiT . #reward) (`shouldBe` Quantity 0)
                , expectField (#state . #getApiT) (`shouldBe` Ready)
                , expectField #delegation (`shouldBe` notDelegating [])
                , expectField walletId (`shouldBe` w ^. walletId)
                , expectField #passphrase (`shouldNotBe` Nothing)
                ]

    it "WALLETS_GET_02, WALLETS_DELETE_01 - Deleted wallet is not available" $ \ctx -> do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx
            (Link.deleteWallet @'Shelley w) Default Empty
        rg <- request @ApiWallet ctx
            (Link.getWallet @'Shelley w) Default Empty
        expectResponseCode @IO HTTP.status404 rg
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) rg

    it "WALLETS_LIST_01 - Created a wallet can be listed" $ \ctx -> do
        let payload = Json [json| {
                "name": "Wallet to be listed",
                "mnemonic_sentence": #{mnemonics18},
                "mnemonic_second_factor": #{mnemonics9},
                "passphrase": "Secure Passphrase",
                "address_pool_gap": 20
                } |]
        _ <- unsafeRequest @ApiWallet ctx (Link.postWallet @'Shelley) payload
        rl <- request @[ApiWallet] ctx (Link.listWallets @'Shelley) Default Empty
        verify rl
            [ expectResponseCode @IO HTTP.status200
            , expectListSize 1
            , expectListField 0
                    (#name . #getApiT . #getWalletName)
                    (`shouldBe` "Wallet to be listed")
            , expectListField 0
                    (#addressPoolGap . #getApiT . #getAddressPoolGap) (`shouldBe` 20)
            , expectListField 0
                    (#balance . #getApiT . #available) (`shouldBe` Quantity 0)
            , expectListField 0
                    (#balance . #getApiT . #total) (`shouldBe` Quantity 0)
            , expectListField 0
                    (#balance . #getApiT . #reward) (`shouldBe` Quantity 0)
            , expectListField 0 #delegation (`shouldBe` notDelegating [])
            , expectListField 0 walletId
                    (`shouldBe` "dfe87fcf0560fb57937a6468ea51e860672fad79")
            ]

    it "WALLETS_LIST_01 - Wallets are listed from oldest to newest" $ \ctx -> do
        let walletDetails = [("1", mnemonics15), ("2", mnemonics18)
                    , ("3", mnemonics21)]
        forM_ walletDetails $ \(name, mnemonics) -> do
            let payload = payloadWith name mnemonics
            request @ApiWallet ctx (Link.postWallet @'Shelley) Default payload

        rl <- request @[ApiWallet] ctx (Link.listWallets @'Shelley) Default Empty
        verify rl
            [ expectResponseCode @IO HTTP.status200
            , expectListSize 3
            , expectListField 0
                (#name . #getApiT . #getWalletName) (`shouldBe` "1")
            , expectListField 1
                (#name . #getApiT . #getWalletName) (`shouldBe` "2")
            , expectListField 2
                (#name . #getApiT . #getWalletName) (`shouldBe` "3")
            ]

    it "WALLETS_LIST_02 - Deleted wallet not listed" $ \ctx -> do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
        rl <- request @[ApiWallet] ctx (Link.listWallets @'Shelley) Default Empty
        verify rl
            [ expectResponseCode @IO HTTP.status200
            , expectListSize 0
            ]

    it "WALLETS_UPDATE_01 - Updated wallet name is available" $ \ctx -> do

        r <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default simplePayload
        let passLastUpdateValue = getFromResponse #passphrase r
        let newName = updateNamePayload "New great name"
        let walId = getFromResponse walletId r
        let expectations = [ expectResponseCode @IO HTTP.status200
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` "New great name")
                    , expectField
                            (#addressPoolGap . #getApiT . #getAddressPoolGap)
                            (`shouldBe` 20)
                    , expectField
                            (#balance . #getApiT . #available) (`shouldBe` Quantity 0)
                    , expectField
                            (#balance . #getApiT . #total) (`shouldBe` Quantity 0)
                    , expectField (#state . #getApiT) (`shouldBe` Ready)
                    , expectField #delegation (`shouldBe` notDelegating [])
                    , expectField walletId (`shouldBe` walId)
                    , expectField #passphrase (`shouldBe` passLastUpdateValue)
                    ]
        eventually "Updated wallet name is available" $ do
            ru <- request @ApiWallet ctx
                ("PUT", "v2/wallets" </> walId) Default newName
            verify ru expectations
            rg <- request @ApiWallet ctx
                ("GET", "v2/wallets" </> walId) Default Empty
            verify rg expectations
            rl <- request @[ApiWallet] ctx ("GET", "v2/wallets") Default Empty
            verify rl
                [ expectResponseCode @IO HTTP.status200
                , expectListSize 1
                , expectListField 0
                        (#name . #getApiT . #getWalletName) (`shouldBe` "New great name")
                , expectListField 0
                        (#addressPoolGap . #getApiT . #getAddressPoolGap) (`shouldBe` 20)
                , expectListField 0
                        (#balance . #getApiT . #available) (`shouldBe` Quantity 0)
                , expectListField 0
                        (#balance . #getApiT . #total) (`shouldBe` Quantity 0)
                , expectListField 0 (#state . #getApiT) (`shouldBe` Ready)
                , expectListField 0 #delegation (`shouldBe` notDelegating [])
                , expectListField 0 walletId (`shouldBe` walId)
                , expectListField 0 #passphrase (`shouldBe` passLastUpdateValue)
                ]

    describe "WALLETS_UPDATE_02 - Various names" $ do
        let walNameMax = T.pack (replicate walletNameMaxLength 'ą')
        let matrix =
                [ ( show walletNameMinLength ++ " char long", "1"
                  , [ expectResponseCode @IO HTTP.status200
                    , expectField
                            (#name . #getApiT . #getWalletName) (`shouldBe` "1")
                    ]
                  )
                , ( show walletNameMaxLength ++ " char long", walNameMax
                  , [ expectResponseCode @IO HTTP.status200
                    , expectField
                            (#name . #getApiT . #getWalletName) (`shouldBe` walNameMax)
                    ]
                  )
                , ( "Russian name", russianWalletName
                  , [ expectResponseCode @IO HTTP.status200
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` russianWalletName)
                    ]
                  )
                , ( "Polish name", polishWalletName
                  , [ expectResponseCode @IO HTTP.status200
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` polishWalletName)
                    ]
                  )
                , ( "Kanji name", kanjiWalletName
                  , [ expectResponseCode @IO HTTP.status200
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` kanjiWalletName)
                    ]
                  )
                , ( "Arabic name", arabicWalletName
                  , [ expectResponseCode @IO HTTP.status200
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` arabicWalletName)
                    ]
                  )
                , ( "Wildcards name", wildcardsWalletName
                  , [ expectResponseCode @IO HTTP.status200
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` wildcardsWalletName)
                    ]
                  )
                ]
        forM_ matrix $ \(title, walName, expectations) -> it title $ \ctx -> do
            r <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default simplePayload
            let newName = updateNamePayload walName
            let endpoint = "v2/wallets" </> (getFromResponse walletId r)
            ru <- request @ApiWallet ctx ("PUT", endpoint) Default newName
            verify ru expectations

    it "WALLETS_UPDATE_03 - Deleted wallet cannot be updated (404)" $ \ctx -> do
        r <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default simplePayload
        let wid = getFromResponse walletId r
        let endpoint = "v2/wallets" </> wid
        _ <- request @ApiWallet ctx ("DELETE", endpoint) Default Empty

        let newName = updateNamePayload "new name"
        ru <- request @ApiWallet ctx ("PUT", endpoint) Default newName
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
            r <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default simplePayload
            let newName = updateNamePayload "new name"
            let endpoint = "v2/wallets" </> (getFromResponse walletId r)
            ru <- request @ApiWallet ctx ("PUT", endpoint) headers newName
            verify ru expectations

    it "WALLETS_UPDATE_PASS_01 - passphaseLastUpdate gets updated" $ \ctx -> do
        r <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default simplePayload
        let payload = updatePassPayload "Secure passphrase" "New passphrase"
        let endpoint = "v2/wallets" </> (getFromResponse walletId r)
                </> ("passphrase" :: Text)
        rup <- request @ApiWallet ctx ("PUT", endpoint) Default payload
        expectResponseCode @IO HTTP.status204 rup

        let getEndpoint = "v2/wallets" </> (getFromResponse walletId r)
        let originalPassUpdateDateTime = getFromResponse #passphrase r
        rg <- request @ApiWallet ctx ("GET", getEndpoint) Default Empty
        expectField #passphrase (`shouldNotBe` originalPassUpdateDateTime) rg

    describe "WALLETS_UPDATE_PASS_02 - New passphrase values" $ do
        let minLength = passphraseMinLength (Proxy @"raw")
        let maxLength = passphraseMaxLength (Proxy @"raw")
        let matrix =
                [ ( show minLength ++ " char long"
                  , T.pack (replicate minLength 'ź')
                  , [ expectResponseCode @IO HTTP.status204
                    ]
                  )
                , ( show maxLength ++ " char long"
                  , T.pack (replicate maxLength 'ą')
                  , [ expectResponseCode @IO HTTP.status204 ]
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
            r <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default simplePayload
            let payload = updatePassPayload "Secure passphrase" passphrase
            let endpoint = "v2/wallets" </> (getFromResponse walletId r)
                    </> ("passphrase" :: Text)
            rup <- request @ApiWallet ctx ("PUT", endpoint) Default payload
            verify rup expectations

    it "WALLETS_UPDATE_PASS_03 - Old passphrase incorrect" $ \ctx -> do
        w <- emptyWalletWith ctx
            ("Wallet to update pass", "cardano-passphrase", 20)
        let payload = updatePassPayload "incorrect-passphrase" "whatever-pass"
        rup <- request @ApiWallet ctx
            (Link.putWalletPassphrase @'Shelley w) Default payload
        expectResponseCode @IO HTTP.status403 rup
        expectErrorMessage errMsg403WrongPass rup

    describe "WALLETS_UPDATE_PASS_03 - Can update pass from pass that's boundary\
    \ value" $ do
        let minLength = passphraseMinLength (Proxy @"raw")
        let maxLength = passphraseMaxLength (Proxy @"raw")
        let matrix =
                [ ( show minLength ++ " char long"
                  , T.pack (replicate minLength 'ź') )
                , ( show maxLength ++ " char long"
                  , T.pack (replicate maxLength 'ą') )
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
            (_, w) <- unsafeRequest @ApiWallet ctx
                (Link.postWallet @'Shelley) createPayload
            let len = passphraseMaxLength (Proxy @"raw")
            let newPass = T.pack $ replicate len '💘'
            let payload = updatePassPayload oldPass newPass
            rup <- request @ApiWallet ctx
                (Link.putWalletPassphrase @'Shelley w) Default payload
            expectResponseCode @IO HTTP.status204 rup

    it "WALLETS_UPDATE_PASS_04 - Deleted wallet is not available" $ \ctx -> do
        r <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default simplePayload
        let payload = updatePassPayload "Secure passphrase" "Secure passphrase2"
        let walId = getFromResponse walletId r
        let delEndp = "v2/wallets" </> walId
        _ <- request @ApiWallet ctx ("DELETE", delEndp) Default Empty
        let updEndp = delEndp </> ("passphrase" :: Text)
        rup <- request @ApiWallet ctx ("PUT", updEndp) Default payload
        expectResponseCode @IO HTTP.status404 rup
        expectErrorMessage (errMsg404NoWallet walId) rup

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
            rup <- request @ApiWallet ctx
                   (Link.putWalletPassphrase @'Shelley wSrc) Default payloadUpdate
            expectResponseCode @IO HTTP.status204 rup

            addrs <- listAddresses @n ctx wDest
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
            r <- request @(ApiTransaction n) ctx
                (Link.createTransaction @'Shelley wSrc) Default payloadTrans
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
                    , [ expectResponseCode @IO HTTP.status204 ]
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
            (_, w) <- unsafeRequest @ApiWallet ctx (Link.postWallet @'Shelley) simplePayload
            let payload = updatePassPayload "Secure passphrase" "Passphrase"
            let endpoint = Link.putWalletPassphrase @'Shelley w
            rup <- request @ApiWallet ctx endpoint headers payload
            verify rup expectations

    it "WALLETS_COIN_SELECTION_01 - \
        \A singleton payment is included in the coin selection output." $
        \ctx -> do
            source <- fixtureWallet ctx
            target <- emptyWallet ctx
            targetAddress : _ <- fmap (view #id) <$> listAddresses @n ctx target
            let amount = Quantity 1
            let payment = AddressAmount targetAddress amount
            selectCoins ctx source (payment :| []) >>= flip verify
                [ expectResponseCode HTTP.status200
                , expectField #inputs (`shouldSatisfy` (not . null))
                , expectField #outputs (`shouldSatisfy` ((> 1) . length))
                , expectField #outputs (`shouldSatisfy` (payment `elem`))
                ]

    let satisfy = flip shouldSatisfy
    it "WALLETS_COIN_SELECTION_02 - \
        \Multiple payments are all included in the coin selection output." $
        \ctx -> do
            let paymentCount = 10
            source <- fixtureWallet ctx
            target <- emptyWallet ctx
            targetAddresses <- fmap (view #id) <$> listAddresses @n ctx target
            let amounts = Quantity <$> [1 ..]
            let payments = NE.fromList
                    $ take paymentCount
                    $ zipWith AddressAmount targetAddresses amounts
            selectCoins ctx source payments >>= flip verify
                [ expectResponseCode
                    HTTP.status200
                , expectField
                    #inputs (`shouldSatisfy` (not . null))
                , expectField
                    #outputs (satisfy $ (> paymentCount) . length)
                , expectField
                    #outputs (satisfy $ flip all payments . flip elem)
                ]

    it "WALLETS_COIN_SELECTION_03 - \
        \Deleted wallet is not available for selection" $ \ctx -> do
        w <- emptyWallet ctx
        (addr:_) <- fmap (view #id) <$> listAddresses @n ctx w
        let payments = NE.fromList [ AddressAmount addr (Quantity 1) ]
        _ <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
        selectCoins ctx w payments >>= flip verify
            [ expectResponseCode @IO HTTP.status404
            , expectErrorMessage (errMsg404NoWallet $ w ^. walletId)
            ]

    it "WALLETS_COIN_SELECTION_03 - \
        \Wrong selection method (not 'random')" $ \ctx -> do
        w <- fixtureWallet ctx
        (addr:_) <- fmap (view #id) <$> listAddresses @n ctx w
        let payments = NE.fromList [ AddressAmount addr (Quantity 1) ]
        let payload = Json [json| { "payments": #{payments} } |]
        let wid = toText $ getApiT $ w ^. #id
        let endpoints = ("POST",) . mconcat <$>
                [ [ "v2/wallets/", wid, "/coin-selections/largest-first" ]
                , [ "v2/wallets/", wid, "/coin-selections" ]
                ]
        forM_ endpoints $ \endpoint -> do
            r <- request @(ApiCoinSelection n) ctx endpoint Default payload
            verify r [ expectResponseCode @IO HTTP.status404 ]

    describe "WALLETS_COIN_SELECTION_04 - HTTP headers" $ do
        let matrix =
                [ ( "No HTTP headers -> 415"
                  , None
                  , [ expectResponseCode @IO HTTP.status415
                    , expectErrorMessage errMsg415
                    ]
                  )
                , ( "Accept: text/plain -> 406"
                  , Headers
                        [ ("Content-Type", "application/json")
                        , ("Accept", "text/plain")
                        ]
                  , [ expectResponseCode @IO HTTP.status406
                    , expectErrorMessage errMsg406
                    ]
                  )
                , ( "No Accept -> 200"
                  , Headers [ ("Content-Type", "application/json") ]
                  , [ expectResponseCode @IO HTTP.status200 ]
                  )
                , ( "No Content-Type -> 415"
                  , Headers [ ("Accept", "application/json") ]
                  , [ expectResponseCode @IO HTTP.status415
                    , expectErrorMessage errMsg415
                    ]
                  )
                , ( "Content-Type: text/plain -> 415"
                  , Headers [ ("Content-Type", "text/plain") ]
                  , [ expectResponseCode @IO HTTP.status415
                    , expectErrorMessage errMsg415
                    ]
                  )
                ]
        forM_ matrix $ \(title, headers, expectations) -> it title $ \ctx -> do
            w <- fixtureWallet ctx
            (addr:_) <- fmap (view #id) <$> listAddresses @n ctx w
            let payments = NE.fromList [ AddressAmount addr (Quantity 1) ]
            let payload = Json [json| { "payments": #{payments} } |]
            r <- request @(ApiCoinSelection n) ctx (Link.selectCoins w) headers payload
            verify r expectations

    it "WALLETS_UTXO_01 - Wallet's inactivity is reflected in utxo" $ \ctx -> do
        w <- emptyWallet ctx
        rStat <- request @ApiUtxoStatistics ctx
                 (Link.getUTxOsStatistics @'Shelley w) Default Empty
        expectResponseCode @IO HTTP.status200 rStat
        expectWalletUTxO [] (snd rStat)

    it "WALLETS_UTXO_02 - Sending and receiving funds updates wallet's utxo." $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx

        --send funds
        addrs <- listAddresses @n ctx wDest
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
            rTrans <- request @(ApiTransaction n) ctx
                (Link.createTransaction @'Shelley wSrc) Default payload
            expectResponseCode @IO HTTP.status202 rTrans

            let coinsSent = map fromIntegral $ take alreadyAbsorbed coins
            eventually "Wallet balance is as expected" $ do
                rGet <- request @ApiWallet ctx
                    (Link.getWallet @'Shelley wDest) Default Empty
                verify rGet
                    [ expectField
                            (#balance . #getApiT . #total)
                            (`shouldBe` Quantity (fromIntegral $ sum coinsSent))
                    , expectField
                            (#balance . #getApiT . #available)
                            (`shouldBe` Quantity (fromIntegral $ sum coinsSent))
                    ]

            --verify utxo
            rStat1 <- request @ApiUtxoStatistics ctx
                (Link.getUTxOsStatistics @'Shelley wDest) Default Empty
            expectResponseCode @IO HTTP.status200 rStat1
            expectWalletUTxO coinsSent (snd rStat1)

    it "WALLETS_UTXO_03 - Deleted wallet is not available for utxo" $ \ctx -> do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w)
            Default Empty
        r <- request @ApiUtxoStatistics ctx (Link.getUTxOsStatistics @'Shelley w)
            Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

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
            r <- request @ApiUtxoStatistics ctx (Link.getUTxOsStatistics @'Shelley w) headers Empty
            verify r expectations

    it "BYRON_WALLETS_UTXO -\
        \ Cannot show Byron wal utxo with shelley ep (404)" $ \ctx -> do
        w <- emptyRandomWallet ctx
        let wid = w ^. walletId
        let endpoint =
                    "v2/wallets"
                    </> wid
                    </> ("statistics/utxos" :: Text)
        r <- request @ApiUtxoStatistics ctx ("GET", endpoint) Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet wid) r

    it "BYRON_WALLETS_UPDATE_PASS -\
        \ Cannot update Byron wal with shelley ep (404)" $ \ctx -> do
        w <- emptyRandomWallet ctx
        let payload = updatePassPayload "Secure passphrase" "Secure passphrase2"
        let wid = w ^. walletId
        let endpoint =
                "v2/wallets"
                </> wid
                </> ("passphrase" :: Text)
        rup <- request @ApiWallet ctx ("PUT", endpoint) Default payload
        expectResponseCode @IO HTTP.status404 rup
        expectErrorMessage (errMsg404NoWallet wid) rup

    it "BYRON_WALLETS_UPDATE -\
        \ Cannot update Byron wal with shelley ep (404)" $ \ctx -> do
        w <- emptyRandomWallet ctx
        let wid = w ^. walletId
        let endpoint = "v2/wallets" </> wid
        let newName = updateNamePayload "new name"
        ru <- request @ApiWallet ctx ("PUT", endpoint) Default newName
        expectResponseCode @IO HTTP.status404 ru
        expectErrorMessage (errMsg404NoWallet wid) ru

    describe "BYRON_MIGRATE_05 -\
        \ migrating from inappropriate wallet types" $ do
        addr <- runIO
            $ encodeAddress @n . head . shelleyAddresses @n
            . entropyToMnemonic @15 <$> genEntropy

        it "Byron" $ \ctx -> do
            sWallet <- emptyRandomWallet ctx
            rDel <- request @ApiWallet ctx
                (Link.deleteWallet @'Byron sWallet) Default Empty
            expectResponseCode @IO HTTP.status204 rDel
            r <- request @[ApiTransaction n] ctx
                (Link.migrateWallet sWallet)
                Default
                (Json [json|
                    { passphrase: #{fixturePassphrase}
                    , addresses: [#{addr}]
                    }|])
            verify r
                [ expectResponseCode @IO HTTP.status404
                , expectErrorMessage (errMsg404NoWallet $ sWallet ^. walletId)
                ]

        it "Icarus" $ \ctx -> do
            sWallet <- emptyIcarusWallet ctx
            rDel <- request @ApiWallet ctx
                (Link.deleteWallet @'Byron sWallet) Default Empty
            expectResponseCode @IO HTTP.status204 rDel
            r <- request @[ApiTransaction n] ctx
                (Link.migrateWallet sWallet)
                Default
                (Json [json|
                    { passphrase: #{fixturePassphrase}
                    , addresses: [#{addr}]
                    }|])
            verify r
                [ expectResponseCode @IO HTTP.status404
                , expectErrorMessage (errMsg404NoWallet $ sWallet ^. walletId)
                ]

        it "Shelley" $ \ctx -> do
            sWallet <- fixtureWallet ctx
            rDel <- request @ApiWallet ctx
                (Link.deleteWallet @'Shelley sWallet) Default Empty
            expectResponseCode @IO HTTP.status204 rDel
            r <- request @[ApiTransaction n] ctx
                (Link.migrateWallet sWallet)
                Default
                (Json [json|
                    { passphrase: #{fixturePassphrase}
                    , addresses: [#{addr}]
                    }|])
            verify r
                [ expectResponseCode @IO HTTP.status404
                , expectErrorMessage (errMsg404NoWallet $ sWallet ^. walletId)
                ]

    it "BYRON_MIGRATE_07 - invalid payload, parser error" $ \ctx -> do
        sourceWallet <- emptyRandomWallet ctx

        r <- request @[ApiTransaction n] ctx
            (Link.migrateWallet sourceWallet )
            Default
            (NonJson "{passphrase:,}")
        expectResponseCode @IO HTTP.status400 r
        expectErrorMessage errMsg400ParseError r

    it "BYRON_GET_02 - Byron ep does not show Shelley wallet" $ \ctx -> do
        w <- emptyWallet ctx
        r <- request @ApiByronWallet ctx
            (Link.getWallet @'Byron w) Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    it "BYRON_GET_03 - Shelley ep does not show Byron wallet" $ \ctx -> do
        w <- emptyRandomWallet ctx
        r <- request @ApiWallet ctx
            (Link.getWallet @'Shelley w) Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    it "BYRON_LIST_02,03 -\
        \ Byron wallets listed only via Byron endpoints \\\
        \ Shelley wallets listed only via new endpoints" $ \ctx -> do
        m1 <- genMnemonics @12
        m2 <- genMnemonics @12
        m3 <- genMnemonics @12
        _ <- emptyByronWalletWith ctx "random" ("byron1", m1, "Secure Passphrase")
        _ <- emptyByronWalletWith ctx "random" ("byron2", m2, "Secure Passphrase")
        _ <- emptyByronWalletWith ctx "random" ("byron3", m3, "Secure Passphrase")

        _ <- emptyWalletWith ctx ("shelley1", "Secure Passphrase", 20)
        _ <- emptyWalletWith ctx ("shelley2", "Secure Passphrase", 20)
        _ <- emptyWalletWith ctx ("shelley3", "Secure Passphrase", 20)

        --list only byron
        rl <- request @[ApiByronWallet] ctx (Link.listWallets @'Byron) Default Empty
        verify rl
            [ expectResponseCode @IO HTTP.status200
            , expectListSize 3
            , expectListField 0
                    (#name . #getApiT . #getWalletName) (`shouldBe` "byron1")
            , expectListField 1
                    (#name . #getApiT . #getWalletName) (`shouldBe` "byron2")
            , expectListField 2
                    (#name . #getApiT . #getWalletName) (`shouldBe` "byron3")
            ]
        --list only shelley
        rl2 <- request @[ApiWallet] ctx (Link.listWallets @'Shelley) Default Empty
        verify rl2
            [ expectResponseCode @IO HTTP.status200
            , expectListSize 3
            , expectListField 0
                    (#name . #getApiT . #getWalletName) (`shouldBe` "shelley1")
            , expectListField 1
                    (#name . #getApiT . #getWalletName) (`shouldBe` "shelley2")
            , expectListField 2
                    (#name . #getApiT . #getWalletName) (`shouldBe` "shelley3")
            ]

    it "BYRON_LIST_04, DELETE_01 -\
        \ Deleted wallets cannot be listed" $ \ctx -> do
        m1 <- genMnemonics @12
        m2 <- genMnemonics @12
        m3 <- genMnemonics @12
        _   <- emptyByronWalletWith ctx "random" ("byron1", m1, "Secure Passphrase")
        wb2 <- emptyByronWalletWith ctx "random" ("byron2", m2, "Secure Passphrase")
        _   <- emptyByronWalletWith ctx "random" ("byron3", m3, "Secure Passphrase")

        _ <- emptyWalletWith ctx ("shelley1", "Secure Passphrase", 20)
        _ <- emptyWalletWith ctx ("shelley2", "Secure Passphrase", 20)
        ws3 <- emptyWalletWith ctx ("shelley3", "Secure Passphrase", 20)

        -- delete
        _ <- request @ApiByronWallet ctx (Link.deleteWallet @'Byron wb2) Default Empty
        _ <- request @ApiWallet ctx (Link.deleteWallet @'Shelley ws3) Default Empty

        --list only byron
        rdl <- request @[ApiByronWallet] ctx (Link.listWallets @'Byron) Default Empty
        verify rdl
            [ expectResponseCode @IO HTTP.status200
            , expectListSize 2
            , expectListField 0
                    (#name . #getApiT . #getWalletName) (`shouldBe` "byron1")
            , expectListField 1
                    (#name . #getApiT . #getWalletName) (`shouldBe` "byron3")
            ]
        --list only shelley
        rdl2 <- request @[ApiWallet] ctx (Link.listWallets @'Shelley) Default Empty
        verify rdl2
            [ expectResponseCode @IO HTTP.status200
            , expectListSize 2
            , expectListField 0
                    (#name . #getApiT . #getWalletName) (`shouldBe` "shelley1")
            , expectListField 1
                    (#name . #getApiT . #getWalletName) (`shouldBe` "shelley2")
            ]

    it "BYRON_DELETE_02 - Byron ep does not delete Shelley wallet" $ \ctx -> do
        w <- emptyWallet ctx
        r <- request @ApiByronWallet ctx (Link.deleteWallet @'Byron w) Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    it "BYRON_DELETE_03 - Shelley ep does not delete Byron wallet" $ \ctx -> do
        w <- emptyRandomWallet ctx
        r <- request @ApiByronWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    it "BYRON_CALCULATE_03 - \
        \Cannot estimate migration for Shelley wallet"
        $ \ctx -> do
            w <- emptyWallet ctx
            let ep = Link.getMigrationInfo w
            r <- request @ApiWalletMigrationInfo ctx ep Default Empty
            expectResponseCode @IO HTTP.status404 r
            expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    it "BYRON_MIGRATE_01 - \
        \after a migration operation successfully completes, the correct \
        \amount eventually becomes available in the target wallet for arbitrary \
        \ number of specified addresses."
        $ \ctx -> do
              testAddressCycling ctx 1
              testAddressCycling ctx 3
              testAddressCycling ctx 10

    it "BYRON_MIGRATE_01 - \
        \ migrate a big wallet requiring more than one tx" $ \ctx -> do
        pendingWith "Byron wallets not support in cardano-node yet"
        -- NOTE
        -- Special mnemonic for which 500 legacy funds are attached to in the
        -- genesis file.
        --
        -- Out of these 500 coins, 100 of them are of 1 Lovelace and are
        -- expected to be treated as dust. The rest are all worth:
        -- 10,000,000,000 lovelace.
        let mnemonics =
                ["collect", "fold", "file", "clown"
                , "injury", "sun", "brass", "diet"
                , "exist", "spike", "behave", "clip"
                ] :: [Text]
        let payloadRestore = Json [json| {
                "name": "Big Byron Wallet",
                "mnemonic_sentence": #{mnemonics},
                "passphrase": #{fixturePassphrase},
                "style": "random"
                } |]
        (_, wOld) <- unsafeRequest @ApiByronWallet ctx
            (Link.postWallet @'Byron) payloadRestore
        eventually "wallet balance greater than 0" $ do
            request @ApiByronWallet ctx
                (Link.getWallet @'Byron wOld)
                Default
                Empty >>= flip verify
                [ expectField (#balance . #available) (.> Quantity 0)
                ]
        let originalBalance = view (#balance . #available . #getQuantity) wOld

        -- Calculate the expected migration fee:
        rFee <- request @ApiWalletMigrationInfo ctx
            (Link.getMigrationInfo wOld)
            Default
            Empty
        verify rFee
            [ expectResponseCode @IO HTTP.status200
            , expectField #migrationCost (.> Quantity 0)
            ]
        let expectedFee = getFromResponse (#migrationCost . #getQuantity) rFee

        -- Migrate to a new empty wallet
        wNew <- emptyWallet ctx
        addrs <- listAddresses @n ctx wNew
        let addr1 = (addrs !! 1) ^. #id

        let payloadMigrate =
                Json [json|
                    { passphrase: #{fixturePassphrase}
                    , addresses: [#{addr1}]
                    }|]
        request @[ApiTransaction n] ctx
            (Link.migrateWallet wOld)
            Default
            payloadMigrate >>= flip verify
            [ expectResponseCode @IO HTTP.status202
            , expectField id ((`shouldBe` 2). length)
            ]

        -- Check that funds become available in the target wallet:
        let expectedBalance = originalBalance - expectedFee
        eventually "wallet balance = expectedBalance" $ do
            request @ApiWallet ctx
                (Link.getWallet @'Shelley wNew)
                Default
                Empty >>= flip verify
                [ expectField
                        (#balance . #getApiT . #available)
                        ( `shouldBe` Quantity expectedBalance)
                , expectField
                        (#balance . #getApiT . #total)
                        ( `shouldBe` Quantity expectedBalance)
                ]

        -- Analyze the target wallet UTxO distribution
        request @ApiUtxoStatistics ctx (Link.getUTxOsStatistics @'Shelley wNew)
            Default
            Empty >>= flip verify
            [ expectField
                #distribution
                ((`shouldBe` (Just 400)) . Map.lookup 10_000_000_000)
            ]

    it "BYRON_MIGRATE_01 - \
        \a migration operation removes all funds from the source wallet."
        $ \ctx -> forM_ [fixtureRandomWallet, fixtureIcarusWallet]
        $ \fixtureByronWallet -> do
            pendingWith "Byron wallets not supported in cardano-node yet."
            -- Restore a Byron wallet with funds, to act as a source wallet:
            sourceWallet <- fixtureByronWallet ctx

            -- Perform a migration from the source wallet to a target wallet:
            targetWallet <- emptyWallet ctx
            addrs <- listAddresses @n ctx targetWallet
            let addr1 = (addrs !! 1) ^. #id

            r0 <- request @[ApiTransaction n] ctx
                (Link.migrateWallet sourceWallet)
                Default
                (Json [json|
                    { passphrase: #{fixturePassphrase}
                    , addresses: [#{addr1}]
                    }|])
            verify r0
                [ expectResponseCode @IO HTTP.status202
                , expectField id (`shouldSatisfy` (not . null))
                ]

            -- Verify that the source wallet has no funds available:
            r1 <- request @ApiByronWallet ctx
                (Link.getWallet @'Byron sourceWallet) Default Empty
            verify r1
                [ expectResponseCode @IO HTTP.status200
                , expectField (#balance . #available) (`shouldBe` Quantity 0)
                ]

    it "BYRON_MIGRATE_02 - \
        \migrating an empty wallet should fail."
        $ \ctx -> forM_ [emptyRandomWallet, emptyIcarusWallet] $ \emptyByronWallet -> do
            sourceWallet <- emptyByronWallet ctx
            targetWallet <- emptyWallet ctx
            addrs <- listAddresses @n ctx targetWallet
            let addr1 = (addrs !! 1) ^. #id
            let payload =
                    Json [json|
                        { passphrase: #{fixturePassphrase}
                        , addresses: [#{addr1}]
                        }|]
            let ep = Link.migrateWallet sourceWallet
            r <- request @[ApiTransaction n] ctx ep Default payload
            let srcId = sourceWallet ^. walletId
            verify r
                [ expectResponseCode @IO HTTP.status403
                , expectErrorMessage (errMsg403NothingToMigrate srcId)
                ]

    it "BYRON_MIGRATE_02 - \
        \migrating wallet with dust should fail."
        $ \ctx -> do
            -- NOTE
            -- Special mnemonic for which wallet with dust
            -- (5 utxos with 60 lovelace in total)
            let mnemonics =
                    [ "suffer", "decorate", "head", "opera"
                    , "yellow", "debate", "visa", "fire"
                    , "salute", "hybrid", "stone", "smart"
                    ] :: [Text]
            let payloadRestore = Json [json| {
                    "name": "Dust Byron Wallet",
                    "mnemonic_sentence": #{mnemonics},
                    "passphrase": #{fixturePassphrase},
                    "style": "random"
                    } |]
            pendingWith "Byron wallets not supported in cardano-node yet."
            (_, sourceWallet) <- unsafeRequest @ApiByronWallet ctx
                (Link.postWallet @'Byron) payloadRestore
            eventually "wallet balance greater than 0" $ do
                request @ApiByronWallet ctx
                    (Link.getWallet @'Byron sourceWallet)
                    Default
                    Empty >>= flip verify
                    [ expectField (#balance . #available) (.> Quantity 0)
                    ]

            targetWallet <- emptyWallet ctx
            addrs <- listAddresses @n ctx targetWallet
            let addr1 = (addrs !! 1) ^. #id
            let payload =
                    Json [json|
                        { passphrase: #{fixturePassphrase}
                        , addresses: [#{addr1}]
                        }|]
            let ep = Link.migrateWallet sourceWallet
            r <- request @[ApiTransaction n] ctx ep Default payload
            let srcId = sourceWallet ^. walletId
            verify r
                [ expectResponseCode @IO HTTP.status403
                , expectErrorMessage (errMsg403NothingToMigrate srcId)
                ]

    it "BYRON_MIGRATE_03 - \
        \actual fee for migration is the same as the predicted fee."
        $ \ctx -> forM_ [fixtureRandomWallet, fixtureIcarusWallet]
        $ \fixtureByronWallet -> do
            pendingWith "Byron wallets not supported in cardano-node yet."
            -- Restore a Byron wallet with funds.
            sourceWallet <- fixtureByronWallet ctx

            -- Request a migration fee prediction.
            let ep0 = (Link.getMigrationInfo sourceWallet)
            r0 <- request @ApiWalletMigrationInfo ctx ep0 Default Empty
            verify r0
                [ expectResponseCode @IO HTTP.status200
                , expectField #migrationCost (.> Quantity 0)
                ]

            -- Perform the migration.
            targetWallet <- emptyWallet ctx
            addrs <- listAddresses @n ctx targetWallet
            let addr1 = (addrs !! 1) ^. #id
            let payload =
                    Json [json|
                        { passphrase: #{fixturePassphrase}
                        , addresses: [#{addr1}]
                        }|]
            let ep1 = Link.migrateWallet sourceWallet
            r1 <- request @[ApiTransaction n] ctx ep1 Default payload
            verify r1
                [ expectResponseCode @IO HTTP.status202
                , expectField id (`shouldSatisfy` (not . null))
                ]

            -- Verify that the fee prediction was correct.
            let actualFee = fromIntegral $ sum $ apiTransactionFee
                    <$> getFromResponse id r1
            let predictedFee =
                    getFromResponse (#migrationCost . #getQuantity) r0
            actualFee `shouldBe` predictedFee

    it "BYRON_MIGRATE_04 - migration fails with a wrong passphrase"
        $ \ctx -> forM_ [fixtureRandomWallet, fixtureIcarusWallet]
        $ \fixtureByronWallet -> do
        pendingWith "Byron wallets not supported in cardano-node yet."
        -- Restore a Byron wallet with funds, to act as a source wallet:
        sourceWallet <- fixtureByronWallet ctx

        -- Perform a migration from the source wallet to a target wallet:
        targetWallet <- emptyWallet ctx
        addrs <- listAddresses @n ctx targetWallet
        let addr1 = (addrs !! 1) ^. #id
        r0 <- request @[ApiTransaction n] ctx
            (Link.migrateWallet sourceWallet )
            Default
            (Json [json|
                { passphrase: "not-the-right-passphrase"
                , addresses: [#{addr1}]
                }|])
        verify r0
            [ expectResponseCode @IO HTTP.status403
            , expectErrorMessage errMsg403WrongPass
            ]

    it "NETWORK_SHELLEY - Wallet has the same tip as network/information" $
        \ctx -> do
            let getNetworkInfo = request @ApiNetworkInformation ctx
                    Link.getNetworkInfo Default Empty
            w <- emptyWallet ctx
            eventually "Wallet has the same tip as network/information" $ do
                sync <- getNetworkInfo
                expectField (#syncProgress . #getApiT) (`shouldBe` Ready) sync

                let epochNum =
                        getFromResponse (#nodeTip . #epochNumber . #getApiT) sync
                let slotNum =
                        getFromResponse (#nodeTip . #slotNumber . #getApiT) sync
                let blockHeight =
                        getFromResponse (#nodeTip . #height) sync

                res <- request @ApiWallet ctx
                    (Link.getWallet @'Shelley w) Default Empty
                verify res
                    [ expectField (#state . #getApiT) (`shouldBe` Ready)
                    , expectField (#tip . #epochNumber . #getApiT) (`shouldBe` epochNum)
                    , expectField (#tip . #slotNumber  . #getApiT) (`shouldBe` slotNum)
                    , expectField (#tip . #height) (`shouldBe` blockHeight)
                    ]
  where
    -- Compute the fee associated with an API transaction.
    apiTransactionFee :: ApiTransaction n -> Word64
    apiTransactionFee t =
        inputBalance t - outputBalance t
      where
        inputBalance = fromIntegral
            . sum
            . fmap (view (#amount . #getQuantity))
            . mapMaybe ApiTypes.source
            . view #inputs
        outputBalance = fromIntegral
            . sum
            . fmap (view (#amount . #getQuantity))
            . view #outputs

    genMnemonics
       :: forall mw ent csz.
           ( ConsistentEntropy ent mw csz
           , ValidEntropySize ent
           , ValidChecksumSize ent csz
           , ent ~ EntropySize mw
           , mw ~ MnemonicWords ent
           )
       => IO [Text]
    genMnemonics =
        mnemonicToText . entropyToMnemonic @mw <$> genEntropy

    testAddressCycling ctx addrNum =
        forM_ [fixtureRandomWallet, fixtureIcarusWallet]
        $ \fixtureByronWallet -> do
            pendingWith "Byron wallets not supported in cardano-node yet."
            -- Restore a Byron wallet with funds, to act as a source wallet:
            sourceWallet <- fixtureByronWallet ctx
            let originalBalance =
                        view (#balance . #available . #getQuantity) sourceWallet

            -- Create an empty target wallet:
            targetWallet <- emptyWallet ctx
            addrs <- listAddresses @n ctx targetWallet
            let addrIds =
                    map (\(ApiTypes.ApiAddress theid _) -> theid) $
                    take addrNum addrs

            -- Calculate the expected migration fee:
            r0 <- request @ApiWalletMigrationInfo ctx
                (Link.getMigrationInfo sourceWallet) Default Empty
            verify r0
                [ expectResponseCode @IO HTTP.status200
                , expectField #migrationCost (.> Quantity 0)
                ]
            let expectedFee = getFromResponse (#migrationCost . #getQuantity) r0

            -- Perform a migration from the source wallet to the target wallet:
            r1 <- request @[ApiTransaction n] ctx
                (Link.migrateWallet sourceWallet)
                Default
                (Json [json|
                    { passphrase: #{fixturePassphrase}
                    , addresses: #{addrIds}
                    }|])
            verify r1
                [ expectResponseCode @IO HTTP.status202
                , expectField id (`shouldSatisfy` (not . null))
                ]

            -- Check that funds become available in the target wallet:
            let expectedBalance = originalBalance - expectedFee
            eventually "Wallet has expectedBalance" $ do
                r2 <- request @ApiWallet ctx
                    (Link.getWallet @'Shelley targetWallet) Default Empty
                verify r2
                    [ expectField
                            (#balance . #getApiT . #available)
                            (`shouldBe` Quantity expectedBalance)
                    , expectField
                            (#balance . #getApiT . #total)
                            (`shouldBe` Quantity expectedBalance)
                    ]
