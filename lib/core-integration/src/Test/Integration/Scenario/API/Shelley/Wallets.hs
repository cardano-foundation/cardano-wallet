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
    ( entropyToMnemonic, genEntropy, mnemonicToText )
import Cardano.Wallet.Api.Types
    ( AddressAmount (..)
    , ApiAddress
    , ApiByronWallet
    , ApiCoinSelection
    , ApiCoinSelectionOutput (..)
    , ApiNetworkInformation
    , ApiT (..)
    , ApiTransaction
    , ApiUtxoStatistics
    , ApiVerificationKey (..)
    , ApiWallet
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( AccountingStyle (..)
    , DerivationIndex (..)
    , DerivationType (..)
    , Index (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    , PaymentAddress
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap (..), coinTypeAda, purposeCIP1852 )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..) )
import Cardano.Wallet.Primitive.Types
    ( walletNameMaxLength, walletNameMinLength )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Control.Monad
    ( forM_ )
import Data.Aeson
    ( ToJSON (..) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.List
    ( isPrefixOf )
import Data.List.NonEmpty
    ( NonEmpty ((:|)) )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( toText )
import Data.Word
    ( Word32, Word64 )
import Test.Hspec
    ( SpecWith, describe, shouldBe, shouldNotBe, shouldSatisfy )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , MnemonicLength (..)
    , Payload (..)
    , emptyByronWalletWith
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
    , fixturePassphrase
    , fixtureWallet
    , fixtureWalletWith
    , genMnemonics
    , getFromResponse
    , json
    , listAddresses
    , minUTxOValue
    , notDelegating
    , rawRequest
    , request
    , selectCoins
    , unsafeRequest
    , verify
    , walletId
    , (</>)
    )
import Test.Integration.Framework.TestData
    ( arabicWalletName
    , errMsg403WrongPass
    , errMsg404NoWallet
    , errMsg406
    , errMsg415
    , kanjiWalletName
    , payloadWith
    , payloadWith'
    , polishWalletName
    , russianWalletName
    , simplePayload
    , updateNamePayload
    , updatePassPayload
    , wildcardsWalletName
    )

-- FIXME:
-- give ways to construct and deconstruct an 'XSignature' in cardano-addresses,
-- e.g. xsignatureFromBytes / xsignatureToBytes so that we can avoid the import
-- of cardano-crypto here.
import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Network.HTTP.Types as HTTP

spec :: forall n t.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    , PaymentAddress n ShelleyKey
    , PaymentAddress n IcarusKey
    , PaymentAddress n ByronKey
    ) => SpecWith (Context t)
spec = describe "SHELLEY_WALLETS" $ do
    it "WALLETS_CREATE_01 - Create a wallet" $ \ctx -> do
        m15 <- genMnemonics M15
        m12 <- genMnemonics M12
        let payload = Json [json| {
                "name": "1st Wallet",
                "mnemonic_sentence": #{m15},
                "mnemonic_second_factor": #{m12},
                "passphrase": #{fixturePassphrase},
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
            , expectField #passphrase (`shouldNotBe` Nothing)
            ]
        let wid = getFromResponse id r
        eventually "Wallet state = Ready" $ do
            rg <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wid) Default Empty
            expectField (#state . #getApiT) (`shouldBe` Ready) rg

    describe "OWASP_INJECTION_CREATE_WALLET_01 - \
             \SQL injection when creating a wallet" $  do
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
                    "mnemonic_sentence": #{explicitMnemonics},
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
                        "quantity": #{minUTxOValue},
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
                        (#balance . #getApiT . #total) (`shouldBe` Quantity minUTxOValue)
                , expectField
                        (#balance . #getApiT . #available) (`shouldBe` Quantity minUTxOValue)
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
                        (#balance . #getApiT . #total) (`shouldBe` Quantity minUTxOValue)
                , expectField
                        (#balance . #getApiT . #available) (`shouldBe` Quantity minUTxOValue)
                ]

    it "WALLETS_CREATE_03,09 - Cannot create wallet that exists" $ \ctx -> do
        m21 <- genMnemonics M21
        let payload = Json [json| {
                "name": "Some Wallet",
                "mnemonic_sentence": #{m21},
                "passphrase": #{fixturePassphrase}
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
            m24 <- genMnemonics M24
            let payload = Json [json| {
                    "name": #{walName},
                    "mnemonic_sentence": #{m24},
                    "passphrase": #{fixturePassphrase}
                    } |]
            r <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default payload
            verify r expectations

    describe "WALLETS_CREATE_05 - Mnemonics" $ do
        let matrix =
             [ ( "15 mnemonic words", M15 )
             , ( "18 mnemonic words", M18 )
             , ( "21 mnemonic words", M21 )
             , ( "24 mnemonic words", M24 )
             ]

        forM_ matrix $ \(title, mnemonics) -> it title $ \ctx -> do
            m <- genMnemonics mnemonics
            let payload = Json [json| {
                    "name": "Just a łallet",
                    "mnemonic_sentence": #{m},
                    "passphrase": #{fixturePassphrase}
                    } |]
            r <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default payload
            verify r [ expectResponseCode @IO HTTP.status201 ]

    describe "WALLETS_CREATE_06 - Mnemonics second factor" $ do
        let matrix =
                 [ ( "9 mnemonic words", M9 )
                 , ( "12 mnemonic words", M12 )
                 ]
        forM_ matrix $ \(title, mnemonics) -> it title $ \ctx -> do
            m15 <- genMnemonics M15
            mSecondFactor <- genMnemonics mnemonics

            let payload = Json [json| {
                    "name": "Just a łallet",
                    "mnemonic_sentence": #{m15},
                    "mnemonic_second_factor": #{mSecondFactor},
                    "passphrase": #{fixturePassphrase}
                    } |]
            r <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default payload
            verify r [ expectResponseCode @IO HTTP.status201 ]

    describe "WALLETS_CREATE_07 - Passphrase" $ do
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
        forM_ matrix $ \(title, passphrase) -> it title $ \ctx -> do
            m24 <- genMnemonics M24
            let payload = Json [json| {
                    "name": "Secure Wallet",
                    "mnemonic_sentence": #{m24},
                    "passphrase": #{passphrase}
                    } |]
            r <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default payload
            verify r [ expectResponseCode @IO HTTP.status201 ]

    describe "WALLETS_CREATE_08 - address_pool_gap" $ do
        let addrPoolMin = fromIntegral @_ @Int $ getAddressPoolGap minBound
        -- Although maxAddressPoolGap = 100k, there are performance issues when
        -- creating/using such wallets. Therefore using max gap that is used in Daedalus.
        -- Performance issues are addressed in ADP-442, ADP-436
        let maxDaedalusGap = (1_000 :: Word32)
        let addrPoolBig = fromIntegral maxDaedalusGap

        let matrix =
                [ ( show addrPoolMin
                  , addrPoolMin
                  , [ expectResponseCode @IO HTTP.status201
                    , expectField (#addressPoolGap . #getApiT) (`shouldBe` minBound)
                    ]
                  )
                , ( show addrPoolBig
                  , addrPoolBig
                  , [ expectResponseCode @IO HTTP.status201
                    , expectField
                        (#addressPoolGap . #getApiT . #getAddressPoolGap)
                        (`shouldBe` maxDaedalusGap)
                  ]
                  )
                ]
        forM_ matrix $ \(title, addrPoolGap, expectations) -> it title $ \ctx -> do
            m24 <- genMnemonics M24
            let payload = payloadWith' "Secure Wallet" m24 (fromIntegral addrPoolGap)
            rW <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default payload
            verify rW expectations

            let w = getFromResponse id rW
            rA <- request @[ApiAddress n] ctx
                (Link.listAddresses @'Shelley w) Default Empty
            _ <- request @ApiWallet ctx
                (Link.deleteWallet @'Shelley w) Default Empty
            verify rA
                [ expectListSize addrPoolGap
                ]

    it "WALLETS_CREATE_08 - default address_pool_gap" $ \ctx -> do
        m21 <- genMnemonics M21
        let payload = Json [json| {
                "name": "Secure Wallet",
                "mnemonic_sentence": #{m21},
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
            m21 <- genMnemonics M21
            let payload = Json [json| {
                    "name": "Secure Wallet",
                    "mnemonic_sentence": #{m21},
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
        m18 <- genMnemonics M18
        m9 <- genMnemonics M9
        let payload = Json [json| {
                "name": "Wallet to be listed",
                "mnemonic_sentence": #{m18},
                "mnemonic_second_factor": #{m9},
                "passphrase": #{fixturePassphrase},
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
            ]

    it "WALLETS_LIST_01 - Wallets are listed from oldest to newest" $ \ctx -> do
        m15 <- genMnemonics M15
        m18 <- genMnemonics M18
        m21 <- genMnemonics M21
        let walletDetails = [("1", m15), ("2", m18)
                    , ("3", m21)]
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
        let payload = updatePassPayload fixturePassphrase "New passphrase"
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
            let payload = updatePassPayload fixturePassphrase passphrase
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
            m24 <- genMnemonics M24
            let createPayload = Json [json| {
                     "name": "Name of the wallet",
                     "mnemonic_sentence": #{m24},
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
        let payload = updatePassPayload fixturePassphrase "Secure passphrase2"
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
                            "quantity": #{minUTxOValue},
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
            let payload = updatePassPayload fixturePassphrase "Passphrase"
            let endpoint = Link.putWalletPassphrase @'Shelley w
            rup <- request @ApiWallet ctx endpoint headers payload
            verify rup expectations

    it "WALLETS_COIN_SELECTION_01 - \
        \A singleton payment is included in the coin selection output." $
        \ctx -> do
            source <- fixtureWallet ctx
            target <- emptyWallet ctx
            targetAddress : _ <- fmap (view #id) <$> listAddresses @n ctx target
            let amount = Quantity minUTxOValue
            let payment = AddressAmount targetAddress amount
            let output = ApiCoinSelectionOutput targetAddress amount
            let isValidDerivationPath path =
                    ( length path == 5 )
                    &&
                    ( [ ApiT $ DerivationIndex $ getIndex purposeCIP1852
                      , ApiT $ DerivationIndex $ getIndex coinTypeAda
                      , ApiT $ DerivationIndex $ getIndex @'Hardened minBound
                      ] `isPrefixOf` NE.toList path
                    )
            selectCoins @_ @'Shelley ctx source (payment :| []) >>= flip verify
                [ expectResponseCode HTTP.status200
                , expectField #inputs
                    (`shouldSatisfy` (not . null))
                , expectField #inputs
                    (`shouldSatisfy` all
                        (isValidDerivationPath . view #derivationPath))
                , expectField #change
                    (`shouldSatisfy` (not . null))
                , expectField #change
                    (`shouldSatisfy` all
                        (isValidDerivationPath . view #derivationPath))
                , expectField #outputs
                    (`shouldBe` [output])
                ]

    it "WALLETS_COIN_SELECTION_02 - \
        \Multiple payments are all included in the coin selection output." $
        \ctx -> do
            let paymentCount = 10
            source <- fixtureWallet ctx
            target <- emptyWallet ctx
            targetAddresses <- fmap (view #id) <$> listAddresses @n ctx target
            let amounts = Quantity <$> [minUTxOValue ..]
            let payments = NE.fromList
                    $ take paymentCount
                    $ zipWith AddressAmount targetAddresses amounts
            let outputs =
                    take paymentCount
                    $ zipWith ApiCoinSelectionOutput targetAddresses amounts
            selectCoins @_ @'Shelley ctx source payments >>= flip verify
                [ expectResponseCode HTTP.status200
                , expectField #inputs (`shouldSatisfy` (not . null))
                , expectField #change (`shouldSatisfy` (not . null))
                , expectField
                    #outputs (`shouldSatisfy` ((L.sort outputs ==) . L.sort))
                ]

    it "WALLETS_COIN_SELECTION_03 - \
        \Deleted wallet is not available for selection" $ \ctx -> do
        w <- emptyWallet ctx
        (addr:_) <- fmap (view #id) <$> listAddresses @n ctx w
        let payments = NE.fromList [ AddressAmount addr (Quantity minUTxOValue) ]
        _ <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
        selectCoins @_ @'Shelley ctx w payments >>= flip verify
            [ expectResponseCode @IO HTTP.status404
            , expectErrorMessage (errMsg404NoWallet $ w ^. walletId)
            ]

    it "WALLETS_COIN_SELECTION_03 - \
        \Wrong selection method (not 'random')" $ \ctx -> do
        w <- fixtureWallet ctx
        (addr:_) <- fmap (view #id) <$> listAddresses @n ctx w
        let payments = NE.fromList [ AddressAmount addr (Quantity minUTxOValue) ]
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
            let payments = NE.fromList [ AddressAmount addr (Quantity minUTxOValue) ]
            let payload = Json [json| { "payments": #{payments} } |]
            r <- request @(ApiCoinSelection n) ctx
                (Link.selectCoins @'Shelley w) headers payload
            verify r expectations

    it "WALLETS_COIN_SELECTION_05 - \
        \No change when payment fee eats leftovers due to minUTxOValue" $
        \ctx -> do
            source  <- fixtureWalletWith @n ctx [minUTxOValue, minUTxOValue]
            eventually "Source wallet balance is as expected" $ do
                rGet <- request @ApiWallet ctx
                    (Link.getWallet @'Shelley source) Default Empty
                verify rGet
                    [ expectField
                            (#balance . #getApiT . #total)
                            (`shouldBe` Quantity (2 * minUTxOValue))
                    , expectField
                            (#balance . #getApiT . #available)
                            (`shouldBe` Quantity (2 * minUTxOValue))
                    ]
            target <- emptyWallet ctx

            targetAddress:_ <- fmap (view #id) <$> listAddresses @n ctx target
            let amount = Quantity minUTxOValue
            let payment = AddressAmount targetAddress amount
            let output = ApiCoinSelectionOutput targetAddress amount
            let isValidDerivationPath path =
                    ( length path == 5 )
                    &&
                    ( [ ApiT $ DerivationIndex $ getIndex purposeCIP1852
                      , ApiT $ DerivationIndex $ getIndex coinTypeAda
                      , ApiT $ DerivationIndex $ getIndex @'Hardened minBound
                      ] `isPrefixOf` NE.toList path
                    )
            selectCoins @_ @'Shelley ctx source (payment :| []) >>= flip verify
                [ expectResponseCode HTTP.status200
                , expectField #inputs
                    (`shouldSatisfy` (not . null))
                , expectField #inputs
                    (`shouldSatisfy` all
                        (isValidDerivationPath . view #derivationPath))
                , expectField #change
                    (`shouldSatisfy` null)
                , expectField #outputs
                    (`shouldBe` [output])
                ]

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
        let coins = [13_000_000::Word64, 43_000_000, 66_000_000, 101_000_000, 1339_000_000]
        let payments = flip map coins $ \c -> [json|{
                "address": #{destination},
                "amount": {
                    "quantity": #{c},
                    "unit": "lovelace"
                }}|]
        let payload = [json|{
                "payments": #{payments},
                "passphrase": "cardano-wallet"
                }|]

        rTrans <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default (Json payload)
        expectResponseCode @IO HTTP.status202 rTrans

        eventually "Wallet balance is as expected" $ do
            rGet <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            verify rGet
                [ expectField
                        (#balance . #getApiT . #total)
                        (`shouldBe` Quantity (fromIntegral $ sum coins))
                , expectField
                        (#balance . #getApiT . #available)
                        (`shouldBe` Quantity (fromIntegral $ sum coins))
                ]

        --verify utxo
        rStat1 <- request @ApiUtxoStatistics ctx
            (Link.getUTxOsStatistics @'Shelley wDest) Default Empty
        expectResponseCode @IO HTTP.status200 rStat1
        expectWalletUTxO coins (snd rStat1)

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

    describe "WALLETS_GET_KEY_01 - golden tests for verification key" $ do
    --- $ cat recovery-phrase.txt
    -- pulp ten light rhythm replace vessel slow drift kingdom amazing negative join auction ugly symptom
    --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley > root.prv
    --- $ cat root.prv \
    --- > | cardano-address key child 1852H/1815H/0H/ROLE/INDEX \
    --- > | cardano-address key public \
        let matrix :: [(AccountingStyle, DerivationIndex, String)]
            matrix =
                [ ( UtxoExternal
                  , DerivationIndex 0
                  , "addr_xvk1tmdggpmj7r2mqfhneqsc5fzfj2j4sxf4j7mqwwsa8w58vz94zr\
                    \463ndsajkjvn82va30fgf22qruc53zxyjp6pu7yd87ppdefd6u98cue5ul9"
                  )
                , ( UtxoInternal
                  , DerivationIndex 100
                  , "addr_xvk1wchen6vz4zz7kpfjld3g89zdcpdv2hzvtsufphgvpjxjkl49pq\
                    \rfd2lgqg228zl7ylax8c5tr0rkys3phfkxd5j6s56c0tfy7r49jhct38kq6"
                  )
                , ( MutableAccount
                  , DerivationIndex 2147483647
                  , "stake_xvk1qy9tp370ze3cfre8f6daz7l85pgk3wpg6s5zqae2yjljwqkx4\
                    \ht28cxx7j5ju0wl8795s3yj8z3te4h2j9eaztll2z4mxhwwkppy53sf49ev5"
                  )
                , ( MultisigScript
                  , DerivationIndex 42
                  , "script_xvk1mjr5lrrlxuvelx94hu2cttmg5pp6cwy5h0sa37qvpcd07pv9g\
                    \23nlvugj5ez9qfxxvkmjwnpn69s48cv572phfy6qpmnwat0hwcdrasapqewe"
                  )
                ]

        forM_ matrix
            $ \(role_, index, expected) -> it (show role_ <> "/" <> show index)
            $ \ctx -> do
                let payload = Json [json|{
                        "name": "Wallet",
                        "mnemonic_sentence": #{explicitMnemonics},
                        "passphrase": #{fixturePassphrase}
                    }|]

                r <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default payload
                verify r [ expectResponseCode @IO HTTP.status201 ]
                let apiWal = getFromResponse id r

                let link = Link.getWalletKey (apiWal ^. id) role_ index
                rGet <- request @ApiVerificationKey ctx link Default Empty
                verify rGet
                    [ expectResponseCode @IO HTTP.status200
                    , expectField id (\k -> toJSON k `shouldBe` toJSON expected)
                    ]

    it "WALLETS_GET_KEY_02 - invalid index for verification key" $ \ctx -> do
        w <- emptyWallet ctx

        let link = Link.getWalletKey w UtxoExternal (DerivationIndex 2147483648)
        r <- request @ApiVerificationKey ctx link Default Empty

        verify r
            [ expectResponseCode @IO HTTP.status403
            , expectErrorMessage
                "It looks like you've provided a derivation index that is out of bound."
            ]

    it "WALLETS_GET_KEY_03 - unknown wallet" $ \ctx -> do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty

        let link = Link.getWalletKey w UtxoExternal (DerivationIndex 0)
        r <- request @ApiVerificationKey ctx link Default Empty

        verify r
            [ expectResponseCode @IO HTTP.status404
            , expectErrorMessage (errMsg404NoWallet $ w ^. walletId)
            ]

    it "WALLETS_SIGNATURES_01 - can verify signature" $ \ctx -> do
        w <- emptyWallet ctx

        let (role_, index) = (MutableAccount, DerivationIndex 0)
        let payload = [json|
                { "passphrase": #{fixturePassphrase}
                , "metadata": {"0": { "string": "please sign this." }}
                }|]

        -- sign metadata
        rSig <- rawRequest ctx
            (Link.signMetadata w role_ index)
            (Headers [(HTTP.hAccept, "*/*"), (HTTP.hContentType, "application/json")])
            (Json payload)
        verify rSig
            [ expectResponseCode @IO HTTP.status200
            ]

        -- get corresponding public key
        rKey <- request @ApiVerificationKey ctx
            (Link.getWalletKey w role_ index)
            Default
            Empty
        verify rKey
            [ expectResponseCode @IO HTTP.status200
            ]

        -- verify the signature
        --
        -- expected metadata serialized as CBOR:
        --
        --     {0:"please sign this."}
        --
        --     ~
        --
        --     A1                                      # map(1)
        --        00                                   # unsigned(0)
        --        71                                   # text(17)
        --           706C65617365207369676E20746869732E # "please sign this."
        --
        let sig = CC.xsignature $ BL.toStrict $ getFromResponse id rSig
        let key = fst $ getFromResponse #getApiVerificationKey rKey
        let msg = unsafeFromHex "A10071706C65617365207369676E20746869732E"

        CC.verify key msg <$> sig `shouldBe` Right True

    it "WALLETS_SIGNATURES_02 - invalid index for signing key" $ \ctx -> do
        w <- emptyWallet ctx

        let payload = [json|
              { "passphrase": #{fixturePassphrase}
              , "metadata": { "0": { "int": 1 } }
              }|]
        let link = Link.signMetadata w UtxoExternal (DerivationIndex 2147483648)
        r <- rawRequest ctx link
            (Headers [(HTTP.hAccept, "*/*"), (HTTP.hContentType, "application/json")])
            (Json payload)

        verify r
            [ expectResponseCode @IO HTTP.status403
            , expectErrorMessage
                "It looks like you've provided a derivation index that is out of bound."
            ]

    it "WALLETS_SIGNATURES_03 - unknown wallet" $ \ctx -> do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty


        let payload = [json|
              { "passphrase": #{fixturePassphrase}
              , "metadata": { "0": { "int": 1 } }
              }|]
        let link = Link.signMetadata w UtxoExternal (DerivationIndex 0)
        r <- rawRequest ctx link
            (Headers [(HTTP.hAccept, "*/*"), (HTTP.hContentType, "application/json")])
            (Json payload)

        verify r
            [ expectResponseCode @IO HTTP.status404
            , expectErrorMessage (errMsg404NoWallet $ w ^. walletId)
            ]

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
        let payload = updatePassPayload fixturePassphrase "Secure passphrase2"
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
        m1 <- genMnemonics M12
        m2 <- genMnemonics M12
        m3 <- genMnemonics M12
        _ <- emptyByronWalletWith ctx "random" ("byron1", m1, fixturePassphrase)
        _ <- emptyByronWalletWith ctx "random" ("byron2", m2, fixturePassphrase)
        _ <- emptyByronWalletWith ctx "random" ("byron3", m3, fixturePassphrase)

        _ <- emptyWalletWith ctx ("shelley1", fixturePassphrase, 20)
        _ <- emptyWalletWith ctx ("shelley2", fixturePassphrase, 20)
        _ <- emptyWalletWith ctx ("shelley3", fixturePassphrase, 20)

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
        m1 <- genMnemonics M12
        m2 <- genMnemonics M12
        m3 <- genMnemonics M12
        _   <- emptyByronWalletWith ctx "random" ("byron1", m1, fixturePassphrase)
        wb2 <- emptyByronWalletWith ctx "random" ("byron2", m2, fixturePassphrase)
        _   <- emptyByronWalletWith ctx "random" ("byron3", m3, fixturePassphrase)

        _ <- emptyWalletWith ctx ("shelley1", fixturePassphrase, 20)
        _ <- emptyWalletWith ctx ("shelley2", fixturePassphrase, 20)
        ws3 <- emptyWalletWith ctx ("shelley3", fixturePassphrase, 20)

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

    it "NETWORK_SHELLEY - Wallet has the same tip as network/information" $
        \ctx -> do
            let getNetworkInfo = request @ApiNetworkInformation ctx
                    Link.getNetworkInfo Default Empty
            w <- emptyWallet ctx
            eventually "Wallet has the same tip as network/information" $ do
                sync <- getNetworkInfo
                expectField (#syncProgress . #getApiT) (`shouldBe` Ready) sync

                let epochNum =
                        getFromResponse (#nodeTip . #slotId . #epochNumber . #getApiT) sync
                let slotNum =
                        getFromResponse (#nodeTip . #slotId . #slotNumber . #getApiT) sync
                let blockHeight =
                        getFromResponse (#nodeTip . #block . #height) sync

                res <- request @ApiWallet ctx
                    (Link.getWallet @'Shelley w) Default Empty
                verify res
                    [ expectField (#state . #getApiT) (`shouldBe` Ready)
                    , expectField (#tip . #slotId . #epochNumber . #getApiT) (`shouldBe` epochNum)
                    , expectField (#tip . #slotId . #slotNumber  . #getApiT) (`shouldBe` slotNum)
                    , expectField (#tip . #block . #height) (`shouldBe` blockHeight)
                    ]

  where
    explicitMnemonics =
        [ "pulp", "ten", "light", "rhythm", "replace"
        , "vessel", "slow", "drift", "kingdom", "amazing"
        , "negative", "join", "auction", "ugly", "symptom"] :: [Text]
