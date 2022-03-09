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
    ( ApiAddress
    , ApiByronWallet
    , ApiNetworkInformation
    , ApiTransaction
    , ApiUtxoStatistics
    , ApiVerificationKeyShelley (..)
    , ApiWallet
    , ApiWalletUtxoSnapshot
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DerivationIndex (..), PaymentAddress, Role (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap (..) )
import Cardano.Wallet.Primitive.Passphrase
    ( PassphraseMaxLength (..), PassphraseMinLength (..) )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..) )
import Cardano.Wallet.Primitive.Types
    ( walletNameMaxLength, walletNameMinLength )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex, unsafeXPub )
import Control.Monad
    ( forM, forM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Resource
    ( runResourceT )
import Data.Aeson
    ( ToJSON (..) )
import Data.ByteArray.Encoding
    ( Base (Base16), convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word32 )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith, describe )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldNotBe )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , MnemonicLength (..)
    , Payload (..)
    , constFixtureWalletNoWait
    , counterexample
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
    , fixtureMultiAssetWallet
    , fixturePassphrase
    , fixtureWallet
    , genMnemonics
    , getFromResponse
    , json
    , listAddresses
    , listFilteredByronWallets
    , listFilteredWallets
    , minUTxOValue
    , notDelegating
    , postWallet
    , postWallet'
    , rawRequest
    , request
    , unsafeResponse
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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Network.HTTP.Types as HTTP

spec :: forall n.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    , PaymentAddress n ShelleyKey
    , PaymentAddress n IcarusKey
    , PaymentAddress n ByronKey
    ) => SpecWith Context
spec = describe "SHELLEY_WALLETS" $ do
    it "WALLETS_CREATE_01 - Create a wallet" $ \ctx -> runResourceT $ do
        m15 <- liftIO $ genMnemonics M15
        m12 <- liftIO $ genMnemonics M12
        let payload = Json [json| {
                "name": "1st Wallet",
                "mnemonic_sentence": #{m15},
                "mnemonic_second_factor": #{m12},
                "passphrase": #{fixturePassphrase},
                "address_pool_gap": 30
                } |]
        r <- postWallet ctx payload
        verify r
            [ expectResponseCode HTTP.status201
            , expectField
                    (#name . #getApiT . #getWalletName) (`shouldBe` "1st Wallet")
            , expectField
                    (#addressPoolGap . #getApiT . #getAddressPoolGap) (`shouldBe` 30)
            , expectField (#balance . #available) (`shouldBe` Quantity 0)
            , expectField (#balance . #total) (`shouldBe` Quantity 0)
            , expectField (#balance . #reward) (`shouldBe` Quantity 0)
            , expectField (#assets . #total) (`shouldBe` mempty)
            , expectField (#assets . #available) (`shouldBe` mempty)
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
        forM_ matrix $ \(nameIn, nameOut) -> it nameIn $ \ctx -> runResourceT $ do
            mnemonics <- liftIO $ genMnemonics M24
            let payload = Json [json| {
                    "name": #{nameIn},
                    "mnemonic_sentence": #{mnemonics},
                    "passphrase": "12345678910"
                    } |]
            r <- postWallet ctx payload
            let wid = getFromResponse id r
            verify r
                [ expectResponseCode HTTP.status201
                , expectField
                    (#name . #getApiT . #getWalletName) (`shouldBe` nameOut)
                , expectField
                    (#addressPoolGap . #getApiT . #getAddressPoolGap) (`shouldBe` 20)
                , expectField
                    (#balance . #available) (`shouldBe` Quantity 0)
                , expectField
                    (#balance . #total) (`shouldBe` Quantity 0)
                , expectField
                    (#balance . #reward) (`shouldBe` Quantity 0)
                , expectField #delegation (`shouldBe` notDelegating [])
                , expectField #passphrase (`shouldNotBe` Nothing)
                ]
            eventually "listed wallet's state = Ready" $ do
                r2 <- request @ApiWallet ctx (Link.getWallet @'Shelley wid) Default Empty
                verify r2
                    [ expectResponseCode HTTP.status200
                    , expectField (#state . #getApiT) (`shouldBe` Ready)
                    ]

    it "WALLETS_CREATE_02 - Restored wallet preserves funds" $ \ctx -> runResourceT $ do
        wSrc <- fixtureWallet ctx
        let minUTxOValue' = minUTxOValue (_mainEra ctx)
        -- create wallet
        mnemonics <- liftIO $ mnemonicToText @15 . entropyToMnemonic <$> genEntropy
        let payldCrt = payloadWith "!st created" mnemonics
        rInit <- postWallet ctx payldCrt
        verify rInit
            [ expectResponseCode HTTP.status201
            , expectField (#balance . #available) (`shouldBe` Quantity 0)
            , expectField (#balance . #total) (`shouldBe` Quantity 0)
            , expectField (#assets . #available) (`shouldBe` mempty)
            , expectField (#assets . #total) (`shouldBe` mempty)
            ]

        --send funds
        let wDest = getFromResponse id rInit
        addrs <- listAddresses @n ctx wDest
        let destination = (addrs !! 1) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{minUTxOValue' },
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]
        rTrans <- request @(ApiTransaction n) ctx
            (Link.createTransactionOld @'Shelley wSrc) Default payload
        expectResponseCode HTTP.status202 rTrans

        eventually "Wallet balance is as expected" $ do
            rGet <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            verify rGet
                [ expectField (#balance . #total)
                    (`shouldBe` Quantity minUTxOValue')
                , expectField (#balance . #available)
                    (`shouldBe` Quantity minUTxOValue')
                , expectField (#assets . #available)
                    (`shouldBe` mempty)
                , expectField (#assets . #total)
                    (`shouldBe` mempty)
                ]

        -- delete wallet
        rDel <- request @ApiWallet ctx (Link.deleteWallet @'Shelley wDest) Default Empty
        expectResponseCode HTTP.status204 rDel

        -- restore and make sure funds are there
        rRestore <- postWallet ctx payldCrt
        expectResponseCode HTTP.status201 rRestore
        eventually "Wallet balance is ok on restored wallet" $ do
            rGet <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            verify rGet
                [ expectField
                        (#balance . #total) (`shouldBe` Quantity minUTxOValue')
                , expectField
                        (#balance . #available) (`shouldBe` Quantity minUTxOValue')
                ]

    it "WALLETS_CREATE_03,09 - Cannot create wallet that exists" $ \ctx -> runResourceT $ do
        m21 <- liftIO $ genMnemonics M21
        let payload = Json [json| {
                "name": "Some Wallet",
                "mnemonic_sentence": #{m21},
                "passphrase": #{fixturePassphrase}
                } |]
        r1 <- postWallet ctx payload
        expectResponseCode HTTP.status201 r1

        r2 <- postWallet ctx payload
        verify r2
            [ expectResponseCode HTTP.status409
            , expectErrorMessage ("This operation would yield a wallet with the\
                \ following id: " ++ T.unpack (getFromResponse walletId r1) ++
                " However, I already know of a wallet with this id.")
            ]

    describe "WALLETS_CREATE_04 - Wallet name" $ do
        let walNameMax = T.pack (replicate walletNameMaxLength 'ą')
        let matrix =
                [ ( show walletNameMinLength ++ " char long", "1"
                  , [ expectResponseCode HTTP.status201
                    , expectField
                            (#name . #getApiT . #getWalletName) (`shouldBe` "1")
                    ]
                  )
                , ( show walletNameMaxLength ++ " char long", walNameMax
                  , [ expectResponseCode HTTP.status201
                    , expectField
                            (#name . #getApiT . #getWalletName) (`shouldBe` walNameMax)
                    ]
                  )
                , ( "Russian name", russianWalletName
                  , [ expectResponseCode HTTP.status201
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` russianWalletName)
                    ]
                  )
                , ( "Polish name", polishWalletName
                  , [ expectResponseCode HTTP.status201
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` polishWalletName)
                    ]
                  )
                , ( "Kanji name", kanjiWalletName
                  , [ expectResponseCode HTTP.status201
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` kanjiWalletName)
                    ]
                  )
                , ( "Arabic name", arabicWalletName
                  , [ expectResponseCode HTTP.status201
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` arabicWalletName)
                    ]
                  )
                , ( "Wildcards name", wildcardsWalletName
                  , [ expectResponseCode HTTP.status201
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` wildcardsWalletName)
                    ]
                  )
                ]
        forM_ matrix $ \(title, walName, expectations) -> it title $ \ctx -> runResourceT $ do
            m24 <- liftIO $ genMnemonics M24
            let payload = Json [json| {
                    "name": #{walName},
                    "mnemonic_sentence": #{m24},
                    "passphrase": #{fixturePassphrase}
                    } |]
            r <- postWallet ctx payload
            verify r expectations

    describe "WALLETS_CREATE_05 - Mnemonics" $ do
        let matrix =
             [ ( "15 mnemonic words", M15 )
             , ( "18 mnemonic words", M18 )
             , ( "21 mnemonic words", M21 )
             , ( "24 mnemonic words", M24 )
             ]

        forM_ matrix $ \(title, mnemonics) -> it title $ \ctx -> runResourceT $ do
            m <- liftIO $ genMnemonics mnemonics
            let payload = Json [json| {
                    "name": "Just a łallet",
                    "mnemonic_sentence": #{m},
                    "passphrase": #{fixturePassphrase}
                    } |]
            r <- postWallet ctx payload
            verify r [ expectResponseCode HTTP.status201 ]

    describe "WALLETS_CREATE_06 - Mnemonics second factor" $ do
        let matrix =
                 [ ( "9 mnemonic words", M9 )
                 , ( "12 mnemonic words", M12 )
                 ]
        forM_ matrix $ \(title, mnemonics) -> it title $ \ctx -> runResourceT $ do
            m15 <- liftIO $ genMnemonics M15
            mSecondFactor <- liftIO $ genMnemonics mnemonics

            let payload = Json [json| {
                    "name": "Just a łallet",
                    "mnemonic_sentence": #{m15},
                    "mnemonic_second_factor": #{mSecondFactor},
                    "passphrase": #{fixturePassphrase}
                    } |]
            r <- postWallet ctx payload
            verify r [ expectResponseCode HTTP.status201 ]

    describe "WALLETS_CREATE_07 - Passphrase" $ do
        let minLength = passphraseMinLength (Proxy @"user")
        let maxLength = passphraseMaxLength (Proxy @"user")
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
        forM_ matrix $ \(title, passphrase) -> it title $ \ctx -> runResourceT $ do
            m24 <- liftIO $ genMnemonics M24
            let payload = Json [json| {
                    "name": "Secure Wallet",
                    "mnemonic_sentence": #{m24},
                    "passphrase": #{passphrase}
                    } |]
            r <- postWallet ctx payload
            verify r [ expectResponseCode HTTP.status201 ]

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
                  , [ expectResponseCode HTTP.status201
                    , expectField (#addressPoolGap . #getApiT) (`shouldBe` minBound)
                    ]
                  )
                , ( show addrPoolBig
                  , addrPoolBig
                  , [ expectResponseCode HTTP.status201
                    , expectField
                        (#addressPoolGap . #getApiT . #getAddressPoolGap)
                        (`shouldBe` maxDaedalusGap)
                  ]
                  )
                ]
        forM_ matrix $ \(title, addrPoolGap, expectations) -> it title $ \ctx -> runResourceT $ do
            m24 <- liftIO $ genMnemonics M24
            let payload = payloadWith' "Secure Wallet" m24 (fromIntegral addrPoolGap)
            rW <- postWallet ctx payload
            verify rW expectations

            let w = getFromResponse id rW
            rA <- request @[ApiAddress n] ctx
                (Link.listAddresses @'Shelley w) Default Empty
            _ <- request @ApiWallet ctx
                (Link.deleteWallet @'Shelley w) Default Empty
            verify rA
                [ expectListSize addrPoolGap
                ]

    it "WALLETS_CREATE_08 - default address_pool_gap" $ \ctx -> runResourceT $ do
        m21 <- liftIO $ genMnemonics M21
        let payload = Json [json| {
                "name": "Secure Wallet",
                "mnemonic_sentence": #{m21},
                "passphrase": "Secure passphrase"
                } |]
        r <- postWallet ctx payload
        verify r
            [ expectResponseCode HTTP.status201
            , expectField
                    (#addressPoolGap . #getApiT . #getAddressPoolGap) (`shouldBe` 20)
            ]

    describe "WALLETS_CREATE_09 - HTTP headers" $ do
        let matrix =
                 [ ( "No HTTP headers -> 415", None
                   , [ expectResponseCode HTTP.status415
                     , expectErrorMessage errMsg415 ]
                   )
                 , ( "Accept: text/plain -> 406"
                   , Headers
                         [ ("Content-Type", "application/json")
                         , ("Accept", "text/plain") ]
                   , [ expectResponseCode HTTP.status406
                     , expectErrorMessage errMsg406 ]
                   )
                 , ( "No Accept -> 201"
                   , Headers [ ("Content-Type", "application/json") ]
                   , [ expectResponseCode HTTP.status201 ]
                   )
                 , ( "No Content-Type -> 415"
                   , Headers [ ("Accept", "application/json") ]
                   , [ expectResponseCode HTTP.status415
                     , expectErrorMessage errMsg415 ]
                   )
                 , ( "Content-Type: text/plain -> 415"
                   , Headers [ ("Content-Type", "text/plain") ]
                   , [ expectResponseCode HTTP.status415
                     , expectErrorMessage errMsg415 ]
                   )
                 ]
        forM_ matrix $ \(title, headers, expectations) -> it title $ \ctx -> runResourceT $ do
            m21 <- liftIO $ genMnemonics M21
            let payload = Json [json| {
                    "name": "Secure Wallet",
                    "mnemonic_sentence": #{m21},
                    "passphrase": "Secure passphrase"
                    } |]
            r <- postWallet' ctx headers payload
            verify r expectations


    it "WALLETS_GET_01 - can get wallet details" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx

        eventually "I can get all wallet details" $ do
            rg <- request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty
            verify rg
                [ expectResponseCode HTTP.status200
                , expectField
                        (#name . #getApiT . #getWalletName) (`shouldBe` "Empty Wallet")
                , expectField
                        (#addressPoolGap . #getApiT . #getAddressPoolGap) (`shouldBe` 20)
                , expectField
                        (#balance . #available) (`shouldBe` Quantity 0)
                , expectField
                        (#balance . #total) (`shouldBe` Quantity 0)
                , expectField
                        (#balance . #reward) (`shouldBe` Quantity 0)
                , expectField (#state . #getApiT) (`shouldBe` Ready)
                , expectField #delegation (`shouldBe` notDelegating [])
                , expectField walletId (`shouldBe` w ^. walletId)
                , expectField #passphrase (`shouldNotBe` Nothing)
                ]

    it "WALLETS_GET_02, WALLETS_DELETE_01 - Deleted wallet is not available" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx
            (Link.deleteWallet @'Shelley w) Default Empty
        rg <- request @ApiWallet ctx
            (Link.getWallet @'Shelley w) Default Empty
        expectResponseCode HTTP.status404 rg
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) rg

    it "WALLETS_LIST_01 - Created a wallet can be listed" $ \ctx -> runResourceT $ do
        m18 <- liftIO $ genMnemonics M18
        m9 <- liftIO $ genMnemonics M9
        let payload = Json [json| {
                "name": "Wallet to be listed",
                "mnemonic_sentence": #{m18},
                "mnemonic_second_factor": #{m9},
                "passphrase": #{fixturePassphrase},
                "address_pool_gap": 20
                } |]
        rp <- postWallet ctx payload
        expectResponseCode HTTP.status201 rp
        let wid = getFromResponse walletId rp
        rl <- listFilteredWallets (Set.singleton wid) ctx
        verify rl
            [ expectResponseCode HTTP.status200
            , expectListSize 1
            , expectListField 0
                    (#name . #getApiT . #getWalletName)
                    (`shouldBe` "Wallet to be listed")
            , expectListField 0
                    (#addressPoolGap . #getApiT . #getAddressPoolGap) (`shouldBe` 20)
            , expectListField 0
                    (#balance . #available) (`shouldBe` Quantity 0)
            , expectListField 0
                    (#balance . #total) (`shouldBe` Quantity 0)
            , expectListField 0
                    (#balance . #reward) (`shouldBe` Quantity 0)
            , expectListField 0 #delegation (`shouldBe` notDelegating [])
            ]

    it "WALLETS_LIST_01 - Wallets are listed from oldest to newest" $ \ctx -> runResourceT $ do
        m15 <- liftIO $ genMnemonics M15
        m18 <- liftIO $ genMnemonics M18
        m21 <- liftIO $ genMnemonics M21
        let walletDetails = [("1", m15), ("2", m18)
                    , ("3", m21)]
        wids <- forM walletDetails $ \(name, mnemonics) -> do
            let payload = payloadWith name mnemonics
            rp <- postWallet ctx payload
            expectResponseCode HTTP.status201 rp
            pure (getFromResponse walletId rp)

        rl <- listFilteredWallets (Set.fromList wids) ctx
        verify rl
            [ expectResponseCode HTTP.status200
            , expectListSize 3
            , expectListField 0
                (#name . #getApiT . #getWalletName) (`shouldBe` "1")
            , expectListField 1
                (#name . #getApiT . #getWalletName) (`shouldBe` "2")
            , expectListField 2
                (#name . #getApiT . #getWalletName) (`shouldBe` "3")
            ]

    it "WALLETS_LIST_02 - Deleted wallet not listed" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
        rl <- listFilteredWallets (Set.singleton $ w ^. walletId) ctx
        verify rl
            [ expectResponseCode HTTP.status200
            , expectListSize 0
            ]

    it "WALLETS_UPDATE_01 - Updated wallet name is available" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        let passLastUpdateValue = w ^. #passphrase
        let newName = updateNamePayload "New great name"
        let walId = w ^. walletId
        let expectations = [ expectResponseCode HTTP.status200
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` "New great name")
                    , expectField
                            (#addressPoolGap . #getApiT . #getAddressPoolGap)
                            (`shouldBe` 20)
                    , expectField
                            (#balance . #available) (`shouldBe` Quantity 0)
                    , expectField
                            (#balance . #total) (`shouldBe` Quantity 0)
                    , expectField #delegation (`shouldBe` notDelegating [])
                    , expectField walletId (`shouldBe` walId)
                    , expectField #passphrase (`shouldBe` passLastUpdateValue)
                    ]
        ru <- request @ApiWallet ctx
            ("PUT", "v2/wallets" </> walId) Default newName
        verify ru expectations
        rg <- request @ApiWallet ctx
            ("GET", "v2/wallets" </> walId) Default Empty
        verify rg expectations
        verify ru expectations

    describe "WALLETS_UPDATE_02 - Various names" $ do
        let walNameMax = T.pack (replicate walletNameMaxLength 'ą')
        let matrix =
                [ ( show walletNameMinLength ++ " char long", "1"
                  , [ expectResponseCode HTTP.status200
                    , expectField
                            (#name . #getApiT . #getWalletName) (`shouldBe` "1")
                    ]
                  )
                , ( show walletNameMaxLength ++ " char long", walNameMax
                  , [ expectResponseCode HTTP.status200
                    , expectField
                            (#name . #getApiT . #getWalletName) (`shouldBe` walNameMax)
                    ]
                  )
                , ( "Russian name", russianWalletName
                  , [ expectResponseCode HTTP.status200
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` russianWalletName)
                    ]
                  )
                , ( "Polish name", polishWalletName
                  , [ expectResponseCode HTTP.status200
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` polishWalletName)
                    ]
                  )
                , ( "Kanji name", kanjiWalletName
                  , [ expectResponseCode HTTP.status200
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` kanjiWalletName)
                    ]
                  )
                , ( "Arabic name", arabicWalletName
                  , [ expectResponseCode HTTP.status200
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` arabicWalletName)
                    ]
                  )
                , ( "Wildcards name", wildcardsWalletName
                  , [ expectResponseCode HTTP.status200
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` wildcardsWalletName)
                    ]
                  )
                ]
        forM_ matrix $ \(title, walName, expectations) -> it title $ \ctx -> runResourceT $ do
            w <- emptyWallet ctx
            let newName = updateNamePayload walName
            let endpoint = "v2/wallets" </> (w ^. walletId)
            ru <- request @ApiWallet ctx ("PUT", endpoint) Default newName
            verify ru expectations

    it "WALLETS_UPDATE_03 - Deleted wallet cannot be updated (404)" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        let wid = w ^. walletId
        let endpoint = "v2/wallets" </> wid
        _ <- request @ApiWallet ctx ("DELETE", endpoint) Default Empty

        let newName = updateNamePayload "new name"
        ru <- request @ApiWallet ctx ("PUT", endpoint) Default newName
        expectResponseCode HTTP.status404 ru
        expectErrorMessage (errMsg404NoWallet wid) ru

    describe "WALLETS_UPDATE_04 - HTTP headers" $ do
        let matrix =
                  [ ( "No HTTP headers -> 415", None
                    , [ expectResponseCode HTTP.status415
                      , expectErrorMessage errMsg415 ]
                    )
                  , ( "Accept: text/plain -> 406"
                    , Headers
                          [ ("Content-Type", "application/json")
                          , ("Accept", "text/plain") ]
                    , [ expectResponseCode HTTP.status406
                      , expectErrorMessage errMsg406 ]
                    )
                  , ( "No Accept -> 200"
                    , Headers [ ("Content-Type", "application/json") ]
                    , [ expectResponseCode HTTP.status200 ]
                    )
                  , ( "No Content-Type -> 415"
                    , Headers [ ("Accept", "application/json") ]
                    , [ expectResponseCode HTTP.status415
                      , expectErrorMessage errMsg415 ]
                    )
                  , ( "Content-Type: text/plain -> 415"
                    , Headers [ ("Content-Type", "text/plain") ]
                    , [ expectResponseCode HTTP.status415
                      , expectErrorMessage errMsg415 ]
                    )
                  ]
        forM_ matrix $ \(title, headers, expectations) -> it title $ \ctx -> runResourceT $ do
            w <- emptyWallet ctx
            let newName = updateNamePayload "new name"
            let endpoint = "v2/wallets" </> (w ^. walletId)
            ru <- request @ApiWallet ctx ("PUT", endpoint) headers newName
            verify ru expectations

    it "WALLETS_UPDATE_PASS_01 - passphaseLastUpdate gets updated" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        let payload = updatePassPayload fixturePassphrase "New passphrase"
        let endpoint = "v2/wallets" </> (w ^. walletId)
                </> ("passphrase" :: Text)
        rup <- request @ApiWallet ctx ("PUT", endpoint) Default payload
        expectResponseCode HTTP.status204 rup

        let getEndpoint = "v2/wallets" </> (w ^. walletId)
        let originalPassUpdateDateTime = w ^. #passphrase
        rg <- request @ApiWallet ctx ("GET", getEndpoint) Default Empty
        expectField #passphrase (`shouldNotBe` originalPassUpdateDateTime) rg

    describe "WALLETS_UPDATE_PASS_02 - New passphrase values" $ do
        let minLength = passphraseMinLength (Proxy @"user")
        let maxLength = passphraseMaxLength (Proxy @"user")
        let matrix =
                [ ( show minLength ++ " char long"
                  , T.pack (replicate minLength 'ź')
                  , [ expectResponseCode HTTP.status204
                    ]
                  )
                , ( show maxLength ++ " char long"
                  , T.pack (replicate maxLength 'ą')
                  , [ expectResponseCode HTTP.status204 ]
                  )
                , ( "Russian passphrase", russianWalletName
                  , [ expectResponseCode HTTP.status204 ]
                  )
                , ( "Polish passphrase", polishWalletName
                  , [ expectResponseCode HTTP.status204 ]
                  )
                , ( "Kanji passphrase", kanjiWalletName
                  , [ expectResponseCode HTTP.status204 ]
                  )
                , ( "Arabic passphrase", arabicWalletName
                  , [ expectResponseCode HTTP.status204 ]
                  )
                , ( "Wildcards passphrase", wildcardsWalletName
                  , [ expectResponseCode HTTP.status204 ]
                  )
                ]
        forM_ matrix $ \(title, passphrase, expectations) -> it title $ \ctx -> runResourceT $ do
            w <- emptyWallet ctx
            let payload = updatePassPayload fixturePassphrase passphrase
            let endpoint = "v2/wallets" </> (w ^. walletId)
                    </> ("passphrase" :: Text)
            rup <- request @ApiWallet ctx ("PUT", endpoint) Default payload
            verify rup expectations

    it "WALLETS_UPDATE_PASS_03 - Old passphrase incorrect" $ \ctx -> runResourceT $ do
        w <- emptyWalletWith ctx
            ("Wallet to update pass", "cardano-passphrase", 20)
        let payload = updatePassPayload "incorrect-passphrase" "whatever-pass"
        rup <- request @ApiWallet ctx
            (Link.putWalletPassphrase @'Shelley w) Default payload
        expectResponseCode HTTP.status403 rup
        expectErrorMessage errMsg403WrongPass rup

    describe "WALLETS_UPDATE_PASS_03 - Can update pass from pass that's boundary\
    \ value" $ do
        let minLength = passphraseMinLength (Proxy @"user")
        let maxLength = passphraseMaxLength (Proxy @"user")
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
        forM_ matrix $ \(title, oldPass) -> it title $ \ctx -> runResourceT $ do
            m24 <- liftIO $ genMnemonics M24
            let createPayload = Json [json| {
                     "name": "Name of the wallet",
                     "mnemonic_sentence": #{m24},
                     "passphrase": #{oldPass}
                     } |]
            w <- unsafeResponse <$> postWallet ctx createPayload
            let len = passphraseMaxLength (Proxy @"user")
            let newPass = T.pack $ replicate len '💘'
            let payload = updatePassPayload oldPass newPass
            rup <- request @ApiWallet ctx
                (Link.putWalletPassphrase @'Shelley w) Default payload
            expectResponseCode HTTP.status204 rup

    it "WALLETS_UPDATE_PASS_04 - Deleted wallet is not available" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        let payload = updatePassPayload fixturePassphrase "Secure passphrase2"
        let walId = w ^. walletId
        let delEndp = "v2/wallets" </> walId
        _ <- request @ApiWallet ctx ("DELETE", delEndp) Default Empty
        let updEndp = delEndp </> ("passphrase" :: Text)
        rup <- request @ApiWallet ctx ("PUT", updEndp) Default payload
        expectResponseCode HTTP.status404 rup
        expectErrorMessage (errMsg404NoWallet walId) rup

    describe "WALLETS_UPDATE_PASS_05,06 - Transaction after updating passphrase" $ do
        let oldPass = "cardano-wallet"
        let newPass = "cardano-wallet2"
        let matrix = [ ("Old passphrase -> fail", oldPass
                       , [ expectResponseCode HTTP.status403
                         , expectErrorMessage errMsg403WrongPass ] )
                     , ("New passphrase -> OK", newPass
                       , [ expectResponseCode HTTP.status202 ] )
                     ]

        forM_ matrix $ \(title, pass, expectations) -> it title $ \ctx -> runResourceT $ do
            wSrc <- fixtureWallet ctx
            wDest <- emptyWallet ctx
            let payloadUpdate = updatePassPayload oldPass newPass
            rup <- request @ApiWallet ctx
                   (Link.putWalletPassphrase @'Shelley wSrc) Default payloadUpdate
            expectResponseCode HTTP.status204 rup

            addrs <- listAddresses @n ctx wDest
            let destination = (addrs !! 1) ^. #id
            let payloadTrans = Json [json|{
                    "payments": [{
                        "address": #{destination},
                        "amount": {
                            "quantity": #{minUTxOValue (_mainEra ctx)},
                            "unit": "lovelace"
                        }
                    }],
                    "passphrase": #{pass}
                    }|]
            r <- request @(ApiTransaction n) ctx
                (Link.createTransactionOld @'Shelley wSrc) Default payloadTrans
            verify r expectations

    describe "WALLETS_UPDATE_PASS_07 - HTTP headers" $ do
        let matrix =
                  [ ( "No HTTP headers -> 415", None
                    , [ expectResponseCode HTTP.status415
                      , expectErrorMessage errMsg415 ]
                    )
                  , ( "Accept: text/plain -> 406"
                    , Headers
                          [ ("Content-Type", "application/json")
                          , ("Accept", "text/plain") ]
                    , [ expectResponseCode HTTP.status204 ]
                    )
                  , ( "No Accept -> 204"
                    , Headers [ ("Content-Type", "application/json") ]
                    , [ expectResponseCode HTTP.status204 ]
                    )
                  , ( "No Content-Type -> 415"
                    , Headers [ ("Accept", "application/json") ]
                    , [ expectResponseCode HTTP.status415
                      , expectErrorMessage errMsg415 ]
                    )
                  , ( "Content-Type: text/plain -> 415"
                    , Headers [ ("Content-Type", "text/plain") ]
                    , [ expectResponseCode HTTP.status415
                      , expectErrorMessage errMsg415 ]
                    )
                  ]
        forM_ matrix $ \(title, headers, expectations) -> it title $ \ctx -> runResourceT $ do
            mnemonic <- liftIO $ genMnemonics M24
            w <- unsafeResponse <$> postWallet ctx (simplePayload mnemonic)
            let payload = updatePassPayload fixturePassphrase "Passphrase"
            let endpoint = Link.putWalletPassphrase @'Shelley w
            rup <- request @ApiWallet ctx endpoint headers payload
            verify rup expectations

    it "WALLETS_UTXO_01 - Wallet's inactivity is reflected in utxo" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        rStat <- request @ApiUtxoStatistics ctx
                 (Link.getUTxOsStatistics @'Shelley w) Default Empty
        expectResponseCode HTTP.status200 rStat
        expectWalletUTxO [] (snd rStat)

    it "WALLET_UTXO_SNAPSHOT_01 - \
        \Can generate UTxO snapshot of empty wallet" $
        \ctx -> runResourceT $ do
            w <- emptyWallet ctx
            rSnap <- request @ApiWalletUtxoSnapshot ctx
                (Link.getWalletUtxoSnapshot @'Shelley w) Default Empty
            expectResponseCode HTTP.status200 rSnap
            expectField #entries (`shouldBe` []) rSnap

    it "WALLET_UTXO_SNAPSHOT_02 - \
        \Can generate UTxO snapshot of pure-ada wallet" $
        \ctx -> runResourceT $ do
            w <- fixtureWallet ctx
            rSnap <- request @ApiWalletUtxoSnapshot ctx
                (Link.getWalletUtxoSnapshot @'Shelley w) Default Empty
            expectResponseCode HTTP.status200 rSnap
            let entries = getFromResponse #entries rSnap
            length entries `shouldBe` 10

    it "WALLET_UTXO_SNAPSHOT_03 - \
        \Can generate UTxO snapshot of multi-asset wallet" $
        \ctx -> runResourceT $ do
            w <- fixtureMultiAssetWallet ctx
            rSnap <- request @ApiWalletUtxoSnapshot ctx
                (Link.getWalletUtxoSnapshot @'Shelley w) Default Empty
            expectResponseCode HTTP.status200 rSnap
            let entries = getFromResponse #entries rSnap
            length entries `shouldBe` 3

    it "WALLETS_UTXO_02 - Sending and receiving funds updates wallet's utxo." $ \ctx -> runResourceT $ do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx

        --send funds
        addrs <- listAddresses @n ctx wDest
        let destination = (addrs !! 1) ^. #id
        let coins :: [Natural]
            coins =
                [13_000_000, 43_000_000, 66_000_000, 101_000_000, 1339_000_000]
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
            (Link.createTransactionOld @'Shelley wSrc) Default (Json payload)
        expectResponseCode HTTP.status202 rTrans

        eventually "Wallet balance is as expected" $ do
            rGet <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            verify rGet
                [ expectField
                        (#balance . #total)
                        (`shouldBe` Quantity (fromIntegral $ sum coins))
                , expectField
                        (#balance . #available)
                        (`shouldBe` Quantity (fromIntegral $ sum coins))
                ]

        --verify utxo
        rStat1 <- request @ApiUtxoStatistics ctx
            (Link.getUTxOsStatistics @'Shelley wDest) Default Empty
        expectResponseCode HTTP.status200 rStat1
        expectWalletUTxO coins (snd rStat1)

    it "WALLETS_UTXO_03 - Deleted wallet is not available for utxo" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w)
            Default Empty
        r <- request @ApiUtxoStatistics ctx (Link.getUTxOsStatistics @'Shelley w)
            Default Empty
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    describe "WALLETS_UTXO_04 - HTTP headers" $ do
        let matrix =
                [ ( "No HTTP headers -> 200"
                  , None
                  , [ expectResponseCode HTTP.status200 ] )
                , ( "Accept: text/plain -> 406"
                  , Headers
                        [ ("Content-Type", "application/json")
                        , ("Accept", "text/plain") ]
                  , [ expectResponseCode HTTP.status406
                    , expectErrorMessage errMsg406 ]
                  )
                , ( "No Accept -> 200"
                  , Headers [ ("Content-Type", "application/json") ]
                  , [ expectResponseCode HTTP.status200 ]
                  )
                , ( "No Content-Type -> 200"
                  , Headers [ ("Accept", "application/json") ]
                  , [ expectResponseCode HTTP.status200 ]
                  )
                , ( "Content-Type: text/plain -> 200"
                  , Headers [ ("Content-Type", "text/plain") ]
                  , [ expectResponseCode HTTP.status200 ]
                  )
                ]
        forM_ matrix $ \(title, headers, expectations) -> it title $ \ctx -> runResourceT $ do
            w <- emptyWallet ctx
            r <- request @ApiUtxoStatistics ctx (Link.getUTxOsStatistics @'Shelley w) headers Empty
            verify r expectations

    it "WALLETS_GET_KEY_01 - golden tests for verification key" $ \ctx -> runResourceT $ do
    --- $ cat recovery-phrase.txt
    -- pulp ten light rhythm replace vessel slow drift kingdom amazing negative join auction ugly symptom
    --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley > root.prv
    --- $ cat root.prv \
    --- > | cardano-address key child 1852H/1815H/0H/ROLE/INDEX \
    --- > | cardano-address key public \
        let matrix :: [(Role, DerivationIndex, String)]
            matrix =
                [ ( UtxoExternal
                  , DerivationIndex 0
                  , "addr_vk1tmdggpmj7r2mqfhneqsc5fzfj2j4sxf4j7mqwwsa8w58vz94zr4seun089"
                  )
                , ( UtxoInternal
                  , DerivationIndex 100
                  , "addr_vk1wchen6vz4zz7kpfjld3g89zdcpdv2hzvtsufphgvpjxjkl49pqrqaj4j0e"
                  )
                , ( MutableAccount
                  , DerivationIndex 2147483647
                  , "stake_vk1qy9tp370ze3cfre8f6daz7l85pgk3wpg6s5zqae2yjljwqkx4htqc7kr4p"
                  )
                ]

        let explicitMnemonics =
                [ "pulp", "ten", "light", "rhythm", "replace"
                , "vessel", "slow", "drift", "kingdom", "amazing"
                , "negative", "join", "auction", "ugly", "symptom"] :: [Text]
        let payload = Json [json|{
                "name": "Wallet",
                "mnemonic_sentence": #{explicitMnemonics},
                "passphrase": #{fixturePassphrase}
            }|]

        r <- postWallet ctx payload
        verify r [ expectResponseCode HTTP.status201 ]
        let apiWal = getFromResponse id r

        forM_ matrix $ \(role_, index, expected) ->
            counterexample (show role_ <> "/" <> show index) $ do
                let link = Link.getWalletKey @'Shelley (apiWal ^. id) role_ index Nothing
                rGet <- request @ApiVerificationKeyShelley ctx link Default Empty
                verify rGet
                    [ expectResponseCode HTTP.status200
                    , expectField id (\k -> toJSON k `shouldBe` toJSON expected)
                    ]

    it "WALLETS_GET_KEY_02 - invalid index for verification key" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx

        let link = Link.getWalletKey @'Shelley w UtxoExternal (DerivationIndex 2147483648) Nothing
        r <- request @ApiVerificationKeyShelley ctx link Default Empty

        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage
                "It looks like you've provided a derivation index that is out of bound."
            ]

    it "WALLETS_GET_KEY_03 - unknown wallet" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty

        let link = Link.getWalletKey @'Shelley w UtxoExternal (DerivationIndex 0) Nothing
        r <- request @ApiVerificationKeyShelley ctx link Default Empty

        verify r
            [ expectResponseCode HTTP.status404
            , expectErrorMessage (errMsg404NoWallet $ w ^. walletId)
            ]

    it "WALLETS_SIGNATURES_01 - can verify signature" $ \ctx -> runResourceT $ do
        w <- constFixtureWalletNoWait ctx

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
        expectResponseCode HTTP.status200 rSig

        -- get corresponding public key
        rKey <- request @ApiVerificationKeyShelley ctx
            (Link.getWalletKey @'Shelley w role_ index Nothing)
            Default
            Empty
        verify rKey
            [ expectResponseCode HTTP.status200
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
        let dummyChainCode = BS.replicate 32 0
        let sigBytes = BL.toStrict $ getFromResponse id rSig
        let sig = CC.xsignature sigBytes
        let key = unsafeXPub $ fst (getFromResponse #getApiVerificationKey rKey) <> dummyChainCode
        let msgHash = unsafeFromHex "1228cd0fea46f9a091172829f0c492c0516dceff67de08f585a4e048a28a6c9f"
        liftIO $ CC.verify key msgHash <$> sig `shouldBe` Right True

        let goldenSig = "680739414d89eb9f4377192171ce3990c7beea6132a04f327d7c954ae9e7fcfe747dd7b4b9b11acefa1aa75216b837fc81e59c24001b96356ba65598ec159d0c" :: ByteString
        convertToBase Base16 sigBytes `shouldBe` goldenSig

    it "WALLETS_SIGNATURES_02 - invalid index for signing key" $ \ctx -> runResourceT $  do
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
            [ expectResponseCode HTTP.status403
            , expectErrorMessage
                "It looks like you've provided a derivation index that is out of bound."
            ]

    it "WALLETS_SIGNATURES_03 - unknown wallet" $ \ctx -> runResourceT $ do
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
            [ expectResponseCode HTTP.status404
            , expectErrorMessage (errMsg404NoWallet $ w ^. walletId)
            ]

    it "BYRON_WALLETS_UTXO -\
        \ Cannot show Byron wal utxo with shelley ep (404)" $ \ctx -> runResourceT $ do
        w <- emptyRandomWallet ctx
        let wid = w ^. walletId
        let endpoint =
                    "v2/wallets"
                    </> wid
                    </> ("statistics/utxos" :: Text)
        r <- request @ApiUtxoStatistics ctx ("GET", endpoint) Default Empty
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet wid) r

    it "BYRON_WALLETS_UPDATE_PASS -\
        \ Cannot update Byron wal with shelley ep (404)" $ \ctx -> runResourceT $ do
        w <- emptyRandomWallet ctx
        let payload = updatePassPayload fixturePassphrase "Secure passphrase2"
        let wid = w ^. walletId
        let endpoint =
                "v2/wallets"
                </> wid
                </> ("passphrase" :: Text)
        rup <- request @ApiWallet ctx ("PUT", endpoint) Default payload
        expectResponseCode HTTP.status404 rup
        expectErrorMessage (errMsg404NoWallet wid) rup

    it "BYRON_WALLETS_UPDATE -\
        \ Cannot update Byron wal with shelley ep (404)" $ \ctx -> runResourceT $ do
        w <- emptyRandomWallet ctx
        let wid = w ^. walletId
        let endpoint = "v2/wallets" </> wid
        let newName = updateNamePayload "new name"
        ru <- request @ApiWallet ctx ("PUT", endpoint) Default newName
        expectResponseCode HTTP.status404 ru
        expectErrorMessage (errMsg404NoWallet wid) ru

    it "BYRON_GET_02 - Byron ep does not show Shelley wallet" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        r <- request @ApiByronWallet ctx
            (Link.getWallet @'Byron w) Default Empty
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    it "BYRON_GET_03 - Shelley ep does not show Byron wallet" $ \ctx -> runResourceT $ do
        w <- emptyRandomWallet ctx
        r <- request @ApiWallet ctx
            (Link.getWallet @'Shelley w) Default Empty
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    it "BYRON_LIST_02,03 -\
        \ Byron wallets listed only via Byron endpoints \\\
        \ Shelley wallets listed only via new endpoints" $ \ctx -> runResourceT $ do
        m1 <- liftIO $ genMnemonics M12
        m2 <- liftIO $ genMnemonics M12
        m3 <- liftIO $ genMnemonics M12
        r1 <- emptyByronWalletWith ctx "random" ("byron1", m1, fixturePassphrase)
        r2 <- emptyByronWalletWith ctx "random" ("byron2", m2, fixturePassphrase)
        r3 <- emptyByronWalletWith ctx "random" ("byron3", m3, fixturePassphrase)

        r4 <- emptyWalletWith ctx ("shelley1", fixturePassphrase, 20)
        r5 <- emptyWalletWith ctx ("shelley2", fixturePassphrase, 20)
        r6 <- emptyWalletWith ctx ("shelley3", fixturePassphrase, 20)

        let wids = Set.fromList
                $ map (view walletId) [r1,r2,r3]
                ++ map (view walletId) [r4,r5,r6]

        --list only byron
        rl <- listFilteredByronWallets wids ctx
        verify rl
            [ expectResponseCode HTTP.status200
            , expectListSize 3
            , expectListField 0
                    (#name . #getApiT . #getWalletName) (`shouldBe` "byron1")
            , expectListField 1
                    (#name . #getApiT . #getWalletName) (`shouldBe` "byron2")
            , expectListField 2
                    (#name . #getApiT . #getWalletName) (`shouldBe` "byron3")
            ]
        --list only shelley
        rl2 <- listFilteredWallets wids ctx
        verify rl2
            [ expectResponseCode HTTP.status200
            , expectListSize 3
            , expectListField 0
                    (#name . #getApiT . #getWalletName) (`shouldBe` "shelley1")
            , expectListField 1
                    (#name . #getApiT . #getWalletName) (`shouldBe` "shelley2")
            , expectListField 2
                    (#name . #getApiT . #getWalletName) (`shouldBe` "shelley3")
            ]

    it "BYRON_LIST_04, DELETE_01 -\
        \ Deleted wallets cannot be listed" $ \ctx -> runResourceT $ do
        m1 <- liftIO $ genMnemonics M12
        m2 <- liftIO $ genMnemonics M12
        m3 <- liftIO $ genMnemonics M12
        _wb1   <- emptyByronWalletWith ctx "random" ("byron1", m1, fixturePassphrase)
        wb2 <- emptyByronWalletWith ctx "random" ("byron2", m2, fixturePassphrase)
        _wb3   <- emptyByronWalletWith ctx "random" ("byron3", m3, fixturePassphrase)

        _ws1 <- emptyWalletWith ctx ("shelley1", fixturePassphrase, 20)
        _ws2 <- emptyWalletWith ctx ("shelley2", fixturePassphrase, 20)
        ws3 <- emptyWalletWith ctx ("shelley3", fixturePassphrase, 20)

        let wids = Set.fromList
                $ map (view walletId) [_wb1,wb2,_wb3]
                ++ map (view walletId) [_ws1,_ws2,ws3]

        -- delete
        _ <- request @ApiByronWallet ctx (Link.deleteWallet @'Byron wb2) Default Empty
        _ <- request @ApiWallet ctx (Link.deleteWallet @'Shelley ws3) Default Empty

        --list only byron
        rdl <- listFilteredByronWallets wids ctx
        verify rdl
            [ expectResponseCode HTTP.status200
            , expectListSize 2
            , expectListField 0
                    (#name . #getApiT . #getWalletName) (`shouldBe` "byron1")
            , expectListField 1
                    (#name . #getApiT . #getWalletName) (`shouldBe` "byron3")
            ]
        --list only shelley
        rdl2 <- listFilteredWallets wids ctx
        verify rdl2
            [ expectResponseCode HTTP.status200
            , expectListSize 2
            , expectListField 0
                    (#name . #getApiT . #getWalletName) (`shouldBe` "shelley1")
            , expectListField 1
                    (#name . #getApiT . #getWalletName) (`shouldBe` "shelley2")
            ]

    it "BYRON_DELETE_02 - Byron ep does not delete Shelley wallet" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        r <- request @ApiByronWallet ctx (Link.deleteWallet @'Byron w) Default Empty
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    it "BYRON_DELETE_03 - Shelley ep does not delete Byron wallet" $ \ctx -> runResourceT $ do
        w <- emptyRandomWallet ctx
        r <- request @ApiByronWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    it "NETWORK_SHELLEY - Wallet has the same tip as network/information" $ \ctx -> runResourceT $ do
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
