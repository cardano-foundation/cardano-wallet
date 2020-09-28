{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Jormungandr.Scenario.API.Transactions
    ( spec
    , fixtureExternalTx
    , getWalletBalance
    , ExternalTxFixture (..)
    , convertToBase
    ) where

import Prelude

import Cardano.Mnemonic
    ( MkSomeMnemonic (..), mnemonicToText )
import Cardano.Wallet.Api.Types
    ( AddressAmount (..)
    , ApiFee (..)
    , ApiT (..)
    , ApiTransaction (..)
    , ApiTxId (..)
    , ApiWallet
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress
    , WalletStyle (..)
    )
import Cardano.Wallet.Jormungandr.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DelegationAddress (..), Passphrase (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Jormungandr
    ( JormungandrKey, KnownNetwork (..), generateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( GenChange (..), IsOwned (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( defaultAddressPoolGap, mkSeqStateFromRootXPrv, purposeCIP1852 )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..) )
import Cardano.Wallet.Primitive.Types
    ( Coin (..)
    , Direction (..)
    , SealedTx (..)
    , Tx (..)
    , TxIn (..)
    , TxOut (..)
    , TxStatus (..)
    , WalletId
    )
import Cardano.Wallet.Transaction
    ( TransactionLayer (..) )
import Control.Monad
    ( forM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Data.ByteArray.Encoding
    ( Base (Base16, Base64), convertFromBase, convertToBase )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Generics.Product.Typed
    ( HasType )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith, describe, it, pendingWith, shouldBe, shouldSatisfy )
import Test.Integration.Faucet
    ( nextWallet )
import Test.Integration.Framework.DSL as DSL
    ( Context (..)
    , Headers (..)
    , MnemonicLength (..)
    , Payload (..)
    , ResourceT
    , TxDescription (..)
    , between
    , emptyRandomWallet
    , emptyWallet
    , eventually
    , expectErrorMessage
    , expectField
    , expectListField
    , expectResponseCode
    , expectSuccess
    , faucetAmt
    , faucetUtxoAmt
    , fixturePassphrase
    , fixtureRandomWallet
    , fixtureRawTx
    , fixtureWallet
    , fixtureWalletWith
    , genMnemonics
    , getFromResponse
    , json
    , listAddresses
    , listAllTransactions
    , postWallet
    , request
    , runResourceT
    , unsafeRequest
    , verify
    , walletId
    , (.>=)
    )
import Test.Integration.Framework.TestData
    ( errMsg400MalformedTxPayload
    , errMsg403Fee
    , errMsg403NotEnoughMoney
    , errMsg404CannotFindTx
    )
import Test.Integration.Jcli
    ( getBlock0H )
import Test.QuickCheck
    ( generate, vector )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Cardano.Wallet.Primitive.CoinSelection as CS
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall n t.
    ( KnownNetwork n
    , DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    , DelegationAddress n JormungandrKey
    ) => SpecWith (Context t)
spec = do
    it "TRANS_CREATE_01 - Single Output Transaction" $ \ctx -> runResourceT @IO $ do
        (wa, wb) <- (,) <$> fixtureWallet ctx <*> fixtureWallet ctx
        let amt = (1 :: Natural)

        addrs <- listAddresses @n ctx wb
        let destination = (addrs !! 1) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": #{fixturePassphrase}
            }|]

        (_, ApiFee (Quantity minFee) (Quantity maxFee)) <- unsafeRequest ctx
            (Link.getTransactionFee @'Shelley wa) payload

        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wa) Default payload

        verify r
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#amount . #getQuantity) $
                between (minFee + amt, maxFee + amt)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#metadata . #getApiTxMetadata) (`shouldBe` Nothing)
            ]

        ra <- request @ApiWallet ctx (Link.getWallet @'Shelley wa) Default Empty
        verify ra
            [ expectSuccess
            , expectField (#balance . #getApiT . #total) $
                between
                    ( Quantity (faucetAmt - maxFee - amt)
                    , Quantity (faucetAmt - minFee - amt)
                    )
            , expectField
                    (#balance . #getApiT . #available)
                    (.>= Quantity (faucetAmt - faucetUtxoAmt))
            ]

        eventually "wa and wb balances are as expected" $ do
            rb <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wb) Default Empty
            expectField
                (#balance . #getApiT . #available)
                (`shouldBe` Quantity (faucetAmt + amt)) rb

            ra2 <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wa) Default Empty
            expectField
                (#balance . #getApiT . #available)
                (`shouldBe` Quantity (faucetAmt - maxFee - amt)) ra2

    it "TRANS_CREATE_02 - Multiple Output Tx to single wallet" $ \ctx -> runResourceT @IO $ do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addrs <- listAddresses @n ctx wDest

        let amt = (1 :: Natural)
        let destination1 = (addrs !! 1) ^. #id
        let destination2 = (addrs !! 2) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination1},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                },
                {
                    "address": #{destination2},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]

        (_, ApiFee (Quantity minFee) (Quantity maxFee)) <- unsafeRequest ctx
            (Link.getTransactionFee @'Shelley wSrc) payload

        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload

        ra <- request @ApiWallet ctx (Link.getWallet @'Shelley wSrc) Default Empty
        verify r
            [ expectResponseCode HTTP.status202
            , expectField (#amount . #getQuantity) $
                between (minFee + (2*amt), maxFee + (2*amt))
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            ]
        verify ra
            [ expectField (#balance . #getApiT . #total) $
                between
                    ( Quantity (faucetAmt - maxFee - (2*amt))
                    , Quantity (faucetAmt - minFee - (2*amt))
                    )
            , expectField
                    (#balance . #getApiT . #available)
                    (.>= Quantity (faucetAmt - 2 * faucetUtxoAmt))
            ]
        eventually "wDest balance is as expected" $ do
            rd <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            verify rd
                [ expectField
                        (#balance . #getApiT . #available)
                        (`shouldBe` Quantity (2*amt))
                , expectField
                        (#balance . #getApiT . #total)
                        (`shouldBe` Quantity (2*amt))
                ]

    it "TRANS_CREATE_04 - Can't cover fee" $ \ctx -> runResourceT @IO $ do
        wDest <- fixtureWallet ctx

        let amt = (1 :: Natural)
        addrs <- listAddresses @n ctx wDest
        let destination = (addrs !! 1) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": #{fixturePassphrase}
            }|]

        (_, ApiFee (Quantity minFee) _) <- unsafeRequest ctx
            (Link.getTransactionFee @'Shelley wDest) payload

        wSrc <- fixtureWalletWith @n ctx [minFee `div` 2]

        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403Fee
            ]

    it "TRANS_CREATE_04 - Not enough money" $ \ctx -> runResourceT @IO $ do
        let (srcAmt, reqAmt) = (1, 1_000_000)
        wSrc <- fixtureWalletWith @n ctx [srcAmt]
        wDest <- emptyWallet ctx

        addrs <- listAddresses @n ctx wDest
        let destination = (addrs !! 1) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{reqAmt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": #{fixturePassphrase}
            }|]

        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage $ errMsg403NotEnoughMoney srcAmt reqAmt
            ]

    it "TRANS_CREATE_09 - 0 amount transaction is accepted on single output tx" $ \ctx -> runResourceT @IO $ do
        (wSrc, payload) <- fixtureZeroAmtSingle ctx
        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload
        expectResponseCode HTTP.status202 r

    it "TRANS_CREATE_09 - 0 amount transaction is accepted on multi-output tx" $ \ctx -> runResourceT @IO $ do
        (wSrc, payload) <- fixtureZeroAmtMulti ctx
        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload
        expectResponseCode HTTP.status202 r

    it "TRANS_CREATE_10 - 'account' outputs" $ \ctx -> runResourceT @IO $ do
        (wSrc, wDest) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
        addrs <- listAddresses @n ctx wDest

        let hrp = [Bech32.humanReadablePart|addr|]
        bytes <- liftIO $ generate (vector 32)
        let (utxoAmt, utxoAddr) =
                ( 14 :: Natural
                , (addrs !! 1) ^. #id
                )
        let (accountAmt, accountAddr) =
                ( 42 :: Natural
                , Bech32.encodeLenient hrp
                $ Bech32.dataPartFromBytes
                $ BS.pack ([addrAccount @n] <> bytes)
                )

        let payload = Json [json|{
                "payments": [
                    { "address": #{utxoAddr}
                    , "amount": {"quantity":#{utxoAmt},"unit":"lovelace"}
                    },
                    { "address": #{accountAddr}
                    , "amount": {"quantity":#{accountAmt},"unit":"lovelace"}
                    }
                ],
                "passphrase": #{fixturePassphrase}
            }|]

        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status202
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            ]

        eventually "Tx is in ledger and wDest balance = utxoAmt" $ do
            request @([ApiTransaction n]) ctx
                (Link.listTransactions @'Shelley wSrc)
                Default
                Empty
                >>= flip verify
                [ expectListField 0 (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 0 (#status . #getApiT) (`shouldBe` InLedger)
                ]
            request @([ApiTransaction n]) ctx
                (Link.listTransactions @'Shelley wDest)
                Default
                Empty
                >>= flip verify
                [ expectListField 0 (#direction . #getApiT) (`shouldBe` Incoming)
                , expectListField 0 (#status . #getApiT) (`shouldBe` InLedger)
                , expectListField 0 #amount (`shouldBe` Quantity utxoAmt)
                ]
            request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest)
                Default
                Empty
                >>= flip verify
                [ expectField
                        (#balance . #getApiT . #available) (`shouldBe` Quantity utxoAmt)
                , expectField
                        (#balance . #getApiT . #total) (`shouldBe` Quantity utxoAmt)
                ]

    it "TRANS_ESTIMATE_09 - \
        \a fee can be estimated for a tx with an output of amount 0 (single)" $ \ctx -> runResourceT @IO $ do
        (wSrc, payload) <- fixtureZeroAmtSingle ctx
        r <- request @ApiFee ctx
            (Link.getTransactionFee @'Shelley wSrc) Default payload
        expectResponseCode HTTP.status202 r

    it "TRANS_ESTIMATE_09 - \
        \a fee can be estimated for a tx with an output of amount 0 (multi)" $ \ctx -> runResourceT @IO $ do
        (wSrc, payload) <- fixtureZeroAmtMulti ctx
        r <- request @ApiFee ctx
            (Link.getTransactionFee @'Shelley wSrc) Default payload
        expectResponseCode HTTP.status202 r

    it "TRANS_LIST_?? - List transactions of a fixture wallet" $ \ctx -> runResourceT @IO $ do
        txs <- fixtureWallet ctx >>= listAllTransactions @n ctx
        liftIO $ do
            length txs `shouldBe` 10
            txs `shouldSatisfy` all (null . view #inputs)

    it "TRANS_EXTERNAL_CREATE_01x - \
        \single output tx signed via jcli" $ \ctx -> runResourceT @IO $ do
        w <- emptyWallet ctx
        addr:_ <- listAddresses @n ctx w
        let amt = 1234
        payload <- liftIO $ fixtureRawTx ctx (getApiT $ fst $ addr ^. #id, amt)
        let headers = Headers
                        [ ("Content-Type", "application/octet-stream")
                        , ("Accept", "application/json")]

        request @ApiTxId ctx Link.postExternalTransaction headers (NonJson payload)
            >>= expectResponseCode HTTP.status202

        eventually ("Wallet balance = " ++ show amt) $ do
            r <- request @ApiWallet ctx
                (Link.getWallet @'Shelley w) Default Empty
            expectField (#balance . #getApiT . #available)
                (`shouldBe` Quantity amt) r
            expectField (#balance . #getApiT . #total)
                (`shouldBe` Quantity amt) r

    describe "TRANS_DELETE_05 - Cannot forget external tx -> 404" $ do
        let txDeleteTest05
                :: (HasType (ApiT WalletId) wal)
                => String
                -> (Context t -> ResourceT IO wal)
                -> SpecWith (Context t)
            txDeleteTest05 title eWallet = it title $ \ctx -> runResourceT $ do
                -- post external tx
                wal <- emptyWallet ctx
                addr:_ <- listAddresses @n ctx wal
                let amt = 1234
                payload <- liftIO $ fixtureRawTx ctx (getApiT $ fst $ addr ^. #id, amt)
                let headers = Headers
                                [ ("Content-Type", "application/octet-stream")
                                , ("Accept", "application/json")]

                r <- request @ApiTxId ctx Link.postExternalTransaction headers (NonJson payload)
                let txid = toText $ getApiT$ getFromResponse #id r

                -- try to forget external tx using wallet or byron-wallet
                w <- eWallet ctx
                let ep = "v2/" <> T.pack title <> "/" <> w ^. walletId
                        <> "/transactions/" <> txid
                ra <- request @ApiTxId ctx ("DELETE", ep) Default Empty
                expectResponseCode HTTP.status404 ra
                expectErrorMessage (errMsg404CannotFindTx txid) ra

                -- tx eventually gets into ledger (funds are on the target wallet)
                eventually ("Wallet balance = " ++ show amt) $ do
                    rg <- request @ApiWallet ctx
                        (Link.getWallet @'Shelley wal) Default Empty
                    expectField (#balance . #getApiT . #available)
                        (`shouldBe` Quantity amt) rg
                    expectField (#balance . #getApiT . #total)
                        (`shouldBe` Quantity amt) rg

        txDeleteTest05 "wallets" emptyWallet
        txDeleteTest05 "byron-wallets" emptyRandomWallet

    it "TRANS_EXTERNAL_CREATE_01api - proper single output transaction and \
       \proper binary format" $ \ctx -> runResourceT $ do
        let toSend = 1 :: Natural
        (ExternalTxFixture wSrc wDest fee bin _) <-
                fixtureExternalTx @n ctx toSend
        let baseOk = Base64
        let encodedSignedTx = T.decodeUtf8 $ convertToBase baseOk bin
        let payload = NonJson . BL.fromStrict . toRawBytes baseOk
        let headers = Headers [ ("Content-Type", "application/octet-stream") ]
        (initTotal, initAvailable) <- liftIO $ getWalletBalance ctx wDest

        r <- request
            @ApiTxId ctx Link.postExternalTransaction headers (payload encodedSignedTx)
        verify r
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        eventually "wDest and wSrc balance is as expected" $ do
            rb <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            verify rb
                [ expectSuccess
                , expectField
                        (#balance . #getApiT . #total . #getQuantity)
                        (`shouldBe` initTotal + toSend)
                , expectField
                        (#balance . #getApiT . #available . #getQuantity)
                        (`shouldBe` initAvailable + toSend)
                ]
            ra <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wSrc) Default Empty
            verify ra
                [ expectField
                        (#balance . #getApiT . #available . #getQuantity)
                        (`shouldBe` faucetAmt - fee - toSend)
                ]

    it "TRANS_EXTERNAL_CREATE_02 - proper single output transaction and \
       \improper binary format" $ \ctx -> runResourceT $ do
        let toSend = 1 :: Natural
        (ExternalTxFixture _ _ _ bin _) <-
                fixtureExternalTx @n ctx toSend
        let baseWrong = Base16
        let wronglyEncodedTx = T.decodeUtf8 $ convertToBase baseWrong bin
        let headers = Headers [ ("Content-Type", "application/octet-stream") ]
        let payloadWrong = NonJson . BL.fromStrict . T.encodeUtf8
        r1 <- request
            @ApiTxId ctx Link.postExternalTransaction headers (payloadWrong wronglyEncodedTx)
        verify r1
            [ expectErrorMessage errMsg400MalformedTxPayload
            , expectResponseCode HTTP.status400
            ]

    it "TRANS_EXTERNAL_CREATE_03 - proper single output transaction and \
       \wrong binary format" $ \ctx -> runResourceT @IO $ do
        let toSend = 1 :: Natural
        (ExternalTxFixture _ _ _ bin _) <- fixtureExternalTx @n ctx toSend
        let payload = NonJson $ BL.fromStrict $ ("\NUL\NUL"<>) $ getSealedTx bin
        let headers = Headers [ ("Content-Type", "application/octet-stream") ]
        r <- request @ApiTxId ctx Link.postExternalTransaction headers payload
        verify r
            [ expectErrorMessage errMsg400MalformedTxPayload
            , expectResponseCode HTTP.status400
            ]

    it "TRANS_EXTERNAL_CREATE_03 - empty payload" $ \ctx -> runResourceT @IO $ do
        _ <- emptyWallet ctx
        let headers = Headers [ ("Content-Type", "application/octet-stream") ]
        r <- request @ApiTxId ctx Link.postExternalTransaction headers Empty
        verify r
            [ expectErrorMessage errMsg400MalformedTxPayload
            , expectResponseCode HTTP.status400
            ]

    it "BYRON_MIGRATE_07x - migrate to inaproppriate addresses" $ \ctx -> runResourceT @IO $ do
        liftIO $ pendingWith "Pending due to\
            \ https://github.com/input-output-hk/cardano-wallet/issues/1658#issuecomment-632137152"
        let addrsInvalid :: [Text] =
                [ "DdzFFzCqrhtCNjPk5Lei7E1FxnoqMoAYtJ8VjAWbFmDb614nNBWBwv3kt6QHJa59cGezzf6piMWsbK7sWRB5sv325QqWdRuusMqqLdMt"
                , "37btjrVyb4KFbHcx5z2vjmdDBm6K68A5wTDJaL9XB8E8wcVfB6o8bJazKLC9tfrNTN3B4uCppR3MS8BYGaz92AqQtUVAKzGBUSQSooFW5pd84jc6Jj"
                ]
        forM_ addrsInvalid $ \addr -> do
            sWallet <- fixtureRandomWallet ctx
            r <- request @[ApiTransaction n] ctx
                (Link.migrateWallet @'Shelley sWallet)
                Default
                (Json [json|
                    { passphrase: #{fixturePassphrase}
                    , addresses: [#{addr}]
                    }|])
            verify r
                [ expectResponseCode HTTP.status400
                , expectErrorMessage
                    "Improper address. Make sure you are using valid Jörmungandr address."
                ]
  where
    fixtureZeroAmtSingle ctx = do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addr:_ <- listAddresses @n ctx wDest

        let destination = addr ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": 0,
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]
        return (wSrc, payload)

    fixtureZeroAmtMulti ctx = do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addrs <- listAddresses @n ctx wDest

        let destination1 = (addrs !! 1) ^. #id
        let destination2 = (addrs !! 2) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination1},
                    "amount": {
                        "quantity": 0,
                        "unit": "lovelace"
                    }
                },
                {
                    "address": #{destination2},
                    "amount": {
                        "quantity": 23,
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]
        return (wSrc, payload)

    toRawBytes base bs = case convertFromBase base (T.encodeUtf8 bs) of
        Left err -> error err
        Right res -> res

data ExternalTxFixture = ExternalTxFixture
    { srcWallet :: ApiWallet
    , dstWallet :: ApiWallet
    , feeMin :: Natural
    , txBinary :: SealedTx
    , txTx :: Tx
    }

-- FIXME
-- Revise this function to be less cryptic and use less partial pattern
-- matching. Many functions used below are not the right functions to use in
-- this context (like getSeqState or isOwned).
--
-- Most of this could be replaced with simple calls of the derivation primitives
-- in AddressDerivation.
fixtureExternalTx
    :: forall n t .
        ( DecodeAddress n
        , DecodeStakeAddress n
        , DelegationAddress n JormungandrKey
        )
    => (Context t)
    -> Natural
    -> ResourceT IO ExternalTxFixture
fixtureExternalTx ctx toSend = do
    -- we use faucet wallet as wSrc
    mnemonicFaucet <- liftIO $ mnemonicToText <$> nextWallet @"shelley" (_faucet ctx)
    let restoreFaucetWallet = Json [json| {
            "name": "Faucet Wallet",
            "mnemonic_sentence": #{mnemonicFaucet},
            "passphrase": #{fixturePassphrase}
            } |]
    r0 <- postWallet ctx restoreFaucetWallet
    verify r0
        [ expectResponseCode HTTP.status201
        , expectField
            (#name . #getApiT . #getWalletName) (`shouldBe` "Faucet Wallet")
        ]
    let wSrc = getFromResponse Prelude.id r0
    -- we take input by lookking at transactions of the faucet wallet
    txsSrc <- listAllTransactions @n ctx wSrc
    let (ApiTransaction (ApiT theTxId) _ _ _ _ _ _ _ outs _ _ _):_ = reverse txsSrc
    let (AddressAmount ((ApiT addrSrc),_) (Quantity amt)):_ = outs
    let (rootXPrv, pwd, st) = getSeqState mnemonicFaucet fixturePassphrase
    -- we create change address
    let (addrChng, st') = genChange (delegationAddress @n) st
    -- we generate address private keys for all source wallet addresses
    let (Just keysAddrSrc) = isOwned st' (rootXPrv, pwd) addrSrc
    let (Just keysAddrChng) = isOwned st' (rootXPrv, pwd) addrChng

    -- we create destination empty wallet
    mnemonics15 <- liftIO $ genMnemonics M15
    let createWallet = Json [json| {
            "name": "Destination Wallet",
            "mnemonic_sentence": #{mnemonics15},
            "passphrase": #{fixturePassphrase}
            } |]
    r1 <- request @ApiWallet ctx ("POST", "v2/wallets") Default createWallet
    expectField
            (#name . #getApiT . #getWalletName) (`shouldBe` "Destination Wallet") r1
    let wid = getFromResponse Prelude.id r1
    eventually "Wallet state is Ready" $ do
        r <- request @ApiWallet ctx (Link.getWallet @'Shelley wid) Default Empty
        expectField (#state . #getApiT) (`shouldBe` Ready) r

    let wDest = getFromResponse Prelude.id r1
    addrsDest <- listAddresses @n ctx wDest
    let addrDest = (head addrsDest) ^. #id
    -- we choose one available address to which money will be transfered
    let addrDest' = getApiT $ fst addrDest
    let (rootXPrv1, pwd1, st1) = getSeqState mnemonics15 fixturePassphrase
    -- we generate address private key for destination address
    let (Just keysAddrDest) = isOwned st1 (rootXPrv1, pwd1) addrDest'

    -- now we are ready to construct transaction with needed witnesses
    let mkKeystore pairs k =
            Map.lookup k (Map.fromList pairs)
    let keystore = mkKeystore
            [ (addrSrc, keysAddrSrc)
            , (addrChng, keysAddrChng)
            , (addrDest', keysAddrDest)
            ]
    let (fee, _) = ctx ^. #_feeEstimator $ PaymentDescription
            { nInputs = 1
            , nOutputs = 1
            , nChanges = 1
            }
    let cs = mempty
            { CS.inputs =
                [ (TxIn theTxId 0, TxOut addrSrc (Coin (fromIntegral amt))) ]
            , CS.outputs =
                [ TxOut addrDest' (Coin (fromIntegral toSend))
                , TxOut addrChng (Coin (fromIntegral $ amt - toSend - fee))
                ]
            }
    tl <- liftIO $ newTransactionLayer <$> getBlock0H
    let rewardAcnt = error "rewardAcnt unused"
    let expSlot = error "expiry slot not needed in jormungandr mkStdTx"
    let (Right (tx, bin)) = mkStdTx tl rewardAcnt keystore expSlot Nothing cs

    return ExternalTxFixture
        { srcWallet = wSrc
        , dstWallet = wDest
        , feeMin = fee
        , txBinary = bin
        , txTx = tx
        }
  where
      getSeqState mnemonic password =
          let (Right seed) = mkSomeMnemonic @'[15] mnemonic
              pwd = Passphrase $ BA.convert $ T.encodeUtf8 password
              rootXPrv = generateKeyFromSeed (seed, Nothing) pwd
          in (rootXPrv
             , pwd
             , mkSeqStateFromRootXPrv @n (rootXPrv, pwd) purposeCIP1852 defaultAddressPoolGap
             )

getWalletBalance :: Context t -> ApiWallet -> IO (Natural, Natural)
getWalletBalance ctx w = do
    r <- request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty
    let total =
            getFromResponse (#balance . #getApiT . #total . #getQuantity) r
    let available =
            getFromResponse (#balance . #getApiT . #available . #getQuantity) r
    return (total, available)
