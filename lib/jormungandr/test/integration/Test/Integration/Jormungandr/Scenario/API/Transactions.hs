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

import Cardano.Wallet.Api.Types
    ( AddressAmount (..)
    , ApiFee
    , ApiT (..)
    , ApiTransaction (..)
    , ApiTxId (..)
    , ApiWallet
    , WalletStyle (..)
    )
import Cardano.Wallet.Jormungandr.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..), Passphrase (..), fromMnemonic )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( KnownNetwork (..), generateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( GenChange (..), IsOwned (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( defaultAddressPoolGap, mkSeqState )
import Cardano.Wallet.Primitive.Mnemonic
    ( mnemonicToText )
import Cardano.Wallet.Primitive.Types
    ( Coin (..)
    , Direction (..)
    , SealedTx (..)
    , SyncProgress (..)
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
    ( SpecWith, describe, it, shouldBe, shouldSatisfy )
import Test.Integration.Faucet
    ( nextWallet )
import Test.Integration.Framework.DSL as DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , TxDescription (..)
    , emptyRandomWallet
    , emptyWallet
    , eventually_
    , expectErrorMessage
    , expectEventually
    , expectEventually'
    , expectFieldSatisfy
    , expectListItemFieldSatisfy
    , expectResponseCode
    , expectSuccess
    , faucetAmt
    , fixturePassphrase
    , fixtureRawTx
    , fixtureWallet
    , getFromResponse
    , json
    , listAddresses
    , listAllTransactions
    , request
    , verify
    , walletId
    )
import Test.Integration.Framework.Request
    ( RequestException )
import Test.Integration.Framework.TestData
    ( errMsg400MalformedTxPayload
    , errMsg404CannotFindTx
    , errMsg405
    , errMsg406
    , errMsg415OctetStream
    , mnemonics15
    )
import Test.Integration.Jcli
    ( getBlock0H )
import Test.QuickCheck
    ( arbitrary, generate, vectorOf )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall t n. (n ~ 'Testnet) => SpecWith (Context t)
spec = do

    it "TRANS_CREATE_09 - 0 amount transaction is accepted on single output tx" $ \ctx -> do
        (wSrc, payload) <- fixtureZeroAmtSingle ctx
        r <- request @(ApiTransaction n) ctx (Link.createTransaction wSrc) Default payload
        expectResponseCode HTTP.status202 r

    it "TRANS_CREATE_09 - 0 amount transaction is accepted on multi-output tx" $ \ctx -> do
        (wSrc, payload) <- fixtureZeroAmtMulti ctx
        r <- request @(ApiTransaction n) ctx (Link.createTransaction wSrc) Default payload
        expectResponseCode HTTP.status202 r

    it "TRANS_CREATE_10 - 'account' outputs" $ \ctx -> do
        (wSrc, wDest) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
        addrs <- listAddresses ctx wDest

        let hrp = [Bech32.humanReadablePart|addr|]
        bytes <- generate (vectorOf 32 arbitrary)
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

        r <- request @(ApiTransaction n) ctx (Link.createTransaction wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status202
            , expectFieldSatisfy (#direction . #getApiT) (== Outgoing)
            , expectFieldSatisfy (#status . #getApiT) (== Pending)
            ]

        eventually_ $ do
            request @([ApiTransaction n]) ctx
                (Link.listTransactions @'Shelley wSrc)
                Default
                Empty
                >>= flip verify
                [ expectListItemFieldSatisfy 0 (#direction . #getApiT) (== Outgoing)
                , expectListItemFieldSatisfy 0 (#status . #getApiT) (== InLedger)
                ]
            request @([ApiTransaction n]) ctx
                (Link.listTransactions @'Shelley wDest)
                Default
                Empty
                >>= flip verify
                [ expectListItemFieldSatisfy 0 (#direction . #getApiT) (== Incoming)
                , expectListItemFieldSatisfy 0 (#status . #getApiT) (== InLedger)
                , expectListItemFieldSatisfy 0 #amount (== Quantity utxoAmt)
                ]
            request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest)
                Default
                Empty
                >>= flip verify
                [ expectFieldSatisfy
                        (#balance . #getApiT . #available) (== Quantity utxoAmt)
                , expectFieldSatisfy
                        (#balance . #getApiT . #total) (== Quantity utxoAmt)
                ]

    it "TRANS_ESTIMATE_09 - \
        \a fee can be estimated for a tx with an output of amount 0 (single)" $ \ctx -> do
        (wSrc, payload) <- fixtureZeroAmtSingle ctx
        r <- request @ApiFee ctx (Link.getTransactionFee wSrc) Default payload
        expectResponseCode HTTP.status202 r

    it "TRANS_ESTIMATE_09 - \
        \a fee can be estimated for a tx with an output of amount 0 (multi)" $ \ctx -> do
        (wSrc, payload) <- fixtureZeroAmtMulti ctx
        r <- request @ApiFee ctx (Link.getTransactionFee wSrc) Default payload
        expectResponseCode HTTP.status202 r

    it "TRANS_LIST_?? - List transactions of a fixture wallet" $ \ctx -> do
        txs <- fixtureWallet ctx >>= listAllTransactions ctx
        length txs `shouldBe` 10
        txs `shouldSatisfy` all (null . view #inputs)

    it "TRANS_EXTERNAL_CREATE_01x - \
        \single output tx signed via jcli" $ \ctx -> do
        w <- emptyWallet ctx
        addr:_ <- listAddresses ctx w
        let amt = 1234
        payload <- fixtureRawTx ctx (getApiT $ fst $ addr ^. #id, amt)
        let headers = Headers
                        [ ("Content-Type", "application/octet-stream")
                        , ("Accept", "application/json")]

        request @ApiTxId ctx Link.postExternalTransaction headers (NonJson payload)
            >>= expectResponseCode HTTP.status202

        expectEventually' ctx (Link.getWallet @'Shelley)
                (#balance . #getApiT . #available) (Quantity amt) w
        expectEventually' ctx (Link.getWallet @'Shelley)
                (#balance . #getApiT . #total) (Quantity amt) w

    describe "TRANS_DELETE_05 - Cannot forget external tx -> 404" $ do
        let txDeleteTest05
                :: (HasType (ApiT WalletId) wal)
                => String
                -> (Context t -> IO wal)
                -> SpecWith (Context t)
            txDeleteTest05 title eWallet = it title $ \ctx -> do
                -- post external tx
                wal <- emptyWallet ctx
                addr:_ <- listAddresses ctx wal
                let amt = 1234
                payload <- fixtureRawTx ctx (getApiT $ fst $ addr ^. #id, amt)
                let headers = Headers
                                [ ("Content-Type", "application/octet-stream")
                                , ("Accept", "application/json")]

                r <- request @ApiTxId ctx Link.postExternalTransaction headers (NonJson payload)
                let txid = toText $ getApiT$ getFromResponse #id r

                -- try to forget external tx using wallet or byron-wallet
                w <- eWallet ctx
                let ep = "v2/" <> T.pack title <> "/" <> w ^. walletId
                        <> "/transactions/" <> txid
                ra <- request @ApiTxId @IO ctx ("DELETE", ep) Default Empty
                expectResponseCode @IO HTTP.status404 ra
                expectErrorMessage (errMsg404CannotFindTx txid) ra

                -- tx eventually gets into ledger (funds are on the target wallet)
                expectEventually' ctx (Link.getWallet @'Shelley)
                        (#balance . #getApiT . #available) (Quantity amt) wal
                expectEventually' ctx (Link.getWallet @'Shelley)
                        (#balance . #getApiT . #total) (Quantity amt) wal


        txDeleteTest05 "wallets" emptyWallet
        txDeleteTest05 "byron-wallets" emptyRandomWallet

    it "TRANS_EXTERNAL_CREATE_01api - proper single output transaction and \
       \proper binary format" $ \ctx -> do
        let toSend = 1 :: Natural
        (ExternalTxFixture wSrc wDest fee bin _) <-
                fixtureExternalTx ctx toSend
        let baseOk = Base64
        let encodedSignedTx = T.decodeUtf8 $ convertToBase baseOk bin
        let payload = NonJson . BL.fromStrict . toRawBytes baseOk
        let headers = Headers [ ("Content-Type", "application/octet-stream") ]
        (initTotal, initAvailable) <- getWalletBalance ctx wDest

        r <- request
            @ApiTxId ctx Link.postExternalTransaction headers (payload encodedSignedTx)
        verify r
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        rb <- request @ApiWallet ctx (Link.getWallet @'Shelley wDest) Default Empty
        verify rb
            [ expectSuccess
            , expectEventually ctx (Link.getWallet @'Shelley)
                    (#balance . #getApiT . #total . #getQuantity)
                    (initTotal + toSend)
            , expectEventually ctx (Link.getWallet @'Shelley)
                    (#balance . #getApiT . #available . #getQuantity)
                    (initAvailable + toSend)
            ]
        ra <- request @ApiWallet ctx (Link.getWallet @'Shelley wSrc) Default Empty
        verify ra
            [ expectEventually ctx (Link.getWallet @'Shelley)
                    (#balance . #getApiT . #available . #getQuantity)
                    (faucetAmt - fee - toSend)
            ]

    it "TRANS_EXTERNAL_CREATE_02 - proper single output transaction and \
       \improper binary format" $ \ctx -> do
        let toSend = 1 :: Natural
        (ExternalTxFixture _ _ _ bin _) <-
                fixtureExternalTx ctx toSend
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
       \wrong binary format" $ \ctx -> do
        let toSend = 1 :: Natural
        (ExternalTxFixture _ _ _ bin _) <- fixtureExternalTx ctx toSend
        let payload = NonJson $ BL.fromStrict $ ("\NUL\NUL"<>) $ getSealedTx bin
        let headers = Headers [ ("Content-Type", "application/octet-stream") ]
        r <- request @ApiTxId ctx Link.postExternalTransaction headers payload
        verify r
            [ expectErrorMessage errMsg400MalformedTxPayload
            , expectResponseCode HTTP.status400
            ]

    it "TRANS_EXTERNAL_CREATE_03 - empty payload" $ \ctx -> do
        _ <- emptyWallet ctx
        let headers = Headers [ ("Content-Type", "application/octet-stream") ]
        r <- request @ApiTxId ctx Link.postExternalTransaction headers Empty
        verify r
            [ expectErrorMessage errMsg400MalformedTxPayload
            , expectResponseCode HTTP.status400
            ]

    describe "TRANS_EXTERNAL_CREATE_04 - \
        \v2/proxy/transactions - Methods Not Allowed" $ do

        let matrix = ["PUT", "DELETE", "CONNECT", "TRACE", "OPTIONS", "GET"]
        forM_ matrix $ \method -> it (show method) $ \ctx -> do
            let payload = NonJson mempty
            let headers = Headers [ ("Content-Type", "application/octet-stream") ]
            let endpoint = "v2/proxy/transactions"
            r <- request @ApiTxId ctx (method, endpoint) headers payload
            expectResponseCode @IO HTTP.status405 r
            expectErrorMessage errMsg405 r

    describe "TRANS_EXTERNAL_CREATE_04 - HTTP headers" $ do
        forM_ (externalTxHeaders @ApiTxId) $ \(title, headers, expectations) ->
            it title $ \ctx -> do
                let payload = NonJson mempty
                r <- request @ApiTxId ctx Link.postExternalTransaction headers payload
                verify r expectations

  where
    externalTxHeaders
        :: (Show a)
        => [( String
            , Headers
            , [(HTTP.Status, Either RequestException a) -> IO ()])
           ]
    externalTxHeaders =
        [ ( "Accept: text/plain -> 406"
          , Headers [ ("Content-Type", "application/octet-stream")
                    , ("Accept", "text/plain") ]
          , [ expectResponseCode @IO HTTP.status406
            , expectErrorMessage errMsg406 ]
        )
        , ( "Content-Type: application/json -> 415"
          , Headers [ ("Content-Type", "application/json") ]
          , [ expectResponseCode @IO HTTP.status415
            , expectErrorMessage errMsg415OctetStream ]
        )
        , ( "Content-Type: application/json -> 415"
          , Headers [ ("Content-Type", "text/plain") ]
          , [ expectResponseCode @IO HTTP.status415
            , expectErrorMessage errMsg415OctetStream ]
        )
        ]
    fixtureZeroAmtSingle ctx = do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addr:_ <- listAddresses ctx wDest

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
        addrs <- listAddresses ctx wDest

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
    :: forall t n. (n ~ 'Testnet)
    => (Context t)
    -> Natural
    -> IO ExternalTxFixture
fixtureExternalTx ctx toSend = do
    -- we use faucet wallet as wSrc
    let password = "cardano-wallet" :: Text
    mnemonicFaucet <- mnemonicToText <$> nextWallet @"shelley" (_faucet ctx)
    let restoreFaucetWallet = Json [json| {
            "name": "Faucet Wallet",
            "mnemonic_sentence": #{mnemonicFaucet},
            "passphrase": #{password}
            } |]
    r0 <- request
        @ApiWallet ctx ("POST", "v2/wallets") Default restoreFaucetWallet
    verify r0
        [ expectResponseCode @IO HTTP.status201
        , expectFieldSatisfy
            (#name . #getApiT . #getWalletName) (== "Faucet Wallet")
        ]
    let wSrc = getFromResponse Prelude.id r0
    -- we take input by lookking at transactions of the faucet wallet
    txsSrc <- listAllTransactions ctx wSrc
    let (ApiTransaction (ApiT theTxId) _ _ _ _ _ _ outs _):_ = reverse txsSrc
    let (AddressAmount ((ApiT addrSrc),_) (Quantity amt)):_ = outs
    let (rootXPrv, pwd, st) = getSeqState mnemonicFaucet password
    -- we create change address
    let (addrChng, st') = genChange () st
    -- we generate address private keys for all source wallet addresses
    let (Just keysAddrSrc) = isOwned st' (rootXPrv, pwd) addrSrc
    let (Just keysAddrChng) = isOwned st' (rootXPrv, pwd) addrChng

    -- we create destination empty wallet
    let password1 = "Secure Passphrase" :: Text
    let createWallet = Json [json| {
            "name": "Destination Wallet",
            "mnemonic_sentence": #{mnemonics15},
            "passphrase": #{password1}
            } |]
    r1 <- request @ApiWallet ctx ("POST", "v2/wallets") Default createWallet
    verify r1
        [ expectFieldSatisfy
            (#name . #getApiT . #getWalletName) (== "Destination Wallet")
        , expectEventually ctx (Link.getWallet @'Shelley) (#state . #getApiT) Ready
        ]
    let wDest = getFromResponse Prelude.id r1
    addrsDest <- listAddresses ctx wDest
    let addrDest = (head addrsDest) ^. #id
    -- we choose one available address to which money will be transfered
    let addrDest' = getApiT $ fst addrDest
    let (rootXPrv1, pwd1, st1) = getSeqState mnemonics15 password1
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
    let theInps =
            [ (TxIn theTxId 0, TxOut addrSrc (Coin (fromIntegral amt))) ]
    let theOuts =
            [ TxOut addrDest' (Coin (fromIntegral toSend))
            , TxOut addrChng (Coin (fromIntegral $ amt - toSend - fee))
            ]
    tl <- newTransactionLayer <$> getBlock0H
    let (Right (tx, bin)) = mkStdTx tl keystore theInps theOuts

    return ExternalTxFixture
        { srcWallet = wSrc
        , dstWallet = wDest
        , feeMin = fee
        , txBinary = bin
        , txTx = tx
        }
  where
      getSeqState mnemonic password =
          let (Right seed) = fromMnemonic @'[15] @"seed" mnemonic
              pwd = Passphrase $ BA.convert $ T.encodeUtf8 password
              rootXPrv = generateKeyFromSeed (seed, mempty) pwd
          in (rootXPrv
             , pwd
             , mkSeqState @n (rootXPrv, pwd) defaultAddressPoolGap
             )

getWalletBalance :: Context t -> ApiWallet -> IO (Natural, Natural)
getWalletBalance ctx w = do
    r <- request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty
    let total =
            getFromResponse (#balance . #getApiT . #total . #getQuantity) r
    let available =
            getFromResponse (#balance . #getApiT . #available . #getQuantity) r
    return (total, available)
