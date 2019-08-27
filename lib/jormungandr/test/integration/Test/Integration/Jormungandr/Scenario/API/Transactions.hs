{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Jormungandr.Scenario.API.Transactions
    ( spec
    , fixtureExternalTx
    , encodeTx
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( AddressAmount (..)
    , ApiFee
    , ApiT (..)
    , ApiTransaction
    , ApiTxId (..)
    , ApiWallet
    )
import Cardano.Wallet.Jormungandr.Binary
    ( MessageType (..), fragmentId, putSignedTx, runPut, signData, withHeader )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr, Network (..) )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Jormungandr.Transaction
    ( sign )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Passphrase (..), fromMnemonic )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( generateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( GenChange (..), IsOwned (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( defaultAddressPoolGap, mkSeqState )
import Cardano.Wallet.Primitive.Types
    ( Coin (..)
    , DecodeAddress (..)
    , EncodeAddress (..)
    , Hash (..)
    , TxIn (..)
    , TxOut (..)
    , TxWitness
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Control.Arrow
    ( second )
import Control.Monad
    ( forM_ )
import Data.ByteArray.Encoding
    ( Base (Base64), convertToBase )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word32, Word64 )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith, describe, it, shouldBe, shouldSatisfy )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , Payload (..)
    , TxDescription (..)
    , balanceAvailable
    , balanceTotal
    , emptyWallet
    , expectErrorMessage
    , expectEventually
    , expectFieldEqual
    , expectResponseCode
    , expectSuccess
    , feeEstimator
    , fixtureWallet
    , fixtureWalletWith
    , for
    , getFromResponse
    , getWalletEp
    , json
    , listAddresses
    , listAllTransactions
    , postExternalTxEp
    , postTxEp
    , postTxFeeEp
    , request
    , verify
    , walletName
    )
import Test.Integration.Framework.TestData
    ( errMsg403TxTooBig, errMsg404MalformedTxPayload, mnemonics15 )

import qualified Cardano.Wallet.Api.Types as API
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall t. (EncodeAddress t, DecodeAddress t) => SpecWith (Context t)
spec = do
    it "TRANS_CREATE_09 - 0 amount transaction is accepted on single output tx" $ \ctx -> do
        (wSrc, payload) <- fixtureZeroAmtSingle ctx
        r <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
        expectResponseCode HTTP.status202 r

    it "TRANS_CREATE_09 - 0 amount transaction is accepted on multi-output tx" $ \ctx -> do
        (wSrc, payload) <- fixtureZeroAmtMulti ctx
        r <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
        expectResponseCode HTTP.status202 r

    it "TRANS_ESTIMATE_09 - \
        \a fee can be estimated for a tx with an output of amount 0 (single)" $ \ctx -> do
        (wSrc, payload) <- fixtureZeroAmtSingle ctx
        r <- request @ApiFee ctx (postTxFeeEp wSrc) Default payload
        expectResponseCode HTTP.status202 r

    it "TRANS_ESTIMATE_09 - \
        \a fee can be estimated for a tx with an output of amount 0 (multi)" $ \ctx -> do
        (wSrc, payload) <- fixtureZeroAmtMulti ctx
        r <- request @ApiFee ctx (postTxFeeEp wSrc) Default payload
        expectResponseCode HTTP.status202 r

    it "TRANS_LIST_?? - List transactions of a fixture wallet" $ \ctx -> do
        txs <- fixtureWallet ctx >>= listAllTransactions ctx
        length txs `shouldBe` 10
        txs `shouldSatisfy` all (null . API.inputs)

    describe "TRANS_CREATE_10, TRANS_ESTIMATE_10 - \
        \Cannot post tx/fee when max tx size reached" $ do
        let matrix =
                [ ( "single output"
                  , (1, 76_000_001 :: Natural)
                  , 76
                  )
                , ( "multi output"
                  , (77, 1)
                  , 47
                  )
                ]
        forM_ matrix $ \(title, (nInps, amt), errInps) -> it title $ \ctx -> do
            wSrc <- fixtureWalletWith ctx (replicate 77 1_000_000)
            wDest <- emptyWallet ctx
            addr <- (view #id . head) <$> listAddresses ctx wDest
            let payments = for (replicate nInps addr) $ \a -> [json|{
                    "address": #{a},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }|]
            let payload = Json [json|{
                    "payments": #{payments},
                    "passphrase": "Secure Passphrase"
                }|]
            fee <- request @ApiFee ctx (postTxFeeEp wSrc) Default payload
            tx <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
            verify fee
             [ expectResponseCode HTTP.status403
             , expectErrorMessage (errMsg403TxTooBig errInps)
             ]
            verify tx
             [ expectResponseCode HTTP.status403
             , expectErrorMessage (errMsg403TxTooBig errInps)
             ]

    it "TRANS_EXTERNAL_CREATE_01 - proper single output transaction and \
       \proper binary format" $ \ctx -> do

        let toSend = 1 :: Natural
        (tx, wits) <- fixtureExternalTx ctx toSend
        let encodedSignedTx = encodeTx (tx, wits) MsgTypeTransaction
        let payload = Json [json|{
                "payload": #{encodedSignedTx}
            }|]
        r <- request @ApiTxId ctx postExternalTxEp Default payload
        verify r
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

    it "TRANS_EXTERNAL_CREATE_02 - proper single output transaction and \
       \wrong binary format" $ \ctx -> do

        let toSend = 1 :: Natural
        (tx, wits) <- fixtureExternalTx ctx toSend
        let wronglyEncodedSignedTx = encodeTx (tx, wits) MsgTypeInitial
        let payload = Json [json|{
                "payload": #{wronglyEncodedSignedTx}
            }|]
        r <- request @ApiTxId ctx postExternalTxEp Default payload
        verify r
            [ expectErrorMessage errMsg404MalformedTxPayload
            , expectResponseCode HTTP.status404
            ]

  where
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

fixtureExternalTx
    :: forall t. (EncodeAddress t, DecodeAddress t)
    => (Context t) -> Natural -> IO (Tx, [TxWitness])
fixtureExternalTx ctx toSend = do
    -- initial source wallet
    let initial = 50000 :: Natural
    wSrc <- fixtureWalletWith ctx [initial]

    -- source empty wallet which is going to take one 100 money inflow from
    -- wSrc and from which we are going to send transaction externally to
    -- another empty wallet, wEmpt
    let password = "Secure Passphrase" :: Text
    let createWallet = Json [json| {
            "name": "1st Wallet",
            "mnemonic_sentence": #{mnemonics15},
            "passphrase": #{password}
            } |]
    r0 <- request @ApiWallet ctx ("POST", "v2/wallets") Default createWallet
    verify r0
        [ expectResponseCode @IO HTTP.status202
        , expectFieldEqual walletName "1st Wallet"
        , expectFieldEqual balanceAvailable 0
        , expectFieldEqual balanceTotal 0
        ]
    let wDest = getFromResponse Prelude.id r0
    let (Right seed) = fromMnemonic @'[15] @"seed" mnemonics15
    let pwd = Passphrase $ BA.convert $ T.encodeUtf8 password
    let rootXPrv = generateKeyFromSeed (seed, mempty) pwd
    let st = mkSeqState @(Jormungandr 'Testnet)
             (rootXPrv, pwd) defaultAddressPoolGap
    let (addrD, st') = genChange pwd st
    addrs <- listAddresses ctx wDest
    let destination = (addrs !! 1) ^. #id
    let amt = 100 :: Natural
    let payload = Json [json|{
                "payments": [{
                "address": #{destination},
                "amount": {
                    "quantity": #{amt},
                    "unit": "lovelace"
                }
            }],
            "passphrase": "Secure Passphrase"
        }|]
    let (feeMin, _feeMax) = ctx ^. feeEstimator $ TxDescription
            { nInputs = 1
            , nOutputs = 1
            }
    r1 <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
    verify r1
        [ expectSuccess
        , expectResponseCode HTTP.status202
        ]
    r2 <- request @ApiWallet ctx (getWalletEp wDest) Default Empty
    verify r2
        [ expectSuccess
        , expectEventually ctx balanceAvailable amt
        ]

    --at this point wDest wallet should have 100 in one of its address
    --now we try to prepare (tx, [txWintess]), encode it and send 1 to
    --empty wallet but using external-transactions endpoint
    txsSrc <- listAllTransactions ctx wSrc
    let txSrc:_ = filter (\tx ->
                              any (\(AddressAmount _ amnt) ->
                                       amnt == Quantity amt)
                              $ NE.toList $ tx ^. #outputs
                         ) txsSrc
    let txHash = getApiT $ txSrc ^. #id
    let out1@(AddressAmount _ amnt1):out2:_ =
            NE.toList $ txSrc ^. #outputs
    let (ix, out) = if amnt1 == Quantity amt then
            (0::Word32, out1)
            else
            (1::Word32, out2)
    let outAddr = getApiT $ fst $ API.address out
    let myRnps = [(TxIn txHash ix, TxOut outAddr (Coin 100))]
    wEmpt <- emptyWallet ctx
    addrsEmpt <- listAddresses ctx wEmpt
    let destEmpt = (addrsEmpt !! 1) ^. #id
    let destAddr = getApiT $ fst destEmpt
    let left = if amt >= (fromIntegral feeMin) + toSend then
            fromIntegral $ amt - (fromIntegral feeMin) - toSend :: Word64
               else
            0 :: Word64
    let txOuts = [ TxOut addrD (Coin left)
                 , TxOut destAddr (Coin (fromIntegral toSend))]
    let inps = fmap (second coin) myRnps
    let block0H = unsafeFromHex
            "301b1c634aa7b586da7243dd66a61bde904bc1755e9a20a9b5b1b0064e70d904"
    let bs = block0H <> getHash (signData inps txOuts)
    let (Just result) = isOwned st' (rootXPrv, pwd) addrD
    let wits = [sign bs result]
    return (Tx (fragmentId inps txOuts wits) inps txOuts, wits)

encodeTx :: (Tx, [TxWitness]) -> MessageType -> Text
encodeTx (tx, wits) msgType =
    let encode ((Tx _ inps' outs'), wits') msgType' = runPut
            $ withHeader msgType'
            $ putSignedTx inps' outs' wits'
    in T.decodeUtf8 $ convertToBase Base64 $ BL.toStrict $
       encode (tx, wits) msgType
