{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Wallet.UI.Deposit.Handlers.Payments.Transaction
where

import Prelude

import Cardano.Binary
    ( DecoderError
    )
import Cardano.Read.Ledger.Tx.CBOR
    ( deserializeTx
    , serializeTx
    )
import Cardano.Wallet.Deposit.IO.Network.Type
    ( ErrPostTx
    )
import Cardano.Wallet.Deposit.Pure
    ( BIP32Path
    , CanSign
    , ErrCreatePayment
    , InspectTx (..)
    )
import Cardano.Wallet.Deposit.Pure.API.Address
    ( NetworkTag (..)
    , getNetworkTag
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
    , WalletResourceM
    , availableBalance
    , canSign
    , createPayment
    , getBIP32PathsForOwnedInputs
    , inspectTx
    , networkTag
    , resolveCurrentEraTx
    , signTx
    , submitTx
    )
import Cardano.Wallet.Deposit.Write
    ( Tx
    , resolvedTx
    )
import Cardano.Wallet.Read
    ( Coin (..)
    , Value (..)
    )
import Cardano.Wallet.UI.Common.Layer
    ( SessionLayer
    )
import Cardano.Wallet.UI.Deposit.API.Payments
    ( NewReceiverValidation (..)
    , Password (..)
    , Payment (..)
    , Receivers
    , Signal
    , State
    , StateA (..)
    , Transaction (..)
    , step
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( catchRunWalletResourceHtml
    )
import Control.Monad.Trans
    ( MonadIO (..)
    , lift
    )
import Control.Monad.Trans.Except
    ( ExceptT (..)
    , runExceptT
    )
import Data.Foldable
    ( Foldable (..)
    )
import Data.Functor
    ( (<&>)
    )
import Data.Semigroup
    ( Sum (..)
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText (..)
    )
import Data.Traversable
    ( for
    )
import Servant
    ( FromHttpApiData (..)
    , Handler
    )

import qualified Cardano.Wallet.Read.Tx as Read
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base16.Lazy as BL16
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Monoidal.Strict as MonoidalMap
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

data PaymentError
    = CreatePaymentError ErrCreatePayment
    | DecodingError DecoderError
    | StateTransitionImpossible
    | PrivateKeyIsMissing
    | SubmissionFailed ErrPostTx
    deriving (Eq, Show)

instance ToText PaymentError where
    toText = \case
        CreatePaymentError e -> "CreatePaymentError: " <> toText e
        DecodingError e -> "DecodingError: " <> T.pack (show e)
        StateTransitionImpossible -> "The state transition is impossible"
        PrivateKeyIsMissing -> "Cannot sign without a private key"
        SubmissionFailed e -> "SubmissionFailed: " <> T.pack (show e)

extractReceivers :: InspectTx -> Receivers
extractReceivers InspectTx{otherOutputs, ourOutputs} =
    fold $ do
        (addr, coin) <-
            otherOutputs
                <> (ourOutputs <&> \(addr, _, c) -> (addr, c))
        pure $ MonoidalMap.singleton addr $ Sum $ fromIntegral coin

mkPayment :: Payment (ExceptT PaymentError WalletResourceM)
mkPayment =
    Payment
        { unsigned = unsignedPayment
        , sign = signPayment
        , submit = submitPayment
        , receivers = receiversPayment
        }

submitPayment
    :: Transaction -> ExceptT PaymentError WalletResourceM ()
submitPayment stx = do
    let etx = deserializeTransaction stx
    liftIO $ print etx
    case etx of
        Left e -> ExceptT $ pure $ Left $ DecodingError e
        Right tx -> do
            e <- do
                liftIO $ print $ Read.getTxId tx
                lift $ submitTx tx
            case e of
                Left e' -> ExceptT $ pure $ Left $ SubmissionFailed e'
                Right () -> pure ()

signPayment
    :: Transaction
    -> Password
    -> ExceptT PaymentError WalletResourceM Transaction
signPayment serializedTx (Password pwd) = do
    let eUnsignedTx = deserializeTransaction serializedTx
    case eUnsignedTx of
        Left e -> ExceptT $ pure $ Left $ DecodingError e
        Right unsignedTx -> do
            mSignedTx <- lift $ signTx unsignedTx pwd
            case mSignedTx of
                Nothing -> ExceptT $ pure $ Left PrivateKeyIsMissing
                Just signedTx -> do
                    paths <- lift $ getBIP32PathsForOwnedInputs signedTx
                    pure $ serializeTransaction paths signedTx

receiversPayment
    :: Transaction -> ExceptT PaymentError WalletResourceM Receivers
receiversPayment stx = do
    let etx = deserializeTransaction stx
    case etx of
        Left e -> ExceptT $ pure $ Left $ DecodingError e
        Right tx -> do
            rtx <- lift $ resolveCurrentEraTx tx
            itx <- lift $ inspectTx rtx
            pure $ extractReceivers itx

unsignedPayment
    :: Receivers -> ExceptT PaymentError WalletResourceM Transaction
unsignedPayment receivers = do
    er <- lift $ createPayment $ do
        (address, Sum amount) <- MonoidalMap.assocs receivers
        pure (address, ValueC (CoinC $ fromIntegral amount) mempty)
    case er of
        Left e -> ExceptT $ pure $ Left $ CreatePaymentError e
        Right rtx -> do
            paths <- lift $ getBIP32PathsForOwnedInputs $ resolvedTx rtx
            pure $ serializeTransaction paths $ resolvedTx rtx

serializeTransaction
    :: [BIP32Path]
    -> Tx
    -> Transaction
serializeTransaction paths =
    conwayEraTransactionExport paths
        . T.decodeUtf8
        . B16.encode
        . BL.toStrict
        . serializeTx

deserializeTransaction :: Transaction -> Either DecoderError Tx
deserializeTransaction =
    deserializeTx
        . BL16.decodeLenient
        . TL.encodeUtf8
        . TL.fromStrict
        . cborHex

data PaymentHandlerResponse
    = ResponseSuccess CanSign (StateA (Transaction, InspectTx))
    | ResponseExceptionPayments PaymentError
    deriving (Eq, Show)

signalHandler
    :: SessionLayer WalletResource
    -> (BL.ByteString -> html)
    -- ^ Function to render the exception as HTML
    -> ( Coin
         -> PaymentHandlerResponse
         -> html
       )
    -> State
    -> Signal
    -> Handler html
signalHandler layer alert render state signal = do
    catchRunWalletResourceHtml layer alert id $ do
        ValueC available _ <- availableBalance
        estate' <- runExceptT $ step mkPayment state signal
        case estate' of
            Left e -> pure $ render available $ ResponseExceptionPayments e
            Right mstate ->
                case mstate of
                    Nothing ->
                        pure
                            $ render available
                            $ ResponseExceptionPayments
                                StateTransitionImpossible
                    Just newState -> do
                        signing <- canSign
                        er <- for newState $ \stx -> do
                            let etx = deserializeTransaction stx
                            case etx of
                                Left e -> pure $ Left e
                                Right tx -> do
                                    rtx <- resolveCurrentEraTx tx
                                    itx <- inspectTx rtx
                                    pure $ Right (stx, itx)
                        case sequence er of
                            Left e ->
                                pure
                                    $ render available
                                    $ ResponseExceptionPayments
                                    $ DecodingError e
                            Right r -> do
                                pure
                                    $ render available
                                    $ ResponseSuccess signing
                                    $ case r of
                                        x -> x

conwayEraTransactionExport :: [BIP32Path] -> Text -> Transaction
conwayEraTransactionExport bip32Paths cborHex =
    Transaction
        { dataType = "Unwitnessed Tx ConwayEra"
        , description = "Ledger Cddl Format"
        , cborHex
        , bip32Paths
        }

data AddressValidationResponse
    = ValidAddress Address Bool
    | InvalidAddress Text

data AmountValidationResponse
    = ValidAmount Double Bool
    | InvalidAmount Text

tagEq :: NetworkTag -> NetworkTag -> Bool
tagEq MainnetTag MainnetTag = True
tagEq TestnetTag TestnetTag = True
tagEq _ _ = False

showTag :: NetworkTag -> Text
showTag MainnetTag = "Mainnet"
showTag TestnetTag = "Testnet"

receiverAddressValidation
    :: SessionLayer WalletResource
    -> (BL.ByteString -> html)
    -> (AddressValidationResponse -> html)
    -> NewReceiverValidation
    -> Handler html
receiverAddressValidation layer alert render nrv = do
    catchRunWalletResourceHtml layer alert id $ do
        tag <- networkTag
        pure $ render $ addressValidationPure tag nrv

addressValidationPure
    :: NetworkTag -> NewReceiverValidation -> AddressValidationResponse
addressValidationPure tag nrv@NewReceiverValidation{addressValidation} =
    case parseUrlPiece <$> addressValidation of
        Nothing -> InvalidAddress "Address cannot be empty"
        Just (Left e) -> InvalidAddress $ "Invalid address: " <> e
        Just (Right addr)
            | getNetworkTag addr `tagEq` tag ->
                ValidAddress addr
                    $ case amountValidationPure tag nrv of
                        ValidAmount _ _ -> True
                        _ -> False
            | otherwise ->
                InvalidAddress
                    $ "Address is not on the "
                        <> showTag tag
                        <> " network"

receiverAmountValidation
    :: SessionLayer WalletResource
    -> (BL.ByteString -> html)
    -> (AmountValidationResponse -> html)
    -> NewReceiverValidation
    -> Handler html
receiverAmountValidation layer alert render nrv =
    catchRunWalletResourceHtml layer alert id $ do
        tag <- networkTag
        pure $ render $ amountValidationPure tag nrv

amountValidationPure
    :: NetworkTag -> NewReceiverValidation -> AmountValidationResponse
amountValidationPure tag nrv@NewReceiverValidation{amountValidation} =
    case parseUrlPiece <$> amountValidation of
        Nothing -> InvalidAmount "Amount cannot be empty"
        Just (Left e) -> InvalidAmount $ "Invalid amount: " <> e
        Just (Right amount)
            | amount <= 0 -> InvalidAmount "Amount must be positive"
            | otherwise -> ValidAmount amount
                $ case addressValidationPure tag nrv of
                    ValidAddress _ _ -> True
                    _ -> False

-- x =
--     Signed
--         ( Transaction
--             { dataType = "Unwitnessed Tx ConwayEra"
--             , description = "Ledger Cddl Format"
--             , cborHex =
--                 "84a400d90102828258208859a7e8e4956c5780fc50862873889c8b4eff77a8ab75a02661 413ef2da8d73008258208859a7e8e4956c5780fc50862873889c8b4eff77a8ab75a02661413ef2da8d7301018282581d60b396a8e776ffc5a50def5c9b9bc720ea760ffe3d9a49743fb73d91ea1a498d588082581d603ba8830 12bf0d9d6ee94245c4e5671ccf4a1b0b0ba111bea2e971a3c1b000000e88b128f32021a00029fed03190e12a0f5f6"
--             }
--         )
--         ( Transaction
--             { dataType = "Unwitnessed Tx ConwayEra"
--             , description = "Ledger Cddl Fo rmat"
--             , cborHex =
--                 "84a400d90102828258208859a7e8e4956c5780fc50862873889c8b4eff77a8ab75a02661413ef2da8d73008258208859a7e8e4956c5780fc50862873889c8b4eff77a8ab75a02661413ef2da8d7301018 282581d60b396a8e776ffc5a50def5c9b9bc720ea760ffe3d9a49743fb73d91ea1a498d588082581d603ba883012bf0d9d6ee94245c4e5671ccf4a1b0b0ba111bea2e971a3c1b000000e88b128f32021a00029fed03190e12a1 00d90102828258202390837b235279492bcf075e3c272bff29affa11b9a8d889d4b726f596f56b835840f605c66ea76391740c413bf07c33b3388c1e30de51e95ce5779405a4aa250febc521c4bdd8f7e0bc2e0556e96247bcf 1354656911c17158b0f5b65834d4ca505825820fb5939a080736db07e626c777c47c8d27f428ab52b3d08a5b163e6988fc743b058408daf8a9a8d4a88a5af76948869cf4f346dbef620a57864a4d9514d5ba3800c0b60b9bd97 6bbb56494e5249cdaef9ba466c6c84360528ca2b7bbaf07ce02d6c0cf5f6"
--             }
--         , InspectTx
--             { ourInputs =
--                 [
--                     ( TxId
--                         { unTxId =
--                             SafeHash
--                                 "8859a7e8e4956c5780fc50862873889c8b4eff77a8ab75a02661413ef2da8d73"
--                         }
--                     , TxIx{unTxIx = 0}
--                     , Coin 1234000000
--                     )
--                 ,
--                     ( TxId
--                         { unTxId =
--                             SafeHash
--                                 "8859a7e8e4956c5780fc50862873889c8b4eff77a8ab75a02661413ef2da8d73"
--                         }
--                     , TxIx{unTxIx = 1}
--                     , Coin 998765834015
--                     )
--                 ]
--             , otherInputs = []
--             , change =
--                 [
--                     ( Addr
--                         Testnet
--                         ( KeyHashObj
--                             ( KeyHash
--                                 { unKeyHash = "3ba883012bf0d9d6ee94245c4e5671ccf4a1b0b0ba111bea2e971a3c"
--                                 }
--                             )
--                         )
--                         StakeRefNull
--                     , Coin 998765662002
--                     )
--                 ]
--             , ourOutputs =
--                 [
--                     ( Add
--                         r
--                         Testnet
--                         ( KeyHashObj
--                             ( KeyHash
--                                 { unKeyHash = "b396a8e776ffc5a50def5c9b9bc720ea760ffe3d9a49743fb73d91ea"
--                                 }
--                             )
--                         )
--                         StakeRefNull
--                     , 1
--                     , Coin 1234000000
--                     )
--                 ]
--             , otherOutputs = []
--             , fee = Coin 172013
--             }
--         )

-- y =
--     Submitted
--         ( Transaction
--             { dataType = "Unwitnessed Tx ConwayEra"
--             , description = "Ledger Cddl Format"
--             , cborHex =
--                 "84a400d90102828258208859a7e8e4956c5780fc50862873889c8b4eff77a8ab75a02661413ef2da8d73008258208859a7e8e4956c5780fc50862873889c8b4eff77a8ab75a02661413ef2da8d7301018282581d60b396a8e776ffc5a50def5c9b9bc720ea760ffe3d9a49743fb73d91ea1a498d588082581d603ba883012bf0d9d6ee94245c4e5671ccf4a1b0b0ba111bea2e971a3c1b000000e88b128f32021a00029fed03190e12a0f5f6"
--             }
--         )
--         ( Transaction
--             { dataType = "Unwitnessed Tx ConwayEra"
--             , description = "Ledger Cddl Format"
--             , cborHex =
--                 "84a400d90102828258208859a7e8e4956c5780fc50862873889c8b4eff77a8ab75a02661413ef2da8d73008258208859a7e8e4956c5780fc50862873889c8b4eff77a8ab75a02661413ef2da8d7301018282581d60b396a8e776ffc5a50def5c9b9bc720ea760ffe3d9a49743fb73d91ea1a498d588082581d603ba883012bf0d9d6ee94245c4e5671ccf4a1b0b0ba111bea2e971a3c1b000000e88b128f32021a00029fed03190e12a100d90102828258202390837b235279492bcf075e3c272bff29affa11b9a8d889d4b726f596f56b835840f605c66ea76391740c413bf07c33b3388c1e30de51e95ce5779405a4aa250febc521c4bdd8f7e0bc2e0556e96247bcf1354656911c17158b0f5b65834d4ca505825820fb5939a080736db07e626c777c47c8d27f428ab52b3d08a5b163e6988fc743b058408daf8a9a8d4a88a5af76948869cf4f346dbef620a57864a4d9514d5ba3800c0b60b9bd976bbb56494e5249cdaef9ba466c6c84360528ca2b7bbaf07ce02d6c0cf5f6"
--             }
--         , InspectTx
--             { ourInputs = []
--             , otherInputs =
--                 [
--                     ( TxId
--                         { unTxId =
--                             SafeHash
--                                 "8859a7e8e4956c5780fc50862873889c8b4eff77a8ab75a02661413ef2da8d73"
--                         }
--                     , TxIx{unTxIx = 0}
--                     )
--                 ,
--                     ( TxId
--                         { unTxId =
--                             SafeHash
--                                 "8859a7e8e4956c5780fc50862873889c8b4eff77a8ab75a02661413ef2da8d73"
--                         }
--                     , TxIx{unTxIx = 1}
--                     )
--                 ]
--             , change =
--                 [
--                     ( Addr
--                         Testnet
--                         ( KeyHashObj
--                             ( KeyHash
--                                 { unKeyHash = "3ba883012bf0d9d6ee94245c4e5671ccf4a1b0b0ba111bea2e971a3c"
--                                 }
--                             )
--                         )
--                         StakeRefNull
--                     , Coin 998765662002
--                     )
--                 ]
--             , ourOutputs =
--                 [
--                     ( Addr
--                         Testnet
--                         ( KeyHashObj
--                             ( KeyHash
--                                 { unKeyHash = "b396a8e776ffc5a50def5c9b9bc720ea760ffe3d9a49743fb73d91ea"
--                                 }
--                             )
--                         )
--                         StakeRefNull
--                     , 1
--                     , Coin 1234000000
--                     )
--                 ]
--             , otherOutputs = []
--             , fee = Coin 172013
--             }
--         )
