{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- API handlers and server using the underlying wallet layer to provide
-- endpoints reachable through HTTP.

module Cardano.Wallet.Api.Server
    ( Listen (..)
    , start
    , withListeningSocket
    ) where

import Prelude

import Cardano.BM.Trace
    ( Trace, logWarning )
import Cardano.Wallet
    ( ErrAdjustForFee (..)
    , ErrCoinSelection (..)
    , ErrCreateUnsignedTx (..)
    , ErrEstimateTxFee (..)
    , ErrListTransactions (..)
    , ErrListUTxOStatistics (..)
    , ErrMkStdTx (..)
    , ErrNoSuchWallet (..)
    , ErrPostTx (..)
    , ErrSignTx (..)
    , ErrStartTimeLaterThanEndTime (..)
    , ErrSubmitTx (..)
    , ErrUpdatePassphrase (..)
    , ErrValidateSelection
    , ErrWalletAlreadyExists (..)
    , ErrWithRootKey (..)
    , ErrWrongPassphrase (..)
    , WalletLayer
    )
import Cardano.Wallet.Api
    ( Addresses, Api, Transactions, Wallets )
import Cardano.Wallet.Api.Types
    ( AddressAmount (..)
    , ApiAddress (..)
    , ApiBlockData (..)
    , ApiErrorCode (..)
    , ApiFee (..)
    , ApiT (..)
    , ApiTransaction (..)
    , ApiTxInput (..)
    , ApiUtxoStatistics (..)
    , ApiWallet (..)
    , Iso8601Time (..)
    , PostTransactionData
    , PostTransactionFeeData
    , WalletBalance (..)
    , WalletPostData (..)
    , WalletPutData (..)
    , WalletPutPassphraseData (..)
    , getApiMnemonicT
    )
import Cardano.Wallet.Network
    ( ErrNetworkUnavailable (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( KeyToAddress, digest, publicKey )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( generateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState (..), defaultAddressPoolGap, mkSeqState )
import Cardano.Wallet.Primitive.Fee
    ( Fee (..) )
import Cardano.Wallet.Primitive.Model
    ( availableBalance, getState, totalBalance )
import Cardano.Wallet.Primitive.Types
    ( Address
    , AddressState
    , Coin (..)
    , DecodeAddress (..)
    , DefineTx (..)
    , EncodeAddress (..)
    , Hash (..)
    , HistogramBar (..)
    , SortOrder (..)
    , TransactionInfo (TransactionInfo)
    , TxIn
    , TxOut (..)
    , UTxOStatistics (..)
    , WalletId (..)
    , WalletMetadata (..)
    )
import Control.Exception
    ( bracket )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT, withExceptT )
import Data.Aeson
    ( (.=) )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Labels
    ()
import Data.List
    ( sortOn )
import Data.Maybe
    ( isJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Streaming.Network
    ( bindPortTCP, bindRandomPortTCP )
import Data.Text
    ( Text )
import Data.Text.Class
    ( toText )
import Data.Time
    ( UTCTime )
import Fmt
    ( Buildable, pretty, (+|), (+||), (|+), (||+) )
import Network.HTTP.Media.RenderHeader
    ( renderHeader )
import Network.HTTP.Types.Header
    ( hContentType )
import Network.Socket
    ( Socket, close )
import Network.Wai.Handler.Warp
    ( Port )
import Network.Wai.Middleware.Logging
    ( newApiLoggerSettings, obfuscateKeys, withApiLogger )
import Network.Wai.Middleware.ServantError
    ( handleRawError )
import Servant
    ( (:<|>) (..)
    , (:>)
    , Application
    , JSON
    , NoContent (..)
    , Server
    , contentType
    , err400
    , err403
    , err404
    , err409
    , err410
    , err500
    , err503
    , serve
    )
import Servant.Server
    ( Handler (..), ServantErr (..) )

import qualified Cardano.Wallet as W
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.Wai.Handler.Warp as Warp

-- | How the server should listen for incoming requests.
data Listen
    = ListenOnPort Port
      -- ^ Listen on given TCP port
    | ListenOnRandomPort
      -- ^ Listen on an unused TCP port, selected at random
    deriving (Show, Eq)

-- | Start the application server, using the given settings and a bound socket.
start
    :: forall t.
        ( DefineTx t
        , KeyToAddress t
        , EncodeAddress t
        , DecodeAddress t
        , Buildable (ErrValidateSelection t)
        )
    => Warp.Settings
    -> Trace IO Text
    -> Socket
    -> WalletLayer (SeqState t) t
    -> IO ()
start settings trace socket wl = do
    withWorkers trace wl
    logSettings <- newApiLoggerSettings <&> obfuscateKeys (const sensitive)
    Warp.runSettingsSocket settings socket
        $ handleRawError handler
        $ withApiLogger trace logSettings
        application
  where
    -- | A Servant server for our wallet API
    server :: Server (Api t)
    server = addresses wl :<|> wallets wl :<|> transactions wl

    application :: Application
    application = serve (Proxy @("v2" :> Api t)) server

    sensitive :: [Text]
    sensitive =
        [ "passphrase"
        , "old_passphrase"
        , "new_passphrase"
        , "mnemonic_sentence"
        , "mnemonic_second_factor"
        ]

-- | Restart restoration workers for existing wallets. This is crucial to keep
-- on syncing wallets after the application has restarted!
withWorkers
    :: (DefineTx t)
    => Trace IO Text
    -> WalletLayer s t
    -> IO ()
withWorkers trace w = do
    W.listWallets w >>= mapM_ worker
  where
    worker wid = runExceptT (W.restoreWallet w wid) >>= \case
        Right () -> return ()
        Left e -> logWarning trace $
            "Wallet has suddenly vanished: "+| wid |+": "+|| e ||+""

-- | Run an action with a TCP socket bound to a port specified by the `Listen`
-- parameter.
withListeningSocket
    :: Listen
    -- ^ Whether to listen on a given port, or random port.
    -> ((Port, Socket) -> IO ())
    -- ^ Action to run with listening socket.
    -> IO ()
withListeningSocket portOpt = bracket acquire release
  where
    acquire = case portOpt of
        ListenOnPort port -> (port,) <$> bindPortTCP port hostPreference
        ListenOnRandomPort -> bindRandomPortTCP hostPreference
    release (_, socket) = liftIO $ close socket
    -- TODO: make configurable, default to secure for now.
    hostPreference = "127.0.0.1"

{-------------------------------------------------------------------------------
                                    Wallets
-------------------------------------------------------------------------------}

wallets
    :: (DefineTx t, KeyToAddress t)
    => WalletLayer (SeqState t) t
    -> Server Wallets
wallets w =
    deleteWallet w
    :<|> getWallet w
    :<|> listWallets w
    :<|> postWallet w
    :<|> putWallet w
    :<|> putWalletPassphrase w
    :<|> getUTxOsStatistics w

deleteWallet
    :: WalletLayer (SeqState t) t
    -> ApiT WalletId
    -> Handler NoContent
deleteWallet w (ApiT wid) = do
    liftHandler $ W.removeWallet w wid
    return NoContent

getWallet
    :: (DefineTx t)
    => WalletLayer (SeqState t) t
    -> ApiT WalletId
    -> Handler ApiWallet
getWallet w wid = fst <$> getWalletWithCreationTime w wid

getWalletWithCreationTime
    :: (DefineTx t)
    => WalletLayer (SeqState t) t
    -> ApiT WalletId
    -> Handler (ApiWallet, UTCTime)
getWalletWithCreationTime w (ApiT wid) = do
    (wallet, meta) <- liftHandler $ W.readWallet w wid
    return (mkApiWallet wallet meta, meta ^. #creationTime)
  where
    mkApiWallet wallet meta = ApiWallet
        { id =
            ApiT wid
        , addressPoolGap =
            ApiT $ getState wallet ^. #externalPool . #gap
        , balance = ApiT $ WalletBalance
            { available =
                Quantity $ availableBalance wallet
            , total =
                Quantity $ totalBalance wallet
            }
        , delegation =
            ApiT $ ApiT <$> meta ^. #delegation
        , name =
            ApiT $ meta ^. #name
        , passphrase =
            ApiT <$> meta ^. #passphraseInfo
        , state =
            ApiT $ meta ^. #status
        }

listWallets
    :: (DefineTx t)
    => WalletLayer (SeqState t) t
    -> Handler [ApiWallet]
listWallets w = do
    wids <- liftIO $ W.listWallets w
    fmap fst . sortOn snd <$>
        mapM (getWalletWithCreationTime w) (ApiT <$> wids)

postWallet
    :: (KeyToAddress t, DefineTx t)
    => WalletLayer (SeqState t) t
    -> WalletPostData
    -> Handler ApiWallet
postWallet w body = do
    let seed = getApiMnemonicT (body ^. #mnemonicSentence)
    let secondFactor =
            maybe mempty getApiMnemonicT (body ^. #mnemonicSecondFactor)
    let pwd = getApiT (body ^. #passphrase)
    let rootXPrv = generateKeyFromSeed (seed, secondFactor) pwd
    let g = maybe defaultAddressPoolGap getApiT (body ^. #addressPoolGap)
    let s = mkSeqState (rootXPrv, pwd) g
    let wid = WalletId $ digest $ publicKey rootXPrv
    _ <- liftHandler $ W.createWallet w wid (getApiT (body ^. #name)) s
    liftHandler $ W.attachPrivateKey w wid (rootXPrv, pwd)
    liftHandler $ W.restoreWallet w wid
    getWallet w (ApiT wid)

putWallet
    :: (DefineTx t)
    => WalletLayer (SeqState t) t
    -> ApiT WalletId
    -> WalletPutData
    -> Handler ApiWallet
putWallet w (ApiT wid) body = do
    case body ^. #name of
        Nothing ->
            return ()
        Just (ApiT wName) ->
            liftHandler $ W.updateWallet w wid (\meta -> meta { name = wName })
    getWallet w (ApiT wid)

putWalletPassphrase
    :: WalletLayer (SeqState t) t
    -> ApiT WalletId
    -> WalletPutPassphraseData
    -> Handler NoContent
putWalletPassphrase w (ApiT wid) body = do
    let (WalletPutPassphraseData (ApiT old) (ApiT new)) = body
    liftHandler $ W.updateWalletPassphrase w wid (old, new)
    return NoContent

getUTxOsStatistics
    :: (DefineTx t)
    => WalletLayer (SeqState t) t
    -> ApiT WalletId
    -> Handler ApiUtxoStatistics
getUTxOsStatistics w (ApiT wid) = do
    (UTxOStatistics histo totalStakes bType) <-
        liftHandler $ W.listUtxoStatistics w wid
    return ApiUtxoStatistics
        { total = Quantity (fromIntegral totalStakes)
        , scale = ApiT bType
        , distribution = Map.fromList $ map (\(HistogramBar k v)-> (k,v)) histo
        }

{-------------------------------------------------------------------------------
                                    Addresses
-------------------------------------------------------------------------------}

addresses
    :: (DefineTx t, KeyToAddress t)
    => WalletLayer (SeqState t) t
    -> Server (Addresses t)
addresses = listAddresses

listAddresses
    :: forall t. (DefineTx t, KeyToAddress t)
    => WalletLayer (SeqState t) t
    -> ApiT WalletId
    -> Maybe (ApiT AddressState)
    -> Handler [ApiAddress t]
listAddresses w (ApiT wid) stateFilter = do
    addrs <- liftHandler $ W.listAddresses w wid
    return $ coerceAddress <$> filter filterCondition addrs
  where
    filterCondition :: (Address, AddressState) -> Bool
    filterCondition = case stateFilter of
        Nothing -> const True
        Just (ApiT s) -> (== s) . snd
    coerceAddress (a, s) = ApiAddress (ApiT a, Proxy @t) (ApiT s)

{-------------------------------------------------------------------------------
                                    Transactions
-------------------------------------------------------------------------------}

transactions
    :: (DefineTx t, KeyToAddress t, Buildable (ErrValidateSelection t))
    => WalletLayer (SeqState t) t
    -> Server (Transactions t)
transactions w =
    createTransaction w
    :<|> listTransactions w
    :<|> postTransactionFee w

createTransaction
    :: forall t.
        ( DefineTx t
        , KeyToAddress t
        , Buildable (ErrValidateSelection t)
        )
    => WalletLayer (SeqState t) t
    -> ApiT WalletId
    -> PostTransactionData t
    -> Handler (ApiTransaction t)
createTransaction w (ApiT wid) body = do
    let outs = coerceCoin <$> (body ^. #payments)
    let pwd = getApiT $ body ^. #passphrase
    selection <- liftHandler $ W.createUnsignedTx w wid outs
    (tx, meta, wit) <- liftHandler $ W.signTx w wid pwd selection
    liftHandler $ W.submitTx w wid (tx, meta, wit)
    return $ mkApiTransaction (txId @t tx)
        (fmap Just <$> selection ^. #inputs) (selection ^. #outputs) meta

mkApiTransaction
    :: forall t.
       Hash "Tx"
    -> [(TxIn, Maybe TxOut)]
    -> [TxOut]
    -> W.TxMeta
    -> ApiTransaction t
mkApiTransaction txid ins outs meta = ApiTransaction
    { id = ApiT txid
    , amount = meta ^. #amount
    , insertedAt = Nothing
    , depth = Quantity 0
    , direction = ApiT (meta ^. #direction)
    , inputs = NE.fromList
        [ApiTxInput (fmap coerceTxOut o) (ApiT i) | (i, o) <- ins]
    , outputs = NE.fromList (coerceTxOut <$> outs)
    , status = ApiT (meta ^. #status)
    }
  where
    coerceTxOut :: TxOut -> AddressAmount t
    coerceTxOut (TxOut addr (Coin c)) =
        AddressAmount (ApiT addr, Proxy @t) (Quantity $ fromIntegral c)

-- Populate an API transaction record with 'TransactionInfo' from the wallet
-- layer.
mkApiTransactionFromInfo :: TransactionInfo -> ApiTransaction t
mkApiTransactionFromInfo (TransactionInfo txid ins outs meta depth txtime) =
    apiTx { depth, insertedAt }
  where
    apiTx = mkApiTransaction txid ins outs meta
    insertedAt = Just (ApiBlockData txtime (ApiT (meta ^. #slotId)))

listTransactions
    :: forall t. (DefineTx t)
    => WalletLayer (SeqState t) t
    -> ApiT WalletId
    -> Maybe Iso8601Time
    -> Maybe Iso8601Time
    -> Maybe (ApiT SortOrder)
    -> Handler [ApiTransaction t]
listTransactions w (ApiT wid) mStart mEnd mOrder = do
    txs <- liftHandler $ W.listTransactions w wid
        (getIso8601Time <$> mStart)
        (getIso8601Time <$> mEnd)
        (maybe defaultSortOrder getApiT mOrder)
    return $ map mkApiTransactionFromInfo txs
  where
    defaultSortOrder :: SortOrder
    defaultSortOrder = Descending

coerceCoin :: AddressAmount t -> TxOut
coerceCoin (AddressAmount (ApiT addr, _) (Quantity c)) =
    TxOut addr (Coin $ fromIntegral c)

postTransactionFee
    :: forall t. (DefineTx t, Buildable (ErrValidateSelection t))
    => WalletLayer (SeqState t) t
    -> ApiT WalletId
    -> PostTransactionFeeData t
    -> Handler ApiFee
postTransactionFee w (ApiT wid) body = do
    let outs = coerceCoin <$> (body ^. #payments)
    (Fee fee) <- liftHandler $ W.estimateTxFee w wid outs
    return ApiFee
        { amount = Quantity (fromIntegral fee)
        }

{-------------------------------------------------------------------------------
                                Error Handling
-------------------------------------------------------------------------------}

-- | Lift our wallet layer into servant 'Handler', by mapping each error to a
-- corresponding servant error.
class LiftHandler e where
    liftHandler :: ExceptT e IO a -> Handler a
    liftHandler action = Handler (withExceptT handler action)
    handler :: e -> ServantErr

apiError :: ServantErr -> ApiErrorCode -> Text -> ServantErr
apiError err code message = err
    { errBody = Aeson.encode $ Aeson.object
        [ "code" .= code
        , "message" .= T.replace "\n" " " message
        ]
    }

-- | Small helper to easy show things to Text
showT :: Show a => a -> Text
showT = T.pack . show

instance LiftHandler ErrNoSuchWallet where
    handler = \case
        ErrNoSuchWallet wid ->
            apiError err404 NoSuchWallet $ mconcat
                [ "I couldn't find a wallet with the given id: "
                , toText wid
                ]

instance LiftHandler ErrWalletAlreadyExists where
    handler = \case
        ErrWalletAlreadyExists wid ->
            apiError err409 WalletAlreadyExists $ mconcat
                [ "This operation would yield a wallet with the following id: "
                , toText wid
                , " However, I already know of a wallet with this id."
                ]

instance LiftHandler ErrWithRootKey where
    handler = \case
        ErrWithRootKeyNoRootKey wid ->
            apiError err404 NoRootKey $ mconcat
                [ "I couldn't find a root private key for the given wallet: "
                , toText wid, ". However, this operation requires that I do "
                , "have such a key. Either there's no such wallet, or I don't "
                , "fully own it."
                ]
        ErrWithRootKeyWrongPassphrase wid ErrWrongPassphrase ->
            apiError err403 WrongEncryptionPassphrase $ mconcat
                [ "The given encryption passphrase doesn't match the one I use "
                , "to encrypt the root private key of the given wallet: "
                , toText wid
                ]

instance Buildable e => LiftHandler (ErrCoinSelection e) where
    handler = \case
        ErrNotEnoughMoney utxo payment ->
            apiError err403 NotEnoughMoney $ mconcat
                [ "I can't process this payment because there's not enough "
                , "UTxO available in the wallet. The total UTxO sums up to "
                , showT utxo, " Lovelace, but I need ", showT payment
                , " Lovelace (excluding fee amount) in order to proceed "
                , " with the payment."
                ]
        ErrUtxoNotEnoughFragmented nUtxo nOuts ->
            apiError err403 UtxoNotEnoughFragmented $ mconcat
                [ "When creating new transactions, I'm not able to re-use "
                , "the same UTxO for different outputs. Here, I only have "
                , showT nUtxo, " available, but there are ", showT nOuts
                , " outputs."
                ]
        ErrMaximumInputsReached n ->
            apiError err403 TransactionIsTooBig $ mconcat
                [ "I had to select ", showT n, " inputs to construct the "
                , "requested transaction. Unfortunately, this would create a "
                , "transaction that is too big, and this would consequently "
                , "be rejected by a core node. Try sending a smaller amount."
                ]
        ErrInputsDepleted ->
            apiError err403 InputsDepleted $ mconcat
                [ "I had to select inputs to construct the "
                , "requested transaction. Unfortunately, one output of the "
                , "transaction depleted all available inputs. "
                , "Try sending a smaller amount."
                ]
        ErrInvalidSelection e ->
            apiError err403 InvalidCoinSelection $ pretty e

instance LiftHandler ErrAdjustForFee where
    handler = \case
        ErrCannotCoverFee missing ->
            apiError err403 CannotCoverFee $ mconcat
                [ "I'm unable to adjust the given transaction to cover the "
                , "associated fee! In order to do so, I'd have to select one "
                , "or more additional inputs, but I can't do that without "
                , "increasing the size of the transaction beyond the "
                , "acceptable limit. Note that I am only missing "
                , showT missing, " Lovelace."
                ]

instance Buildable e => LiftHandler (ErrCreateUnsignedTx e) where
    handler = \case
        ErrCreateUnsignedTxNoSuchWallet e -> handler e
        ErrCreateUnsignedTxCoinSelection e -> handler e
        ErrCreateUnsignedTxFee e -> handler e

instance Buildable e => LiftHandler (ErrEstimateTxFee e) where
    handler = \case
        ErrEstimateTxFeeNoSuchWallet e -> handler e
        ErrEstimateTxFeeCoinSelection e -> handler e

instance LiftHandler ErrListUTxOStatistics where
    handler = \case
        ErrListUTxOStatisticsNoSuchWallet e -> handler e

instance LiftHandler ErrSignTx where
    handler = \case
        ErrSignTx (ErrKeyNotFoundForAddress addr) ->
            apiError err403 KeyNotFoundForAddress $ mconcat
                [ "I couldn't sign the given transaction: I haven't found the "
                , "corresponding private key for the following output address: "
                , pretty addr, ". Are you sure this address belongs to a known "
                , "wallet?"
                ]
        ErrSignTxNoSuchWallet e -> (handler e)
            { errHTTPCode = 410
            , errReasonPhrase = errReasonPhrase err410
            }
        ErrSignTxWithRootKey e@ErrWithRootKeyNoRootKey{} -> (handler e)
            { errHTTPCode = 410
            , errReasonPhrase = errReasonPhrase err410
            }
        ErrSignTxWithRootKey e@ErrWithRootKeyWrongPassphrase{} -> handler e

instance LiftHandler ErrSubmitTx where
    handler = \case
        ErrSubmitTxNetwork e -> case e of
            ErrPostTxNetworkUnreachable e' ->
                handler e'
            ErrPostTxBadRequest err ->
                apiError err500 CreatedInvalidTransaction $ mconcat
                    [ "That's embarassing. It looks like I've created an "
                    , "invalid transaction that could not be parsed by the "
                    , "node. Here's an error message that may help with "
                    , "debugging: ", pretty err
                    ]
            ErrPostTxProtocolFailure err ->
                apiError err500 RejectedByCoreNode $ mconcat
                    [ "I successfully submitted a transaction, but "
                    , "unfortunately it was rejected by a relay. This could be "
                    , "because the fee was not large enough, or because the "
                    , "transaction conflicts with another transaction that "
                    , "uses one or more of the same inputs, or it may be due "
                    , "to some other reason. Here's an error message that may "
                    , "help with debugging: ", pretty err
                    ]
        ErrSubmitTxNoSuchWallet e@ErrNoSuchWallet{} -> (handler e)
            { errHTTPCode = 410
            , errReasonPhrase = errReasonPhrase err410
            }

instance LiftHandler ErrNetworkUnavailable where
    handler = \case
        ErrNetworkUnreachable err ->
            apiError err503 NetworkUnreachable $ mconcat
                [ "The node backend is unreachable: ", err
                , ". Trying again in a bit might work."
                ]
        ErrNetworkInvalid network ->
            apiError err503 NetworkMisconfigured $ mconcat
                [ "The node backend is configured for the wrong network: "
                , network, "."
                ]

instance LiftHandler ErrUpdatePassphrase where
    handler = \case
        ErrUpdatePassphraseNoSuchWallet e -> handler e
        ErrUpdatePassphraseWithRootKey e  -> handler e

instance LiftHandler ErrListTransactions where
    handler = \case
        ErrListTransactionsNoSuchWallet e -> handler e
        ErrListTransactionsStartTimeLaterThanEndTime e -> handler e

instance LiftHandler ErrStartTimeLaterThanEndTime where
    handler err = apiError err400 StartTimeLaterThanEndTime $ mconcat
        [ "The specified start time '"
        , toText $ Iso8601Time $ startTime err
        , "' is later than the specified end time '"
        , toText $ Iso8601Time $ endTime err
        , "'."
        ]

instance LiftHandler ServantErr where
    handler err@(ServantErr code _ body headers)
      | not (isJSON body) = case code of
        400 -> apiError err' BadRequest (utf8 body)
        404 -> apiError err' NotFound $ mconcat
            [ "I couldn't find the requested endpoint. If the endpoint "
            , "contains path parameters, please ensure they are well-formed, "
            , "otherwise I won't be able to route them correctly."
            ]
        405 -> apiError err' MethodNotAllowed $ mconcat
            [ "You've reached a known endpoint but I don't know how to handle "
            , "the HTTP method specified. Please double-check both the "
            , "endpoint and the method: one of them is likely to be incorrect "
            , "(for example: POST instead of PUT, or GET instead of POST...)."
            ]
        406 -> apiError err' NotAcceptable $ mconcat
            [ "It seems as though you don't accept 'application/json', but "
            , "unfortunately I only speak 'application/json'! Please "
            , "double-check your 'Accept' request header and make sure it's "
            , "set to 'application/json'."
            ]
        415 -> apiError err' UnsupportedMediaType $ mconcat
            [ "I'm really sorry but I only understand 'application/json'. I "
            , "need you to tell me what language you're speaking in order for "
            , "me to understand your message. Please double-check your "
            , "'Content-Type' request header and make sure it's set to "
            , "'application/json'."
            ]
        _ -> apiError err' UnexpectedError $ mconcat
            [ "It looks like something unexpected went wrong. Unfortunately I "
            , "don't yet know how to handle this type of situation. Here's "
            , "some information about what happened: ", utf8 body
            ]
      | otherwise = err
      where
        utf8 = T.replace "\"" "'" . T.decodeUtf8 . BL.toStrict
        isJSON = isJust . Aeson.decode @Aeson.Value
        err' = err
            { errHeaders =
                ( hContentType
                , renderHeader $ contentType $ Proxy @JSON
                ) : headers
            }
