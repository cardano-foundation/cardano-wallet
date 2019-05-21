{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- API handlers and server using the underlying wallet layer to provide
-- endpoints reachable through HTTP.

module Cardano.Wallet.Api.Server
    ( server
    , middlewares
    ) where

import Prelude

import Cardano.Wallet
    ( ErrAdjustForFee (..)
    , ErrCoinSelection (..)
    , ErrCreateUnsignedTx (..)
    , ErrMkStdTx (..)
    , ErrNetworkUnreachable (..)
    , ErrNoSuchWallet (..)
    , ErrPostTx (..)
    , ErrSignTx (..)
    , ErrSubmitTx (..)
    , ErrUpdatePassphrase (..)
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
    , ApiErrorCode (..)
    , ApiT (..)
    , ApiTransaction (..)
    , ApiWallet (..)
    , PostTransactionData
    , WalletBalance (..)
    , WalletPostData (..)
    , WalletPutData (..)
    , WalletPutPassphraseData (..)
    , getApiMnemonicT
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( KeyToAddress, digest, generateKeyFromSeed, publicKey )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( SeqState (..), defaultAddressPoolGap, mkSeqState )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelectionOptions (..) )
import Cardano.Wallet.Primitive.Model
    ( availableBalance, getState, totalBalance )
import Cardano.Wallet.Primitive.Types
    ( AddressState
    , Coin (..)
    , TxId (..)
    , TxOut (..)
    , WalletId (..)
    , WalletMetadata (..)
    )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT, withExceptT )
import Data.Aeson
    ( (.=) )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Labels
    ()
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( toText )
import Fmt
    ( pretty )
import Network.HTTP.Media.RenderHeader
    ( renderHeader )
import Network.HTTP.Types.Header
    ( hContentType )
import Network.Wai
    ( Middleware )
import Network.Wai.Middleware.ServantError
    ( handleRawError )
import Servant
    ( (:<|>) (..)
    , JSON
    , NoContent (..)
    , Server
    , contentType
    , err403
    , err404
    , err409
    , err410
    , err500
    , err503
    )
import Servant.Server
    ( Handler (..), ServantErr (..) )

import qualified Cardano.Wallet as W
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | A Servant server for our wallet API
server :: (TxId t, KeyToAddress t) => WalletLayer (SeqState t) t -> Server Api
server w =
    addresses w :<|> wallets w :<|> transactions w

-- | A list of additional middlewares running on top of our server
middlewares :: [Middleware]
middlewares =
    [ handleRawError handler ]

{-------------------------------------------------------------------------------
                                    Wallets
-------------------------------------------------------------------------------}

wallets :: KeyToAddress t => WalletLayer (SeqState t) t -> Server Wallets
wallets w =
    deleteWallet w
    :<|> getWallet w
    :<|> listWallets w
    :<|> postWallet w
    :<|> putWallet w
    :<|> putWalletPassphrase w

deleteWallet
    :: WalletLayer (SeqState t) t
    -> ApiT WalletId
    -> Handler NoContent
deleteWallet w (ApiT wid) = do
    liftHandler $ W.removeWallet w wid
    return NoContent

getWallet
    :: WalletLayer (SeqState t) t
    -> ApiT WalletId
    -> Handler ApiWallet
getWallet w (ApiT wid) = do
    (wallet, meta) <- liftHandler $ W.readWallet w wid
    return ApiWallet
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
    :: WalletLayer (SeqState t) t
    -> Handler [ApiWallet]
listWallets w = do
    wids <- liftIO $ W.listWallets w
    mapM (getWallet w) (ApiT <$> wids)

postWallet
    :: KeyToAddress t
    => WalletLayer (SeqState t) t
    -> WalletPostData
    -> Handler ApiWallet
postWallet w body = do
    let seed = getApiMnemonicT (body ^. #mnemonicSentence)
    let secondFactor = maybe mempty getApiMnemonicT (body ^. #mnemonicSecondFactor)
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
    :: WalletLayer (SeqState t) t
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

{-------------------------------------------------------------------------------
                                    Addresses
-------------------------------------------------------------------------------}

addresses :: WalletLayer (SeqState t) t -> Server Addresses
addresses = listAddresses

listAddresses
    :: WalletLayer (SeqState t) t
    -> ApiT WalletId
    -> Maybe (ApiT AddressState)
    -> Handler [ApiAddress]
listAddresses w (ApiT wid) _ = do
    addrs <- liftHandler $ W.listAddresses w wid
    return $ coerceAddress <$> addrs
  where
    coerceAddress (a, s) = ApiAddress (ApiT a) (ApiT s)

{-------------------------------------------------------------------------------
                                    Transactions
-------------------------------------------------------------------------------}

transactions :: TxId t => WalletLayer (SeqState t) t -> Server Transactions
transactions = createTransaction

createTransaction
    :: forall t. (TxId t)
    => WalletLayer (SeqState t) t
    -> ApiT WalletId
    -> PostTransactionData
    -> Handler ApiTransaction
createTransaction w (ApiT wid) body = do
    -- FIXME Compute the options based on the transaction's size / inputs
    let opts = CoinSelectionOptions { maximumNumberOfInputs = 10 }
    let outs = coerceCoin <$> (body ^. #payments)
    let pwd = getApiT $ body ^. #passphrase
    selection <- liftHandler $ W.createUnsignedTx w wid opts outs
    (tx, meta, wit) <- liftHandler $ W.signTx w wid pwd selection
    liftHandler $ W.submitTx w wid (tx, meta, wit)
    return ApiTransaction
        { id = ApiT (txId @t tx)
        , amount = meta ^. #amount
        , insertedAt = Nothing
        , depth = Quantity 0
        , direction = ApiT (meta ^. #direction)
        , inputs = NE.fromList (coerceTxOut . snd <$> selection ^. #inputs)
        , outputs = NE.fromList (coerceTxOut <$> tx ^. #outputs)
        , status = ApiT (meta ^. #status)
        }
  where
    coerceCoin :: AddressAmount -> TxOut
    coerceCoin (AddressAmount (ApiT addr) (Quantity c)) =
        TxOut addr (Coin $ fromIntegral c)
    coerceTxOut :: TxOut -> AddressAmount
    coerceTxOut (TxOut addr (Coin c)) =
        AddressAmount (ApiT addr) (Quantity $ fromIntegral c)

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
                , toText wid, " However, I already know a wallet with such id"
                ]

instance LiftHandler ErrWithRootKey where
    handler = \case
        ErrWithRootKeyNoRootKey wid ->
            apiError err404 NoRootKey $ mconcat
                [ "I couldn't find any root private key for the given wallet: "
                , toText wid, " This operation however requires that I do hold "
                , "one. Either there's no such wallet, or I don't fully own it."
                ]
        ErrWithRootKeyWrongPassphrase wid ErrWrongPassphrase ->
            apiError err403 WrongEncryptionPassphrase $ mconcat
                [ "The given encryption passphrase doesn't match the one I use "
                , "to encrypt the root private key of the given wallet: "
                , toText wid
                ]

instance LiftHandler ErrCoinSelection where
    handler = \case
        ErrNotEnoughMoney utxo payment ->
            apiError err403 NotEnoughMoney $ mconcat
                [ "I can't accomodate such payment because there's not enough "
                , "available UTxO in the wallet. The total UTxO sums up to "
                , showT utxo, " Lovelace, but I need ", showT payment
                , " Lovelace (fee included) in order to proceed with the "
                , "payment."
                ]

        ErrUtxoNotEnoughFragmented nUtxo nOuts ->
            apiError err403 UtxoNotEnoughFragmented $ mconcat
                [ "There's a restriction in the way I can construct "
                , "transactions: I do not re-use a same UTxO for different "
                , "outputs. Here, I only have ", showT nUtxo, "available but "
                , "there are ", showT nOuts, " outputs."
                ]
        ErrMaximumInputsReached n ->
            apiError err403 TransactionIsTooBig $ mconcat
                [ "I had to select as many as ", showT n, " inputs to construct "
                , "the requested transaction. This would create a transaction "
                , "that is too big and would be rejected by a core node. Try "
                , "sending a smaller amount."
                ]

instance LiftHandler ErrAdjustForFee where
    handler = \case
        ErrCannotCoverFee missing ->
            apiError err403 CannotCoverFee $ mconcat
                [ "I can't adjust the given transaction to cover for fee! "
                , "In order to do so, I'd have to select some additional inputs "
                , "but I can't without increasing the size of the transaction "
                , "out of an acceptable limit. Note that I am only missing "
                , showT missing, " Lovelace."
                ]

instance LiftHandler ErrCreateUnsignedTx where
    handler = \case
        ErrCreateUnsignedTxNoSuchWallet e -> handler e
        ErrCreateUnsignedTxCoinSelection e -> handler e
        ErrCreateUnsignedTxFee e -> handler e

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
            ErrPostTxNetworkUnreachable (ErrNetworkUnreachable err) ->
                apiError err503 NetworkUnreachable $ mconcat
                    [ "Aw crap! I couldn't submit the transaction: the network "
                    , "is unreachable: ", pretty err, " Trying again in a bit "
                    , "might work."
                    ]
            ErrPostTxBadRequest err ->
                apiError err500 CreatedInvalidTransaction $ mconcat
                    [ "That is embarassing. It looks like I have created an "
                    , "invalid transaction which failed to be parsed by the "
                    , "node. Here's perhaps an error message that will help "
                    , "debugging: ", pretty err
                    ]
            ErrPostTxProtocolFailure err ->
                apiError err500 RejectedByCoreNode $ mconcat
                    [ "I successfully submitted a transaction but it was "
                    , "rejected by a relay. This could be because there was not "
                    , "enough fee, because it now conflicts with another "
                    , "transactions using some identical inputs or because of "
                    , "another reason. "
                    , "Here's a hint: ", pretty err
                    ]
        ErrSubmitTxNoSuchWallet e@ErrNoSuchWallet{} -> (handler e)
            { errHTTPCode = 410
            , errReasonPhrase = errReasonPhrase err410
            }

instance LiftHandler ErrUpdatePassphrase where
    handler = \case
        ErrUpdatePassphraseNoSuchWallet e -> handler e
        ErrUpdatePassphraseWithRootKey e  -> handler e

instance LiftHandler ServantErr where
    handler err@(ServantErr code _ body headers) = case code of
        400 -> apiError err' BadRequest (utf8 body)
        404 -> apiError err' NotFound $ mconcat
            [ "I couldn't find the requested endpoint? If the endpoint contains "
            , "path parameters, make sure they are well-formed otherwise I "
            , "won't be able to route them correctly."
            ]
        405 -> apiError err' MethodNotAllowed $ mconcat
            [ "You've reached a known endpoint but I don't know how to handle "
            , "this HTTP method for it. Please double-check both the endpoint "
            , "and the method used: one of them is likely incorrect (e.g. POST "
            , "instead of PUT, or GET instead of POST...)."
            ]
        406 -> apiError err' NotAcceptable $ mconcat
            [ "It seems like you don't accept 'application/json' however, I "
            , "only speak 'application/json'! Please, double-check your "
            , "'Accept' request header and make sure it's set to "
            , "'application/json'."
            ]
        415 -> apiError err' UnsupportedMediaType $ mconcat
            [ "I am sorry but I only speak 'application/json' and I need you to "
            , "tell me what you're speaking before I can comprehend it. Please, "
            , "double-check your 'Content-Type' request header and make sure "
            , "it's set to 'application/json'."
            ]
        _ -> apiError err' UnexpectedError $ mconcat
            [ "Looks like something went wrong and I wasn't ready for this. "
            , "Here is a hint about what happened: ", utf8 body
            ]
      where
        utf8 = T.decodeUtf8 . BL.toStrict
        err' = err
            { errHeaders =
                ( hContentType
                , renderHeader $ contentType $ Proxy @JSON
                ) : headers
            }
