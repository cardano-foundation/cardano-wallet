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
    ) where

import Prelude

import Cardano.Wallet
    ( ErrCreateUnsignedTx (..)
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
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Labels
    ()
import Data.Quantity
    ( Quantity (..) )
import Fmt
    ( pretty )
import Servant
    ( (:<|>) (..)
    , NoContent (..)
    , Server
    , err403
    , err404
    , err409
    , err410
    , err500
    )
import Servant.Server
    ( Handler (..), ServantErr (..) )

import qualified Cardano.Wallet as W
import qualified Data.List.NonEmpty as NE


-- | A Servant server for our wallet API
server :: (TxId t, KeyToAddress t) => WalletLayer (SeqState t) t -> Server Api
server w =
    addresses w :<|> wallets w :<|> transactions w

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

-- FIXME
--
-- For now, just "dumb" mapping from our internal errors to servant errors.
-- In practice, we want to create nice error messages giving as much details as
-- we can.

instance LiftHandler ErrNoSuchWallet where
    handler = \case
        ErrNoSuchWallet _ -> err404

instance LiftHandler ErrWalletAlreadyExists where
    handler = \case
        ErrWalletAlreadyExists _ -> err409

instance LiftHandler ErrWithRootKey where
    handler = \case
        ErrWithRootKeyNoRootKey _ -> err404
        ErrWithRootKeyWrongPassphrase ErrWrongPassphrase -> err403

instance LiftHandler ErrCreateUnsignedTx where
    handler = \case
        ErrCreateUnsignedTxNoSuchWallet _ -> err404
        ErrCreateUnsignedTxCoinSelection _ -> err403
        ErrCreateUnsignedTxFee _ -> err403

instance LiftHandler ErrSignTx where
    handler = \case
        ErrSignTx (KeyNotFoundForAddress addr) -> err403
            { errBody =
                "Error signing transaction: corresponding private key for the \
                \following output address was not found: " <> pretty addr
            }
        ErrSignTxNoSuchWallet _ -> err410
        ErrSignTxWithRootKey e -> handler e

instance LiftHandler ErrSubmitTx where
    handler = \case
        ErrSubmitTxNetwork e -> case e of
            ErrPostTxNetworkUnreachable (ErrNetworkUnreachable err) -> err500
                { errBody =
                    "Err submitting transaction: network is unreachable: "
                    <> pretty err
                }
            ErrPostTxBadRequest err -> err500
                { errBody =
                    "Err submitting transaction: request rejected by node: "
                    <> pretty err
                }
            ErrPostTxProtocolFailure err -> err500
                { errBody =
                    "Err submitting transaction: protocol failure: "
                    <> pretty err
                }
        ErrSubmitTxNoSuchWallet _ -> err404

instance LiftHandler ErrUpdatePassphrase where
    handler = \case
        ErrUpdatePassphraseNoSuchWallet _ -> err404
        ErrUpdatePassphraseWithRootKey e -> handler e
