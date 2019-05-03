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
    , ErrNoSuchWallet (..)
    , ErrSignTx (..)
    , ErrSubmitTx (..)
    , ErrWalletAlreadyExists (..)
    , WalletLayer
    )
import Cardano.Wallet.Api
    ( Addresses, Api, Transactions, Wallets )
import Cardano.Wallet.Api.Types
    ( ApiAddress (..)
    , ApiCoins (..)
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
    ( AddressState, Coin (..), TxId (..), TxOut (..), WalletId (..) )
import Control.Monad.Catch
    ( throwM )
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
import Servant
    ( (:<|>) (..)
    , NoContent (..)
    , Server
    , err403
    , err404
    , err409
    , err410
    , err500
    , err501
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
            ApiT $ meta ^. #passphraseInfo
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
    let g = maybe defaultAddressPoolGap getApiT (body ^.  #addressPoolGap)
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
putWallet _ _ _ =
    throwM err501

putWalletPassphrase
    :: WalletLayer (SeqState t) t
    -> ApiT WalletId
    -> WalletPutPassphraseData
    -> Handler NoContent
putWalletPassphrase _ _ _ =
    throwM err501

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
listAddresses _ _ _ =
    throwM err501

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
    let outs = coerceCoin <$> (body ^. #targets)
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
    coerceCoin :: ApiCoins -> TxOut
    coerceCoin (ApiCoins (ApiT addr) (Quantity c)) =
        TxOut addr (Coin $ fromIntegral c)
    coerceTxOut :: TxOut -> ApiCoins
    coerceTxOut (TxOut addr (Coin c)) =
        ApiCoins (ApiT addr) (Quantity $ fromIntegral c)

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

instance LiftHandler ErrCreateUnsignedTx where
    handler = \case
        ErrCreateUnsignedTxNoSuchWallet _ -> err404
        ErrCreateUnsignedTxCoinSelection _ -> err403
        ErrCreateUnsignedTxFee _ -> err403

instance LiftHandler ErrSignTx where
    handler = \case
        ErrSignTx _ -> err500
        ErrSignTxNoSuchWallet _ -> err410
        ErrSignTxWrongPassphrase _ -> err403

instance LiftHandler ErrSubmitTx where
    handler _ = err500
