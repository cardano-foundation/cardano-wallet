{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Api.Server
    ( server
    ) where

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- API handlers and server using the underlying wallet layer to provide
-- endpoints reachable through HTTP.

import Prelude

import Cardano.Wallet
    ( ReadWalletError (..), WalletLayer (..) )
import Cardano.Wallet.Api
    ( Addresses, Api, Wallets )
import Cardano.Wallet.Api.Types
    ( Address
    , AddressState
    , ApiT (..)
    , Wallet (..)
    , WalletId
    , WalletPostData (..)
    , WalletPutData
    , WalletPutPassphraseData
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( SeqState )
import Control.Monad.Catch
    ( throwM )
import Control.Monad.Trans.Except
    ( ExceptT, withExceptT )
import Servant
    ( (:<|>) (..), NoContent, Server, err404, err501 )
import Servant.Server
    ( Handler (..), ServantErr (..) )

server :: WalletLayer SeqState -> Server Api
server w =
    addresses w :<|> wallets w

{-------------------------------------------------------------------------------
                                    Wallets
-------------------------------------------------------------------------------}

wallets :: WalletLayer SeqState -> Server Wallets
wallets w =
    deleteWallet w
    :<|> getWallet w
    :<|> listWallets w
    :<|> postWallet w
    :<|> putWallet w
    :<|> putWalletPassphrase w

deleteWallet
    :: WalletLayer SeqState
    -> WalletId
    -> Handler NoContent
deleteWallet _ _ =
    throwM err501

getWallet
    :: WalletLayer SeqState
    -> WalletId
    -> Handler Wallet
getWallet w wid = do
    throwM err501

listWallets
    :: WalletLayer SeqState
    -> Handler [Wallet]
listWallets _ =
    throwM err501

postWallet
    :: WalletLayer SeqState
    -> WalletPostData
    -> Handler Wallet
postWallet _ _ =
    throwM err501

putWallet
    :: WalletLayer SeqState
    -> WalletId
    -> WalletPutData
    -> Handler Wallet
putWallet _ _ _ =
    throwM err501

putWalletPassphrase
    :: WalletLayer SeqState
    -> WalletId
    -> WalletPutPassphraseData
    -> Handler NoContent
putWalletPassphrase _ _ _ =
    throwM err501

{-------------------------------------------------------------------------------
                                    Addresses
-------------------------------------------------------------------------------}

addresses :: WalletLayer SeqState -> Server Addresses
addresses = listAddresses

listAddresses
    :: WalletLayer SeqState
    -> WalletId
    -> Maybe (ApiT AddressState)
    -> Handler [Address]
listAddresses _ _ _ =
    throwM err501

{-------------------------------------------------------------------------------
                                    Handlers
-------------------------------------------------------------------------------}

-- | Lift our wallet layer into servant 'Handler', by mapping each error to a
-- corresponding servant error.
class LiftHandler e where
    liftHandler :: ExceptT e IO a -> Handler a
    liftHandler action = Handler (withExceptT handler action)
    handler :: e -> ServantErr
