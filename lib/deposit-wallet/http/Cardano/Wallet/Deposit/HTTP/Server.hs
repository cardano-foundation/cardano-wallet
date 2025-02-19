-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Implementation of our HTTP API.
module Cardano.Wallet.Deposit.HTTP.Server
    ( api
    , server
    )
where

import Prelude

import Cardano.Wallet.Deposit.HTTP.Types.API
    ( API
    )
import Cardano.Wallet.Deposit.HTTP.Types.JSON
    ( Address
    , ApiT (..)
    , Customer
    )
import Cardano.Wallet.Deposit.IO
    ( WalletBootEnv
    )
import Cardano.Wallet.Deposit.Pure.State.Creation
    ( createMnemonicFromWords
    , credentialsFromEncodedXPub
    , credentialsFromMnemonics
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
    , WalletResourceM
    , customerAddress
    , listCustomers
    )
import Cardano.Wallet.Deposit.REST.Catch
    ( catchRunWalletResourceM
    )
import Cardano.Wallet.Deposit.REST.Wallet.Create
    ( PostWalletViaMnemonic (..)
    , PostWalletViaXPub (..)
    )
import Control.Tracer
    ( Tracer
    )
import Data.Functor
    ( ($>)
    )
import Data.Proxy
    ( Proxy (..)
    )
import Servant
    ( Handler
    , NoContent (..)
    , err500
    , (:<|>) (..)
    )
import Servant.Server
    ( Server
    )

import qualified Cardano.Wallet.Deposit.REST as REST

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}
api :: Proxy API
api = Proxy

server
    :: Tracer IO ()
    -- ^ Tracer for wallet tip changes
    -> Tracer IO String
    -> FilePath
    -> WalletBootEnv IO
    -> WalletResource
    -> Server API
server wtc tr dbDir wb r =
    listCustomerH r
        :<|> queryAddressH r
        :<|> createWalletViaMnemonic wtc tr dbDir wb r
        :<|> createWalletViaXPub wtc tr dbDir wb r

createWalletViaMnemonic
    :: Tracer IO ()
    -- ^ Tracer for wallet tip changes
    -> Tracer IO String
    -> FilePath
    -> WalletBootEnv IO
    -> WalletResource
    -> PostWalletViaMnemonic
    -> Handler NoContent
createWalletViaMnemonic
    wtc
    tracer
    dir
    boot
    resource
    (PostWalletViaMnemonic mnemonics' passphrase' users') = do
        case createMnemonicFromWords mnemonics' of
            Left e -> fail $ show e
            Right someMnemonic -> do
                let
                    initWallet :: WalletResourceM ()
                    initWallet =
                        REST.initWallet
                            wtc
                            tracer
                            boot
                            dir
                            (credentialsFromMnemonics someMnemonic passphrase')
                            (fromIntegral users')
                onlyOnWalletIntance resource initWallet $> NoContent

createWalletViaXPub
    :: Tracer IO ()
    -- ^ Tracer for wallet tip changes
    -> Tracer IO String
    -> FilePath
    -> WalletBootEnv IO
    -> WalletResource
    -> PostWalletViaXPub
    -> Handler NoContent
createWalletViaXPub
    wtc
    tracer
    dir
    boot
    resource
    (PostWalletViaXPub xpubText users') = do
        result <- onlyOnWalletIntance resource initWallet
        case result of
            Left e -> fail e
            Right () -> pure NoContent
      where
        initWallet :: WalletResourceM (Either String ())
        initWallet = case credentialsFromEncodedXPub xpubText of
            Left e -> pure $ Left $ show e
            Right credentials ->
                Right
                    <$> REST.initWallet
                        wtc
                        tracer
                        boot
                        dir
                        credentials
                        (fromIntegral users')

listCustomerH
    :: WalletResource
    -> Handler (ApiT [(Customer, Address)])
listCustomerH wr = ApiT <$> onlyOnWalletIntance wr listCustomers

queryAddressH
    :: WalletResource
    -> ApiT Customer
    -> Handler (ApiT Address)
queryAddressH wr (ApiT customer) = do
    mAddr <- onlyOnWalletIntance wr $ customerAddress customer
    case mAddr of
        Nothing -> fail $ "Address not found for customer " <> show customer
        Just a -> pure $ ApiT a

onlyOnWalletIntance
    :: WalletResource
    -> WalletResourceM a
    -> Handler a
onlyOnWalletIntance wr = catchRunWalletResourceM wr err500
