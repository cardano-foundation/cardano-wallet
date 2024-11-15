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
    ( credentialsFromEncodedXPub
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
    ( PostWalletViaMenmonic (..)
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
    :: Tracer IO String
    -> FilePath
    -> WalletBootEnv IO
    -> WalletResource
    -> Server API
server tr dbDir wb r =
    listCustomerH r
        :<|> queryAddressH r
        :<|> createWalletViaMnemonic tr dbDir wb r
        :<|> createWalletViaXPub tr dbDir wb r

createWalletViaMnemonic
    :: Tracer IO String
    -> FilePath
    -> WalletBootEnv IO
    -> WalletResource
    -> PostWalletViaMenmonic
    -> Handler NoContent
createWalletViaMnemonic
    tracer
    dir
    boot
    resource
    (PostWalletViaMenmonic mnemonics' passphrase' users') =
        onlyOnWalletIntance resource initWallet $> NoContent
      where
        initWallet :: WalletResourceM ()
        initWallet =
            REST.initWallet
                tracer
                boot
                dir
                (credentialsFromMnemonics mnemonics' passphrase')
                (fromIntegral users')

createWalletViaXPub
    :: Tracer IO String
    -> FilePath
    -> WalletBootEnv IO
    -> WalletResource
    -> PostWalletViaXPub
    -> Handler NoContent
createWalletViaXPub
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
