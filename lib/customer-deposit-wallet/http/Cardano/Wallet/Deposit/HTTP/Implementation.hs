-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Implementation of our HTTP API.
--
module Cardano.Wallet.Deposit.HTTP.Implementation
    ( api
    , implementation
    )
    where

import Cardano.Wallet.Deposit.HTTP.Types.API
    ( CustomerAPI
    )
import Data.Proxy
    ( Proxy (..)
    )
import Servant
    ( (:<|>) (..)
    )
import Servant.Server
    ( Server
    )

import qualified Cardano.Wallet.Deposit.HTTP.Endpoints as HTTP
import qualified Cardano.Wallet.Deposit.IO as Wallet

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}
api :: Proxy CustomerAPI
api = Proxy

implementation :: Wallet.WalletInstance -> Server CustomerAPI
implementation w =
    HTTP.listCustomers w
    :<|> HTTP.customerAddress w
