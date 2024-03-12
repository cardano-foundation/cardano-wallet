-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- HTTP API for the wallet.
--
module Cardano.Wallet.Deposit.HTTP
    ( serveHTTP
    )
    where

import Prelude

import Cardano.Wallet.Deposit.HTTP.Implementation
    ( api
    , implementation
    )
import Servant.Server
    ( Application
    , serve
    )

import qualified Cardano.Wallet.Deposit.IO as Wallet
import qualified Network.Wai.Handler.Warp

{-----------------------------------------------------------------------------
    Serving our HTTP API
------------------------------------------------------------------------------}

app :: Wallet.WalletInstance -> Application
app = serve api . implementation

serveHTTP :: Wallet.WalletInstance -> IO ()
serveHTTP = Network.Wai.Handler.Warp.run 8090 . app
