{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Faucet.Http.Server
    ( TcpPort (..)
    , serve
    , initApp
    ) where

import Prelude

import qualified Cardano.Faucet as Faucet
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant

import Cardano.Faucet
    ( serveAddresses
    , serveMenmonic
    , serveMnemonics
    )
import Cardano.Faucet.FaucetM
    ( FaucetM
    , FaucetState (..)
    , runFaucetM
    )
import Cardano.Faucet.Http.Api.Servant
    ( FaucetApi
    )
import Data.Proxy
    ( Proxy (..)
    )
import Network.Wai.Middleware.RequestLogger
    ( logStdoutDev
    )
import Servant
    ( (:<|>) (..)
    )

--------------------------------------------------------------------------------

newtype TcpPort = TcpPort Int
    deriving newtype (Show)

serve :: TcpPort -> IO ()
serve (TcpPort port) = do
    app <- initApp
    putStrLn $ "Serving Faucet API on port " <> show port
    Warp.run port $ logStdoutDev app

initApp :: IO Wai.Application
initApp = Servant.serve api . server <$> Faucet.initialState

api :: Proxy FaucetApi
api = Proxy

server :: FaucetState -> Servant.Server FaucetApi
server state0 =
    Servant.hoistServer api (runFaucetM state0) faucetServer
  where
    serveMnemmonicOrAddresses len index =
        serveMenmonic len index :<|> serveAddresses len index
    faucetServer :: Servant.ServerT FaucetApi FaucetM
    faucetServer len = serveMnemonics len :<|> serveMnemmonicOrAddresses len
