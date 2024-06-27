{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Mock implementation of a blockchain for the purpose of testing.

TODO:
* Make the blockchain more real.
-}
module Test.Scenario.Blockchain
    ( assert

    , ScenarioEnv
    , withScenarioEnvMock
    , withWalletEnvMock

    , Faucet
    , ada
    , payFromFaucet

    , signTx
    , submitTx
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPrv
    )
import Cardano.Wallet.Deposit.IO.DB
    ( SqlContext (..)
    , withSqlContextInMemory
    )
import Cardano.Wallet.Deposit.IO.Network.Mock
    ( newNetworkEnvMock
    )
import Cardano.Wallet.Deposit.IO.Network.Type
    ( NetworkEnv (..)
    )
import Cardano.Wallet.Deposit.Pure
    ( BIP32Path
    )
import Control.Tracer
    ( nullTracer
    )
import Data.Store
    ( newStore
    )
import GHC.Stack
    ( HasCallStack
    )

import qualified Cardano.Wallet.Deposit.IO as Wallet
import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Cardano.Wallet.Deposit.Write as Write
import qualified Data.Map.Strict as Map

{-----------------------------------------------------------------------------
    Logic
------------------------------------------------------------------------------}

assert :: HasCallStack => Bool -> IO ()
assert True = pure ()
assert False = error "Assertion failed!"

{-----------------------------------------------------------------------------
    Environment
------------------------------------------------------------------------------}
-- | Environment for scenarios.
data ScenarioEnv = ScenarioEnv
    { genesisData :: Read.GenesisData
    , networkEnv :: NetworkEnv IO Read.Block
    , faucet :: Faucet
    }

-- | Acquire and release a mock environment for a blockchain
withScenarioEnvMock :: (ScenarioEnv -> IO a) -> IO a
withScenarioEnvMock action = do
    networkEnv <- newNetworkEnvMock
    action
        $ ScenarioEnv
            { genesisData = error "TODO: Mock Genesis Data"
            , networkEnv
            , faucet = Faucet{xprv = error "TODO: Faucet xprv"}
            }

-- | Acquire and release a mock environment for a wallet.
withWalletEnvMock
    :: ScenarioEnv
    -> (Wallet.WalletEnv IO -> IO a)
    -> IO a
withWalletEnvMock ScenarioEnv{..} action =
    withSqlContextInMemory nullTracer
        $ \SqlContext{runSqlM} -> do
            database <- runSqlM newStore
            let walletEnv = Wallet.WalletEnv
                    { Wallet.logger = nullTracer
                    , Wallet.genesisData = genesisData
                    , Wallet.networkEnv = networkEnv
                    , Wallet.database = database
                    , Wallet.atomically = runSqlM
                    }
            action walletEnv

{-----------------------------------------------------------------------------
    Faucet
------------------------------------------------------------------------------}
newtype Faucet = Faucet
    { xprv :: XPrv
    }

ada :: Integer -> Write.Value
ada = Write.mkAda

payFromFaucet :: ScenarioEnv -> [(Write.Address, Write.Value)] -> IO ()
payFromFaucet env destinations =
    submitTx env tx
  where
    toTxOut (addr, value) = Write.mkTxOut addr value
    txBody = Write.TxBody
        { Write.spendInputs = mempty
        , Write.collInputs = mempty
        , Write.txouts = Map.fromList $ zip [0..] $ map toTxOut destinations
        , Write.collRet = Nothing
        }
    tx = signTx (xprv (faucet env)) [] txBody

{-----------------------------------------------------------------------------
    Transaction submission
------------------------------------------------------------------------------}

signTx :: XPrv -> [BIP32Path] -> Write.TxBody -> Write.Tx
signTx _ _ txbody =
    Write.Tx
        { Write.txbody = txbody
        , Write.txwits = ()
        }

submitTx :: ScenarioEnv -> Write.Tx -> IO ()
submitTx env tx = do
    _ <- postTx (networkEnv env) tx
    pure ()
