{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
module Cardano.Wallet.Deposit.IO
    (
    -- * Types
      WalletEnv
    , WalletInstance

    -- * Operations
    -- ** Initialization
    , withWallet

    -- ** Mapping between customers and addresses
    , listCustomers
    , createAddress

    -- ** Reading from the blockchain
    , availableBalance
    , getCustomerHistory

    -- ** Writing to the blockchain
    , createPayment
    ) where

import Prelude

import Cardano.Wallet.Deposit.Pure
    ( Customer
    , WalletState
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    )
import Control.Tracer
    ( Tracer
    , contramap
    )
import Data.Bifunctor
    ( first
    )
import Data.List.NonEmpty
    ( NonEmpty
    )

import qualified Cardano.Wallet.Deposit.IO.DB as DB
import qualified Cardano.Wallet.Deposit.IO.Network.Type as Network
import qualified Cardano.Wallet.Deposit.Pure as Wallet
import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Cardano.Wallet.Deposit.Write as Write
import qualified Control.Concurrent.Async as Async
import qualified Data.DBVar as DBVar
import qualified Data.Delta as Delta
    ( Replace (..)
    )
import qualified Data.Delta.Update as Delta
import qualified Data.Store as Store

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}
data WalletEnv m =
    WalletEnv
        { logger :: Tracer m WalletLog
        , genesisData :: Read.GenesisData
        , networkEnv :: Network.NetworkEnv m Read.Block
        , database :: Store.UpdateStore DB.SqlM Wallet.DeltaWalletState
        , atomically :: forall a. DB.SqlM a -> m a
        }

data WalletInstance = WalletInstance
    { env :: WalletEnv IO
    , walletState :: DBVar.DBVar DB.SqlM Wallet.DeltaWalletState
    }

{-----------------------------------------------------------------------------
    Helpers
------------------------------------------------------------------------------}
-- | Convenience to apply an 'Update' to the 'WalletState' via the 'DBLayer'.
onWalletState
    :: WalletInstance
    -> Delta.Update Wallet.DeltaWalletState r
    -> IO r
onWalletState WalletInstance{env,walletState} update' =
    atomically env $ Delta.onDBVar walletState update'

-- | Convenience to read the 'WalletState'.
--
-- Use 'onWalletState' if you want to use the result in an atomic update.
readWalletState :: WalletInstance -> IO WalletState
readWalletState WalletInstance{env,walletState} =
    atomically env $ DBVar.readDBVar walletState

{-----------------------------------------------------------------------------
    Operations
    Initialization
------------------------------------------------------------------------------}
withWallet :: WalletEnv IO -> (WalletInstance -> IO a) -> IO a
withWallet env@WalletEnv{..} action = do
    walletState <- loadWalletStateFromDatabase
    let w = WalletInstance{env,walletState}
    Async.withAsync (doChainSync w) $ \_ -> action w
  where
    loadWalletStateFromDatabase = atomically $ do
        es <- Store.loadS database
        case es of
            Left _ ->
                DBVar.initDBVar database $ Wallet.fromGenesis genesisData
            Right _ ->
                DBVar.loadDBVar database

    doChainSync = Network.chainSync networkEnv trChainSync . chainFollower
    trChainSync = contramap (\_ -> WalletLogDummy) logger
    chainFollower w = Network.ChainFollower
        { checkpointPolicy = undefined
        , readChainPoints = undefined
        , rollForward = rollForward w
        , rollBackward = rollBackward w
        }

{-----------------------------------------------------------------------------
    Operations
------------------------------------------------------------------------------}
listCustomers :: WalletInstance -> IO [(Customer, Address)]
listCustomers w =
    Wallet.listCustomers <$> readWalletState w

createAddress :: Customer -> WalletInstance -> IO Address
createAddress c w =
    onWalletState w
        $ Delta.updateWithResult
        $ \s0 ->
            let (r,s1) = Wallet.createAddress c s0
            in  (Delta.Replace s1, r)

{-----------------------------------------------------------------------------
    Operations
    Reading from the blockchain
------------------------------------------------------------------------------}
availableBalance :: WalletInstance -> IO Read.Value
availableBalance w =
    Wallet.availableBalance <$> readWalletState w

getCustomerHistory :: WalletInstance -> Customer -> IO [Wallet.TxSummary]
getCustomerHistory w c =
    Wallet.getCustomerHistory c <$> readWalletState w

rollForward :: WalletInstance -> NonEmpty Read.Block -> tip -> IO ()
rollForward w blocks _nodeTip =
    onWalletState w
        $ Delta.update
        $ Delta.Replace . Wallet.rollForwardMany blocks

rollBackward :: WalletInstance -> Read.ChainPoint -> IO Read.ChainPoint
rollBackward w point =
    onWalletState w
        $ Delta.updateWithResult
        $ first Delta.Replace . Wallet.rollBackward point

{-----------------------------------------------------------------------------
    Operations
    Writing to blockchain
------------------------------------------------------------------------------}

createPayment
    :: WalletInstance -> [(Address, Read.Value)] -> IO (Maybe Write.Tx)
createPayment w destinations =
    Wallet.createPayment destinations <$> readWalletState w

{-----------------------------------------------------------------------------
    Logging
------------------------------------------------------------------------------}
data WalletLog
    = WalletLogDummy
