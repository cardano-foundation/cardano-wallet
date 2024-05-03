{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
module Cardano.Wallet.Deposit.IO
    (
    -- * Types
      WalletEnv (..)
    , WalletInstance

    -- * Operations
    -- ** Initialization
    , withWalletInit
    , Word31
    , withWalletLoad

    -- ** Mapping between customers and addresses
    , listCustomers
    , createAddress

    -- ** Reading from the blockchain
    , getWalletTip
    , availableBalance
    , getCustomerHistory
    , getCustomerHistories

    -- ** Writing to the blockchain
    , createPayment
    , getBIP32PathsForOwnedInputs
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPub
    )
import Cardano.Wallet.Deposit.Pure
    ( Customer
    , WalletState
    , Word31
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
import qualified Data.Map.Strict as Map
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
    -- FIXME: Propagation of exceptions from Pure to IO.

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
-- | Initialize a new wallet in the given environment.
withWalletInit
    :: WalletEnv IO
    -> XPub
    -> Word31
    -> (WalletInstance -> IO a)
    -> IO a
withWalletInit env@WalletEnv{..} xpub knownCustomerCount action = do
    walletState <- atomically
        $ DBVar.initDBVar database
        $ Wallet.fromXPubAndGenesis xpub knownCustomerCount genesisData
    withWalletDBVar env walletState action

-- | Load an existing wallet from the given environment.
withWalletLoad
    :: WalletEnv IO
    -> (WalletInstance -> IO a)
    -> IO a
withWalletLoad env@WalletEnv{..} action = do
    walletState <- atomically $ DBVar.loadDBVar database
    withWalletDBVar env walletState action

withWalletDBVar
    :: WalletEnv IO
    -> DBVar.DBVar DB.SqlM Wallet.DeltaWalletState
    -> (WalletInstance -> IO a)
    -> IO a
withWalletDBVar env@WalletEnv{..} walletState action = do
    let w = WalletInstance{env,walletState}
    Async.withAsync (doChainSync w) $ \_ -> action w
  where
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
getWalletTip :: WalletInstance -> IO Read.ChainPoint
getWalletTip w =
    Wallet.getWalletTip <$> readWalletState w

availableBalance :: WalletInstance -> IO Read.Value
availableBalance w =
    Wallet.availableBalance <$> readWalletState w

getCustomerHistory :: Customer -> WalletInstance -> IO [Wallet.TxSummary]
getCustomerHistory c w =
    Wallet.getCustomerHistory c <$> readWalletState w

getCustomerHistories
    :: (Read.ChainPoint, Read.ChainPoint)
    -> WalletInstance
    -> IO (Map.Map Customer Wallet.ValueTransfer)
getCustomerHistories a w =
    Wallet.getCustomerHistories a <$> readWalletState w

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
    :: [(Address, Read.Value)] -> WalletInstance -> IO (Maybe Write.TxBody)
createPayment a w =
    Wallet.createPayment a <$> readWalletState w

getBIP32PathsForOwnedInputs
    :: Write.TxBody -> WalletInstance -> IO [()]
getBIP32PathsForOwnedInputs a w =
    Wallet.getBIP32PathsForOwnedInputs a <$> readWalletState w

{-----------------------------------------------------------------------------
    Logging
------------------------------------------------------------------------------}
data WalletLog
    = WalletLogDummy
