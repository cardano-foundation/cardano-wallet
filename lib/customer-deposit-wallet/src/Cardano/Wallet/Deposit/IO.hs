{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
module Cardano.Wallet.Deposit.IO
    (
    -- * Types
      WalletEnv (..)
    , WalletBootEnv (..)
    , WalletPublicIdentity (..)
    , WalletInstance

    -- * Operations
    -- ** Initialization
    , withWalletInit
    , Word31
    , withWalletLoad

    -- ** Mapping between customers and addresses
    , listCustomers
    , customerAddress

    -- ** Reading from the blockchain
    , getWalletTip
    , availableBalance
    , getCustomerHistory
    , getCustomerHistories

    -- ** Writing to the blockchain
    , createPayment
    , getBIP32PathsForOwnedInputs
    , signTxBody
    , WalletStore
    , walletPublicIdentity
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPub
    )
import Cardano.Wallet.Address.BIP32
    ( BIP32Path
    )
import Cardano.Wallet.Deposit.Pure
    ( Customer
    , WalletPublicIdentity (..)
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

-- | The environment needed to initialize a wallet, before a database is
-- connected.
data WalletBootEnv m = WalletBootEnv
        { logger :: Tracer m WalletLog
        -- ^ Logger for the wallet.
        , genesisData :: Read.GenesisData
        -- ^ Genesis data for the wallet.
        , networkEnv :: Network.NetworkEnv m Read.Block
        -- ^ Network environment for the wallet.
    }

-- | The wallet store type.
type WalletStore = Store.UpdateStore IO Wallet.DeltaWalletState

-- | The full environment needed to run a wallet.
data WalletEnv m =
    WalletEnv
        { bootEnv :: WalletBootEnv m
        -- ^ The boot environment.
        , store :: WalletStore
        -- ^ The store for the wallet.
        }

data WalletInstance = WalletInstance
    { env :: WalletEnv IO
    , walletState :: DBVar.DBVar IO Wallet.DeltaWalletState
    }

{-----------------------------------------------------------------------------
    Helpers
------------------------------------------------------------------------------}
-- | Convenience to apply an 'Update' to the 'WalletState' via the 'DBLayer'.
onWalletState
    :: WalletInstance
    -> Delta.Update Wallet.DeltaWalletState r
    -> IO r
onWalletState WalletInstance{walletState} =
    Delta.onDBVar walletState
    -- FIXME: Propagation of exceptions from Pure to IO.

-- | Convenience to read the 'WalletState'.
--
-- Use 'onWalletState' if you want to use the result in an atomic update.
readWalletState :: WalletInstance -> IO WalletState
readWalletState WalletInstance{walletState} =
    DBVar.readDBVar walletState

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
withWalletInit
    env@WalletEnv
        { bootEnv = WalletBootEnv{genesisData}
        , ..
        }
    xpub
    knownCustomerCount
    action = do
        walletState <-
            DBVar.initDBVar store
                $ Wallet.fromXPubAndGenesis xpub knownCustomerCount genesisData
        withWalletDBVar env walletState action

-- | Load an existing wallet from the given environment.
withWalletLoad
    :: WalletEnv IO
    -> (WalletInstance -> IO a)
    -> IO a
withWalletLoad env@WalletEnv{..} action = do
    walletState <- DBVar.loadDBVar store
    withWalletDBVar env walletState action

withWalletDBVar
    :: WalletEnv IO
    -> DBVar.DBVar IO Wallet.DeltaWalletState
    -> (WalletInstance -> IO a)
    -> IO a
withWalletDBVar
    env@WalletEnv{bootEnv = WalletBootEnv{logger, networkEnv}}
    walletState
    action = do
        let w = WalletInstance{env, walletState}
        Async.withAsync (doChainSync w) $ \_ -> action w
      where
        doChainSync = Network.chainSync networkEnv trChainSync . chainFollower
        trChainSync = contramap (\_ -> WalletLogDummy) logger
        chainFollower w =
            Network.ChainFollower
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

customerAddress :: Customer -> WalletInstance -> IO (Maybe Address)
customerAddress c w = Wallet.customerAddress c <$> readWalletState w

walletPublicIdentity :: WalletInstance -> IO WalletPublicIdentity
walletPublicIdentity w = do
    state <- readWalletState w
    pure $ WalletPublicIdentity
        { pubXpub = Wallet.walletXPub state
        , pubNextUser = Wallet.trackedCustomers state
        }
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
    :: Write.TxBody -> WalletInstance -> IO [BIP32Path]
getBIP32PathsForOwnedInputs a w =
    Wallet.getBIP32PathsForOwnedInputs a <$> readWalletState w

signTxBody :: Write.TxBody -> WalletInstance -> IO (Maybe Write.Tx)
signTxBody txbody w = Wallet.signTxBody txbody <$> readWalletState w

{-----------------------------------------------------------------------------
    Logging
------------------------------------------------------------------------------}
data WalletLog
    = WalletLogDummy
