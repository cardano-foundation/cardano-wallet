{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.Deposit.IO
    ( -- * Types
      WalletEnv (..)
    , WalletStore
    , WalletBootEnv (..)
    , WalletPublicIdentity (..)
    , WalletInstance

      -- * Operations

      -- ** Initialization
    , withWalletInit
    , Word31
    , withWalletLoad
    , walletPublicIdentity

      -- ** Mapping between customers and addresses
    , listCustomers
    , customerAddress
    , addressToCustomer
    , ResolveAddress

      -- ** Reading from the blockchain
    , getWalletTip
    , availableBalance
    , getTxHistoryByCustomer
    , getTxHistoryByTime
    , getCustomerDeposits
    , getAllDeposits

      -- ** Writing to the blockchain

      -- *** Create transactions
    , createPayment
    , inspectTx

      -- *** Sign transactions
    , getBIP32PathsForOwnedInputs
    , signTx

      -- *** Submit transactions
    , submitTx
    , listTxsInSubmission

      -- * Internals
    , onWalletState
    , networkTag
    , readWalletState
    , resolveCurrentEraTx
    , canSign
    ) where

import Prelude

import Cardano.Wallet.Address.BIP32
    ( BIP32Path
    )
import Cardano.Wallet.Deposit.IO.Network.Type
    ( NetworkEnv (slotToUTCTime)
    )
import Cardano.Wallet.Deposit.Pure
    ( Credentials
    , CurrentEraResolvedTx
    , Customer
    , ValueTransfer
    , WalletPublicIdentity (..)
    , WalletState
    , Word31
    )
import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( ByCustomer
    , ByTime
    , LookupTimeFromSlot
    )
import Cardano.Wallet.Deposit.Pure.State.Creation
    ( CanSign
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , TxId
    , WithOrigin
    )
import Cardano.Wallet.Network.Checkpoints.Policy
    ( defaultPolicy
    )
import Control.Tracer
    ( Tracer
    , contramap
    , traceWith
    )
import Data.Bifunctor
    ( first
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Map.Strict
    ( Map
    )
import Data.Time
    ( UTCTime
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
    , networkEnv :: Network.NetworkEnv m (Read.EraValue Read.Block)
    -- ^ Network environment for the wallet.
    }

-- | The wallet store type.
type WalletStore = Store.UpdateStore IO Wallet.DeltaWalletState

-- | The full environment needed to run a wallet.
data WalletEnv m = WalletEnv
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
    :: Tracer IO () -- wallet tip changes
    -> WalletEnv IO
    -> Credentials
    -> Word31
    -> (WalletInstance -> IO a)
    -> IO a
withWalletInit
    wtc
    env@WalletEnv
        { bootEnv = WalletBootEnv{genesisData}
        , ..
        }
    credentials
    customers
    action = do
        walletState <-
            DBVar.initDBVar store
                $ Wallet.fromCredentialsAndGenesis
                    credentials
                    customers
                    genesisData
        withWalletDBVar wtc env walletState action

-- | Load an existing wallet from the given environment.
withWalletLoad
    :: Tracer IO () -- wallet tip changes
    -> WalletEnv IO
    -> (WalletInstance -> IO a)
    -> IO a
withWalletLoad wtc env@WalletEnv{..} action = do
    walletState <- DBVar.loadDBVar store
    withWalletDBVar wtc env walletState action

withWalletDBVar
    :: Tracer IO () -- wallet tip changes
    -> WalletEnv IO
    -> DBVar.DBVar IO Wallet.DeltaWalletState
    -> (WalletInstance -> IO a)
    -> IO a
withWalletDBVar
    wtc
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
                { checkpointPolicy = defaultPolicy
                , readChainPoints = do
                    walletTip <- Wallet.getWalletTip <$> readWalletState w
                    pure
                        [ walletTip
                        , Read.GenesisPoint
                        ]
                , rollForward = rollForward w wtc
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
    pure
        $ WalletPublicIdentity
            { pubXpub = Wallet.walletXPub state
            , pubNextUser = Wallet.trackedCustomers state
            }

type ResolveAddress = Address -> Maybe Customer

addressToCustomer :: WalletInstance -> IO ResolveAddress
addressToCustomer w = do
    state <- readWalletState w
    pure $ flip Wallet.addressToCustomer state

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

getTxHistoryByCustomer :: WalletInstance -> IO ByCustomer
getTxHistoryByCustomer w =
    Wallet.getTxHistoryByCustomer <$> readWalletState w

getTxHistoryByTime :: WalletInstance -> IO ByTime
getTxHistoryByTime w = Wallet.getTxHistoryByTime <$> readWalletState w

getCustomerDeposits
    :: WalletInstance
    -> Customer
    -> Maybe (WithOrigin UTCTime, WithOrigin UTCTime)
    -> IO (Map TxId ValueTransfer)
getCustomerDeposits w c i =
    Wallet.getCustomerDeposits c i <$> readWalletState w

getAllDeposits
    :: WalletInstance
    -> Maybe (WithOrigin UTCTime, WithOrigin UTCTime)
    -> IO (Map Customer ValueTransfer)
getAllDeposits w i =
    Wallet.getAllDeposits i <$> readWalletState w

rollForward
    :: WalletInstance
    -> Tracer IO () -- wallet tip changes
    -> NonEmpty (Read.EraValue Read.Block)
    -> tip
    -> IO ()
rollForward w wtc blocks _nodeTip = do
    timeFromSlot <- slotResolver w
    onWalletState w
        $ Delta.update
        $ Delta.Replace
            . Wallet.rollForwardMany
                timeFromSlot
                blocks
    traceWith wtc ()
    x <- readWalletState w
    x `seq` pure ()

rollBackward
    :: WalletInstance -> Read.ChainPoint -> IO Read.ChainPoint
rollBackward w point = do
    timeFromSlot <- slotResolver w
    onWalletState w
        $ Delta.updateWithResult
        $ first Delta.Replace . Wallet.rollBackward timeFromSlot point

-- | Compute a slot resolver for the given slots.
slotResolver
    :: WalletInstance
    -> IO LookupTimeFromSlot
slotResolver w = do
    slotToUTCTime
        $ networkEnv
        $ bootEnv
        $ env w

networkTag :: WalletInstance -> IO Read.NetworkTag
networkTag w = do
    Wallet.networkTag <$> readWalletState w

{-----------------------------------------------------------------------------
    Operations
    Constructing transactions
------------------------------------------------------------------------------}

createPayment
    :: [(Address, Read.Value)]
    -> WalletInstance
    -> IO (Either Wallet.ErrCreatePayment CurrentEraResolvedTx)
createPayment a w = do
    timeTranslation <- Network.getTimeTranslation network
    pparams <-
        Network.currentPParams network
    Wallet.createPayment pparams timeTranslation a <$> readWalletState w
  where
    network = networkEnv $ bootEnv $ env w

inspectTx
    :: CurrentEraResolvedTx
    -> WalletInstance
    -> IO Wallet.InspectTx
inspectTx tx w = flip Wallet.inspectTx tx <$> readWalletState w

resolveCurrentEraTx
    :: Write.Tx
    -> WalletInstance
    -> IO CurrentEraResolvedTx
resolveCurrentEraTx tx w =
    Wallet.resolveCurrentEraTx tx <$> readWalletState w

{-----------------------------------------------------------------------------
    Operations
    Signing transactions
------------------------------------------------------------------------------}

canSign :: WalletInstance -> IO CanSign
canSign w = do
    Wallet.canSign <$> readWalletState w

getBIP32PathsForOwnedInputs
    :: Write.Tx -> WalletInstance -> IO [BIP32Path]
getBIP32PathsForOwnedInputs a w =
    Wallet.getBIP32PathsForOwnedInputs a <$> readWalletState w

signTx
    :: Write.Tx -> Wallet.Passphrase -> WalletInstance -> IO (Maybe Write.Tx)
signTx a b w = Wallet.signTx a b <$> readWalletState w

{-----------------------------------------------------------------------------
    Operations
    Pending transactions
------------------------------------------------------------------------------}

submitTx
    :: Write.Tx -> WalletInstance -> IO (Either Network.ErrPostTx ())
submitTx tx w = do
    e <- Network.postTx network tx
    case e of
        Right _ -> do
            onWalletState w
                $ Delta.update
                $ Delta.Replace . Wallet.addTxSubmission tx
            pure $ Right ()
        _ -> pure e
  where
    network = networkEnv $ bootEnv $ env w

listTxsInSubmission :: WalletInstance -> IO [Write.Tx]
listTxsInSubmission w =
    Wallet.listTxsInSubmission <$> readWalletState w

{-----------------------------------------------------------------------------
    Logging
------------------------------------------------------------------------------}
data WalletLog
    = WalletLogDummy
    deriving (Show)
