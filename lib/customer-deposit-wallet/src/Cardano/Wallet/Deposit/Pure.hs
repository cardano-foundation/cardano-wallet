{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Deposit.Pure
    ( -- * Types
      WalletState
    , DeltaWalletState
    , WalletPublicIdentity (..)

      -- * Operations

      -- ** Mapping between customers and addresses
    , Customer
    , listCustomers
    , addressToCustomer
    , deriveAddress
    , knownCustomer
    , knownCustomerAddress
    , isCustomerAddress
    , fromRawCustomer
    , customerAddress
    , trackedCustomers
    , walletXPub

      -- ** Reading from the blockchain
    , fromXPubAndGenesis
    , Word31
    , getWalletTip
    , availableBalance
    , rollForwardMany
    , rollForwardOne
    , rollBackward
    , ValueTransfer (..)
    , getTxHistoryByCustomer
    , getTxHistoryByTime

      -- ** Writing to the blockchain
    , createPayment
    , BIP32Path (..)
    , DerivationType (..)
    , getBIP32PathsForOwnedInputs
    , signTxBody
    , addTxSubmission
    , listTxsInSubmission

      -- * Internal, for testing
    , availableUTxO
    , getCustomerDeposits
    , getAllDeposits
    ) where

import Prelude hiding
    ( lookup
    )

import Cardano.Crypto.Wallet
    ( XPrv
    , XPub
    )
import Cardano.Wallet.Address.BIP32
    ( BIP32Path (..)
    , DerivationType (..)
    )
import Cardano.Wallet.Deposit.Map
    ( Map
    , W
    , lookupMap
    , value
    )
import Cardano.Wallet.Deposit.Map.Timed
    ( Timed
    , TimedSeq
    , extractInterval
    , monoid
    )
import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( ByCustomer
    , ByTime
    , DownTime
    , TxHistory (..)
    )
import Cardano.Wallet.Deposit.Pure.UTxO.UTxOHistory
    ( UTxOHistory
    )
import Cardano.Wallet.Deposit.Pure.UTxO.ValueTransfer
    ( ValueTransfer (..)
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , TxId
    , WithOrigin
    )
import Data.Foldable
    ( fold
    , foldl'
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Map.Monoidal.Strict
    ( MonoidalMap (..)
    )
import Data.Maybe
    ( mapMaybe
    )
import Data.Ord
    ( Down (..)
    )
import Data.Set
    ( Set
    )
import Data.Time
    ( UTCTime
    )
import Data.Word.Odd
    ( Word31
    )

import qualified Cardano.Wallet.Deposit.Pure.Address as Address
import qualified Cardano.Wallet.Deposit.Pure.Balance as Balance
import qualified Cardano.Wallet.Deposit.Pure.RollbackWindow as Rollback
import qualified Cardano.Wallet.Deposit.Pure.Submissions as Sbm
import qualified Cardano.Wallet.Deposit.Pure.UTxO as UTxO
import qualified Cardano.Wallet.Deposit.Pure.UTxO.UTxOHistory as UTxOHistory
import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Cardano.Wallet.Deposit.Write as Write
import qualified Data.Delta as Delta
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}
type Customer = Address.Customer

data WalletState = WalletState
    { walletTip :: Read.ChainPoint
    , addresses :: !Address.AddressState
    , utxoHistory :: !UTxOHistory.UTxOHistory
    , txHistory :: !TxHistory
    , submissions :: Sbm.TxSubmissions
    , rootXSignKey :: Maybe XPrv
    -- , info :: !WalletInfo
    }

type DeltaWalletState = Delta.Replace WalletState

data WalletPublicIdentity = WalletPublicIdentity
    { pubXpub :: XPub
    , pubNextUser :: Word31
    }
    deriving (Show)

{-----------------------------------------------------------------------------
    Operations
    Mapping between customers and addresses
------------------------------------------------------------------------------}

listCustomers :: WalletState -> [(Customer, Address)]
listCustomers =
    Address.listCustomers . addresses

customerAddress :: Customer -> WalletState -> Maybe Address
customerAddress c = L.lookup c . listCustomers

addressToCustomer :: Address -> WalletState -> Maybe Customer
addressToCustomer address =
    Map.lookup address
        . Map.fromList
        . fmap (\(a, c) -> (c, a))
        . listCustomers

-- depend on the public key only, not on the entire wallet state
deriveAddress :: WalletState -> (Customer -> Address)
deriveAddress w =
    Address.deriveCustomerAddress
        (Address.getNetworkTag as)
        (Address.getXPub as)
  where
    as = addresses w

-- FIXME: More performant with a double index.
knownCustomer :: Customer -> WalletState -> Bool
knownCustomer c = (c `elem`) . map fst . listCustomers

knownCustomerAddress :: Address -> WalletState -> Bool
knownCustomerAddress address =
    Address.knownCustomerAddress address . addresses

isCustomerAddress :: Address -> WalletState -> Bool
isCustomerAddress address =
    flip Address.isCustomerAddress address . addresses

fromRawCustomer :: Word31 -> Customer
fromRawCustomer = id

-- | Maximum 'Customer' that is being tracked.
trackedCustomers :: WalletState -> Customer
trackedCustomers = Address.getMaxCustomer . addresses

walletXPub :: WalletState -> XPub
walletXPub = Address.getXPub . addresses

{-----------------------------------------------------------------------------
    Operations
    Reading from the blockchain
------------------------------------------------------------------------------}

fromXPubAndGenesis
    :: XPub -> Word31 -> Read.GenesisData -> WalletState
fromXPubAndGenesis xpub knownCustomerCount genesisData =
    WalletState
        { walletTip = Read.GenesisPoint
        , addresses =
            Address.fromXPubAndCount network xpub knownCustomerCount
        , utxoHistory = UTxOHistory.empty initialUTxO
        , txHistory = mempty
        , submissions = Sbm.empty
        , rootXSignKey = Nothing
        }
  where
    network = Read.getNetworkId genesisData
    initialUTxO = mempty

getWalletTip :: WalletState -> Read.ChainPoint
getWalletTip = walletTip

rollForwardMany
    :: NonEmpty (Read.EraValue Read.Block) -> WalletState -> WalletState
rollForwardMany blocks w = foldl' (flip rollForwardOne) w blocks

rollForwardOne
    :: Read.EraValue Read.Block -> WalletState -> WalletState
rollForwardOne (Read.EraValue block) w =
    w
        { walletTip = Read.getChainPoint block
        , utxoHistory = rollForwardUTxO isOurs block (utxoHistory w)
        , submissions = Delta.apply (Sbm.rollForward block) (submissions w)
        }
  where
    isOurs :: Address -> Bool
    isOurs = Address.isOurs (addresses w)

rollForwardUTxO
    :: Read.IsEra era
    => (Address -> Bool)
    -> Read.Block era
    -> UTxOHistory
    -> UTxOHistory
rollForwardUTxO isOurs block u =
    UTxOHistory.rollForward slot deltaUTxO u
  where
    (deltaUTxO, _) = Balance.applyBlock isOurs block (UTxOHistory.getUTxO u)
    slot = Read.getEraSlotNo $ Read.getEraBHeader block

rollBackward
    :: Read.ChainPoint
    -> WalletState
    -> (WalletState, Read.ChainPoint)
rollBackward targetPoint w =
    ( w
        { walletTip = actualPoint
        , utxoHistory =
            UTxOHistory.rollBackward actualSlot (utxoHistory w)
        , submissions =
            Delta.apply (Sbm.rollBackward actualSlot) (submissions w)
        }
    , actualPoint
    )
  where
    h = utxoHistory w

    targetSlot = Read.slotFromChainPoint targetPoint
    actualSlot = Read.slotFromChainPoint actualPoint

    -- NOTE: We don't keep enough information about
    -- the block hashes to roll back to
    -- any other point than the target point (or genesis).
    actualPoint =
        if (targetSlot `Rollback.member` UTxOHistory.getRollbackWindow h)
            -- FIXME: Add test for rollback window of `submissions`
            then targetPoint
            else Read.GenesisPoint

availableBalance :: WalletState -> Read.Value
availableBalance = UTxO.balance . availableUTxO

availableUTxO :: WalletState -> UTxO.UTxO
availableUTxO w =
    Balance.availableUTxO utxo pending
  where
    pending = listTxsInSubmission w
    utxo = UTxOHistory.getUTxO $ utxoHistory w

getTxHistoryByCustomer :: WalletState -> ByCustomer
getTxHistoryByCustomer state = byCustomer $ txHistory state

getTxHistoryByTime :: WalletState -> ByTime
getTxHistoryByTime state = byTime $ txHistory state

getCustomerDeposits
    :: Customer
    -> Maybe (WithOrigin UTCTime, WithOrigin UTCTime)
    -> WalletState
    -> Map.Map TxId ValueTransfer
getCustomerDeposits c interval s = fold $ do
    fmap (wonders interval . value . snd)
        $ lookupMap c
        $ getTxHistoryByCustomer s

getAllDeposits
    :: Maybe (WithOrigin UTCTime, WithOrigin UTCTime)
    -> WalletState
    -> Map.Map Customer ValueTransfer
getAllDeposits interval s =
    wonders interval
        $ value
        $ getTxHistoryByTime s

wonders
    :: (Ord k, Monoid w, Foldable (Map xs), Monoid (Map xs ValueTransfer))
    => Maybe (WithOrigin UTCTime, WithOrigin UTCTime)
    -> TimedSeq DownTime (Map (W w k : xs) ValueTransfer)
    -> Map.Map k ValueTransfer
wonders interval =
    getMonoidalMap
        . monoid
        . fmap (fmap fold . value)
        . extractInterval' interval
  where
    extractInterval'
        :: Monoid a
        => Maybe (WithOrigin UTCTime, WithOrigin UTCTime)
        -> TimedSeq (DownTime) a
        -> Timed (DownTime) a
    extractInterval' Nothing = mempty
    extractInterval' (Just (t1, t2)) = extractInterval (Down t1) (Down t2)

{-----------------------------------------------------------------------------
    Operations
    Writing to blockchain
------------------------------------------------------------------------------}

createPayment
    :: [(Address, Write.Value)] -> WalletState -> Maybe Write.TxBody
createPayment = undefined

-- needs balanceTx
-- needs to sign the transaction

getBIP32PathsForOwnedInputs
    :: Write.TxBody -> WalletState -> [BIP32Path]
getBIP32PathsForOwnedInputs txbody w =
    getBIP32Paths w
        . resolveInputAddresses
        $ Write.spendInputs txbody <> Write.collInputs txbody
  where
    resolveInputAddresses :: Set Read.TxIn -> [Read.Address]
    resolveInputAddresses ins =
        map (Read.address . snd)
            . UTxO.toList
            $ UTxO.restrictedBy (availableUTxO w) ins

getBIP32Paths :: WalletState -> [Read.Address] -> [BIP32Path]
getBIP32Paths w =
    mapMaybe $ Address.getBIP32Path (addresses w)

signTxBody :: Write.TxBody -> WalletState -> Maybe Write.Tx
signTxBody _txbody _w = undefined

addTxSubmission :: Write.Tx -> WalletState -> WalletState
addTxSubmission _tx _w = undefined

listTxsInSubmission :: WalletState -> Set Write.Tx
-- listTxsInSubmission = Sbm.listInSubmission . submissions
listTxsInSubmission _ = Set.empty
