{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    , getEraSlotOfBlock

      -- ** Writing to the blockchain
    , ErrCreatePayment (..)
    , createPayment
    , BIP32Path (..)
    , DerivationType (..)
    , getBIP32PathsForOwnedInputs
    , signTx
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
import Cardano.Wallet.Deposit.Pure.Balance
    ( ValueTransferMap
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
    , WithOrigin (..)
    , getEraSlotOfBlock
    )
import Cardano.Wallet.Deposit.Time
    ( LookupTimeFromSlot
    )
import Control.Monad.Trans.Except
    ( runExceptT
    )
import Data.Bifunctor
    ( first
    )
import Data.Digest.CRC32
    ( crc32
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
import qualified Cardano.Wallet.Deposit.Pure.API.TxHistory as TxHistory
import qualified Cardano.Wallet.Deposit.Pure.Balance as Balance
import qualified Cardano.Wallet.Deposit.Pure.RollbackWindow as Rollback
import qualified Cardano.Wallet.Deposit.Pure.Submissions as Sbm
import qualified Cardano.Wallet.Deposit.Pure.UTxO as UTxO
import qualified Cardano.Wallet.Deposit.Pure.UTxO.UTxOHistory as UTxOHistory
import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Cardano.Wallet.Deposit.Write as Write
import qualified Cardano.Wallet.Read.Hash as Hash
import qualified Control.Monad.Random.Strict as Random
import qualified Data.Delta as Delta
import qualified Data.List as L
import qualified Data.Map.Strict as Map

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}
type Customer = Address.Customer

data WalletState = WalletState
    { walletTip :: Read.ChainPoint
    -- ^ The wallet includes information from all blocks until
    -- and including this one.
    , addresses :: !Address.AddressState
    -- ^ Addresses and public keys known to this wallet.
    , utxoHistory :: !UTxOHistory.UTxOHistory
    -- ^ UTxO of this wallet, with support for rollbacks.
    , txHistory :: !TxHistory
    -- ^ (Summarized) transaction history of this wallet.
    , submissions :: Sbm.TxSubmissions
    -- ^ Queue of pending transactions.
    , rootXSignKey :: Maybe XPrv
    -- ^ Maybe a private key for signing transactions.
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
        , utxoHistory = UTxOHistory.fromOrigin initialUTxO
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
    :: LookupTimeFromSlot
    -> NonEmpty (Read.EraValue Read.Block)
    -> WalletState
    -> WalletState
rollForwardMany timeFromSlot blocks w =
    foldl' (flip $ rollForwardOne timeFromSlot) w blocks

rollForwardOne
    :: LookupTimeFromSlot
    -> Read.EraValue Read.Block
    -> WalletState
    -> WalletState
rollForwardOne timeFromSlot (Read.EraValue block) w =
    w
        { walletTip = Read.getChainPoint block
        , utxoHistory = utxoHistory'
        , submissions = Delta.apply (Sbm.rollForward block) (submissions w)
        , txHistory =
            TxHistory.rollForward
                valueTransfers
                (`addressToCustomer` w)
                timeFromSlot
                (getEraSlotOfBlock block)
                (txHistory w)
        }
  where
    (utxoHistory', valueTransfers) =
        rollForwardUTxO isOurs block (utxoHistory w)
    isOurs :: Address -> Bool
    isOurs = Address.isOurs (addresses w)

rollForwardUTxO
    :: Read.IsEra era
    => (Address -> Bool)
    -> Read.Block era
    -> UTxOHistory
    -> (UTxOHistory, ValueTransferMap)
rollForwardUTxO isOurs block u =
    (UTxOHistory.rollForward slot deltaUTxO u, valueTransfers)
  where
    (deltaUTxO, _, valueTransfers) =
        Balance.applyBlock isOurs block (UTxOHistory.getUTxO u)
    slot = Read.getEraSlotNo $ Read.getEraBHeader block

rollBackward
    :: LookupTimeFromSlot
    -> Read.ChainPoint
    -> WalletState
    -> (WalletState, Read.ChainPoint)
rollBackward timeFromSlot targetPoint w =
    ( w
        { walletTip = actualPoint
        , utxoHistory =
            UTxOHistory.rollBackward actualSlot (utxoHistory w)
        , submissions =
            Delta.apply (Sbm.rollBackward actualSlot) (submissions w)
        , txHistory =
            TxHistory.rollBackward timeFromSlot actualSlot (txHistory w)
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
            then -- FIXME: Add test for rollback window of `submissions`
                targetPoint
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
    Constructing transactions
------------------------------------------------------------------------------}
data ErrCreatePayment
    = ErrCreatePaymentNotRecentEra (Read.EraValue Read.Era)
    | ErrCreatePaymentBalanceTx (Write.ErrBalanceTx Write.Conway)
    deriving (Eq, Show)

-- | Create a payment to a list of destinations.
createPayment
    :: Read.EraValue Read.PParams
    -> Write.TimeTranslation
    -> [(Address, Write.Value)]
    -> WalletState
    -> Either ErrCreatePayment Write.Tx
createPayment (Read.EraValue (Read.PParams pparams :: Read.PParams era)) a b w =
    case Read.theEra :: Read.Era era of
        Read.Conway ->
            first ErrCreatePaymentBalanceTx
                $ createPaymentConway pparams a b w
        era' -> Left $ ErrCreatePaymentNotRecentEra (Read.EraValue era')

-- | In the Conway era: Create a payment to a list of destinations.
createPaymentConway
    :: Write.PParams Write.Conway
    -> Write.TimeTranslation
    -> [(Address, Write.Value)]
    -> WalletState
    -> Either (Write.ErrBalanceTx Write.Conway) Write.Tx
createPaymentConway pparams timeTranslation destinations w =
    fmap (Read.Tx . fst)
        . flip Random.evalRand (pilferRandomGen w)
        . runExceptT
        . balance
            (availableUTxO w)
            (addresses w)
        . mkPartialTx
        $ paymentTxBody
  where
    paymentTxBody :: Write.TxBody
    paymentTxBody =
        Write.TxBody
            { spendInputs = mempty
            , collInputs = mempty
            , txouts =
                Map.fromList
                    $ zip [(toEnum 0) ..]
                    $ map (uncurry Write.mkTxOut) destinations
            , collRet = Nothing
            , expirySlot = Just . computeExpirySlot $ walletTip w
            }

    mkPartialTx :: Write.TxBody -> Write.PartialTx Write.Conway
    mkPartialTx txbody =
        Write.PartialTx
            { tx = Read.unTx $ Write.mkTx txbody
            , extraUTxO = mempty :: Write.UTxO Write.Conway
            , redeemers = mempty
            , stakeKeyDeposits = Write.StakeKeyDepositMap mempty
            , timelockKeyWitnessCounts = Write.TimelockKeyWitnessCounts mempty
            }

    balance utxo addressState =
        Write.balanceTx
            pparams
            timeTranslation
            Write.AllKeyPaymentCredentials
            (Write.constructUTxOIndex $ Write.toConwayUTxO utxo)
            (changeAddressGen addressState)
            ()

    changeAddressGen s =
        Write.ChangeAddressGen
            { Write.genChangeAddress =
                first Read.decompactAddr . Address.newChangeAddress s
            , Write.maxLengthChangeAddress =
                Read.decompactAddr $ Address.mockMaxLengthChangeAddress s
            }

-- | Use entropy contained in the current 'WalletState'
-- to construct a pseudorandom seed.
-- (NOT a viable source of cryptographic randomness.)
--
-- Possible downsides of this approach:
--
-- 1. security/privacy
-- 2. concurrency
-- 3. retries for different coin selections
pilferRandomGen :: WalletState -> Random.StdGen
pilferRandomGen =
    Random.mkStdGen . fromEnum . fromChainPoint . walletTip
  where
    fromChainPoint (Read.GenesisPoint) = 0
    fromChainPoint (Read.BlockPoint _ headerHash) =
        crc32 $ Hash.hashToBytes headerHash

-- | Compute an expiry slot from a current 'ChainPoint'.
computeExpirySlot :: Read.ChainPoint -> Read.SlotNo
computeExpirySlot Read.GenesisPoint = 0
computeExpirySlot (Read.BlockPoint slotNo _) =
    slotNo + hour
  where
    hour = 60 * 60

{-----------------------------------------------------------------------------
    Operations
    Signing transactions
------------------------------------------------------------------------------}

getBIP32PathsForOwnedInputs :: Write.Tx -> WalletState -> [BIP32Path]
getBIP32PathsForOwnedInputs tx w =
    getBIP32Paths w $ resolveInputAddresses inputs
  where
    inputs = Read.getInputs tx <> Read.getCollateralInputs tx

    resolveInputAddresses :: Set Read.TxIn -> [Read.Address]
    resolveInputAddresses ins =
        map (Read.address . snd)
            . UTxO.toList
            $ UTxO.restrictedBy (availableUTxO w) ins

getBIP32Paths :: WalletState -> [Read.Address] -> [BIP32Path]
getBIP32Paths w =
    mapMaybe $ Address.getBIP32Path (addresses w)

signTx :: Write.Tx -> WalletState -> Maybe Write.Tx
signTx _tx _w = undefined

{-----------------------------------------------------------------------------
    Operations
    Pending transactions
------------------------------------------------------------------------------}

addTxSubmission :: Write.Tx -> WalletState -> WalletState
addTxSubmission tx w =
    w
        { submissions = Delta.apply (Sbm.add tx) (submissions w)
        }

listTxsInSubmission :: WalletState -> [Write.Tx]
listTxsInSubmission = Sbm.listInSubmission . submissions
