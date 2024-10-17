{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Deposit.Handlers.Deposits
    ( depositsHistory
    , DepositsHistory
    , DepositsHistoryByCustomer
    , DepositsHistoryByCustomerAndSlot
    , DepositsWindow (..)
    , PageHandler (..)
    , SolveAddress
    , depositsHistoryWindowHandler
    , depositsPageHandler
    , depositsDetailsPageHandler
    , getFakeDepositsHistory
    , mkMDepositsHistory
    ) where

import Prelude

import Cardano.Wallet.Deposit.IO.Network.Mock
    ( unsafeSlotOfUTCTime
    )
import Cardano.Wallet.Deposit.IO.Network.Type
    ( NetworkEnv (..)
    )
import Cardano.Wallet.Deposit.Pure
    ( Customer
    , ValueTransfer (..)
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , Slot
    , WithOrigin (..)
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
    , WalletResourceM
    , getValueTransfersWithTxIds
    , listCustomers
    )
import Cardano.Wallet.Read
    ( SlotNo (..)
    , TxId
    )
import Cardano.Wallet.UI.Common.Layer
    ( SessionLayer
    )
import Cardano.Wallet.UI.Deposit.API
    ( DepositsParams (..)
    , DownTime
    , Window (..)
    )
import Cardano.Wallet.UI.Deposit.Handlers.Addresses.Transactions
    ( txIdR
    , valueTransferG
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( catchRunWalletResourceHtml
    , solveAddress
    )
import Cardano.Wallet.UI.Lib.Paging
    ( next
    , nextPage
    , previous
    )
import Control.Concurrent.STM
    ( TVar
    , atomically
    , newTVarIO
    , readTVarIO
    , writeTVar
    )
import Control.Monad
    ( replicateM
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Monad.Trans
    ( lift
    )
import Control.Monad.Trans.Maybe
    ( MaybeT (..)
    , hoistMaybe
    )
import Data.Foldable
    ( Foldable (..)
    )
import Data.Map.Monoidal
    ( MonoidalMap (..)
    )
import Data.Map.Strict
    ( Map
    )
import Data.Monoid
    ( First (..)
    )
import Data.Ord
    ( Down (..)
    )
import Data.Time
    ( DayOfWeek
    , DiffTime
    , UTCTime (..)
    , diffUTCTime
    , getCurrentTime
    , pattern YearMonthDay
    , secondsToDiffTime
    , secondsToNominalDiffTime
    , weekFirstDay
    )
import GHC.IO
    ( unsafePerformIO
    )
import Servant
    ( Handler
    )
import System.Random.MWC.Distributions
    ( standard
    )
import System.Random.Stateful
    ( UniformRange (uniformRM)
    , mkStdGen
    , runStateGen_
    )

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Monoidal as MonoidalMap
import qualified Data.Map.Strict as Map
import Data.Maybe
    ( maybeToList
    )

type DepositsHistoryRaw = Map Slot (Map Address (Map TxId ValueTransfer))

type DepositsHistoryByCustomer =
    MonoidalMap
        Customer
        (First Address, MonoidalMap TxId (First Slot, ValueTransfer))

type DepositsHistoryByCustomerAndSlot =
    MonoidalMap
        (Customer, Slot)
        (First Address, MonoidalMap TxId ValueTransfer)

mkDepositsHistoryByCustomerAndSlot
    :: DepositsHistoryByCustomer
    -> DepositsHistoryByCustomerAndSlot
mkDepositsHistoryByCustomerAndSlot m = fold $ do
    (customer, (address, transfers)) <- MonoidalMap.assocs m
    (txId, (First (Just slot), transfer)) <- MonoidalMap.assocs transfers
    pure $ MonoidalMap.singleton
        (customer, slot)
        (address, MonoidalMap.singleton txId transfer)

data DepositsWindow = DepositsWindow
    { depositsWindowSlot :: !Slot
    , depositsWindowTransfers :: !DepositsHistoryByCustomer
    }

instance Semigroup DepositsWindow where
    DepositsWindow s1 t1 <> DepositsWindow _s2 t2 =
        DepositsWindow s1 (t1 <> t2)

type DepositsHistory =
    MonoidalMap
        (Down (WithOrigin UTCTime))
        DepositsWindow

quantizeSeconds :: DiffTime -> Integer -> DiffTime
quantizeSeconds t q =
    q' * fromIntegral @Integer (floor (t / q'))
  where
    q' = secondsToDiffTime q

quantize :: DayOfWeek -> Window -> UTCTime -> UTCTime
quantize _ Year (UTCTime (YearMonthDay y _ _) _) = UTCTime (YearMonthDay y 1 1) 0
quantize _ Month (UTCTime (YearMonthDay y m _) _) = UTCTime (YearMonthDay y m 1) 0
quantize fdk Week (UTCTime d _) = UTCTime d' 0
  where
    d' = weekFirstDay fdk d
quantize _ Day (UTCTime d _) = UTCTime d 0
quantize _ Hour12 (UTCTime d t) = UTCTime d (quantizeSeconds t $ 12 * 3600)
quantize _ Hour6 (UTCTime d t) = UTCTime d (quantizeSeconds t $ 6 * 3600)
quantize _ Hour4 (UTCTime d t) = UTCTime d (quantizeSeconds t $ 4 * 3600)
quantize _ Hour2 (UTCTime d t) = UTCTime d (quantizeSeconds t $ 2 * 3600)
quantize _ Hour1 (UTCTime d t) = UTCTime d (quantizeSeconds t 3600)
quantize _ Minute30 (UTCTime d t) = UTCTime d (quantizeSeconds t 1800)
quantize _ Minute15 (UTCTime d t) = UTCTime d (quantizeSeconds t 900)
quantize _ Minute10 (UTCTime d t) = UTCTime d (quantizeSeconds t 600)
quantize _ Minute5 (UTCTime d t) = UTCTime d (quantizeSeconds t 300)

depositsHistory
    :: Map Slot (WithOrigin UTCTime)
    -> SolveAddress
    -> DepositsParams
    -> DepositsHistoryRaw
    -> DepositsHistory
depositsHistory times customerOfAddress DepositsParams{..} raw = fold $ do
    (slot, transfers) <- Map.toList raw
    let transfers' = fmap (First $ Just slot,) <$> transfers
        customerTransfers = Map.fromList $ do
            (address, transfer) <- Map.toList transfers'
            customer <- maybeToList $ customerOfAddress address
            pure (customer, (First $ Just address, MonoidalMap transfer))
    time <-
        maybe
            []
            ( fmap pure . fmap
                $ quantize depositsFirstWeekDay depositsWindow
            )
            $ Map.lookup slot times
    pure
        $ MonoidalMap.singleton (Down time)
        $ DepositsWindow slot
        $ MonoidalMap customerTransfers

type SolveAddress = Address -> Maybe Customer

data PageHandler m a b = PageHandler
    { pagePrevious :: a -> m (Maybe a)
    , pageNext :: a -> m (Maybe a)
    , page :: a -> m b
    , start :: m (Maybe a)
    }

mkMDepositsHistory
    :: MonadIO m
    => SolveAddress
    -> DepositsParams
    -> NetworkEnv IO block
    -> m DepositsHistoryRaw
    -> m DepositsHistory
mkMDepositsHistory
    customerOfAddress
    params
    network
    retrieve = do
        raw <- retrieve
        times <- liftIO $ slotsToUTCTimes network $ Map.keysSet raw
        pure $ depositsHistory times customerOfAddress params raw

depositsPageHandler
    :: MonadIO m
    => DepositsParams
    -> m DepositsHistory
    -> Int
    -> PageHandler m DownTime DepositsHistory
depositsPageHandler
    DepositsParams{depositsFirstWeekDay, depositsWindow}
    newDepositsHistory
    rows =
        PageHandler
            { pagePrevious = \t -> runMaybeT $ do
                MonoidalMap transfers <- lift newDepositsHistory
                hoistMaybe $ previous rows transfers t
            , pageNext = \t -> runMaybeT $ do
                MonoidalMap transfers <- lift newDepositsHistory
                hoistMaybe $ next rows transfers t
            , page = \t -> do
                MonoidalMap transfers <- newDepositsHistory
                pure $ MonoidalMap $ nextPage rows t transfers
            , start = do
                c <- liftIO getCurrentTime
                pure $ Just $ Down $ At $ quantize depositsFirstWeekDay depositsWindow c
            }

depositsDetailsPageHandler
    :: (Monad m)
    => m (MonoidalMap DownTime DepositsWindow)
    -> DownTime
    -> Int
    -> PageHandler m (DownTime, (Customer, Slot))
        DepositsHistoryByCustomerAndSlot
depositsDetailsPageHandler
    newDepositsHistory
    time
    rows =
        PageHandler
            { pagePrevious = \(t, k) -> runMaybeT $ do
                MonoidalMap r <- newDepositsWindow t
                k' <- hoistMaybe $ previous rows r k
                pure (t, k')
            , pageNext = \(t, k) -> runMaybeT $ do
                MonoidalMap r <- newDepositsWindow t
                k' <- hoistMaybe $ next rows r k
                pure (t, k')
            , page = \(t, k) -> fmap fold $ runMaybeT $ do
                MonoidalMap r <- newDepositsWindow t
                pure $ MonoidalMap $ nextPage rows k r
            , start = runMaybeT $ do
                r <- newDepositsWindow time
                ((c,s), _) <- hoistMaybe $ MonoidalMap.lookupMin r
                pure (time, (c, s))

            }
      where
        newDepositsWindow t = do
            transfers <- lift newDepositsHistory
            DepositsWindow _ r <-
                hoistMaybe $ MonoidalMap.lookup t transfers
            pure $ mkDepositsHistoryByCustomerAndSlot r

depositsHistoryWindowHandler
    :: NetworkEnv IO a
    -> SessionLayer WalletResource
    -> (DepositsWindow -> html)
    -> (BL.ByteString -> html)
    -> DepositsParams
    -> WithOrigin UTCTime
    -> Handler html
depositsHistoryWindowHandler network layer render alert params start = do
    liftIO $ print params
    catchRunWalletResourceHtml layer alert id
        $ do
            customerOfAddress <- solveAddress
            transfers <-
                if depositsFakeData params
                    then getFakeDepositsHistory
                    else getValueTransfersWithTxIds
            times <- liftIO $ slotsToUTCTimes network $ Map.keysSet transfers
            pure $ case MonoidalMap.lookup (Down start)
                $ depositsHistory times customerOfAddress params transfers of
                Just window -> render window
                Nothing -> alert "No deposits found for that time period"

--------------------------------------------------------------------------------
-- Fake data
--------------------------------------------------------------------------------

type FakeDepositsCache = TVar (Maybe (UTCTime, [Address], DepositsHistoryRaw))

getFakeDepositsHistory
    :: WalletResourceM DepositsHistoryRaw
getFakeDepositsHistory = do
    addresses <- fmap snd <$> listCustomers
    liftIO $ fakeDeposits addresses

{-# NOINLINE fakeDepositsCache #-}
fakeDepositsCache :: FakeDepositsCache
fakeDepositsCache = unsafePerformIO $ newTVarIO Nothing

fakeDeposits :: [Address] -> IO DepositsHistoryRaw
fakeDeposits addresses = do
    now <- getCurrentTime
    cache <- readTVarIO fakeDepositsCache
    let generate = do
            let newDeposits = fakeDepositsCreate now addresses
            atomically $ writeTVar fakeDepositsCache $ Just (now, addresses, newDeposits)
            pure newDeposits
    case cache of
        Just (now', addresses', deposits)
            | diffUTCTime now now'
                < secondsToNominalDiffTime 6000000 -- better not do it on the full set TODO
                && addresses' == addresses -> do
                putStrLn "Using cached fake deposits"
                pure deposits
        Just _ -> do
            putStrLn "Regenerating fake deposits"
            generate
        Nothing -> do
            putStrLn "Generating fake deposits"
            generate

fakeDepositsCreate
    :: UTCTime
    -> [Address]
    -> Map Slot (Map Address (Map TxId ValueTransfer))
fakeDepositsCreate now addresses = runStateGen_ (mkStdGen 0) $ \g -> do
    let ns = 10000
    fmap (getMonoidalMap . fmap (getMonoidalMap . fmap getMonoidalMap) . fold)
        $ replicateM ns
        $ do
            slot <- case unsafeSlotOfUTCTime now of
                Origin -> pure Origin
                At (SlotNo n) -> do
                    slotInt <-
                        floor
                            . (fromIntegral n -)
                            . (* fromIntegral n)
                            . (abs)
                            <$> standard g
                    -- _ <- error $ show slotInt
                    pure
                        $ if slotInt < 0
                            then Origin
                            else At (SlotNo $ fromIntegral @Integer slotInt)
            addressNumber <- uniformRM (0, length addresses - 1) g
            let address = addresses !! addressNumber
            value <- valueTransferG g
            txId <- txIdR g
            pure
                $ MonoidalMap.singleton slot
                $ MonoidalMap.singleton address
                $ MonoidalMap.singleton txId value
