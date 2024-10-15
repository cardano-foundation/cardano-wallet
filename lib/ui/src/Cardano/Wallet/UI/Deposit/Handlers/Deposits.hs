{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Deposit.Handlers.Deposits
    ( depositsHistory
    , DepositsHistory
    , DepositsWindow (..)
    , DepositsHandlers (..)
    , SolveAddress
    , depositsHistoryWindowHandler
    , depositsHandlers
    , getFakeDepositsHistory
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
import Data.Map.Monoidal.Strict
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
import qualified Data.Map.Monoidal.Strict as MonoidalMap
import qualified Data.Map.Strict as Map

type DepositsHistoryRaw = Map Slot (Map Address (Map TxId ValueTransfer))

data DepositsWindow = DepositsWindow
    { depositsWindowSlot :: !Slot
    , depositsWindowTransfers
        :: !(MonoidalMap Address (MonoidalMap TxId (First Slot, ValueTransfer)))
    }

instance Semigroup DepositsWindow where
    DepositsWindow s1 t1 <> DepositsWindow _s2 t2 =
        DepositsWindow s1 (t1 <> t2)

type DepositsHistory =
    MonoidalMap
        (Down (WithOrigin UTCTime))
        DepositsWindow

-- diffTimeOfindow :: Window -> NominalDiffTime
-- diffTimeOfindow Year = 365 * 24 * 3600
-- diffTimeOfindow Month = 30 * 24 * 3600
-- diffTimeOfindow Week = 7 * 24 * 3600
-- diffTimeOfindow Day = 24 * 3600
-- diffTimeOfindow Hour12 = 12 * 3600
-- diffTimeOfindow Hour6 = 6 * 3600
-- diffTimeOfindow Hour4 = 4 * 3600
-- diffTimeOfindow Hour2 = 2 * 3600
-- diffTimeOfindow Hour1 = 3600
-- diffTimeOfindow Minute30 = 30 * 60
-- diffTimeOfindow Minute15 = 15 * 60
-- diffTimeOfindow Minute10 = 10 * 60
-- diffTimeOfindow Minute5 = 5 * 60

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
    -> DepositsParams
    -> DepositsHistoryRaw
    -> DepositsHistory
depositsHistory times DepositsParams{..} raw = fold $ do
    (slot, transfers) <- Map.toList raw
    let transfers' = fmap (First $ Just slot,) <$> transfers
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
        $ MonoidalMap
        $ MonoidalMap <$> transfers'

type SolveAddress = Address -> Maybe Customer

data DepositsHandlers m = DepositsHandlers
    { depositsPrevious :: DownTime -> m (Maybe DownTime)
    , depositsNext :: DownTime -> m (Maybe DownTime)
    , depositsRetrieve :: DownTime -> m DepositsHistory
    , depositsNow :: m (Maybe DownTime)
    }

depositsHandlers
    :: NetworkEnv IO a
    -> WalletResourceM DepositsHistoryRaw
    -> Int
    -- ^ Number of rows per page
    -> DepositsParams
    -> DepositsHandlers (WalletResourceM)
depositsHandlers network retrieve rows
    params@DepositsParams{depositsFirstWeekDay, depositsWindow} =
    DepositsHandlers
        { depositsPrevious = \t -> runMaybeT $ do
            MonoidalMap transfers <- lift newHistory
            hoistMaybe $ previous rows transfers t
        , depositsNext = \t -> runMaybeT $ do
            MonoidalMap transfers <- lift newHistory
            hoistMaybe $ next rows transfers t
        , depositsRetrieve = \t -> do
            MonoidalMap transfers <- newHistory
            pure $ MonoidalMap $ nextPage rows t transfers
        , depositsNow = depositNow
        }
  where
    newHistory = do
        raw <- retrieve
        times <- liftIO $ slotsToUTCTimes network $ Map.keysSet raw
        pure $ depositsHistory times params raw
    depositNow = do
        now <- liftIO getCurrentTime
        pure $ Just $ Down $ At $ quantize
            depositsFirstWeekDay depositsWindow now

depositsHistoryWindowHandler
    :: NetworkEnv IO a
    -> SessionLayer WalletResource
    -> (SolveAddress -> DepositsWindow -> html)
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
                $ depositsHistory times params transfers of
                Just window -> render customerOfAddress window
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
