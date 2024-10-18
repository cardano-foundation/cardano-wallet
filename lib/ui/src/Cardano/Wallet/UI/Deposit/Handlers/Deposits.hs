{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.UI.Deposit.Handlers.Deposits where

import Prelude hiding
    ( lookup
    )

import Cardano.Wallet.Deposit.IO.Network.Mock
    ( unsafeSlotOfUTCTime
    , unsafeUTCTimeOfSlot
    )
import Cardano.Wallet.Deposit.Map
    ( Map (Map, Value)
    , W
    , forMap
    , forPatched
    , lookup
    , openMap
    , singletonMap
    , singletonPatched
    , type (^^^)
    , withMap
    , withPatched
    )
import Cardano.Wallet.Deposit.Pure
    ( Customer
    , ValueTransfer (..)
    )
import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( ByTime
    , DownTime
    , ResolveAddress
    , ResolveSlot
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , Slot
    , WithOrigin (..)
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
    , WalletResourceM
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
    , Window (..)
    )
import Cardano.Wallet.UI.Deposit.Handlers.Addresses.Transactions
    ( txIdR
    , valueTransferG
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( catchRunWalletResourceHtml
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
    ()
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
    , addDays
    , addGregorianMonthsRollOver
    , addGregorianYearsRollOver
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
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Map.Monoidal.Strict as MonoidalMap
import qualified Data.Map.Strict as Map

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

nextQuantizedTime :: DayOfWeek -> Window -> UTCTime -> UTCTime
nextQuantizedTime _ Year (UTCTime d s) = UTCTime (addGregorianYearsRollOver (-1) d) s
nextQuantizedTime _ Month (UTCTime d s) = UTCTime (addGregorianMonthsRollOver (-1) d) s
nextQuantizedTime fdk Week (UTCTime d s) = UTCTime (weekFirstDay fdk $ addDays (-7) d) s
nextQuantizedTime _ Day (UTCTime d s) = UTCTime (addDays (-1) d) s
nextQuantizedTime _ Hour12 (UTCTime d s) = UTCTime d (s - 12 * 3600)
nextQuantizedTime _ Hour6 (UTCTime d s) = UTCTime d (s - 6 * 3600)
nextQuantizedTime _ Hour4 (UTCTime d s) = UTCTime d (s - 4 * 3600)
nextQuantizedTime _ Hour2 (UTCTime d s) = UTCTime d (s - 2 * 3600)
nextQuantizedTime _ Hour1 (UTCTime d s) = UTCTime d (s - 3600)
nextQuantizedTime _ Minute30 (UTCTime d s) = UTCTime d (s - 1800)
nextQuantizedTime _ Minute15 (UTCTime d s) = UTCTime d (s - 900)
nextQuantizedTime _ Minute10 (UTCTime d s) = UTCTime d (s - 600)
nextQuantizedTime _ Minute5 (UTCTime d s) = UTCTime d (s - 300)

minKey :: MonoidalMap k a -> Maybe k
minKey = fmap fst . MonoidalMap.lookupMin

quantizeByTime'
    :: Monoid a
    => DayOfWeek
    -> Window
    -> MonoidalMap DownTime a
    -> MonoidalMap DownTime a
quantizeByTime' fdk w mm = case MonoidalMap.lookupMin mm of
    Just ((Down (At t)), _) ->
        let
            t' = quantize fdk w t
            nt = Down $ At $ nextQuantizedTime fdk w t'
            (before, match, after) = MonoidalMap.splitLookup nt mm
            after' =
                maybe
                    after
                    (\v -> MonoidalMap.insert nt v after)
                    match
        in
            MonoidalMap.singleton (Down (At t')) (fold before)
                <> quantizeByTime' fdk w after'
    Just (Down Origin, _) ->
        let
            (before, match, after) = MonoidalMap.splitLookup (Down Origin) mm
            before' =
                maybe
                    before
                    (\v -> MonoidalMap.insert (Down Origin) v before)
                    match
        in
            MonoidalMap.singleton (Down Origin) (fold before')
                <> quantizeByTime' fdk w after
    Nothing -> MonoidalMap.empty

quantizeByTime :: DayOfWeek -> Window -> ByTime -> ByTime
quantizeByTime fdk w (Map mm) = Map $ quantizeByTime' fdk w mm
data PageHandler m a b = PageHandler
    { pagePrevious :: a -> m (Maybe a)
    , pageNext :: a -> m (Maybe a)
    , page :: a -> m b
    , start :: m (Maybe a)
    }

depositsPageHandler
    :: MonadIO m
    => DepositsParams
    -> m ByTime
    -> Int
    -> PageHandler m DownTime ByTime
depositsPageHandler
    DepositsParams{depositsFirstWeekDay, depositsWindow}
    newDepositsHistory
    rows =
        PageHandler
            { pagePrevious = \t -> runMaybeT $ do
                m <- lift newDepositsHistory'
                hoistMaybe
                    $ withMap m
                    $ \(MonoidalMap transfers) -> previous rows transfers t
            , pageNext = \t -> runMaybeT $ do
                m <- lift newDepositsHistory'
                hoistMaybe
                    $ withMap m
                    $ \(MonoidalMap transfers) -> next rows transfers t
            , page = \t -> do
                m <- newDepositsHistory'
                liftIO $ print $ MonoidalMap.keys $ openMap m
                pure
                    $ forMap m
                    $ \(MonoidalMap transfers) ->
                        MonoidalMap $ nextPage rows t transfers
            , start = do
                m <- newDepositsHistory'
                pure $ withMap m minKey
            }
      where
        newDepositsHistory' =
            quantizeByTime
                depositsFirstWeekDay
                depositsWindow
                <$> newDepositsHistory

type InWindow =
    Map
        '[ W (First Slot) Customer
         , W (First Address) TxId
         ]
        ValueTransfer

expandByTime :: ByTime -> DownTime ^^^ InWindow
expandByTime = openMap

depositsDetailsPageHandler
    :: forall m
     . (MonadIO m)
    => DepositsParams
    -> m ByTime
    -> DownTime
    -> Int
    -> PageHandler m (DownTime, Customer) InWindow
depositsDetailsPageHandler
    DepositsParams{depositsFirstWeekDay, depositsWindow}
    newDepositsHistory
    time
    rows =
        PageHandler
            { pagePrevious = \(t, k) -> runMaybeT $ do
                m <- newDepositsWindow t
                fmap (t,)
                    $ hoistMaybe
                    $ withPatched m
                    $ \_ (MonoidalMap r) ->
                        previous rows r k
            , pageNext = \(t, k) -> runMaybeT $ do
                m <- newDepositsWindow t
                fmap (t,)
                    $ hoistMaybe
                    $ withPatched m
                    $ \_ (MonoidalMap r) ->
                        next rows r k
            , page = \(t, k) -> fmap fold $ runMaybeT $ do
                liftIO $ print t
                m <- newDepositsWindow t
                liftIO $ putStrLn "page"
                liftIO $ print m
                liftIO $ print k
                liftIO $ putStrLn "page"
                let p = forPatched m $ \_ (MonoidalMap r) ->
                        MonoidalMap $ nextPage rows k r
                liftIO $ print p
                pure p
            , start = runMaybeT $ do
                m <- newDepositsWindow time
                hoistMaybe
                    $ fmap (time,)
                    $ withPatched m
                    $ \_ -> minKey
            }
      where
        newDepositsWindow :: DownTime -> MaybeT m InWindow
        newDepositsWindow t = do
            transfers' <- lift newDepositsHistory
            let transfers'' =
                    quantizeByTime
                        depositsFirstWeekDay
                        depositsWindow
                        transfers'
            hoistMaybe $ lookup t transfers''

depositsHistoryWindowHandler
    :: SessionLayer WalletResource
    -> (InWindow -> html)
    -> (BL.ByteString -> html)
    -> DepositsParams
    -> WithOrigin UTCTime
    -> Handler html
depositsHistoryWindowHandler layer render alert
    DepositsParams{depositsFirstWeekDay, depositsWindow, depositsFakeData}
    start = do
    catchRunWalletResourceHtml layer alert id
        $ do
            transfers <-
                if depositsFakeData
                    then getFakeDepositsHistory
                    else error "depositsHistoryWindowHandler: real data not implemented"
            let transfers' =
                    quantizeByTime
                        depositsFirstWeekDay
                        depositsWindow
                        transfers
            pure $ case lookup (Down start) transfers' of
                Just window -> render window
                Nothing -> alert $
                    "No deposits found for that time period" <> " "
                        <> BL8.pack (show start)

-- --------------------------------------------------------------------------------
-- -- Fake data
-- --------------------------------------------------------------------------------

type FakeDepositsCache = TVar (Maybe (UTCTime, [Address], ByTime))

getFakeDepositsHistory
    :: WalletResourceM ByTime
getFakeDepositsHistory = do
    addresses <- listCustomers
    let solveAddress x =
            Map.lookup x
                $ Map.fromList
                $ fmap (\(c, a) -> (a, c)) addresses
    liftIO
        $ fakeDeposits
            solveAddress
            (fmap Down <$> unsafeUTCTimeOfSlot)
        $ snd <$> addresses

{-# NOINLINE fakeDepositsCache #-}
fakeDepositsCache :: FakeDepositsCache
fakeDepositsCache = unsafePerformIO $ newTVarIO Nothing

fakeDeposits
    :: ResolveAddress
    -> ResolveSlot
    -> [Address]
    -> IO ByTime
fakeDeposits solveAddress solveSlot addresses = do
    now <- getCurrentTime
    cache <- readTVarIO fakeDepositsCache
    let generate = do
            let newDeposits = fakeDepositsCreate now solveAddress solveSlot addresses
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
    -> ResolveAddress
    -> ResolveSlot
    -> [Address]
    -> ByTime
fakeDepositsCreate now solveAddress solveSlot addresses =
    runStateGen_ (mkStdGen 0) $ \g -> do
        let ns = 10000
        fmap mconcat
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
                let customer = case solveAddress address of
                        Just c -> c
                        Nothing -> error "fakeDepositsCreate: address not found"
                let time = case solveSlot slot of
                        Just t -> t
                        Nothing -> error "fakeDepositsCreate: slot not found"
                pure
                    $ singletonMap time
                    $ singletonPatched (First $ Just slot) customer
                    $ singletonPatched (First $ Just address) txId
                    $ Value value
