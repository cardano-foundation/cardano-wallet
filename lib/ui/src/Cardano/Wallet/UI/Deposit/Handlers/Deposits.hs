{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Deposit.Handlers.Deposits
    ( depositsHistoryHandler
    , depositsHistory
    , DepositsHistory
    , DepositsWindow (..)
    ) where

import Prelude

import Cardano.Wallet.Deposit.IO.Network.Mock
    ( unsafeSlotOfUTCTime
    )
import Cardano.Wallet.Deposit.IO.Network.Type
    ( NetworkEnv (..)
    )
import Cardano.Wallet.Deposit.Pure
    ( ValueTransfer (..)
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , Slot
    , WithOrigin (..)
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
    , getValueTransfers
    , listCustomers
    )
import Cardano.Wallet.Read
    ( SlotNo (..)
    )
import Cardano.Wallet.UI.Common.Layer
    ( SessionLayer
    )
import Cardano.Wallet.UI.Deposit.API
    ( DepositsParams (..)
    , Window (..)
    )
import Cardano.Wallet.UI.Deposit.Handlers.Addresses.Transactions
    ( valueTransferG
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( catchRunWalletResourceHtml
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
import Data.Foldable
    ( Foldable (..)
    )
import Data.Map.Monoidal.Strict
    ( MonoidalMap (..)
    )
import Data.Map.Strict
    ( Map
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

type DepositsHistoryRaw = Map Slot (Map Address ValueTransfer)

data DepositsWindow = DepositsWindow
    { depositsWindowSlot :: !Slot
    , depositsWindowTransfers :: !(MonoidalMap Address ValueTransfer)
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
    -> DepositsParams
    -> DepositsHistoryRaw
    -> DepositsHistory
depositsHistory times DepositsParams{..} raw = fold $ do
    (slot, transfers) <- Map.toList raw
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
        $ MonoidalMap transfers

depositsHistoryHandler
    :: NetworkEnv IO a
    -> SessionLayer WalletResource
    -> (DepositsHistory -> html)
    -> (BL.ByteString -> html)
    -> DepositsParams
    -> Handler html
depositsHistoryHandler network layer render alert params = do
    catchRunWalletResourceHtml layer alert id
        $ do
            transfers <-
                if depositsFakeData params
                    then do
                        now <- liftIO getCurrentTime
                        addresses <- fmap snd <$> listCustomers
                        liftIO $ fakeDeposits now addresses
                    else getValueTransfers
            times <- liftIO $ slotsToUTCTimes network $ Map.keysSet transfers

            let
                based = case depositsViewStart params of
                    Nothing -> transfers
                    Just start ->
                        let acceptedTimes = Map.filter (<= At start) times
                        in  Map.restrictKeys transfers (Map.keysSet acceptedTimes)
            pure
                $ render
                $ MonoidalMap.take 1000
                $ depositsHistory times params based

--------------------------------------------------------------------------------
-- Fake data
--------------------------------------------------------------------------------

type FakeDepositsCache = TVar (Maybe (UTCTime, [Address], DepositsHistoryRaw))

{-# NOINLINE fakeDepositsCache #-}
fakeDepositsCache :: FakeDepositsCache
fakeDepositsCache = unsafePerformIO $ newTVarIO Nothing

fakeDeposits :: UTCTime -> [Address] -> IO DepositsHistoryRaw
fakeDeposits now addresses = do
    cache <- readTVarIO fakeDepositsCache
    let generate = do
            let newDeposits = fakeDepositsCreate now addresses
            atomically $ writeTVar fakeDepositsCache $ Just (now, addresses, newDeposits)
            pure newDeposits
    case cache of
        Just (now', addresses', deposits)
            | diffUTCTime now now' < secondsToNominalDiffTime 60 && addresses' == addresses -> do
                putStrLn "Using cached fake deposits"
                pure deposits
        Just _ -> do
            putStrLn "Regenerating fake deposits"
            generate
        Nothing -> do
            putStrLn "Generating fake deposits"
            generate

fakeDepositsCreate :: UTCTime -> [Address] -> Map Slot (Map Address ValueTransfer)
fakeDepositsCreate now addresses = runStateGen_ (mkStdGen 0) $ \g -> do
    -- ns <- uniformRM (1000, 100000) g
    let ns = 100000
    fmap (fmap getMonoidalMap . fold) $ replicateM ns $ do
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
        pure $ Map.singleton slot $ MonoidalMap.singleton address value
