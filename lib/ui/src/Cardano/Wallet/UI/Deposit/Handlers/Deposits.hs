{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.UI.Deposit.Handlers.Deposits
    ( depositsHistoryHandler
    , depositsHistory
    , DepositsHistory
    , DepositsWindow (..)
    ) where

import Prelude

import Cardano.Wallet.Deposit.IO.Network.Type
    ( NetworkEnv (..)
    )
import Cardano.Wallet.Deposit.Pure
    ( ValueTransfer
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , Slot
    , WithOrigin
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
    , getValueTransfers
    )
import Cardano.Wallet.UI.Common.Layer
    ( SessionLayer
    )
import Cardano.Wallet.UI.Deposit.API
    ( DepositsParams (..)
    , Window (..)
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( catchRunWalletResourceHtml
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
    , diffTimeToPicoseconds
    , pattern YearMonthDay
    , picosecondsToDiffTime
    , weekFirstDay
    )
import Servant
    ( Handler
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

quantizeSeconds :: DiffTime -> Int -> DiffTime
quantizeSeconds t q =
    picosecondsToDiffTime
        $ fromIntegral q * (diffTimeToPicoseconds t `div` fromIntegral q)

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
            transfers <- getValueTransfers
            times <- liftIO $ slotsToUTCTimes network $ Map.keysSet transfers
            render . depositsHistory times params <$> getValueTransfers
