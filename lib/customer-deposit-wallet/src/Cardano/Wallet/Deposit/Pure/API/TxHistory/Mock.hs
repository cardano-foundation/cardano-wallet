{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Deposit.Pure.API.TxHistory.Mock
    ( mockTxHistoryByTime
    )
where

import Prelude

import Cardano.Wallet.Deposit.IO.Network.Mock
    ( unsafeSlotOfUTCTime
    )
import Cardano.Wallet.Deposit.Map
    ( Map (..)
    , singletonMap
    , singletonPatched
    )
import Cardano.Wallet.Deposit.Pure
    ( ValueTransfer (..)
    )
import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( ByTime
    , ResolveAddress
    , ResolveSlot
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , Value
    , WithOrigin (..)
    )
import Cardano.Wallet.Read
    ( Coin
    , SlotNo (..)
    , TxId
    , Value (..)
    , txIdFromHash
    )
import Cardano.Wallet.Read.Hash
    ( hashFromStringAsHex
    )
import Control.Monad
    ( replicateM
    )
import Data.Maybe
    ( fromJust
    )
import Data.Monoid
    ( First (..)
    )
import Data.Time
    ( UTCTime (..)
    )
import System.Random.MWC.Distributions
    ( standard
    )
import System.Random.Stateful
    ( StatefulGen
    , UniformRange (uniformRM)
    , mkStdGen
    , runStateGen_
    )

mockTxHistoryByTime
    :: UTCTime
    -- ^ Current time.
    -> ResolveAddress
    -- ^ Compute a customer from an address.
    -> ResolveSlot
    -- ^ Compute a time from a slot.
    -> [Address]
    -- ^ List of addresses to use.
    -> Int
    -- ^ Number of deposits to create.
    -> ByTime
mockTxHistoryByTime now solveAddress solveSlot addresses ns =
    runStateGen_ (mkStdGen 0) $ \g -> do
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

txIdR :: StatefulGen g m => g -> m TxId
txIdR g = do
    ls <-
        fmap (concatMap $ replicate 8)
            $ replicateM 8
            $ hexOfInt <$> uniformRM (0, 15) g
    pure $ unsafeMkTxId ls

valueTransferG :: StatefulGen g m => g -> m ValueTransfer
valueTransferG g = do
    spentOrReceived <- uniformRM (0, 11) g
    spent <- createSpent g spentOrReceived
    received <- createReceived g spentOrReceived
    pure $ ValueTransfer{..}

unsafeMkTxId :: String -> TxId
unsafeMkTxId = txIdFromHash . fromJust . hashFromStringAsHex

hexOfInt :: Int -> Char
hexOfInt n = "0123456789abcdef" !! (n `mod` 16)

randomValue :: StatefulGen g f => g -> Coin -> f Value
randomValue g l = ValueC <$> uniformRM (0, l) g <*> pure mempty

maxLovelaces :: Coin
maxLovelaces = 1_000_000_000

createSpent :: StatefulGen g f => g -> Int -> f Value
createSpent g r = randomValue g l
  where
    l = if r >= 0 && r < 5 || r == 11 then maxLovelaces else 0

createReceived :: StatefulGen g f => g -> Int -> f Value
createReceived g r = randomValue g l
  where
    l = if r >= 5 && r <= 11 then maxLovelaces else 0
