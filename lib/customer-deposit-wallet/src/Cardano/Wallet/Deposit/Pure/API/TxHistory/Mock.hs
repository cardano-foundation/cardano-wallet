{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Deposit.Pure.API.TxHistory.Mock
    ( mockTxHistory
    )
where

import Prelude

import Cardano.Wallet.Deposit.Map
    ( Map (..)
    , singletonMap
    , toFinger
    )
import Cardano.Wallet.Deposit.Pure
    ( ValueTransfer (..)
    )
import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( LookupTimeFromSlot
    , ResolveAddress
    , TxHistory (..)
    , firstJust
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    )
import Cardano.Wallet.Read
    ( Coin
    , SlotNo (..)
    , TxId
    , Value (..)
    , WithOrigin (..)
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
import Data.Ord
    ( Down (..)
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

import qualified Cardano.Wallet.Deposit.Time as Time

-- | Create a mock 'TxHistory' with a given number of deposits.
-- The fingertree monoid is not designed to sort by time, so the
-- we fold a Map instead and convert afterwards.
-- In reality we are going to roll forward and backward with ordered elements.
mockTxHistory
    :: UTCTime
    -- ^ Current time.
    -> ResolveAddress
    -- ^ Compute a customer from an address.
    -> LookupTimeFromSlot
    -- ^ Compute a time from a slot.
    -> [Address]
    -- ^ List of addresses to use.
    -> Int
    -- ^ Number of deposits to create.
    -> TxHistory
mockTxHistory now solveAddress solveSlot addresses ns =
    let (Map w f, byTime) = runStateGen_ (mkStdGen 0) $ \g ->
            fmap mconcat
                $ replicateM ns
                $ do
                    slot <- case Time.unsafeSlotOfUTCTime now of
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
                            Just t -> Down t
                            Nothing -> error "fakeDepositsCreate: slot not found"
                        singletonByTime =
                            singletonMap () time
                                $ singletonMap (firstJust slot) customer
                                $ singletonMap (firstJust address) txId
                                $ Value value
                        singletonByCustomer =
                            singletonMap () customer
                                $ singletonMap (firstJust address) time
                                $ singletonMap (firstJust slot) txId
                                $ Value value
                    pure (singletonByCustomer, singletonByTime)
        byCustomer' = Map w $ fmap toFinger f
    in  TxHistory byCustomer' (toFinger byTime)

txIdR :: StatefulGen g m => g -> m TxId
txIdR g = do
    ls <-
        replicateM 64
            $ hexOfInt <$> uniformRM (0, 15) g
    pure $ unsafeMkTxId ls

valueTransferG :: StatefulGen g m => g -> m ValueTransfer
valueTransferG g = do
    spentOrReceived <- uniformRM (0, 11) g
    spent <- createSpent g spentOrReceived
    received <- createReceived g spentOrReceived
    pure $ ValueTransferC{..}

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
