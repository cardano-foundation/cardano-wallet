{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright: © 2022 IOHK
License: Apache-2.0

Data type 'Submissions' for storing a set of submitted transactions.

-}
module Cardano.Wallet.Submissions.TxStatus
    ( HasTxId (..)
    , TxStatus (..)
    , _Expired
    , _InLedger
    , _InSubmission
    , _Unknown
    , status
    , slotObservation
    , TxStatuses
    )
    where

import Prelude

import Control.Lens
    ( makePrisms )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe )

import qualified Data.Map.Strict as Map

-- | Classify values with an Id.
class HasTxId a where
    type TxId a
    txId :: a -> TxId a

-- | Status of a transactions tracked in the submissions store.
data TxStatus slot tx where
    -- | A transaction which is waiting to appear in on the blockchain.
    InSubmission
      :: slot -- ^ expiration slot
      -> tx -- ^ transaction
      -> TxStatus slot tx
    -- | A transaction which has been accepted on the blockchain.
    InLedger
      :: slot -- ^ expiration slot
      -> slot -- ^ accetpance slot
      -> tx -- ^ transaction
      -> TxStatus slot tx
    -- | A transaction which has expired.
    Expired
      :: slot -- ^ expiration slot
      -> tx -- ^ transaction
      -> TxStatus slot tx
    -- | A transaction which is not tracked by the submissions store.
    Unknown :: TxStatus slot tx
    deriving (Show, Eq)

makePrisms ''TxStatus

type TxStatuses slot tx = Map (TxId tx) (TxStatus slot tx )

-- | Extract a transaction status based on its identifier.
status
    :: Ord (TxId tx)
    => TxId tx
    -> TxStatuses slot tx
    -> TxStatus slot tx
status id' m = fromMaybe Unknown $ Map.lookup id' m

slotObservation :: TxStatus slot tx -> Maybe slot
slotObservation = \case
    InSubmission s _ -> Just s
    InLedger _ s _ -> Just s
    Expired s _ -> Just s
    Unknown -> Nothing
