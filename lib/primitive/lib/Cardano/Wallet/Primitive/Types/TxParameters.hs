{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLabels #-}

module Cardano.Wallet.Primitive.Types.TxParameters where

import Prelude

import Cardano.Wallet.Primitive.Types.FeePolicy
    ( FeePolicy
    )
import Cardano.Wallet.Primitive.Types.TokenBundleMaxSize
    ( TokenBundleMaxSize
    )
import Control.DeepSeq
    ( NFData
    )
import Control.Lens
    ( (^.)
    )
import Data.Generics.Labels
    ()
import Data.Quantity
    ( Quantity (Quantity)
    )
import Data.Text.Class
    ( ToText (toText)
    )
import Data.Word
    ( Word16
    )
import Fmt
    ( Buildable (..)
    , Builder
    , listF'
    )
import GHC.Generics
    ( Generic
    )
import GHC.Natural
    ( Natural
    )

-- | Parameters that relate to the construction of __transactions__.
data TxParameters = TxParameters
    { getFeePolicy :: FeePolicy
    -- ^ Formula for calculating the transaction fee.
    , getTxMaxSize :: Quantity "byte" Word16
    -- ^ Maximum size of a transaction (soft or hard limit).
    , getTokenBundleMaxSize :: TokenBundleMaxSize
    -- ^ Maximum size of a serialized `TokenBundle` (_maxValSize in the
    -- Alonzo ledger)
    , getMaxExecutionUnits :: ExecutionUnits
    -- ^ Max total script execution resources units allowed per tx
    }
    deriving (Generic, Show, Eq)

instance NFData TxParameters

instance Buildable TxParameters where
    build :: TxParameters -> Builder
    build txp =
        listF'
            id
            [ "Fee policy: " <> feePolicyF (txp ^. #getFeePolicy)
            , "Tx max size: " <> txMaxSizeF (txp ^. #getTxMaxSize)
            , "max exec units: " <> maxExUnitsF (txp ^. #getMaxExecutionUnits)
            ]
      where
        feePolicyF = build . toText
        txMaxSizeF (Quantity s) = build s
        maxExUnitsF = build

data ExecutionUnits = ExecutionUnits
    { executionSteps
        :: Natural
    -- ^ This corresponds roughly to the time to execute a script.
    , executionMemory
        :: Natural
    -- ^ This corresponds roughly to the peak memory used during script
    -- execution.
    }
    deriving (Eq, Generic, Show)

instance NFData ExecutionUnits

instance Buildable ExecutionUnits where
    build (ExecutionUnits steps mem) =
        build $ "max steps: " <> show steps <> ", max memory: " <> show mem
