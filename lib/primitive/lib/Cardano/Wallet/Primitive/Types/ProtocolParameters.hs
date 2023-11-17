{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Primitive.Types.ProtocolParameters
    ( ProtocolParameters (..)
    )
where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin
    )
import Cardano.Wallet.Primitive.Types.DecentralizationLevel
    ( DecentralizationLevel
    )
import Cardano.Wallet.Primitive.Types.EpochNo
    ( EpochNo
    )
import Cardano.Wallet.Primitive.Types.EraInfo
    ( EraInfo
    )
import Cardano.Wallet.Primitive.Types.ExecutionUnitPrices
    ( ExecutionUnitPrices
    )
import Cardano.Wallet.Primitive.Types.TxParameters
    ( TxParameters
    )
import Control.DeepSeq
    ( NFData (..)
    )
import Control.Lens
    ( (^.)
    )
import Data.Word
    ( Word16
    )
import Fmt
    ( Buildable (..)
    , blockListF'
    , indentF
    )
import GHC.Generics
    ( Generic
    )
import Numeric.Natural
    ( Natural
    )

-- | Protocol parameters that can be changed through the update system.
data ProtocolParameters = ProtocolParameters
    { decentralizationLevel
        :: DecentralizationLevel
    -- ^ The current level of decentralization in the network.
    , txParameters
        :: TxParameters
    -- ^ Parameters that affect transaction construction.
    , desiredNumberOfStakePools
        :: Word16
    -- ^ The current desired number of stakepools in the network.
    -- Also known as k parameter.
    , stakeKeyDeposit
        :: Coin
    -- ^ Registering a stake key requires storage on the node and as such
    -- needs a deposit. There may be more actions that require deposit
    -- (such as registering a stake pool).
    , eras
        :: EraInfo EpochNo
    -- ^ Contains information about when each era did start if it has
    -- already happened, or otherwise when it will start, if the hard-fork
    -- time is confirmed on-chain.
    --
    -- Note: this is not a practical way to tell the current era.
    , maximumCollateralInputCount
        :: Word16
    -- ^ Limit on the maximum number of collateral inputs present in a
    -- transaction.
    , minimumCollateralPercentage
        :: Natural
    -- ^ Specifies the minimum required amount of collateral as a
    -- percentage of the total transaction fee.
    , executionUnitPrices
        :: Maybe ExecutionUnitPrices
    -- ^ The prices for 'ExecutionUnits' as a fraction of a 'Lovelace' and
    -- used to determine the fee for the use of a script within a
    -- transaction, based on the 'ExecutionUnits' needed by the use of
    -- the script.
    }
    deriving (Eq, Generic, Show)

instance NFData ProtocolParameters where
    rnf ProtocolParameters{..} =
        mconcat
            [ rnf decentralizationLevel
            , rnf txParameters
            , rnf desiredNumberOfStakePools
            , rnf stakeKeyDeposit
            , rnf eras
            , rnf maximumCollateralInputCount
            , rnf minimumCollateralPercentage
            , rnf executionUnitPrices
            -- currentLedgerProtocolParameters is omitted
            ]

instance Buildable ProtocolParameters where
    build pp =
        blockListF'
            ""
            id
            [ "Decentralization level: "
                <> build (pp ^. #decentralizationLevel)
            , "Transaction parameters: "
                <> build (pp ^. #txParameters)
            , "Desired number of pools: "
                <> build (pp ^. #desiredNumberOfStakePools)
            , "Eras:\n"
                <> indentF 2 (build (pp ^. #eras))
            , "Execution unit prices: "
                <> maybe "not specified" build (pp ^. #executionUnitPrices)
            ]
