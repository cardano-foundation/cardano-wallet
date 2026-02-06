{-# LANGUAGE DuplicateRecordFields #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Data types relating to the consensus about the blockchain.
-}
module Cardano.Wallet.Read.Chain
    (
    -- * Points on the blockchain
    -- ** Slot
      WithOrigin (At, Origin)
    , Slot
    , slotFromChainPoint

    -- ** ChainPoint
    , ChainPoint (..)
    , getChainPoint
    , prettyChainPoint
    , chainPointFromChainTip

    -- ** ChainTip
    , ChainTip (..)
    , getChainTip
    , prettyChainTip

    -- * Genesis
    -- ** Genesis Data
    , GenesisData
    , GenesisHash
    , GenesisDataError
    , readGenesisData
    , genesisHashMainnet
    , mockGenesisDataMainnet

    -- ** NetworkId
    , NetworkId (Mainnet, Testnet)
    , NetworkMagic (..)
    , getNetworkId
    ) where

import Cardano.Wallet.Read.Chain.Genesis
    ( GenesisData
    , GenesisDataError
    , GenesisHash
    , NetworkId (..)
    , NetworkMagic (..)
    , genesisHashMainnet
    , getNetworkId
    , mockGenesisDataMainnet
    , readGenesisData
    )
import Cardano.Wallet.Read.Chain.Point
    ( ChainPoint (..)
    , ChainTip (..)
    , Slot
    , WithOrigin (At, Origin)
    , chainPointFromChainTip
    , getChainPoint
    , getChainTip
    , prettyChainPoint
    , prettyChainTip
    , slotFromChainPoint
    )
