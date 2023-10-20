{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Command-line option passing for cardano-wallet shelley.
--

module Cardano.Wallet.Network.Config
    ( -- * Network
      NetworkConfiguration (..)
    , parseGenesisData
    ) where

import Prelude

import Cardano.Chain.Genesis
    ( GenesisData (..)
    , readGenesisData
    )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkId (..)
    )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , NetworkParameters (..)
    )
import Control.Monad.Trans.Except
    ( ExceptT (..)
    , withExceptT
    )
import Ouroboros.Network.Magic
    ( NetworkMagic (..)
    )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (..)
    )

import qualified Cardano.Wallet.Byron.Compatibility as Byron
import qualified Cardano.Wallet.Primitive.Types.ProtocolMagic as W


-- | Shelley hard fork network configuration has two genesis data.
-- As a special case for mainnet, we hardcode the byron genesis data.
data NetworkConfiguration where
    -- | Mainnet does not have network discrimination.
    MainnetConfig
        :: NetworkConfiguration

    -- | Testnet has network magic.
    TestnetConfig
        :: FilePath
        -- ^ Genesis data in JSON format, for byron era.
        -> NetworkConfiguration
  deriving (Show, Eq)


parseGenesisData
    :: NetworkConfiguration
    -> ExceptT String IO
        ( NetworkId
        , NetworkParameters
        , NodeToClientVersionData
        , Block
        )
parseGenesisData = \case
    MainnetConfig ->
        pure
            ( NMainnet
            , Byron.mainnetNetworkParameters
            , NodeToClientVersionData
                { networkMagic = NetworkMagic $
                    fromIntegral $ W.getProtocolMagic W.mainnetMagic
                , query = False
                }
            , Byron.emptyGenesis
                (genesisParameters Byron.mainnetNetworkParameters)
            )

    TestnetConfig byronGenesisFile -> do
        (genesisData, genesisHash) <-
            withExceptT show $ readGenesisData byronGenesisFile

        let (np, outs) = Byron.fromGenesisData (genesisData, genesisHash)
            protoMagic = W.getProtocolMagic
                $ Byron.fromProtocolMagicId
                $ gdProtocolMagicId genesisData
        pure
            ( NTestnet $ fromIntegral protoMagic
            , np
            , NodeToClientVersionData
                { networkMagic = NetworkMagic $ fromIntegral protoMagic
                , query = False
                }
            , Byron.genesisBlockFromTxOuts (genesisParameters np) outs
            )
