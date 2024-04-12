{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Cardano.Wallet.Network.RestorationMode
    ( RestorationMode (..)
    , RestorationPoint (..)
    , getRestorationPoint
    ) where

import Prelude

import Cardano.Wallet.Network
    ( ErrFetchBlock
    , NetworkLayer (..)
    , currentNodeTip
    )
import Cardano.Wallet.Primitive.Ledger.Read.Block.Header
    ( getBlockHeader
    )
import Cardano.Wallet.Primitive.Types.Block
    ( BlockHeader (..)
    , chainPointFromBlockHeader'
    )
import Cardano.Wallet.Primitive.Types.GenesisParameters
    ( GenesisParameters (..)
    )
import GHC.Generics
    ( Generic
    )

import qualified Cardano.Wallet.Read as Read

data RestorationMode
    = RestoreFromGenesis
    | RestoreFromTip
    | RestoreFromBlock Read.SlotNo Read.RawHeaderHash
    deriving (Eq, Show, Generic)

data RestorationPoint
    = RestorationPointAtGenesis
    | RestorationPoint BlockHeader
    deriving (Eq, Show, Generic)

-- | Retrieve the chain point to start the restoration from.
getRestorationPoint
    :: GenesisParameters
    -> RestorationMode
    -> NetworkLayer IO Read.ConsensusBlock
    -> IO (Either ErrFetchBlock RestorationPoint)
getRestorationPoint genesisParams mode netLayer = do
    case mode of
        RestoreFromGenesis -> pure $ Right RestorationPointAtGenesis
        RestoreFromTip -> do
            bh <- currentNodeTip netLayer
            case chainPointFromBlockHeader' bh of
                Read.GenesisPoint -> pure $ Right RestorationPointAtGenesis
                Read.BlockPoint slot blockhash -> do
                    getRestorationPoint
                        genesisParams
                        (RestoreFromBlock slot blockhash)
                        netLayer
        RestoreFromBlock slot blockhash -> do
            eblock <- fetchNextBlock netLayer (Read.BlockPoint slot blockhash)
            pure $ case eblock of
                Left e -> Left e
                Right block' ->
                    Right
                        $ RestorationPoint
                        $ getBlockHeader genesisHash block'
  where
    genesisHash = getGenesisBlockHash genesisParams
