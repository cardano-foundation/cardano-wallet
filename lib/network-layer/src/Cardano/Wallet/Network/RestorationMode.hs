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
import Cardano.Wallet.Primitive.Slotting
    ( SlotNo
    )
import Cardano.Wallet.Primitive.Types.Block
    ( BlockHeader (..)
    , ChainPoint (..)
    )
import Cardano.Wallet.Primitive.Types.GenesisParameters
    ( GenesisParameters (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import GHC.Generics
    ( Generic
    )

import qualified Cardano.Wallet.Read as Read

data RestorationMode
    = RestoreFromGenesis
    | RestoreFromTip
    | RestoreFromBlock (Hash "BlockHeader") SlotNo
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
            getRestorationPoint
                genesisParams
                (RestoreFromBlock (headerHash bh) (slotNo bh))
                netLayer
        RestoreFromBlock hash slot -> do
            eblock <- fetchNextBlock netLayer $ ChainPoint slot hash
            pure $ case eblock of
                Left e -> Left e
                Right block' ->
                    Right
                        $ RestorationPoint
                        $ getBlockHeader genesisHash block'
  where
    genesisHash = getGenesisBlockHash genesisParams
