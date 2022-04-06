{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Shelley.Network
    ( -- * Top-Level Interface
      withNetworkLayer

      -- * Logging
    , NetworkLayerLog (..)
    ) where

import Prelude

import qualified Cardano.Api as Cardano
import qualified Cardano.Wallet.Shelley.Network.Blockfrost as Blockfrost
import qualified Cardano.Wallet.Shelley.Network.Node as Node

import Cardano.BM.Tracing
    ( HasPrivacyAnnotation, HasSeverityAnnotation (..), Tracer )
import Cardano.Wallet.Network
    ( NetworkLayer )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance )
import Cardano.Wallet.Primitive.Types
    ( NetworkParameters )
import Cardano.Wallet.Shelley.BlockchainSource
    ( BlockchainSource (..) )
import Control.Monad.Trans.Cont
    ( ContT (ContT) )
import Data.Functor.Contravariant
    ( (>$<) )
import Data.Text.Class
    ( ToText (toText) )
import GHC.Stack
    ( HasCallStack )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock, StandardCrypto )

data NetworkLayerLog
    = NodeNetworkLog Node.Log
    | BlockfrostNetworkLog Blockfrost.Log

instance ToText NetworkLayerLog where
    toText = \case
      NodeNetworkLog l -> toText l
      BlockfrostNetworkLog l -> toText l

instance HasPrivacyAnnotation NetworkLayerLog

instance HasSeverityAnnotation NetworkLayerLog where
    getSeverityAnnotation = \case
        NodeNetworkLog l -> getSeverityAnnotation l
        BlockfrostNetworkLog l -> getSeverityAnnotation l

withNetworkLayer
    :: HasCallStack
    => Tracer IO NetworkLayerLog
    -> BlockchainSource
    -> Cardano.NetworkId
    -> NetworkParameters
    -> SyncTolerance
    -> ContT r IO (NetworkLayer IO (CardanoBlock StandardCrypto))
withNetworkLayer tr blockchainSrc net netParams tol =
    ContT $ case blockchainSrc of
        NodeSource nodeConn ver ->
            let tr' = NodeNetworkLog >$< tr
            in Node.withNetworkLayer tr' net netParams nodeConn ver tol
        BlockfrostSource project ->
            let tr' = BlockfrostNetworkLog >$< tr
            in Blockfrost.withNetworkLayer tr' net netParams project

