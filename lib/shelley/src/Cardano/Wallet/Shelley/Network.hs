{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.Shelley.Network
    ( -- * Top-Level Interface
      withNetworkLayer

      -- * Logging
    , NetworkLayerLog (..)
    ) where

import Prelude

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
import Cardano.Wallet.Shelley.Compatibility
    ( CardanoBlock, StandardCrypto )
import Cardano.Wallet.Shelley.Network.Discriminant
    ( SomeNetworkDiscriminant, networkDiscriminantToId )
import Control.Monad.Trans.Cont
    ( ContT (ContT) )
import Data.Functor.Contravariant
    ( (>$<) )
import Data.Text.Class
    ( ToText (toText) )
import GHC.Stack
    ( HasCallStack )
import Ouroboros.Network.Client.Wallet
    ( PipeliningStrategy )

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
    -> PipeliningStrategy (CardanoBlock StandardCrypto)
    -> BlockchainSource
    -> SomeNetworkDiscriminant
    -> NetworkParameters
    -> SyncTolerance
    -> ContT r IO (NetworkLayer IO (CardanoBlock StandardCrypto))
withNetworkLayer tr pipeliningStrategy blockchainSrc net netParams tol =
    ContT $ case blockchainSrc of
        NodeSource nodeConn ver ->
            let tr' = NodeNetworkLog >$< tr
                netId = networkDiscriminantToId net
            in Node.withNetworkLayer 
                tr' pipeliningStrategy netId netParams nodeConn ver tol
        BlockfrostSource project ->
            let tr' = BlockfrostNetworkLog >$< tr
            in Blockfrost.withNetworkLayer tr' net netParams project
