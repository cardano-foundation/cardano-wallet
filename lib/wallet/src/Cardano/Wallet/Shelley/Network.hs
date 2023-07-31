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

import qualified Cardano.Wallet.Shelley.Network.Node as Node

import Cardano.BM.Tracing
    ( HasPrivacyAnnotation, HasSeverityAnnotation (..), Tracer )
import Cardano.Wallet.Network
    ( NetworkLayer )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkId )
import Cardano.Wallet.Primitive.Types
    ( NetworkParameters )
import Cardano.Wallet.Shelley.BlockchainSource
    ( BlockchainSource (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( CardanoBlock, StandardCrypto )
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

newtype NetworkLayerLog = NodeNetworkLog Node.Log

instance ToText NetworkLayerLog where
    toText = \case
      NodeNetworkLog l -> toText l

instance HasPrivacyAnnotation NetworkLayerLog

instance HasSeverityAnnotation NetworkLayerLog where
    getSeverityAnnotation = \case
        NodeNetworkLog l -> getSeverityAnnotation l

withNetworkLayer
    :: HasCallStack
    => Tracer IO NetworkLayerLog
    -> PipeliningStrategy (CardanoBlock StandardCrypto)
    -> BlockchainSource
    -> NetworkId
    -> NetworkParameters
    -> ContT r IO (NetworkLayer IO (CardanoBlock StandardCrypto))
withNetworkLayer tr pipeliningStrategy blockchainSrc _net netParams =
    ContT $ case blockchainSrc of
        NodeSource nodeConn ver tol ->
            let tr' = NodeNetworkLog >$< tr
            in Node.withNetworkLayer
                tr' pipeliningStrategy netParams nodeConn ver tol
