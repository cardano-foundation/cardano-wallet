{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- This module contains the ticking function that is responsible for invoking
-- block acquisition functionality and executing it in periodic fashion.
--
-- Known limitations: the ticking function makes sure action is not executed on
-- already consumed block, but does not check and handle block gaps (aka
-- catching up).

module Cardano.Wallet.BlockSyncer
  (
    BlockHeadersConsumed(..)
  , tickingFunction
  ) where


import Prelude

import Cardano.Wallet.Primitive
    ( Block (..), BlockHeader )
import Control.Concurrent
    ( threadDelay )
import Data.Time.Units
    ( Millisecond, toMicroseconds )

import qualified Data.List as L


newtype BlockHeadersConsumed =
    BlockHeadersConsumed [BlockHeader]
    deriving (Show, Eq)

storingLimit :: Int
storingLimit = 2160

tickingFunction
    :: IO [Block]
    -- ^ a way to get a new block
    -> (Block -> IO ())
    -- ^ action taken on a new block
    -> Millisecond
    -- ^ tick time
    -> BlockHeadersConsumed
    -> IO ()
tickingFunction getNextBlocks action tickTime = go
    where
      go
          :: BlockHeadersConsumed
          -> IO ()
      go (BlockHeadersConsumed headersConsumed) = do
          blocksDownloaded <- getNextBlocks
          let blocksToProcess =
                  filter (checkIfAlreadyConsumed headersConsumed) (L.nub blocksDownloaded)
          mapM_ action blocksToProcess
          threadDelay $ (fromIntegral . toMicroseconds) tickTime
          go $ BlockHeadersConsumed
              $ take storingLimit
              $ map header blocksToProcess ++ headersConsumed

      checkIfAlreadyConsumed
          :: [BlockHeader]
          -> Block
          -> Bool
      checkIfAlreadyConsumed consumedHeaders (Block theHeader _) =
          theHeader `L.notElem` consumedHeaders
