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
  ( BlockHeadersConsumed(..)
  , tickingFunction
  , startBlockSyncer
  ) where


import Prelude

import Cardano.NetworkLayer
    ( nextBlocks )
import Cardano.NetworkLayer.HttpBridge
    ( newNetworkLayer )
import Cardano.Wallet.Primitive
    ( Block (..), BlockHeader (..), SlotId (..) )
import Control.Concurrent
    ( threadDelay )
import Control.Monad.Except
    ( runExceptT )
import Data.IORef
    ( newIORef, readIORef, writeIORef )
import qualified Data.List as L
import Data.Text
    ( Text )
import Data.Time.Units
    ( Millisecond, toMicroseconds )
import Fmt
    ( fmt, (+||), (||+) )
import System.Exit
    ( die )

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
          let blocksToProcess = filter
                  (checkIfAlreadyConsumed headersConsumed)
                  (L.nub blocksDownloaded)
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

-- | Start the chain producer process, consuming blocks by printing their slot.
startBlockSyncer :: Text -> Int -> IO ()
startBlockSyncer networkName port = do
    network <- newNetworkLayer networkName port

    startSlotRef <- newIORef (SlotId 0 0)

    let chunkSize = 2160 * 10 * 2 -- two epochs
        interval = 20000 :: Millisecond

        produceBlocks :: IO [Block]
        produceBlocks = do
            start <- readIORef startSlotRef
            res <- runExceptT $ nextBlocks network chunkSize start
            case res of
                Left err -> die $ fmt $ "Chain producer error: "+||err||+""
                Right [] -> pure []
                Right blocks -> do
                    let start' = slotId . header . last $ blocks
                    writeIORef startSlotRef start'
                    pure blocks

        logBlock :: Block -> IO ()
        logBlock block = putStrLn msg
            where
                msg = fmt $ "Received block "+||slotId h||+""
                h = header block

    tickingFunction produceBlocks logBlock interval (BlockHeadersConsumed [])
