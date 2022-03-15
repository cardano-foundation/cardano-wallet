{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Network Layer implementation that uses Blockfrost API
--
module Cardano.Wallet.Shelley.Network.Blockfrost
    ( withNetworkLayer
    , Log
    ) where

import Prelude

import qualified Blockfrost.Client as BF
import qualified Data.Text as T

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Tracer
    ( Tracer )
import Cardano.BM.Tracing
    ( HasSeverityAnnotation (getSeverityAnnotation), traceWith )
import Cardano.Wallet.Logging
    ( BracketLog, bracketTracer )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..), SlotNo (SlotNo) )
import Control.Concurrent
    ( threadDelay )
import Control.Monad
    ( forever )
import Control.Monad.Error.Class
    ( throwError )
import Control.Monad.Trans.Except
    ( ExceptT (ExceptT), runExceptT )
import Data.Bifunctor
    ( first )
import Data.Functor.Contravariant
    ( (>$<) )
import Data.Text.Class
    ( ToText (..) )
import Fmt
    ( pretty )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock, StandardCrypto )
import UnliftIO.Async
    ( async, link )

data BlockfrostError
    = ClientError BF.BlockfrostError
    | NoSlotError BF.Block
    deriving (Show)

data Log
    = MsgBlockfrostClientError BlockfrostError
    | MsgWatcherUpdate BlockHeader BracketLog

instance ToText Log where
    toText = \case
        MsgBlockfrostClientError e ->
            "Blockfrost client error: " <> T.pack (show e)
        MsgWatcherUpdate blockHeader bracketLog ->
            "Update watcher with tip: " <> pretty blockHeader <>
            ". Callback " <> toText bracketLog <> ". "

instance HasSeverityAnnotation Log where
    getSeverityAnnotation = \case
      MsgBlockfrostClientError _ -> Warning
      MsgWatcherUpdate _ _ -> Info

withNetworkLayer
    :: Tracer IO Log
    -> BF.Project
    -> (NetworkLayer IO (CardanoBlock StandardCrypto) -> IO a)
    -> IO a
withNetworkLayer tr project k = k NetworkLayer
    { chainSync = \_tr _chainFollower -> pure ()
    , lightSync = Nothing
    , currentNodeTip = undefined
    , currentNodeEra = undefined
    , currentProtocolParameters = undefined
    , currentNodeProtocolParameters = undefined
    , currentSlottingParameters = undefined
    , watchNodeTip = watchNodeTip
    , postTx = undefined
    , stakeDistribution = undefined
    , getCachedRewardAccountBalance = undefined
    , fetchRewardAccountBalances = undefined
    , timeInterpreter = undefined
    , syncProgress = undefined
    }

  where

    watchNodeTip :: (BlockHeader -> IO ()) -> IO ()
    watchNodeTip callback = link =<< async (pollNodeTip callback)

    pollNodeTip :: (BlockHeader -> IO ()) -> IO ()
    pollNodeTip callback = forever $ do
        runExceptT fetchLatestBlockHeader >>= \case
            Left err -> traceWith tr (MsgBlockfrostClientError err)
            Right header ->
                bracketTracer (MsgWatcherUpdate header >$< tr) $ callback header
        threadDelay 2_000_000

    runBlockfrost :: BF.BlockfrostClientT IO a -> ExceptT BlockfrostError IO a
    runBlockfrost =
        ExceptT . (first ClientError <$>) . BF.runBlockfrostClientT project

    fetchLatestBlockHeader :: ExceptT BlockfrostError IO BlockHeader
    fetchLatestBlockHeader = do
        block@BF.Block{..} <- runBlockfrost BF.getLatestBlock
        slotNo <- case _blockSlot of
            Just (BF.Slot s) -> pure $ SlotNo $ fromIntegral s
            Nothing -> throwError $ NoSlotError block
        pure BlockHeader
            { slotNo
            , blockHeight = undefined -- Quantity "block" Word32
            , headerHash = undefined -- !(Hash "BlockHeader")
            , parentHeaderHash = undefined -- !(Maybe (Hash "BlockHeader"))
            }
