{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
module Cardano.Wallet.Shelley.Prototype1 where

import Prelude

import Cardano.Wallet.Shelley.Network
    ( point1, runExperiment )
import Data.Quantity
    ( Quantity (..) )
import Ouroboros.Consensus.Ledger.Byron
    ( ByronBlock )
import Ouroboros.Network.Block
    ( Point )

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.UTxO as CC
import qualified Cardano.Crypto as CC
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.ByteArray as BA
import qualified Data.List.NonEmpty as NE
import qualified Ouroboros.Consensus.Ledger.Byron as O
import qualified Ouroboros.Network.Block as O

main :: IO ()
main = do
    runExperiment
        point1
        1000
        rollForward
        rollBackward

convertTx :: CC.Tx -> W.Tx
convertTx tx = W.Tx
    { W.txId = convertHash $ CC.hash tx
    , W.resolvedInputs = map convertTxIn $ NE.toList $ CC.txInputs tx
    , W.outputs = map convertTxOut $ NE.toList $ CC.txOutputs tx
    }
  where
    convertHash = W.Hash . BA.convert

    convertTxIn (CC.TxInUtxo txH idx) = (W.TxIn (convertHash txH) idx, coin)
       where
          coin = error "1. We can't get the coin of Byron inputs!"

    convertTxOut o = W.TxOut
        (convertAddr $ CC.txOutAddress o)
        (convertCoin $ CC.txOutValue o)
      where
        convertAddr = error "todo use toBinary"
        -- Byron addresses should be no problem. I'm not sure how Shelley
        -- addresses will look.
        convertCoin = W.Coin . CC.unsafeGetLovelace

convertBlockHeader :: ByronBlock -> W.BlockHeader
convertBlockHeader b = W.BlockHeader
    { W.slotId = convertSlot (O.blockSlot b)

    , W.blockHeight = convertBlockNo $ O.blockNo b
    , W.headerHash = convertHash $ O.blockHash b
    , W.parentHeaderHash = convertChainHash $ O.blockPrevHash b
    }
  where
    convertHash = W.Hash . BA.convert . O.unByronHash

    -- The cardano-ledger equivalent is EpochAndSlotCount, i.e epoch
    -- number and the slot counted from the start of the epoch.
    --
    -- But block headers only contain the slot number (SlotNo), counted
    -- from genesis.
    --
    -- Assuming the epoch-length doesn't change, we can easily do the
    -- conversion. The SlotId isn't interesting to the wallet, so long term, we
    -- should concider using SlotNo instead.
    convertSlot = W.fromFlatSlot (W.EpochLength 21600) . O.unSlotNo

    convertChainHash x = case x of
        O.BlockHash h ->
            convertHash h
        O.GenesisHash ->
            error "how do we represent the genesis hash?"
            -- Seems like a minor problem.
    convertBlockNo = Quantity . fromIntegral . O.unBlockNo

-- Goal: see what info we can extract from blocks!
rollForward :: ByronBlock -> IO ()
rollForward b = do
    let slot = show (O.unSlotNo . O.byronBlockSlotNo $ b)
    let hash = show $ O.byronBlockHash b
    putStrLn "\n\n"
    putStrLn $ "=== SlotNo " ++ slot
    putStrLn $ "=== " ++ hash
    putStrLn "\n"
    print $ convertBlockHeader b
    case O.byronBlockRaw b  of
        CC.ABOBBlock cb -> do
            let body = CC.blockBody cb
            let txPay = CC.bodyTxPayload body
            let txAuxs = CC.aUnTxPayload txPay
            let txs = map CC.taTx txAuxs
            print $ length txs
        CC.ABOBBoundary _ ->
            putStrLn "Our friend the Epoch Boundary Block!"

rollBackward :: Point ByronBlock -> IO ()
rollBackward p = do
    putStrLn $ "rollback to: " ++ show p
