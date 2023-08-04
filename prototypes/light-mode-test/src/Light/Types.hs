module Light.Types where

import Blockfrost.Client
import Data.Foldable
  ( maximumBy
  )
import Data.Ord
  ( comparing
  )
import Data.Text
  ( Text
  )

type Blocks = [Block]

type Coin = Lovelaces

-- | yes. Stake address, encoded as Bech32.
type RewardAccount = Address

type PoolId = Text

type SyncProgress = Double

instance Ord TransactionUtxos where
  compare = comparing (unTxHash . _transactionUtxosHash)

instance Ord Address where
  compare = comparing (\(Address a) -> a)

{-----------------------------------------------------------------------------
    ChainPoint
------------------------------------------------------------------------------}

-- | Point on the blockchain.
data ChainPoint
  = Origin
  | At !Integer !BlockHash
  deriving (Eq, Show)

-- | Choose the latest point from a series of 'ChainPoint'.
latest :: [ChainPoint] -> ChainPoint
latest = maximumBy (comparing height)
  where
    height Origin = -1
    height (At h _) = h

-- | Get the 'ChainPoint' that the block is associated to.
fromBlock :: Block -> ChainPoint
fromBlock b = case _blockHeight b of
  Nothing -> Origin
  Just height -> At height (_blockHash b)
