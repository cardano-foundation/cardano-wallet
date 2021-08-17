module Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( genUTxO
    , genUTxOLarge
    , genUTxOLargeN
    , shrinkUTxO
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn, TxOut )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxIn, genTxInLargeRange, genTxOut, shrinkTxIn, shrinkTxOut )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Control.Monad
    ( replicateM )
import Test.QuickCheck
    ( Gen, choose, shrinkList, sized )
import Test.QuickCheck.Extra
    ( shrinkInterleaved )

import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- UTxO sets generated according to the size parameter
--------------------------------------------------------------------------------

genUTxO :: Gen UTxO
genUTxO = sized $ \size -> do
    entryCount <- choose (0, size)
    UTxO . Map.fromList <$> replicateM entryCount genEntry

shrinkUTxO :: UTxO -> [UTxO]
shrinkUTxO
    = take 16
    . fmap (UTxO . Map.fromList)
    . shrinkList shrinkEntry
    . Map.toList
    . unUTxO

genEntry :: Gen (TxIn, TxOut)
genEntry = (,) <$> genTxIn <*> genTxOut

shrinkEntry :: (TxIn, TxOut) -> [(TxIn, TxOut)]
shrinkEntry (i, o) = uncurry (,) <$> shrinkInterleaved
    (i, shrinkTxIn)
    (o, shrinkTxOut)

--------------------------------------------------------------------------------
-- Large UTxO sets
--------------------------------------------------------------------------------

genUTxOLarge :: Gen UTxO
genUTxOLarge = do
    entryCount <- choose (1024, 4096)
    genUTxOLargeN entryCount

genUTxOLargeN :: Int -> Gen UTxO
genUTxOLargeN entryCount = do
    UTxO . Map.fromList <$> replicateM entryCount genEntryLargeRange

genEntryLargeRange :: Gen (TxIn, TxOut)
genEntryLargeRange = (,)
    <$> genTxInLargeRange
    -- Note that we don't need to choose outputs from a large range, as inputs
    -- are already chosen from a large range:
    <*> genTxOut
