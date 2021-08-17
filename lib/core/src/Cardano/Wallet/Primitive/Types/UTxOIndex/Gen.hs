module Cardano.Wallet.Primitive.Types.UTxOIndex.Gen
    ( genUTxOIndexSmall
    , genUTxOIndexLarge
    , genUTxOIndexLargeN
    , shrinkUTxOIndexSmall
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn, TxOut )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxIn, genTxInLargeRange, genTxOut, shrinkTxIn, shrinkTxOut )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( UTxOIndex )
import Control.Monad
    ( replicateM )
import Test.QuickCheck
    ( Gen, choose, frequency, shrinkList )
import Test.QuickCheck.Extra
    ( shrinkInterleaved )

import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex

--------------------------------------------------------------------------------
-- Small indices
--------------------------------------------------------------------------------

genUTxOIndexSmall :: Gen UTxOIndex
genUTxOIndexSmall = do
    entryCount <- frequency
        [ (1, pure 0)
        , (1, pure 1)
        , (32, choose (2, 64))
        ]
    UTxOIndex.fromSequence <$> replicateM entryCount genEntrySmallRange

shrinkUTxOIndexSmall :: UTxOIndex -> [UTxOIndex]
shrinkUTxOIndexSmall
    = take 16
    . fmap UTxOIndex.fromSequence
    . shrinkList shrinkEntrySmallRange
    . UTxOIndex.toList

genEntrySmallRange :: Gen (TxIn, TxOut)
genEntrySmallRange = (,)
    <$> genTxIn
    <*> genTxOut

shrinkEntrySmallRange :: (TxIn, TxOut) -> [(TxIn, TxOut)]
shrinkEntrySmallRange (i, o) = uncurry (,) <$> shrinkInterleaved
    (i, shrinkTxIn)
    (o, shrinkTxOut)

--------------------------------------------------------------------------------
-- Large indices
--------------------------------------------------------------------------------

genUTxOIndexLarge :: Gen UTxOIndex
genUTxOIndexLarge = do
    entryCount <- choose (1024, 4096)
    genUTxOIndexLargeN entryCount

genUTxOIndexLargeN :: Int -> Gen UTxOIndex
genUTxOIndexLargeN entryCount = do
    UTxOIndex.fromSequence <$> replicateM entryCount genEntryLargeRange

genEntryLargeRange :: Gen (TxIn, TxOut)
genEntryLargeRange = (,)
    <$> genTxInLargeRange
    -- Note that we don't need to choose outputs from a large range, as inputs
    -- are already chosen from a large range:
    <*> genTxOut
