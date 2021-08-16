module Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( genUTxOSmall
    , genUTxOLarge
    , genUTxOLargeN
    , shrinkUTxOSmall
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
    ( Gen, choose, frequency, shrinkList )
import Test.QuickCheck.Extra
    ( shrinkInterleaved )

import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Small UTxO sets
--------------------------------------------------------------------------------

genUTxOSmall :: Gen UTxO
genUTxOSmall = do
    entryCount <- frequency
        [ (1, pure 0)
        , (1, pure 1)
        , (32, choose (2, 64))
        ]
    UTxO . Map.fromList <$> replicateM entryCount genEntrySmallRange

shrinkUTxOSmall :: UTxO -> [UTxO]
shrinkUTxOSmall
    = take 16
    . fmap (UTxO . Map.fromList)
    . shrinkList shrinkEntrySmallRange
    . Map.toList
    . unUTxO

genEntrySmallRange :: Gen (TxIn, TxOut)
genEntrySmallRange = (,)
    <$> genTxIn
    <*> genTxOut

shrinkEntrySmallRange :: (TxIn, TxOut) -> [(TxIn, TxOut)]
shrinkEntrySmallRange (i, o) = uncurry (,) <$> shrinkInterleaved
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
