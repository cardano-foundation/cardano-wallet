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
    ( shrinkTxInSmallRange, shrinkTxOutSmallRange )
import Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( genUTxOLargeN, genUTxOSmall )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( UTxOIndex )
import Test.QuickCheck
    ( Gen, choose, shrinkList )
import Test.QuickCheck.Extra
    ( shrinkInterleaved )

import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex

--------------------------------------------------------------------------------
-- Small indices
--------------------------------------------------------------------------------

genUTxOIndexSmall :: Gen UTxOIndex
genUTxOIndexSmall = UTxOIndex.fromUTxO <$> genUTxOSmall

shrinkUTxOIndexSmall :: UTxOIndex -> [UTxOIndex]
shrinkUTxOIndexSmall
    = take 16
    . fmap UTxOIndex.fromSequence
    . shrinkList shrinkEntrySmallRange
    . UTxOIndex.toList

shrinkEntrySmallRange :: (TxIn, TxOut) -> [(TxIn, TxOut)]
shrinkEntrySmallRange (i, o) = uncurry (,) <$> shrinkInterleaved
    (i, shrinkTxInSmallRange)
    (o, shrinkTxOutSmallRange)

--------------------------------------------------------------------------------
-- Large indices
--------------------------------------------------------------------------------

genUTxOIndexLarge :: Gen UTxOIndex
genUTxOIndexLarge = do
    entryCount <- choose (1024, 4096)
    genUTxOIndexLargeN entryCount

genUTxOIndexLargeN :: Int -> Gen UTxOIndex
genUTxOIndexLargeN entryCount = do
    UTxOIndex.fromUTxO <$> genUTxOLargeN entryCount
