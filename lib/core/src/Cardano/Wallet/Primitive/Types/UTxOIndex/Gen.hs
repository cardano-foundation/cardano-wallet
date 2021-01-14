module Cardano.Wallet.Primitive.Types.UTxOIndex.Gen
    ( genUTxOIndexSmall
    , shrinkUTxOIndexSmall
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn, TxOut )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxInSmallRange
    , genTxOutSmallRange
    , shrinkTxInSmallRange
    , shrinkTxOutSmallRange
    )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( UTxOIndex )
import Control.Monad
    ( replicateM )
import Test.QuickCheck
    ( Gen, choose, frequency, shrinkList )
import Test.QuickCheck.Extra
    ( shrinkInterleaved )

import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex

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
    <$> genTxInSmallRange
    <*> genTxOutSmallRange

shrinkEntrySmallRange :: (TxIn, TxOut) -> [(TxIn, TxOut)]
shrinkEntrySmallRange (i, o) = uncurry (,) <$> shrinkInterleaved
    (i, shrinkTxInSmallRange)
    (o, shrinkTxOutSmallRange)
