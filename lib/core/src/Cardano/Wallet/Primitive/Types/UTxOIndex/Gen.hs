module Cardano.Wallet.Primitive.Types.UTxOIndex.Gen
    ( genUTxOIndex
    , genUTxOIndexLarge
    , genUTxOIndexLargeN
    , shrinkUTxOIndex
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( genUTxO, genUTxOLarge, genUTxOLargeN, shrinkUTxO )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( UTxOIndex )
import Test.QuickCheck
    ( Gen )

import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex

--------------------------------------------------------------------------------
-- Indices generated according to the size parameter
--------------------------------------------------------------------------------

genUTxOIndex :: Gen UTxOIndex
genUTxOIndex = UTxOIndex.fromUTxO <$> genUTxO

shrinkUTxOIndex :: UTxOIndex -> [UTxOIndex]
shrinkUTxOIndex = fmap UTxOIndex.fromUTxO . shrinkUTxO . UTxOIndex.toUTxO

--------------------------------------------------------------------------------
-- Large indices
--------------------------------------------------------------------------------

genUTxOIndexLarge :: Gen UTxOIndex
genUTxOIndexLarge = UTxOIndex.fromUTxO <$> genUTxOLarge

genUTxOIndexLargeN :: Int -> Gen UTxOIndex
genUTxOIndexLargeN n = UTxOIndex.fromUTxO <$> genUTxOLargeN n
