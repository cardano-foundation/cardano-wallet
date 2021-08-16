module Cardano.Wallet.Primitive.Types.UTxOIndex.Gen
    ( genUTxOIndexSmall
    , genUTxOIndexLarge
    , genUTxOIndexLargeN
    , shrinkUTxOIndexSmall
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( genUTxOLarge, genUTxOLargeN, genUTxOSmall, shrinkUTxOSmall )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( UTxOIndex )
import Test.QuickCheck
    ( Gen )

import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex

--------------------------------------------------------------------------------
-- Small indices
--------------------------------------------------------------------------------

genUTxOIndexSmall :: Gen UTxOIndex
genUTxOIndexSmall = UTxOIndex.fromUTxO <$> genUTxOSmall

shrinkUTxOIndexSmall :: UTxOIndex -> [UTxOIndex]
shrinkUTxOIndexSmall =
    fmap UTxOIndex.fromUTxO . shrinkUTxOSmall  . UTxOIndex.toUTxO

--------------------------------------------------------------------------------
-- Large indices
--------------------------------------------------------------------------------

genUTxOIndexLarge :: Gen UTxOIndex
genUTxOIndexLarge = UTxOIndex.fromUTxO <$> genUTxOLarge

genUTxOIndexLargeN :: Int -> Gen UTxOIndex
genUTxOIndexLargeN n = UTxOIndex.fromUTxO <$> genUTxOLargeN n
