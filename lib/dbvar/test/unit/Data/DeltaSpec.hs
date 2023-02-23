{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Data.DeltaSpec
    ( spec
    ) where

import Prelude

import Data.DBVar
    ( Store (..), newCachedStore, newStore )
import Data.Delta
import Fmt
import Test.DBVar
    ( prop_StoreUpdates )
import Test.Hspec
    ( Spec, describe, it, parallel )
import Test.QuickCheck
    ( elements )
import Test.QuickCheck.Gen
    ( Gen )
import Test.QuickCheck.Monadic
    ( monadicIO, run )

spec :: Spec
spec = do
    parallel $ describe "Data.Delta" $ do
        it "Dummy test, to be expanded"
            True
    describe "CachedStore" $ do
        it "respects store laws" $ monadicIO $ do
            cachedStore <- run $ do
                testStore <- newStore @TestStoreDelta
                writeS testStore 0
                newCachedStore testStore
            prop_StoreUpdates run
                cachedStore
                (pure 0)
                genTestStoreDeltas

genTestStoreDeltas :: TestStoreBase -> Gen TestStoreDelta
genTestStoreDeltas _ = elements [AddOne, AddTwo, RemoveOne]

newtype TestStoreBase = TestStoreBase Int
    deriving (Show, Eq, Num )

data TestStoreDelta
    = AddOne
    | AddTwo
    | RemoveOne

    deriving (Show, Eq)
instance Buildable TestStoreDelta where
    build = build . show

instance Delta TestStoreDelta where
    type Base TestStoreDelta = TestStoreBase
    apply AddOne = ( + 1)
    apply AddTwo = ( + 2)
    apply RemoveOne = ( subtract 1)
