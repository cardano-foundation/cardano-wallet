{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
module Data.StoreSpec
  ( spec
  )
where

import Data.Delta
  ( Delta (..)
  )
import Data.Store
  ( Store (..)
  , UpdateStore
  , newCachedStore
  , newStore
  )
import Fmt
  ( Buildable (..)
  )
import Test.Hspec
  ( Spec
  , describe
  , it
  , parallel
  )
import Test.QuickCheck
  ( elements
  , generate
  , (===)
  )
import Test.QuickCheck.Gen
  ( Gen
  , listOf
  )
import Test.QuickCheck.Monadic
  ( monadicIO
  , run
  )
import Test.Store
  ( prop_StoreUpdate
  )
import Prelude

spec :: Spec
spec = do
  parallel $ describe "Data.Delta" $ do
    it
      "Dummy test, to be expanded"
      True
  describe "CachedStore" $ do
    it "respects store laws"
      $ let
          setupStore = do
            testStore <- newTestStore
            resetTestStoreBase testStore
            newCachedStore testStore
        in
          prop_StoreUpdate
            id
            setupStore
            (pure emptyTestStore)
            $ const genTestStoreDeltas

    it "behaves like the cached one" $ monadicIO $ run $ do
      das <- generate $ listOf genTestStoreDeltas

      testStore <- newTestStore

      cachedStore <- newCachedStore testStore

      resetTestStoreBase testStore
      updateStore cachedStore das
      Right cachedFinal <- loadS cachedStore

      resetTestStoreBase testStore
      updateStore testStore das
      Right originalFinal <- loadS testStore

      pure $ cachedFinal === originalFinal

newTestStore :: IO (UpdateStore IO TestStoreDelta)
newTestStore = newStore

updateStore :: Monad m => Store m qa da -> [da] -> m ()
updateStore store = mapM_ (updateS store Nothing)

genTestStoreDeltas :: Gen TestStoreDelta
genTestStoreDeltas = elements [AddOne, AddTwo, RemoveOne]

resetTestStoreBase :: (Base da ~ TestStoreBase) => Store m qa da -> m ()
resetTestStoreBase store = writeS store emptyTestStore

emptyTestStore :: TestStoreBase
emptyTestStore = TestStoreBase []

newtype TestStoreBase = TestStoreBase [Int]
  deriving (Show, Eq)

data TestStoreDelta
  = AddOne
  | AddTwo
  | RemoveOne
  deriving (Show, Eq)

instance Buildable TestStoreDelta where
  build = build . show

instance Delta TestStoreDelta where
  type Base TestStoreDelta = TestStoreBase
  apply AddOne = overTestStoreBase (1 :)
  apply AddTwo = overTestStoreBase (2 :)
  apply RemoveOne = overTestStoreBase (drop 1)

overTestStoreBase :: ([Int] -> [Int]) -> TestStoreBase -> TestStoreBase
overTestStoreBase f (TestStoreBase xs) = TestStoreBase (f xs)
