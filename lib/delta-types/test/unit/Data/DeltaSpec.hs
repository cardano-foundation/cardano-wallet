{-# LANGUAGE TypeFamilies #-}

module Data.DeltaSpec
  ( spec
  )
where

import Test.Hspec
  ( Spec
  , describe
  , it
  , parallel
  )
import Prelude

spec :: Spec
spec = do
  parallel $ describe "Data.Delta" $ do
    it
      "Dummy test, to be expanded"
      True
