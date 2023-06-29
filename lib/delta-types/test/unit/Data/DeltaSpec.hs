{-# LANGUAGE TypeFamilies #-}

module Data.DeltaSpec
    ( spec
    ) where

import Prelude

import Test.Hspec
    ( Spec
    , describe
    , it
    , parallel
    )

spec :: Spec
spec = do
    parallel $ describe "Data.Delta" $ do
        it
            "Dummy test, to be expanded"
            True
