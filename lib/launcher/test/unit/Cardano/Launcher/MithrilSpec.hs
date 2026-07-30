{-# LANGUAGE OverloadedStrings #-}

module Cardano.Launcher.MithrilSpec
    ( spec
    ) where

import Cardano.Launcher.Mithril
    ( MithrilExePath (..)
    , downloadMithrilWith
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Prelude

spec :: Spec
spec = describe "downloadMithrilWith PATH lookup" $ do
    it "skips download when finder returns Just path" $ do
        let foundPath = "/fake/mithril-client"
            finder query = do
                query `shouldBe` "mithril-client"
                pure (Just foundPath)
            download _ =
                fail
                    "download should not be called when mithril-client is on PATH"
        result <- downloadMithrilWith finder download "/tmp/unused-workdir"
        mithrilExePath result `shouldBe` foundPath

    it "falls through to download when finder returns Nothing" $ do
        let sentinel = "/downloaded/mithril-client"
            workDir = "/tmp/test-workdir"
            finder _ = pure Nothing
            download dir = do
                dir `shouldBe` workDir
                pure (MithrilExePath sentinel)
        result <- downloadMithrilWith finder download workDir
        mithrilExePath result `shouldBe` sentinel
