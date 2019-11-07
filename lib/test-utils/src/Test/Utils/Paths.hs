{-# LANGUAGE TemplateHaskell #-}

module Test.Utils.Paths
    ( testDirectory
    ) where

import Data.FileEmbed
    ( makeRelativeToProject )
import Language.Haskell.TH.Syntax
    ( liftData )

import Prelude

testDirectory :: FilePath
testDirectory = $(makeRelativeToProject "test" >>= liftData)
