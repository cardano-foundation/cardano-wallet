-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- File path related test utilities.
module Test.Utils.FilePath
  ( PathElement (..)
  )
where

import System.FilePath.Windows
  ( makeValid
  )
import Test.QuickCheck
  ( Arbitrary (..)
  , elements
  , listOf1
  , scale
  )
import Test.Utils.Platform
  ( isWindows
  )
import Prelude

-- | A file or directory name. The 'Arbitrary' instance will generate values
-- which are valid on Windows and POSIX.
--
-- <https://docs.microsoft.com/en-us/windows/win32/fileio/naming-a-file>
newtype PathElement = PathElement FilePath deriving (Show, Eq)

instance Arbitrary PathElement where
  arbitrary = PathElement . makeValid' <$> limitLen genName
    where
      genName = listOf1 (elements ['a' .. 'z'])
      limitLen = scale (max 250)

makeValid' :: FilePath -> FilePath
makeValid' = if isWindows then makeValid else id
