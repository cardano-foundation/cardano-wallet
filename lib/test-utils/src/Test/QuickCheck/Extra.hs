{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Extra helper functions for QuickCheck
--

module Test.QuickCheck.Extra
    ( reasonablySized
    ) where

import Prelude

import Test.QuickCheck
    ( Gen, scale )

-- | Resize a generator to grow with the size parameter, but remains reasonably
-- sized. That is handy when testing on data-structures that can be arbitrarily
-- large and, when large entities don't really bring any value to the test
-- itself.
--
-- It uses a square root function which makes the size parameter grows
-- quadratically slower than normal. That is,
--
--     +-------------+------------------+
--     | Normal Size | Reasonable Size  |
--     | ----------- + ---------------- +
--     | 0           | 0                |
--     | 1           | 1                |
--     | 10          | 3                |
--     | 100         | 10               |
--     | 1000        | 31               |
--     +-------------+------------------+
--
reasonablySized :: Gen a -> Gen a
reasonablySized = scale (ceiling . sqrt @Double . fromIntegral)
