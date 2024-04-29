module SpecHook where

import Test.Hspec
    ( Spec
    , parallel
    )

-- Run all tests in parallel by default.
--
-- See: https://hspec.github.io/parallel-spec-execution.html
--
hook :: Spec -> Spec
hook = parallel
