module SpecHook where

import Test.Hspec

hook :: Spec -> Spec
hook = parallel
