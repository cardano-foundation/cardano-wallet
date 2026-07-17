-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Tests for the 'MVar' implementation of the pool 'DBLayer' interface.
module Cardano.Pool.DB.MVarSpec
    ( spec
    ) where

import Cardano.Pool.DB.Properties
    ( properties
    )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyTimeInterpreter
    )
import Test.Hspec
    ( Spec
    , describe
    )
import Prelude

import qualified Cardano.Pool.DB.MVar as MVar

spec :: Spec
spec =
    describe "MVar"
        $ properties (\action -> MVar.newDBLayer ti >>= action)
  where
    ti = dummyTimeInterpreter
