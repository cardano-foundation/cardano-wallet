{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.DB.Pure.ImplementationSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Address.Discovery.Sequential
    ( SeqState (..) )
import Cardano.Wallet.DB.Properties
    ( properties )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyTimeInterpreter )
import Cardano.Wallet.Read.NetworkId
    ( NetworkDiscriminant (..) )
import Control.Monad.IO.Class
    ( liftIO )
import Test.Hspec
    ( Spec, before, describe )
import Test.Utils.Platform
    ( pendingOnMacOS )

import qualified Cardano.Wallet.DB.Pure.Layer as PureLayer

spec :: Spec
spec =
    before (pendingOnMacOS "#2472: timeouts in CI mac builds")
    $ describe "PureLayer"
    $ properties $ \wid test -> do
        run <- liftIO $ PureLayer.newDBFresh @_ @(SeqState 'Mainnet ShelleyKey)
            dummyTimeInterpreter wid
        test run
