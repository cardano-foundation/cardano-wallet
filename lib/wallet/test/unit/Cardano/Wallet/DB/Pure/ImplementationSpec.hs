{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DB.Pure.ImplementationSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.DB.Properties
    ( properties )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyTimeInterpreter )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Read.NetworkId
    ( NetworkDiscriminant (..) )
import Control.DeepSeq
    ( NFData )
import Control.Monad.IO.Class
    ( liftIO )
import Test.Hspec
    ( Spec, before, describe )
import Test.QuickCheck
    ( Arbitrary (..) )
import Test.Utils.Platform
    ( pendingOnMacOS )

import qualified Cardano.Wallet.DB.Pure.Layer as PureLayer

spec :: Spec
spec =
    before (pendingOnMacOS "#2472: timeouts in CI mac builds")
    $ describe "PureLayer"
    $ properties $ \wid test -> do
        run <- liftIO $ PureLayer.newDBLayer @_ @(SeqState 'Mainnet ShelleyKey)
            dummyTimeInterpreter wid
        test run

newtype DummyStatePureLayer = DummyStatePureLayer Int
    deriving (Show, Eq)

instance Arbitrary DummyStatePureLayer where
    shrink _ = []
    arbitrary = DummyStatePureLayer <$> arbitrary

deriving instance NFData DummyStatePureLayer

instance IsOurs DummyStatePureLayer Address where
    isOurs _ num = (Nothing, num)
