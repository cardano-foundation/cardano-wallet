{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Integration.Framework.DSL.Network where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiNetworkInformation
    )
import Cardano.Wallet.Primitive.Types
    ( EpochNo
    )
import Control.Lens
    ( (^.)
    )
import Test.Hspec.Expectations.Lifted
    ( shouldSatisfy
    )
import Test.Integration.Framework.DSL
    ( eventually
    )
import Test.Integration.Framework.DSL.TestM
    ( TestM
    , pattern Partial
    , request
    )

import qualified Cardano.Wallet.Api.Clients.Testnet.Network as C

tipInfo :: TestM EpochNo
tipInfo = do
    Partial (netInfo :: ApiNetworkInformation) <- request C.networkInformation
    pure $ netInfo ^. #nodeTip . #slotId . #epochNumber . #getApiT

waitSomeEpochs :: Int -> TestM ()
waitSomeEpochs n = do
    now <- tipInfo
    eventually "Waiting some epochs" $ do
        current <- tipInfo
        current `shouldSatisfy` (>= now + fromIntegral n)
