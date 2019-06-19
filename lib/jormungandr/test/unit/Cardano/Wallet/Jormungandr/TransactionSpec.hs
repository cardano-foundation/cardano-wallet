{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Jormungandr.TransactionSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Environment
    ( KnownNetwork (..), Network (..) )

spec :: Spec
spec = do
    describe "estimateSize" $ do
        it "Estimated size is zero"
            (withMaxSuccess 2500 $ property $ propSizeEstimation $ Proxy @'Mainnet)
        it "Estimated size is zero"
            (withMaxSuccess 2500 $ property $ propSizeEstimation $ Proxy @'Testnet)

{-------------------------------------------------------------------------------
                                Size Estimation
-------------------------------------------------------------------------------}

propSizeEstimation
    :: forall n. (KnownNetwork n)
    => Proxy n
    -> (ShowFmt CoinSelection)
    -> Property
propSizeEstimation _ (ShowFmt sel) =
    let
        calcSize = estimateSize (newTransactionLayer @n) sel
    in calcSize `shouldBe` Quantity 0
